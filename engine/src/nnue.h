#pragma once
#include "bitboard.h"
#include "defs.h"
#include <algorithm>
#include <array>
#include <cstdint>
#include <cstring>
#include <memory>

#if defined(__ARM_NEON) || defined(__aarch64__)
#include <arm_neon.h>
#define STALLION_SIMD_NEON 1
#elif defined(__AVX2__)
#include <immintrin.h>
#define STALLION_SIMD_AVX2 1
#elif defined(__SSSE3__)
#include <tmmintrin.h>
#define STALLION_SIMD_SSSE3 1
#elif defined(__SSE2__)
#include <emmintrin.h>
#define STALLION_SIMD_SSE2 1
#endif

constexpr size_t KING_BUCKETS = 16;
constexpr size_t INPUT_SIZE = NNUE_INPUT_SIZE; // 13316 = 12288 base + 1028 extra
constexpr size_t LAYER1_SIZE = 1024;

constexpr int KingBucketTable[64] = {
    0,  1,  2,  3,  3,  2,  1,  0,
    0,  1,  2,  3,  3,  2,  1,  0,
    4,  5,  6,  7,  7,  6,  5,  4,
    4,  5,  6,  7,  7,  6,  5,  4,
    8,  9, 10, 11, 11, 10,  9,  8,
    8,  9, 10, 11, 11, 10,  9,  8,
   12, 13, 14, 15, 15, 14, 13, 12,
   12, 13, 14, 15, 15, 14, 13, 12
};

constexpr int SCRELU_MIN = 0;
constexpr int SCRELU_MAX = 255;

constexpr int SCALE = 400;

constexpr int QA = 255;
constexpr int QB = 64;
constexpr int QAB = QA * QB;

constexpr size_t OUTPUT_BUCKETS = 8;

struct alignas(64) NNUE_Params {
  std::array<int16_t, INPUT_SIZE * LAYER1_SIZE> feature_v;
  std::array<int16_t, LAYER1_SIZE> feature_bias;
  std::array<std::array<int16_t, LAYER1_SIZE * 2>, OUTPUT_BUCKETS> output_v;
  std::array<int16_t, OUTPUT_BUCKETS> output_bias;
};

inline std::unique_ptr<NNUE_Params> g_nnue_data = nullptr;

inline const NNUE_Params *g_nnue = nullptr;
inline bool nnue_loaded = false;

extern "C" {
extern const unsigned char stallion_nnue[];
extern const unsigned char stallion_nnue_end[];
}

inline std::unique_ptr<NNUE_Params> read_nnue_embedded() {
  // Only the 8-output-bucket format is supported. The bytes come from the
  // embedded net (net_embed.S); no network file is read at runtime.
  constexpr size_t words = INPUT_SIZE * LAYER1_SIZE + LAYER1_SIZE + OUTPUT_BUCKETS * LAYER1_SIZE * 2 + OUTPUT_BUCKETS;
  constexpr size_t payload = words * 2;
  constexpr size_t padded = (payload + 63) / 64 * 64;

  const unsigned char *const data = stallion_nnue;
  const size_t length = static_cast<size_t>(stallion_nnue_end - stallion_nnue);

  if (length != payload && length != padded) return nullptr;

  auto loaded_params = std::make_unique<NNUE_Params>();
  size_t offset = 0;
  auto read_value = [&]() noexcept -> int16_t {
    const uint16_t u = static_cast<uint16_t>(data[offset]) |
                       (static_cast<uint16_t>(data[offset + 1]) << 8);
    offset += 2;
    return static_cast<int16_t>(u);
  };

  for (auto &v : loaded_params->feature_v) v = read_value();
  for (auto &v : loaded_params->feature_bias) v = read_value();

  for (size_t b = 0; b < OUTPUT_BUCKETS; ++b) {
    for (auto &v : loaded_params->output_v[b]) v = read_value();
  }
  for (size_t b = 0; b < OUTPUT_BUCKETS; ++b) {
    loaded_params->output_bias[b] = read_value();
  }
  return loaded_params;
}

inline bool load_embedded_nnue() {
  auto net = read_nnue_embedded();
  if (net) {
    g_nnue_data = std::move(net);
    g_nnue = g_nnue_data.get();
    nnue_loaded = true;
    return true;
  }
  return false;
}

constexpr inline std::pair<size_t, size_t> feature_indices(int piece, int sq, size_t w_bucket = 0, size_t b_bucket = 0) noexcept {
  if (piece < Pieces::WPawn || piece > Pieces::BKing || !is_valid_square(sq)) {
    return {0, 0};
  }
  constexpr size_t color_stride = 384;
  constexpr size_t piece_stride = 64;

  const size_t base = static_cast<size_t>((piece >> 1) - 1);
  const size_t color = static_cast<size_t>(piece & 1);

  const size_t whiteIdx = w_bucket * 768 + color * color_stride + base * piece_stride + static_cast<size_t>(sq);
  const size_t blackIdx = b_bucket * 768 + (color ^ 1) * color_stride + base * piece_stride + static_cast<size_t>(sq ^ 56);

  return {whiteIdx, blackIdx};
}

template <size_t HiddenSize = LAYER1_SIZE>
struct alignas(64) Accumulator {
  alignas(64) std::array<int32_t, HiddenSize> white;
  alignas(64) std::array<int32_t, HiddenSize> black;

  inline void init(const int16_t *bias_ptr) noexcept {
    std::copy_n(bias_ptr, HiddenSize, white.begin());
    std::copy_n(bias_ptr, HiddenSize, black.begin());
  }

  Accumulator() = default;
  Accumulator(const Accumulator &other) noexcept {
    std::memcpy(white.data(), other.white.data(), sizeof(white));
    std::memcpy(black.data(), other.black.data(), sizeof(black));
  }
  Accumulator &operator=(const Accumulator &other) noexcept {
    if (this != &other) {
      std::memcpy(white.data(), other.white.data(), sizeof(white));
      std::memcpy(black.data(), other.black.data(), sizeof(black));
    }
    return *this;
  }
};

constexpr inline int32_t screlu(int32_t x) noexcept {
  const int32_t clipped = std::clamp(x, SCRELU_MIN, SCRELU_MAX);
  return clipped * clipped;
}

inline int64_t screlu_flatten(const std::array<int32_t, LAYER1_SIZE> &us,
                             const std::array<int32_t, LAYER1_SIZE> &them,
                             const std::array<int16_t, LAYER1_SIZE * 2> &weights) noexcept {
#if defined(STALLION_SIMD_AVX2)
  const __m256i zero = _mm256_setzero_si256();
  const __m256i max255 = _mm256_set1_epi32(255);
  __m256i acc64 = _mm256_setzero_si256();

  auto process = [&](const int32_t *acc_in, const int16_t *w_in) noexcept {
    for (size_t i = 0; i < LAYER1_SIZE; i += 16) {
      __m256i u0 = _mm256_loadu_si256(reinterpret_cast<const __m256i *>(acc_in + i));
      __m256i u1 = _mm256_loadu_si256(reinterpret_cast<const __m256i *>(acc_in + i + 8));

      u0 = _mm256_min_epi32(_mm256_max_epi32(u0, zero), max255);
      u1 = _mm256_min_epi32(_mm256_max_epi32(u1, zero), max255);

      __m256i c16 = _mm256_packs_epi32(u0, u1);
      c16 = _mm256_permute4x64_epi64(c16, _MM_SHUFFLE(3, 1, 2, 0));

      __m256i c_lo = _mm256_cvtepu16_epi32(_mm256_castsi256_si128(c16));
      __m256i c_hi = _mm256_cvtepu16_epi32(_mm256_extracti128_si256(c16, 1));
      __m256i sq_lo = _mm256_mullo_epi32(c_lo, c_lo);
      __m256i sq_hi = _mm256_mullo_epi32(c_hi, c_hi);

      __m256i w16 = _mm256_loadu_si256(reinterpret_cast<const __m256i *>(w_in + i));
      __m256i w_lo = _mm256_cvtepi16_epi32(_mm256_castsi256_si128(w16));
      __m256i w_hi = _mm256_cvtepi16_epi32(_mm256_extracti128_si256(w16, 1));

      __m256i prod_lo = _mm256_mullo_epi32(sq_lo, w_lo);
      __m256i prod_hi = _mm256_mullo_epi32(sq_hi, w_hi);

      acc64 = _mm256_add_epi64(acc64, _mm256_cvtepi32_epi64(_mm256_castsi256_si128(prod_lo)));
      acc64 = _mm256_add_epi64(acc64, _mm256_cvtepi32_epi64(_mm256_extracti128_si256(prod_lo, 1)));
      acc64 = _mm256_add_epi64(acc64, _mm256_cvtepi32_epi64(_mm256_castsi256_si128(prod_hi)));
      acc64 = _mm256_add_epi64(acc64, _mm256_cvtepi32_epi64(_mm256_extracti128_si256(prod_hi, 1)));
    }
  };

  process(us.data(), weights.data());
  process(them.data(), weights.data() + LAYER1_SIZE);

  __m128i low_128 = _mm256_castsi256_si128(acc64);
  __m128i high_128 = _mm256_extracti128_si256(acc64, 1);
  __m128i sum_128 = _mm_add_epi64(low_128, high_128);
  int64_t total = _mm_extract_epi64(sum_128, 0) + _mm_extract_epi64(sum_128, 1);
  return total / QA;

#elif defined(STALLION_SIMD_NEON)
  const int32x4_t zero32 = vdupq_n_s32(0);
  const int32x4_t max255 = vdupq_n_s32(255);
  int64x2_t acc0 = vdupq_n_s64(0);
  int64x2_t acc1 = vdupq_n_s64(0);

  auto process = [&](const int32_t *acc_in, const int16_t *w_in) noexcept {
    for (size_t i = 0; i < LAYER1_SIZE; i += 8) {
      int32x4_t a0 = vld1q_s32(acc_in + i);
      int32x4_t a1 = vld1q_s32(acc_in + i + 4);

      a0 = vminq_s32(vmaxq_s32(a0, zero32), max255);
      a1 = vminq_s32(vmaxq_s32(a1, zero32), max255);

      int16x4_t c0 = vmovn_s32(a0);
      int16x4_t c1 = vmovn_s32(a1);

      int32x4_t sq0 = vmull_s16(c0, c0);
      int32x4_t sq1 = vmull_s16(c1, c1);

      int16x8_t w = vld1q_s16(w_in + i);
      int32x4_t w0 = vmovl_s16(vget_low_s16(w));
      int32x4_t w1 = vmovl_s16(vget_high_s16(w));

      int32x4_t prod0 = vmulq_s32(sq0, w0);
      int32x4_t prod1 = vmulq_s32(sq1, w1);

      acc0 = vpadalq_s32(acc0, prod0);
      acc1 = vpadalq_s32(acc1, prod1);
    }
  };

  process(us.data(), weights.data());
  process(them.data(), weights.data() + LAYER1_SIZE);

  int64x2_t total2 = vaddq_s64(acc0, acc1);
  int64_t total = vgetq_lane_s64(total2, 0) + vgetq_lane_s64(total2, 1);
  return total / QA;

#else
  int64_t sum = 0;
  #pragma unroll 4
  for (size_t i = 0; i < LAYER1_SIZE; ++i) {
    sum += screlu(us[i]) * static_cast<int64_t>(weights[i]);
    sum += screlu(them[i]) * static_cast<int64_t>(weights[LAYER1_SIZE + i]);
  }
  return sum / QA;
#endif
}

class alignas(64) NNUE_State {
public:
  alignas(64) Accumulator<LAYER1_SIZE> m_accumulator_stack[MaxSearchDepth];
  Accumulator<LAYER1_SIZE> *m_curr = &m_accumulator_stack[0];
  uint8_t m_w_bucket[MaxSearchDepth]{};
  uint8_t m_b_bucket[MaxSearchDepth]{};
  int16_t m_pre_w[MaxSearchDepth][NNUE_EXTRA_SLOTS]{};
  int16_t m_pre_b[MaxSearchDepth][NNUE_EXTRA_SLOTS]{};
  int m_pre_nw[MaxSearchDepth]{};
  int m_pre_nb[MaxSearchDepth]{};
  int m_idx = 0;

  NNUE_State() = default;
  NNUE_State(const NNUE_State &other) noexcept { *this = other; }
  NNUE_State &operator=(const NNUE_State &other) noexcept {
    if (this != &other) {
      m_idx = other.m_idx;
      std::copy_n(other.m_accumulator_stack, m_idx + 1, m_accumulator_stack);
      std::copy_n(other.m_w_bucket, m_idx + 1, m_w_bucket);
      std::copy_n(other.m_b_bucket, m_idx + 1, m_b_bucket);
      std::copy_n(&other.m_pre_w[0][0], (m_idx + 1) * NNUE_EXTRA_SLOTS, &m_pre_w[0][0]);
      std::copy_n(&other.m_pre_b[0][0], (m_idx + 1) * NNUE_EXTRA_SLOTS, &m_pre_b[0][0]);
      std::copy_n(other.m_pre_nw, m_idx + 1, m_pre_nw);
      std::copy_n(other.m_pre_nb, m_idx + 1, m_pre_nb);
      m_curr = &m_accumulator_stack[m_idx];
    }
    return *this;
  }

  inline void store_extra_lists(const BoardState &position, int level) noexcept {
    if (level < 0 || level >= MaxSearchDepth) return;
    int buf[NNUE_EXTRA_SLOTS];
    int nw = collect_extra_features(position.board.data(), position.colors_bb.data(),
                                    position.pieces_bb.data(), false, buf, NNUE_EXTRA_SLOTS);
    m_pre_nw[level] = nw < 0 ? 0 : nw;
    for (int i = 0; i < m_pre_nw[level]; ++i) m_pre_w[level][i] = static_cast<int16_t>(buf[i]);
    int nb = collect_extra_features(position.board.data(), position.colors_bb.data(),
                                    position.pieces_bb.data(), true, buf, NNUE_EXTRA_SLOTS);
    m_pre_nb[level] = nb < 0 ? 0 : nb;
    for (int i = 0; i < m_pre_nb[level]; ++i) m_pre_b[level][i] = static_cast<int16_t>(buf[i]);
  }

  inline void pop() noexcept {
    if (m_idx > 0) {
      --m_idx;
      m_curr = &m_accumulator_stack[m_idx];
    }
  }

  inline void add_extra_view(const BoardState &position, bool flip) noexcept {
    if (!g_nnue || !nnue_loaded) return;
    int extra[NNUE_EXTRA_SLOTS];
    const int n = collect_extra_features(position.board.data(), position.colors_bb.data(),
                                         position.pieces_bb.data(), flip, extra, NNUE_EXTRA_SLOTS);
    if (n < 0) return;
    auto &acc = flip ? m_curr->black : m_curr->white;
    const int16_t *F = g_nnue->feature_v.data();
    for (int k = 0; k < n; ++k) {
      const size_t off = static_cast<size_t>(extra[k]) * LAYER1_SIZE;
      #pragma unroll 4
      for (size_t i = 0; i < LAYER1_SIZE; ++i) {
        acc[i] += F[off + i];
      }
    }
  }

  inline void apply_extra_delta(const int *rem_w, int nrw, const int *add_w, int naw,
                                const int *rem_b, int nrb, const int *add_b, int nab) noexcept {
    if (!g_nnue || !nnue_loaded) return;
    const int16_t *F = g_nnue->feature_v.data();
    auto *W = m_curr->white.data();
    auto *B = m_curr->black.data();
    for (int k = 0; k < nrw; ++k) {
      const int16_t *f = F + static_cast<size_t>(rem_w[k]) * LAYER1_SIZE;
      for (size_t i = 0; i < LAYER1_SIZE; ++i) W[i] -= f[i];
    }
    for (int k = 0; k < naw; ++k) {
      const int16_t *f = F + static_cast<size_t>(add_w[k]) * LAYER1_SIZE;
      for (size_t i = 0; i < LAYER1_SIZE; ++i) W[i] += f[i];
    }
    for (int k = 0; k < nrb; ++k) {
      const int16_t *f = F + static_cast<size_t>(rem_b[k]) * LAYER1_SIZE;
      for (size_t i = 0; i < LAYER1_SIZE; ++i) B[i] -= f[i];
    }
    for (int k = 0; k < nab; ++k) {
      const int16_t *f = F + static_cast<size_t>(add_b[k]) * LAYER1_SIZE;
      for (size_t i = 0; i < LAYER1_SIZE; ++i) B[i] += f[i];
    }
  }

  inline void push_null() noexcept {
    if (m_idx >= MaxSearchDepth - 1 || !g_nnue || !nnue_loaded) return;
    m_accumulator_stack[m_idx + 1] = m_accumulator_stack[m_idx];
    m_w_bucket[m_idx + 1] = m_w_bucket[m_idx];
    m_b_bucket[m_idx + 1] = m_b_bucket[m_idx];
    std::copy_n(m_pre_w[m_idx], NNUE_EXTRA_SLOTS, m_pre_w[m_idx + 1]);
    std::copy_n(m_pre_b[m_idx], NNUE_EXTRA_SLOTS, m_pre_b[m_idx + 1]);
    m_pre_nw[m_idx + 1] = m_pre_nw[m_idx];
    m_pre_nb[m_idx + 1] = m_pre_nb[m_idx];
    ++m_idx;
    m_curr = &m_accumulator_stack[m_idx];
  }

  inline int evaluate(int color, int piece_count = 32) const noexcept {
    if (!g_nnue || !nnue_loaded) return 0;
    const auto &us = (color == Colors::White) ? m_curr->white : m_curr->black;
    const auto &them = (color == Colors::White) ? m_curr->black : m_curr->white;
    const size_t b = static_cast<size_t>(std::clamp((piece_count - 1) / 4, 0, 7));
    const int64_t output = screlu_flatten(us, them, g_nnue->output_v[b]);
    return static_cast<int>(std::clamp<int64_t>(
        (output + g_nnue->output_bias[b]) * SCALE / QAB, -MaxEval, MaxEval));
  }

  inline void reset_nnue(const BoardState &position) noexcept {
    m_idx = 0;
    m_curr = &m_accumulator_stack[0];
    if (!g_nnue || !nnue_loaded) return;

    const uint64_t w_kbb = position.colors_bb[Colors::White] & position.pieces_bb[PieceTypes::King];
    const uint64_t b_kbb = position.colors_bb[Colors::Black] & position.pieces_bb[PieceTypes::King];
    const int wking = w_kbb ? __builtin_ctzll(w_kbb) : 4;
    const int bking = b_kbb ? __builtin_ctzll(b_kbb) : 60;

    const size_t w_b = static_cast<size_t>(KingBucketTable[wking]);
    const size_t b_b = static_cast<size_t>(KingBucketTable[bking ^ 56]);
    m_w_bucket[0] = static_cast<uint8_t>(w_b);
    m_b_bucket[0] = static_cast<uint8_t>(b_b);

    m_curr->init(g_nnue->feature_bias.data());

    uint64_t occ = position.colors_bb[0] | position.colors_bb[1];
    while (occ) {
      const int sq = pop_lsb(occ);
      const int piece = position.board[sq];
      if (piece >= Pieces::WPawn && piece <= Pieces::BKing) {
        const auto [white_idx, black_idx] = feature_indices(piece, sq, w_b, b_b);
        const size_t white_off = white_idx * LAYER1_SIZE;
        const size_t black_off = black_idx * LAYER1_SIZE;
        #pragma unroll 4
        for (size_t i = 0; i < LAYER1_SIZE; ++i) {
          m_curr->white[i] += g_nnue->feature_v[white_off + i];
          m_curr->black[i] += g_nnue->feature_v[black_off + i];
        }
      }
    }
    add_extra_view(position, false);
    add_extra_view(position, true);
    store_extra_lists(position, 0);
  }

  inline void refresh_white(const BoardState &position, size_t new_w_bucket) noexcept {
    if (!g_nnue || !nnue_loaded) return;
    std::copy_n(g_nnue->feature_bias.data(), LAYER1_SIZE, m_curr->white.begin());
    uint64_t occ = position.colors_bb[0] | position.colors_bb[1];
    constexpr size_t color_stride = 384;
    constexpr size_t piece_stride = 64;
    while (occ) {
      const int sq = pop_lsb(occ);
      const int piece = position.board[sq];
      if (piece >= Pieces::WPawn && piece <= Pieces::BKing) {
        const size_t base = static_cast<size_t>((piece >> 1) - 1);
        const size_t color = static_cast<size_t>(piece & 1);
        const size_t white_idx = new_w_bucket * 768 + color * color_stride + base * piece_stride + static_cast<size_t>(sq);
        const size_t off = white_idx * LAYER1_SIZE;
        #pragma unroll 4
        for (size_t i = 0; i < LAYER1_SIZE; ++i) {
          m_curr->white[i] += g_nnue->feature_v[off + i];
        }
      }
    }
    add_extra_view(position, false);
  }

  inline void refresh_black(const BoardState &position, size_t new_b_bucket) noexcept {
    if (!g_nnue || !nnue_loaded) return;
    std::copy_n(g_nnue->feature_bias.data(), LAYER1_SIZE, m_curr->black.begin());
    uint64_t occ = position.colors_bb[0] | position.colors_bb[1];
    constexpr size_t color_stride = 384;
    constexpr size_t piece_stride = 64;
    while (occ) {
      const int sq = pop_lsb(occ);
      const int piece = position.board[sq];
      if (piece >= Pieces::WPawn && piece <= Pieces::BKing) {
        const size_t base = static_cast<size_t>((piece >> 1) - 1);
        const size_t color = static_cast<size_t>(piece & 1);
        const size_t black_idx = new_b_bucket * 768 + (color ^ 1) * color_stride + base * piece_stride + static_cast<size_t>(sq ^ 56);
        const size_t off = black_idx * LAYER1_SIZE;
        #pragma unroll 4
        for (size_t i = 0; i < LAYER1_SIZE; ++i) {
          m_curr->black[i] += g_nnue->feature_v[off + i];
        }
      }
    }
    add_extra_view(position, true);
  }

  inline void add_sub(int from_piece, int from, int to_piece, int to) noexcept {
    if (m_idx >= MaxSearchDepth - 1 || !g_nnue || !nnue_loaded) return;
    const size_t wb = m_w_bucket[m_idx];
    const size_t bb = m_b_bucket[m_idx];
    m_w_bucket[m_idx + 1] = static_cast<uint8_t>(wb);
    m_b_bucket[m_idx + 1] = static_cast<uint8_t>(bb);

    const auto [wf, bf] = feature_indices(from_piece, from, wb, bb);
    const auto [wt, bt] = feature_indices(to_piece, to, wb, bb);

    const auto &curr = m_accumulator_stack[m_idx];
    auto &next = m_accumulator_stack[m_idx + 1];

    const size_t off_wt = wt * LAYER1_SIZE, off_wf = wf * LAYER1_SIZE;
    const size_t off_bt = bt * LAYER1_SIZE, off_bf = bf * LAYER1_SIZE;

    #pragma unroll 4
    for (size_t i = 0; i < LAYER1_SIZE; ++i) {
      next.white[i] = curr.white[i] + g_nnue->feature_v[off_wt + i] - g_nnue->feature_v[off_wf + i];
      next.black[i] = curr.black[i] + g_nnue->feature_v[off_bt + i] - g_nnue->feature_v[off_bf + i];
    }
    ++m_idx;
    m_curr = &m_accumulator_stack[m_idx];
  }

  inline void add_sub_sub(int from_piece, int from, int to_piece, int to, int captured, int captured_sq) noexcept {
    if (m_idx >= MaxSearchDepth - 1 || !g_nnue || !nnue_loaded) return;
    const size_t wb = m_w_bucket[m_idx];
    const size_t bb = m_b_bucket[m_idx];
    m_w_bucket[m_idx + 1] = static_cast<uint8_t>(wb);
    m_b_bucket[m_idx + 1] = static_cast<uint8_t>(bb);

    const auto [wf, bf] = feature_indices(from_piece, from, wb, bb);
    const auto [wt, bt] = feature_indices(to_piece, to, wb, bb);
    const auto [wc, bc] = feature_indices(captured, captured_sq, wb, bb);

    const auto &curr = m_accumulator_stack[m_idx];
    auto &next = m_accumulator_stack[m_idx + 1];

    const size_t off_wt = wt * LAYER1_SIZE, off_wf = wf * LAYER1_SIZE, off_wc = wc * LAYER1_SIZE;
    const size_t off_bt = bt * LAYER1_SIZE, off_bf = bf * LAYER1_SIZE, off_bc = bc * LAYER1_SIZE;

    #pragma unroll 4
    for (size_t i = 0; i < LAYER1_SIZE; ++i) {
      next.white[i] = curr.white[i] + g_nnue->feature_v[off_wt + i] - g_nnue->feature_v[off_wf + i] - g_nnue->feature_v[off_wc + i];
      next.black[i] = curr.black[i] + g_nnue->feature_v[off_bt + i] - g_nnue->feature_v[off_bf + i] - g_nnue->feature_v[off_bc + i];
    }
    ++m_idx;
    m_curr = &m_accumulator_stack[m_idx];
  }

  inline void add_add_sub_sub(int p1, int from1, int to1, int p2, int from2, int to2) noexcept {
    if (m_idx >= MaxSearchDepth - 1 || !g_nnue || !nnue_loaded) return;
    const size_t wb = m_w_bucket[m_idx];
    const size_t bb = m_b_bucket[m_idx];
    m_w_bucket[m_idx + 1] = static_cast<uint8_t>(wb);
    m_b_bucket[m_idx + 1] = static_cast<uint8_t>(bb);

    const auto [w1f, b1f] = feature_indices(p1, from1, wb, bb);
    const auto [w1t, b1t] = feature_indices(p1, to1, wb, bb);
    const auto [w2f, b2f] = feature_indices(p2, from2, wb, bb);
    const auto [w2t, b2t] = feature_indices(p2, to2, wb, bb);

    const auto &curr = m_accumulator_stack[m_idx];
    auto &next = m_accumulator_stack[m_idx + 1];

    const size_t off_w1t = w1t * LAYER1_SIZE, off_w1f = w1f * LAYER1_SIZE;
    const size_t off_w2t = w2t * LAYER1_SIZE, off_w2f = w2f * LAYER1_SIZE;
    const size_t off_b1t = b1t * LAYER1_SIZE, off_b1f = b1f * LAYER1_SIZE;
    const size_t off_b2t = b2t * LAYER1_SIZE, off_b2f = b2f * LAYER1_SIZE;

    #pragma unroll 4
    for (size_t i = 0; i < LAYER1_SIZE; ++i) {
      next.white[i] = curr.white[i] + g_nnue->feature_v[off_w1t + i] - g_nnue->feature_v[off_w1f + i]
                                    + g_nnue->feature_v[off_w2t + i] - g_nnue->feature_v[off_w2f + i];
      next.black[i] = curr.black[i] + g_nnue->feature_v[off_b1t + i] - g_nnue->feature_v[off_b1f + i]
                                    + g_nnue->feature_v[off_b2t + i] - g_nnue->feature_v[off_b2f + i];
    }
    ++m_idx;
    m_curr = &m_accumulator_stack[m_idx];
  }
};

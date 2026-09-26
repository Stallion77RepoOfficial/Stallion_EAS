#pragma once
#include "bitboard.h"
#include "defs.h"
#include <algorithm>
#include <array>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <memory>

#if defined(__AVX512F__) && defined(__AVX512BW__)
#include <immintrin.h>
#define STALLION_SIMD_AVX512 1
#elif defined(__ARM_NEON) || defined(__aarch64__)
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

constexpr size_t INPUT_SIZE = NNUE_INPUT_SIZE;
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

constexpr size_t OUTPUT_BUCKETS = 16;

struct alignas(64) NNUE_Params {
  std::array<int16_t, INPUT_SIZE * LAYER1_SIZE> feature_v;
  std::array<int16_t, LAYER1_SIZE> feature_bias;
  std::array<std::array<int16_t, LAYER1_SIZE * 2>, OUTPUT_BUCKETS> output_v;
  std::array<int16_t, OUTPUT_BUCKETS> output_bias;
};

inline std::unique_ptr<NNUE_Params> g_nnue_data = nullptr;

inline const NNUE_Params *g_nnue = nullptr;

extern "C" {
extern const unsigned char stallion_nnue[];
extern const unsigned char stallion_nnue_end[];
}

inline std::unique_ptr<NNUE_Params> read_nnue_embedded() {
  // Only the 16-output-bucket format is supported. The bytes come from the
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
    return true;
  }
  return false;
}

constexpr inline std::pair<size_t, size_t> feature_indices(int piece, int sq, size_t w_bucket, size_t b_bucket) noexcept {
  if (piece < Pieces::WPawn || piece > Pieces::BKing || !is_valid_square(sq) ||
      w_bucket >= NNUE_KING_BUCKETS || b_bucket >= NNUE_KING_BUCKETS) {
    std::exit(EXIT_FAILURE);
  }
  constexpr size_t color_stride = NNUE_FEATURES_PER_COLOR;
  constexpr size_t piece_stride = NNUE_FEATURES_PER_PIECE;

  const size_t base = static_cast<size_t>((piece >> 1) - 1);
  const size_t color = static_cast<size_t>(piece & 1);

  const size_t whiteIdx = w_bucket * NNUE_FEATURES_PER_KING_BUCKET + color * color_stride + base * piece_stride + static_cast<size_t>(sq);
  const size_t blackIdx = b_bucket * NNUE_FEATURES_PER_KING_BUCKET + (color ^ 1) * color_stride + base * piece_stride + static_cast<size_t>(sq ^ 56);

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
#if defined(STALLION_SIMD_AVX512)
  const __m512i zero = _mm512_setzero_si512();
  const __m512i max255 = _mm512_set1_epi32(255);
  __m512i acc64 = _mm512_setzero_si512();

  auto process = [&](const int32_t *acc_in, const int16_t *w_in) noexcept {
    for (size_t i = 0; i < LAYER1_SIZE; i += 16) {
      __m512i a = _mm512_loadu_si512(reinterpret_cast<const void *>(acc_in + i));
      a = _mm512_min_epi32(_mm512_max_epi32(a, zero), max255);
      __m512i sq = _mm512_mullo_epi32(a, a);

      __m256i w16 = _mm256_loadu_si256(reinterpret_cast<const __m256i *>(w_in + i));
      __m512i w = _mm512_cvtepi16_epi32(w16);

      __m512i prod = _mm512_mullo_epi32(sq, w);
      acc64 = _mm512_add_epi64(acc64, _mm512_cvtepi32_epi64(_mm512_castsi512_si256(prod)));
      acc64 = _mm512_add_epi64(acc64, _mm512_cvtepi32_epi64(_mm512_extracti64x4_epi64(prod, 1)));
    }
  };

  process(us.data(), weights.data());
  process(them.data(), weights.data() + LAYER1_SIZE);

  return _mm512_reduce_add_epi64(acc64) / QA;

#elif defined(STALLION_SIMD_AVX2)
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

#elif defined(STALLION_SIMD_SSSE3) || defined(STALLION_SIMD_SSE2)
  const __m128i zero = _mm_setzero_si128();
  const __m128i max255 = _mm_set1_epi32(255);
  __m128i acc_lo = _mm_setzero_si128();
  __m128i acc_hi = _mm_setzero_si128();

  auto process = [&](const int32_t *acc_in, const int16_t *w_in) noexcept {
    for (size_t i = 0; i < LAYER1_SIZE; i += 4) {
      __m128i a = _mm_loadu_si128(reinterpret_cast<const __m128i *>(acc_in + i));
      __m128i neg = _mm_cmpgt_epi32(zero, a);
      a = _mm_andnot_si128(neg, a);
      __m128i over = _mm_cmpgt_epi32(a, max255);
      a = _mm_or_si128(_mm_and_si128(over, max255), _mm_andnot_si128(over, a));

      __m128i a16 = _mm_packs_epi32(a, a);
      __m128i sq = _mm_mullo_epi16(a16, a16);
      __m128i w16 = _mm_loadl_epi64(reinterpret_cast<const __m128i *>(w_in + i));
      __m128i wneg = _mm_cmpgt_epi16(zero, w16);
      __m128i wabs = _mm_sub_epi16(_mm_xor_si128(w16, wneg), wneg);
      __m128i mlo = _mm_mullo_epi16(sq, wabs);
      __m128i mhi = _mm_mulhi_epu16(sq, wabs);
      __m128i prod = _mm_unpacklo_epi16(mlo, mhi);
      __m128i sgn32 = _mm_unpacklo_epi16(wneg, wneg);
      prod = _mm_sub_epi32(_mm_xor_si128(prod, sgn32), sgn32);

      __m128i psign = _mm_cmpgt_epi32(zero, prod);
      acc_lo = _mm_add_epi64(acc_lo, _mm_unpacklo_epi32(prod, psign));
      acc_hi = _mm_add_epi64(acc_hi, _mm_unpackhi_epi32(prod, psign));
    }
  };

  process(us.data(), weights.data());
  process(them.data(), weights.data() + LAYER1_SIZE);

  __m128i sum2 = _mm_add_epi64(acc_lo, acc_hi);
  int64_t total = _mm_cvtsi128_si64(sum2) + _mm_cvtsi128_si64(_mm_srli_si128(sum2, 8));
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

// Pieces of one position, one bitboard per piece code (2..13).
using PieceSet = std::array<uint64_t, 14>;

inline PieceSet piece_set(const BoardState &position) noexcept {
  PieceSet pieces{};
  for (int piece = Pieces::WPawn; piece <= Pieces::BKing; ++piece)
    pieces[piece] = position.colors_bb[piece & 1] & position.pieces_bb[piece >> 1];
  return pieces;
}

// Feature rows to add to and subtract from one accumulator perspective.
struct RowDelta {
  const int16_t *add[NNUE_FEATURE_SLOTS];
  const int16_t *sub[NNUE_FEATURE_SLOTS];
  int n_add = 0, n_sub = 0;
};

// out = in + sum(add rows) - sum(sub rows), in register-sized tiles so each
// accumulator value is read and written once however many rows change.
// out may equal in. Integer sums: the result does not depend on the order.
inline void apply_rows(int32_t *out, const int32_t *in, const RowDelta &delta) noexcept {
  constexpr size_t Tile = 64;
  for (size_t base = 0; base < LAYER1_SIZE; base += Tile) {
    int32_t tile[Tile];
    for (size_t i = 0; i < Tile; ++i) tile[i] = in[base + i];
    for (int k = 0; k < delta.n_add; ++k) {
      const int16_t *row = delta.add[k] + base;
      for (size_t i = 0; i < Tile; ++i) tile[i] += row[i];
    }
    for (int k = 0; k < delta.n_sub; ++k) {
      const int16_t *row = delta.sub[k] + base;
      for (size_t i = 0; i < Tile; ++i) tile[i] -= row[i];
    }
    for (size_t i = 0; i < Tile; ++i) out[base + i] = tile[i];
  }
}

// Accumulators are built lazily: a move only records the new pieces, and
// evaluate() brings the current level up to date from the nearest computed
// ancestor by adding the piece and extra-feature differences. A king-bucket
// change for a perspective goes through the refresh cache instead.
class alignas(64) NNUE_State {
public:
  struct Level {
    PieceSet pieces{};
    std::array<int16_t, NNUE_EXTRA_SLOTS> extra_w{};
    std::array<int16_t, NNUE_EXTRA_SLOTS> extra_b{};
    int n_w = 0, n_b = 0;
    uint8_t w_bucket = 0, b_bucket = 0;
    bool computed = false;
  };

  // Refresh cache ("Finny table"): per perspective and king bucket, the last
  // accumulator built for that bucket with the pieces and extra features it
  // holds. Each entry is always self-consistent, so it is neither copied nor
  // reset.
  struct RefreshEntry {
    alignas(64) std::array<int32_t, LAYER1_SIZE> acc;
    PieceSet pieces{};
    std::array<int16_t, NNUE_EXTRA_SLOTS> extra{};
    int n_extra = 0;
  };

  alignas(64) Accumulator<LAYER1_SIZE> m_accumulator_stack[MaxSearchDepth];
  Level m_levels[MaxSearchDepth];
  int m_idx = 0;
  bool m_initialized = false;
  std::unique_ptr<RefreshEntry[]> m_refresh;

  NNUE_State() = default;
  NNUE_State(const NNUE_State &other) noexcept { *this = other; }
  NNUE_State &operator=(const NNUE_State &other) noexcept {
    if (this != &other) {
      m_idx = other.m_idx;
      m_initialized = other.m_initialized;
      std::copy_n(other.m_levels, m_idx + 1, m_levels);
      for (int level = 0; level <= m_idx; ++level)
        if (m_levels[level].computed)
          m_accumulator_stack[level] = other.m_accumulator_stack[level];
    }
    return *this;
  }

  inline void reset_nnue(const BoardState &position) noexcept {
    if (!g_nnue) std::exit(EXIT_FAILURE);
    m_idx = 0;
    m_initialized = true;
    Level &level = m_levels[0];
    level.pieces = piece_set(position);
    compute_extras(position, level);
    refresh_side(false);
    refresh_side(true);
    level.computed = true;
  }

  inline void push(const BoardState &position) noexcept {
    if (!g_nnue || !m_initialized || m_idx >= MaxSearchDepth - 1) std::exit(EXIT_FAILURE);
    ++m_idx;
    m_levels[m_idx].pieces = piece_set(position);
    m_levels[m_idx].computed = false;
  }

  inline void pop() noexcept {
    if (!g_nnue || !m_initialized || m_idx <= 0) std::exit(EXIT_FAILURE);
    --m_idx;
  }

  inline int evaluate(const BoardState &position) noexcept {
    if (!g_nnue || !m_initialized) std::exit(EXIT_FAILURE);
    if (piece_set(position) != m_levels[m_idx].pieces) std::exit(EXIT_FAILURE);
    update(position);
    const int piece_count = pop_count(position.colors_bb[0] | position.colors_bb[1]);
    if (piece_count < 2 || piece_count > 32) std::exit(EXIT_FAILURE);
    const auto &acc = m_accumulator_stack[m_idx];
    const bool white = position.color == Colors::White;
    const size_t b = static_cast<size_t>(std::clamp((piece_count - 1) / 2, 0, static_cast<int>(OUTPUT_BUCKETS) - 1));
    const int64_t output = screlu_flatten(white ? acc.white : acc.black, white ? acc.black : acc.white,
                                          g_nnue->output_v[b]);
    return static_cast<int>(std::clamp<int64_t>(
        (output + g_nnue->output_bias[b]) * SCALE / QAB, -MaxEval, MaxEval));
  }

private:
  static inline void compute_extras(const BoardState &position, Level &level) noexcept {
    int buf[NNUE_EXTRA_SLOTS];
    const int nw = collect_extra_features(position.board.data(), position.colors_bb.data(),
                                          position.pieces_bb.data(), false, buf, NNUE_EXTRA_SLOTS);
    if (nw < 0) std::exit(EXIT_FAILURE);
    for (int i = 0; i < nw; ++i) level.extra_w[i] = static_cast<int16_t>(buf[i]);
    const int nb = mirror_extra_features(buf, nw, buf, NNUE_EXTRA_SLOTS);
    if (nb < 0) std::exit(EXIT_FAILURE);
    for (int i = 0; i < nb; ++i) level.extra_b[i] = static_cast<int16_t>(buf[i]);
    level.n_w = nw;
    level.n_b = nb;
    const int wking = get_lsb(level.pieces[Pieces::WKing]);
    const int bking = get_lsb(level.pieces[Pieces::BKing]);
    level.w_bucket = static_cast<uint8_t>(KingBucketTable[wking]);
    level.b_bucket = static_cast<uint8_t>(KingBucketTable[bking ^ 56]);
  }

  static inline void piece_rows(const PieceSet &from, const PieceSet &to, size_t bucket, bool black,
                                RowDelta &delta) noexcept {
    const int16_t *F = g_nnue->feature_v.data();
    for (int piece = Pieces::WPawn; piece <= Pieces::BKing; ++piece) {
      uint64_t removed = from[piece] & ~to[piece], added = to[piece] & ~from[piece];
      while (removed) {
        const auto idx = feature_indices(piece, pop_lsb(removed), bucket, bucket);
        delta.sub[delta.n_sub++] = F + (black ? idx.second : idx.first) * LAYER1_SIZE;
      }
      while (added) {
        const auto idx = feature_indices(piece, pop_lsb(added), bucket, bucket);
        delta.add[delta.n_add++] = F + (black ? idx.second : idx.first) * LAYER1_SIZE;
      }
    }
  }

  // Both lists are sorted; emits the rows of the symmetric difference.
  static inline void extra_rows(const int16_t *from, int n_from, const int16_t *to, int n_to,
                                RowDelta &delta) noexcept {
    const int16_t *F = g_nnue->feature_v.data();
    int i = 0, j = 0;
    while (i < n_from || j < n_to) {
      if (j == n_to || (i < n_from && from[i] < to[j])) {
        delta.sub[delta.n_sub++] = F + static_cast<size_t>(from[i++]) * LAYER1_SIZE;
      } else if (i == n_from || to[j] < from[i]) {
        delta.add[delta.n_add++] = F + static_cast<size_t>(to[j++]) * LAYER1_SIZE;
      } else {
        ++i;
        ++j;
      }
    }
  }

  // Builds one perspective of the current level through the refresh cache.
  inline void refresh_side(bool black) noexcept {
    if (!m_refresh) {
      m_refresh = std::make_unique<RefreshEntry[]>(2 * NNUE_KING_BUCKETS);
      for (size_t i = 0; i < 2 * NNUE_KING_BUCKETS; ++i)
        std::copy_n(g_nnue->feature_bias.data(), LAYER1_SIZE, m_refresh[i].acc.begin());
    }
    const Level &level = m_levels[m_idx];
    const size_t bucket = black ? level.b_bucket : level.w_bucket;
    auto &entry = m_refresh[(black ? NNUE_KING_BUCKETS : 0) + bucket];
    const int16_t *extra = black ? level.extra_b.data() : level.extra_w.data();
    const int n_extra = black ? level.n_b : level.n_w;
    RowDelta delta;
    piece_rows(entry.pieces, level.pieces, bucket, black, delta);
    extra_rows(entry.extra.data(), entry.n_extra, extra, n_extra, delta);
    apply_rows(entry.acc.data(), entry.acc.data(), delta);
    entry.pieces = level.pieces;
    std::copy_n(extra, n_extra, entry.extra.begin());
    entry.n_extra = n_extra;
    auto &acc = m_accumulator_stack[m_idx];
    std::copy_n(entry.acc.begin(), LAYER1_SIZE, (black ? acc.black : acc.white).begin());
  }

  inline void update(const BoardState &position) noexcept {
    Level &now = m_levels[m_idx];
    if (now.computed) return;
    int ancestor = m_idx - 1;
    while (!m_levels[ancestor].computed) --ancestor;
    const Level &from = m_levels[ancestor];
    const auto &from_acc = m_accumulator_stack[ancestor];
    auto &acc = m_accumulator_stack[m_idx];
    if (now.pieces == from.pieces) {
      const PieceSet pieces = now.pieces;
      now = from;
      now.pieces = pieces;
      acc = from_acc;
      return;
    }
    compute_extras(position, now);
    for (const bool black : {false, true}) {
      const size_t bucket = black ? now.b_bucket : now.w_bucket;
      if (bucket != (black ? from.b_bucket : from.w_bucket)) {
        refresh_side(black);
        continue;
      }
      RowDelta delta;
      piece_rows(from.pieces, now.pieces, bucket, black, delta);
      if (black) {
        extra_rows(from.extra_b.data(), from.n_b, now.extra_b.data(), now.n_b, delta);
        apply_rows(acc.black.data(), from_acc.black.data(), delta);
      } else {
        extra_rows(from.extra_w.data(), from.n_w, now.extra_w.data(), now.n_w, delta);
        apply_rows(acc.white.data(), from_acc.white.data(), delta);
      }
    }
    now.computed = true;
  }
};

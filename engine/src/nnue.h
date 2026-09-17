#pragma once
#include "defs.h"
#include <algorithm>
#include <array>
#include <cstdint>
#include <cstring>
#include <fstream>
#include <memory>
#include <string>
#include <vector>

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

constexpr size_t INPUT_SIZE = 768;
constexpr size_t LAYER1_SIZE = 1024;

constexpr int SCRELU_MIN = 0;
constexpr int SCRELU_MAX = 255;

constexpr int SCALE = 400;

constexpr int QA = 255;
constexpr int QB = 64;
constexpr int QAB = QA * QB;

struct alignas(64) NNUE_Params {
  std::array<int16_t, INPUT_SIZE * LAYER1_SIZE> feature_v;
  std::array<int16_t, LAYER1_SIZE> feature_bias;
  std::array<int16_t, LAYER1_SIZE * 2> output_v;
  int16_t output_bias;
};

inline std::unique_ptr<NNUE_Params> g_nnue_base = nullptr;

inline const NNUE_Params *g_nnue = nullptr;
inline bool nnue_loaded = false;
inline bool use_nnue = true;

inline std::unique_ptr<NNUE_Params> read_nnue_binary(const std::string &path) {
  constexpr size_t words = INPUT_SIZE * LAYER1_SIZE + LAYER1_SIZE * 3 + 1;
  constexpr size_t payload = words * 2;
  constexpr size_t padded = (payload + 63) / 64 * 64;
  std::ifstream file(path, std::ios::binary | std::ios::ate);
  if (!file) return nullptr;
  const auto length = file.tellg();
  if (length != std::streamoff(payload) && length != std::streamoff(padded)) return nullptr;
  std::vector<uint8_t> data(payload);
  file.seekg(0);
  if (!file.read(reinterpret_cast<char *>(data.data()), payload)) return nullptr;
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
  for (auto &v : loaded_params->output_v) v = read_value();
  loaded_params->output_bias = read_value();
  return loaded_params;
}

inline bool load_nnue_base(const std::string &path = "nets/base.nnue") {
  auto net = read_nnue_binary(path);
  if (net) {
    g_nnue_base = std::move(net);
    g_nnue = g_nnue_base.get();
    nnue_loaded = true;
    return true;
  }
  return false;
}

constexpr inline std::pair<size_t, size_t> feature_indices(int piece, int sq) noexcept {
  if (piece < Pieces::WPawn || piece > Pieces::BKing || !is_valid_square(sq)) {
    return {0, 0};
  }
  constexpr size_t color_stride = 384;
  constexpr size_t piece_stride = 64;

  const size_t base = static_cast<size_t>((piece >> 1) - 1);
  const size_t color = static_cast<size_t>(piece & 1);

  const size_t whiteIdx = color * color_stride + base * piece_stride + static_cast<size_t>(sq);
  const size_t blackIdx = (color ^ 1) * color_stride + base * piece_stride + static_cast<size_t>(sq ^ 56);

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
  int m_idx = 0;

  NNUE_State() = default;
  NNUE_State(const NNUE_State &other) noexcept { *this = other; }
  NNUE_State &operator=(const NNUE_State &other) noexcept {
    if (this != &other) {
      m_idx = other.m_idx;
      std::copy_n(other.m_accumulator_stack, m_idx + 1, m_accumulator_stack);
      m_curr = &m_accumulator_stack[m_idx];
    }
    return *this;
  }

  inline void pop() noexcept {
    if (m_idx > 0) {
      --m_idx;
      m_curr = &m_accumulator_stack[m_idx];
    }
  }

  inline void push_null() noexcept {
    if (m_idx >= MaxSearchDepth - 1 || !g_nnue || !nnue_loaded) return;
    m_accumulator_stack[m_idx + 1] = m_accumulator_stack[m_idx];
    ++m_idx;
    m_curr = &m_accumulator_stack[m_idx];
  }

  inline int evaluate(int color) const noexcept {
    if (!g_nnue || !nnue_loaded) return 0;
    const auto &us = (color == Colors::White) ? m_curr->white : m_curr->black;
    const auto &them = (color == Colors::White) ? m_curr->black : m_curr->white;
    const int64_t output = screlu_flatten(us, them, g_nnue->output_v);
    return static_cast<int>(std::clamp<int64_t>(
        (output + g_nnue->output_bias) * SCALE / QAB, -MaxEval, MaxEval));
  }

  inline void reset_nnue(const BoardState &position) noexcept {
    m_idx = 0;
    m_curr = &m_accumulator_stack[0];
    if (!g_nnue || !nnue_loaded) return;

    m_curr->init(g_nnue->feature_bias.data());

    uint64_t occ = position.colors_bb[0] | position.colors_bb[1];
    while (occ) {
      const int sq = pop_lsb(occ);
      const int piece = position.board[sq];
      if (piece >= Pieces::WPawn && piece <= Pieces::BKing) {
        const auto [white_idx, black_idx] = feature_indices(piece, sq);
        const size_t white_off = white_idx * LAYER1_SIZE;
        const size_t black_off = black_idx * LAYER1_SIZE;
        #pragma unroll 4
        for (size_t i = 0; i < LAYER1_SIZE; ++i) {
          m_curr->white[i] += g_nnue->feature_v[white_off + i];
          m_curr->black[i] += g_nnue->feature_v[black_off + i];
        }
      }
    }
  }

  inline void add_sub(int from_piece, int from, int to_piece, int to) noexcept {
    if (m_idx >= MaxSearchDepth - 1 || !g_nnue || !nnue_loaded) return;
    const auto [wf, bf] = feature_indices(from_piece, from);
    const auto [wt, bt] = feature_indices(to_piece, to);

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
    const auto [wf, bf] = feature_indices(from_piece, from);
    const auto [wt, bt] = feature_indices(to_piece, to);
    const auto [wc, bc] = feature_indices(captured, captured_sq);

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
    const auto [w1f, b1f] = feature_indices(p1, from1);
    const auto [w1t, b1t] = feature_indices(p1, to1);
    const auto [w2f, b2f] = feature_indices(p2, from2);
    const auto [w2t, b2t] = feature_indices(p2, to2);

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

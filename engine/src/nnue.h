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

#if defined(__ARM_NEON) || defined(__aarch64__)
#include <arm_neon.h>
#define STALLION_SIMD_NEON 1
#endif

constexpr size_t INPUT_SIZE = NNUE_INPUT_SIZE;
constexpr size_t LAYER1_SIZE = NNUE_HIDDEN_SIZE;
static_assert(LAYER1_SIZE > 0 && LAYER1_SIZE % 64 == 0);
using NNUE_AccumulatorValue = int16_t;
using NNUE_WideValue = int32_t;

constexpr int16_t KingBucketTable[64] = {
     0,  1,  2,  3,  3,  2,  1,  0,
     4,  5,  6,  7,  7,  6,  5,  4,
     8,  9, 10, 11, 11, 10,  9,  8,
    12, 13, 14, 15, 15, 14, 13, 12,
    16, 17, 18, 19, 19, 18, 17, 16,
    20, 21, 22, 23, 23, 22, 21, 20,
    24, 25, 26, 27, 27, 26, 25, 24,
    28, 29, 30, 31, 31, 30, 29, 28
};

constexpr int16_t SCRELU_MIN = 0;
constexpr int16_t SCRELU_MAX = 255;

constexpr int16_t SCALE = 400;

constexpr int16_t QA = 255;
constexpr int16_t QB = 64;
constexpr int16_t QAB = QA * QB;

constexpr size_t OUTPUT_BUCKETS = NNUE_OUTPUT_BUCKETS;
struct alignas(64) NNUE_Params {
  std::array<int16_t, INPUT_SIZE * LAYER1_SIZE> feature_v;
  std::array<int16_t, LAYER1_SIZE> feature_bias;
  std::array<std::array<int16_t, LAYER1_SIZE * 2>, OUTPUT_BUCKETS> output_v;
  std::array<int16_t, OUTPUT_BUCKETS> output_bias;
};

inline std::unique_ptr<NNUE_Params> g_nnue_data = nullptr;

inline const NNUE_Params *g_nnue = nullptr;
inline bool g_nnue_output_products_fit_i16 = false;
extern "C" {
extern const unsigned char stallion_nnue[];
extern const unsigned char stallion_nnue_end[];
}

inline std::unique_ptr<NNUE_Params> read_nnue_embedded() {
  constexpr size_t words = INPUT_SIZE * LAYER1_SIZE + LAYER1_SIZE + OUTPUT_BUCKETS * LAYER1_SIZE * 2 + OUTPUT_BUCKETS;
  constexpr size_t header = 32;
  constexpr size_t payload = header + words * sizeof(int16_t);
  constexpr size_t padded = (payload + 63) / 64 * 64;
  const unsigned char *const data = stallion_nnue;
  const size_t length = static_cast<size_t>(stallion_nnue_end - stallion_nnue);
  constexpr unsigned char magic[8] = {'S','T','L','N','N','1','6',0};
  if ((length != payload && length != padded) || std::memcmp(data, magic, 8)) return nullptr;
  size_t offset = 8;
  auto dimension = [&]() {
    uint64_t value = 0;
    for (size_t byte = 0; byte < 8; ++byte) value |= uint64_t(data[offset++]) << (8 * byte);
    return value;
  };
  if (dimension() != INPUT_SIZE || dimension() != LAYER1_SIZE || dimension() != OUTPUT_BUCKETS) return nullptr;
  auto value = [&]() {
    uint16_t u = uint16_t(data[offset]) | (uint16_t(data[offset + 1]) << 8);
    offset += 2;
    int16_t v;
    std::memcpy(&v, &u, sizeof(v));
    return v;
  };
  auto net = std::make_unique<NNUE_Params>();
  for (auto &v : net->feature_v) v = value();
  for (auto &v : net->feature_bias) v = value();
  for (auto &weights : net->output_v) for (auto &v : weights) v = value();
  for (auto &v : net->output_bias) v = value();
  return net;
}

// Prove representation safety for these coefficients, including all 64 squares
// and add-before-subtract incremental updates. No coefficient is clipped.
inline bool nnue_arithmetic_fits(const NNUE_Params &net) noexcept {
  for (size_t bucket = 0; bucket < NNUE_KING_BUCKETS; ++bucket) {
    for (size_t neuron = 0; neuron < LAYER1_SIZE; ++neuron) {
      int32_t low = 0, high = 0;
      for (size_t square = 0; square < 64; ++square) {
        int32_t smallest = 0, largest = 0;
        for (size_t piece = 0; piece < 12; ++piece) {
          const int32_t v = net.feature_v[(bucket * 768 + piece * 64 + square) * LAYER1_SIZE + neuron];
          smallest = std::min(smallest, v);
          largest = std::max(largest, v);
        }
        low += smallest;
        high += largest;
      }
      const int32_t bias = net.feature_bias[neuron];
      if (2 * low + bias < INT16_MIN || 2 * high + bias > INT16_MAX) return false;
    }
  }
  for (size_t bucket = 0; bucket < OUTPUT_BUCKETS; ++bucket) {
    int64_t low = 0, high = 0;
    for (const int16_t weight : net.output_v[bucket]) {
      low += std::min<int64_t>(weight, 0) * 65025;
      high += std::max<int64_t>(weight, 0) * 65025;
    }
    if (low < INT32_MIN || high > INT32_MAX ||
        (low / QA + net.output_bias[bucket]) * SCALE < INT32_MIN ||
        (high / QA + net.output_bias[bucket]) * SCALE > INT32_MAX) return false;
  }
  return true;
}

inline bool load_embedded_nnue() {
  auto net = read_nnue_embedded();
  if (!net || !nnue_arithmetic_fits(*net)) return false;
  g_nnue_output_products_fit_i16 = std::all_of(
      net->output_v.begin(), net->output_v.end(), [](const auto &weights) {
        return std::all_of(weights.begin(), weights.end(),
                           [](int16_t w) { return w >= -128 && w <= 128; });
      });
  g_nnue_data = std::move(net);
  g_nnue = g_nnue_data.get();
  return true;
}

constexpr inline std::pair<size_t, size_t> feature_indices(int16_t piece, int16_t sq, size_t w_bucket, size_t b_bucket) noexcept {
  if (piece < Pieces::WPawn || piece > Pieces::BKing || sq < 0 || sq >= 64 ||
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

struct alignas(64) Accumulator {
  alignas(64) std::array<NNUE_AccumulatorValue, LAYER1_SIZE> white;
  alignas(64) std::array<NNUE_AccumulatorValue, LAYER1_SIZE> black;

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

constexpr inline int32_t screlu(int16_t x) noexcept {
  const int32_t clipped = std::clamp<int32_t>(x, SCRELU_MIN, SCRELU_MAX);
  return clipped * clipped;
}

inline int32_t screlu_flatten(const std::array<int16_t, LAYER1_SIZE> &us,
                             const std::array<int16_t, LAYER1_SIZE> &them,
                             const std::array<int16_t, LAYER1_SIZE * 2> &weights) noexcept {
#if defined(STALLION_SIMD_NEON)
  if (g_nnue_output_products_fit_i16) {
    int32x4_t us_low = vdupq_n_s32(0), us_high = vdupq_n_s32(0);
    int32x4_t them_low = vdupq_n_s32(0), them_high = vdupq_n_s32(0);
    const int16x8_t zero = vdupq_n_s16(0), maximum = vdupq_n_s16(255);
    for (size_t i = 0; i < LAYER1_SIZE; i += 8) {
      const auto add = [&](const int16_t *values, const int16_t *coefficients,
                           int32x4_t &low, int32x4_t &high) {
        const int16x8_t x = vminq_s16(vmaxq_s16(vld1q_s16(values), zero), maximum);
        // For these coefficients x*w fits int16 exactly; x*(x*w) is x*x*w.
        const int16x8_t product = vmulq_s16(x, vld1q_s16(coefficients));
        low = vmlal_s16(low, vget_low_s16(x), vget_low_s16(product));
        high = vmlal_s16(high, vget_high_s16(x), vget_high_s16(product));
      };
      add(us.data() + i, weights.data() + i, us_low, us_high);
      add(them.data() + i, weights.data() + LAYER1_SIZE + i, them_low, them_high);
    }
    return vaddvq_s32(vaddq_s32(vaddq_s32(us_low, us_high),
                               vaddq_s32(them_low, them_high))) / QA;
  }
  int32x4_t us_low = vdupq_n_s32(0), us_high = vdupq_n_s32(0);
  int32x4_t them_low = vdupq_n_s32(0), them_high = vdupq_n_s32(0);
  const int16x8_t zero = vdupq_n_s16(0), maximum = vdupq_n_s16(255);
  for (size_t i = 0; i < LAYER1_SIZE; i += 8) {
    const auto add = [&](const int16_t *values, const int16_t *coefficients,
                         int32x4_t &low, int32x4_t &high) {
      const int16x8_t x = vminq_s16(vmaxq_s16(vld1q_s16(values), zero), maximum);
      const int16x8_t w = vld1q_s16(coefficients);
      low = vaddq_s32(low, vmulq_s32(vmull_s16(vget_low_s16(x), vget_low_s16(x)), vmovl_s16(vget_low_s16(w))));
      high = vaddq_s32(high, vmulq_s32(vmull_s16(vget_high_s16(x), vget_high_s16(x)), vmovl_s16(vget_high_s16(w))));
    };
    add(us.data() + i, weights.data() + i, us_low, us_high);
    add(them.data() + i, weights.data() + LAYER1_SIZE + i, them_low, them_high);
  }
  // The loader bounds every positive/negative sum, including these subsets.
  const int32x4_t sum = vaddq_s32(vaddq_s32(us_low, us_high), vaddq_s32(them_low, them_high));
  return vaddvq_s32(sum) / QA;
#else
  int32_t sum = 0;
  for (size_t i = 0; i < LAYER1_SIZE; ++i) {
    sum += screlu(us[i]) * weights[i];
    sum += screlu(them[i]) * weights[LAYER1_SIZE + i];
  }
  return sum / QA;
#endif
}

// Pieces of one position, one bitboard per piece code (2..13).
using PieceSet = std::array<uint64_t, 14>;

inline PieceSet piece_set(const BoardState &position) noexcept {
  PieceSet pieces{};
  for (int16_t piece = Pieces::WPawn; piece <= Pieces::BKing; ++piece)
    pieces[piece] = position.colors_bb[piece & 1] & position.pieces_bb[piece >> 1];
  return pieces;
}

// Feature rows to add to and subtract from one accumulator perspective.
struct RowDelta {
  const int16_t *add[NNUE_FEATURE_SLOTS];
  const int16_t *sub[NNUE_FEATURE_SLOTS];
  int16_t n_add = 0, n_sub = 0;
};

// Every partial update is covered by nnue_arithmetic_fits(), including aliasing.
inline void apply_rows(int16_t *out, const int16_t *in, const RowDelta &delta) noexcept {
  constexpr size_t Tile = 64;
  for (size_t base = 0; base < LAYER1_SIZE; base += Tile) {
#if defined(STALLION_SIMD_NEON)
    int16x8_t tile[Tile / 8];
    for (size_t i = 0; i < Tile / 8; ++i) tile[i] = vld1q_s16(in + base + 8 * i);
    for (int k = 0; k < delta.n_add; ++k)
      for (size_t i = 0; i < Tile / 8; ++i) tile[i] = vaddq_s16(tile[i], vld1q_s16(delta.add[k] + base + 8 * i));
    for (int k = 0; k < delta.n_sub; ++k)
      for (size_t i = 0; i < Tile / 8; ++i) tile[i] = vsubq_s16(tile[i], vld1q_s16(delta.sub[k] + base + 8 * i));
    for (size_t i = 0; i < Tile / 8; ++i) vst1q_s16(out + base + 8 * i, tile[i]);
#else
    int32_t tile[Tile];
    for (size_t i = 0; i < Tile; ++i) tile[i] = in[base + i];
    for (int k = 0; k < delta.n_add; ++k)
      for (size_t i = 0; i < Tile; ++i) tile[i] += delta.add[k][base + i];
    for (int k = 0; k < delta.n_sub; ++k)
      for (size_t i = 0; i < Tile; ++i) tile[i] -= delta.sub[k][base + i];
    for (size_t i = 0; i < Tile; ++i) out[base + i] = static_cast<int16_t>(tile[i]);
#endif
  }
}

// Accumulators are built lazily: a move only records the new pieces, and
// evaluate() brings the current level up to date from the nearest computed
// ancestor by adding the piece differences. A king-bucket
// change for a perspective goes through the refresh cache instead.
class alignas(64) NNUE_State {
public:
  struct Level {
    PieceSet pieces{};
    int16_t w_bucket = 0, b_bucket = 0;
    // Computed levels may share an immutable ancestor accumulator.
    int16_t accumulator = -1;
  };

  // Refresh cache ("Finny table"): per perspective and king bucket, the last
  // accumulator built for that bucket with the pieces it
  // holds. Each entry is always self-consistent, so it is neither copied nor
  // reset.
  struct RefreshEntry {
    alignas(64) std::array<NNUE_AccumulatorValue, LAYER1_SIZE> acc;
    PieceSet pieces{};
  };

  alignas(64) Accumulator m_accumulator_stack[MaxSearchDepth];
  Level m_levels[MaxSearchDepth];
  int16_t m_idx = 0;
  bool m_initialized = false;
  std::unique_ptr<RefreshEntry[]> m_refresh;

  NNUE_State() = default;
  NNUE_State(const NNUE_State &other) noexcept { *this = other; }
  NNUE_State &operator=(const NNUE_State &other) noexcept {
    if (this != &other) {
      m_idx = other.m_idx;
      m_initialized = other.m_initialized;
      std::copy_n(other.m_levels, m_idx + 1, m_levels);
      for (int16_t level = 0; level <= m_idx; ++level)
        if (m_levels[level].accumulator == level)
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
    compute_metadata(level);
    refresh_side(false);
    refresh_side(true);
    level.accumulator = 0;
  }

  inline void push(const BoardState &) noexcept {
    if (!g_nnue || !m_initialized || m_idx >= MaxSearchDepth - 1) std::exit(EXIT_FAILURE);
    ++m_idx;
    m_levels[m_idx].accumulator = -1;
  }

  inline void pop() noexcept {
    if (!g_nnue || !m_initialized || m_idx <= 0) std::exit(EXIT_FAILURE);
    --m_idx;
  }

  inline int32_t evaluate(const BoardState &position) noexcept {
    if (!g_nnue || !m_initialized) std::exit(EXIT_FAILURE);
    update(position);
    const int16_t piece_count = pop_count(position.colors_bb[0] | position.colors_bb[1]);
    if (piece_count < 2) std::exit(EXIT_FAILURE);
    const auto &acc = m_accumulator_stack[m_levels[m_idx].accumulator];
    const bool white = position.color == Colors::White;
    const size_t b = static_cast<size_t>(std::clamp<int16_t>((piece_count - 1) / 2, 0, static_cast<int16_t>(OUTPUT_BUCKETS) - 1));
    const NNUE_WideValue output = screlu_flatten(white ? acc.white : acc.black, white ? acc.black : acc.white,
                                          g_nnue->output_v[b]);
    return static_cast<int32_t>(std::clamp<NNUE_WideValue>(
        (output + g_nnue->output_bias[b]) * SCALE / QAB, -MaxEval, MaxEval));
  }

private:
  static inline void compute_metadata(Level &level) noexcept {
    const int16_t wking = get_lsb(level.pieces[Pieces::WKing]);
    const int16_t bking = get_lsb(level.pieces[Pieces::BKing]);
    level.w_bucket = static_cast<int16_t>(KingBucketTable[wking]);
    level.b_bucket = static_cast<int16_t>(KingBucketTable[bking ^ 56]);
  }

  static inline void piece_rows(const PieceSet &from, const PieceSet &to, size_t bucket, bool black,
                                RowDelta &delta) noexcept {
    const int16_t *F = g_nnue->feature_v.data();
    for (int16_t piece = Pieces::WPawn; piece <= Pieces::BKing; ++piece) {
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

  static inline void piece_rows_both(const PieceSet &from, const PieceSet &to,
                                     size_t w_bucket, size_t b_bucket,
                                     RowDelta &w_delta, RowDelta &b_delta) noexcept {
    const int16_t *F = g_nnue->feature_v.data();
    for (int16_t piece = Pieces::WPawn; piece <= Pieces::BKing; ++piece) {
      uint64_t removed = from[piece] & ~to[piece], added = to[piece] & ~from[piece];
      while (removed) {
        const auto [w_idx, b_idx] = feature_indices(piece, pop_lsb(removed), w_bucket, b_bucket);
        w_delta.sub[w_delta.n_sub++] = F + w_idx * LAYER1_SIZE;
        b_delta.sub[b_delta.n_sub++] = F + b_idx * LAYER1_SIZE;
      }
      while (added) {
        const auto [w_idx, b_idx] = feature_indices(piece, pop_lsb(added), w_bucket, b_bucket);
        w_delta.add[w_delta.n_add++] = F + w_idx * LAYER1_SIZE;
        b_delta.add[b_delta.n_add++] = F + b_idx * LAYER1_SIZE;
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
    RowDelta delta;
    piece_rows(entry.pieces, level.pieces, bucket, black, delta);
    apply_rows(entry.acc.data(), entry.acc.data(), delta);
    entry.pieces = level.pieces;
    auto &acc = m_accumulator_stack[m_idx];
    std::copy_n(entry.acc.begin(), LAYER1_SIZE, (black ? acc.black : acc.white).begin());
  }

  inline void update(const BoardState &position) noexcept {
    Level &now = m_levels[m_idx];
    if (now.accumulator >= 0) return;
    now.pieces = piece_set(position);
    int16_t ancestor = m_idx - 1;
    while (m_levels[ancestor].accumulator < 0) --ancestor;
    const Level &from = m_levels[ancestor];
    const auto &from_acc = m_accumulator_stack[from.accumulator];
    auto &acc = m_accumulator_stack[m_idx];
    if (now.pieces == from.pieces) {
      now.w_bucket = from.w_bucket;
      now.b_bucket = from.b_bucket;
      now.accumulator = from.accumulator;
      return;
    }
    compute_metadata(now);
    if (now.w_bucket == from.w_bucket && now.b_bucket == from.b_bucket) {
      RowDelta w_delta, b_delta;
      piece_rows_both(from.pieces, now.pieces, static_cast<size_t>(now.w_bucket),
                      static_cast<size_t>(now.b_bucket), w_delta, b_delta);
      apply_rows(acc.white.data(), from_acc.white.data(), w_delta);
      apply_rows(acc.black.data(), from_acc.black.data(), b_delta);
      now.accumulator = m_idx;
      return;
    }
    for (const bool black : {false, true}) {
      const size_t bucket = black ? now.b_bucket : now.w_bucket;
      if (bucket != (black ? from.b_bucket : from.w_bucket)) {
        refresh_side(black);
        continue;
      }
      RowDelta delta;
      piece_rows(from.pieces, now.pieces, bucket, black, delta);
      if (black) {
        apply_rows(acc.black.data(), from_acc.black.data(), delta);
      } else {
        apply_rows(acc.white.data(), from_acc.white.data(), delta);
      }
    }
    now.accumulator = m_idx;
  }
};

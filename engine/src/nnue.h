#pragma once
#include "defs.h"
#include <algorithm>
#include <array>
#include <cstdint>
#include <cstring>
#include <fstream>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

constexpr size_t INPUT_SIZE = 768;
constexpr size_t LAYER1_SIZE = 768;

constexpr int SCRELU_MIN = 0;
constexpr int SCRELU_MAX = 255;

constexpr int SCALE = 400;

constexpr int QA = 255;
constexpr int QB = 64;
constexpr int QAB = QA * QB; // 16320

struct alignas(64) NNUE_Params {
  std::array<int16_t, INPUT_SIZE * LAYER1_SIZE> feature_v;
  std::array<int16_t, LAYER1_SIZE> feature_bias;
  std::array<int16_t, LAYER1_SIZE * 2> output_v;
  int16_t output_bias;
};

inline std::unique_ptr<NNUE_Params> g_nnue_base = nullptr;
inline std::unique_ptr<NNUE_Params> g_nnue_aggressive = nullptr;
inline const NNUE_Params *g_nnue = nullptr;
inline bool nnue_loaded = false;
inline bool use_nnue = true;

inline std::unique_ptr<NNUE_Params> read_nnue_binary(const std::string &path) {
  std::ifstream file(path, std::ios::binary);
  if (!file.is_open()) {
    file.open("nets/" + path, std::ios::binary);
  }
  if (!file.is_open()) {
    return nullptr;
  }

  auto params = std::make_unique<NNUE_Params>();
  file.read(reinterpret_cast<char *>(params.get()), sizeof(NNUE_Params));
  if (file.gcount() < static_cast<std::streamsize>(sizeof(NNUE_Params))) {
    return nullptr;
  }

  return params;
}

inline bool load_nnue_base(const std::string &path = "nets/base.nnue") {
  auto net = read_nnue_binary(path);
  if (net) {
    g_nnue_base = std::move(net);
    if (!g_nnue)
      g_nnue = g_nnue_base.get();
    nnue_loaded = true;
    return true;
  }
  return false;
}

inline bool load_nnue_aggressive(const std::string &path = "nets/aggressive.nnue") {
  auto net = read_nnue_binary(path);
  if (net) {
    g_nnue_aggressive = std::move(net);
    nnue_loaded = true;
    return true;
  }
  return false;
}

inline bool load_nnue_file(const std::string &path = "nets/base.nnue") {
  bool ok1 = load_nnue_base(path);
  bool ok2 = load_nnue_aggressive("nets/aggressive.nnue");
  return ok1 || ok2;
}

inline void select_active_nnue(int phase) {
  if (!nnue_loaded) {
    g_nnue = nullptr;
    return;
  }

  if (phase == PhaseTypes::Endgame || !g_nnue_aggressive) {
    g_nnue = g_nnue_base ? g_nnue_base.get() : g_nnue_aggressive.get();
  } else {
    g_nnue = g_nnue_aggressive.get();
  }
}

constexpr inline std::pair<size_t, size_t> feature_indices(int piece, int sq) noexcept {
  if (piece < Pieces::WPawn || piece > Pieces::BKing || !is_valid_square(sq)) {
    return {0, 0};
  }
  constexpr size_t color_stride = 384;
  constexpr size_t piece_stride = 64;

  const size_t base = static_cast<size_t>(piece / 2 - 1);
  const size_t color = static_cast<size_t>(piece & 1);

  const size_t whiteIdx = color * color_stride + base * piece_stride + static_cast<size_t>(sq);
  const size_t blackIdx = (color ^ 1) * color_stride + base * piece_stride + static_cast<size_t>(sq ^ 56);

  return {whiteIdx, blackIdx};
}

template <size_t HiddenSize = LAYER1_SIZE>
struct alignas(64) Accumulator {
  alignas(64) std::array<int16_t, HiddenSize> white;
  alignas(64) std::array<int16_t, HiddenSize> black;

  inline void init(const int16_t *bias_ptr) {
    std::memcpy(white.data(), bias_ptr, sizeof(int16_t) * HiddenSize);
    std::memcpy(black.data(), bias_ptr, sizeof(int16_t) * HiddenSize);
  }

  Accumulator() = default;
  Accumulator(const Accumulator &other) {
    std::memcpy(white.data(), other.white.data(), sizeof(white));
    std::memcpy(black.data(), other.black.data(), sizeof(black));
  }
  Accumulator &operator=(const Accumulator &other) {
    if (this != &other) {
      std::memcpy(white.data(), other.white.data(), sizeof(white));
      std::memcpy(black.data(), other.black.data(), sizeof(black));
    }
    return *this;
  }
};

constexpr inline int32_t screlu(int16_t x) noexcept {
  const int32_t clipped = std::clamp(static_cast<int32_t>(x), SCRELU_MIN, SCRELU_MAX);
  return clipped * clipped;
}

inline int32_t screlu_flatten(const std::array<int16_t, LAYER1_SIZE> &us,
                             const std::array<int16_t, LAYER1_SIZE> &them,
                             const std::array<int16_t, LAYER1_SIZE * 2> &weights) {
  int32_t sum = 0;
  #pragma unroll 4
  for (size_t i = 0; i < LAYER1_SIZE; ++i) {
    sum += screlu(us[i]) * static_cast<int32_t>(weights[i]);
    sum += screlu(them[i]) * static_cast<int32_t>(weights[LAYER1_SIZE + i]);
  }
  return sum / QA;
}

class alignas(64) NNUE_State {
public:
  alignas(64) Accumulator<LAYER1_SIZE> m_accumulator_stack[MaxSearchDepth];
  Accumulator<LAYER1_SIZE> *m_curr = &m_accumulator_stack[0];
  int m_idx = 0;

  inline void pop() {
    if (m_idx > 0) {
      --m_idx;
      m_curr = &m_accumulator_stack[m_idx];
    }
  }

  inline void push_null() {
    if (m_idx >= MaxSearchDepth - 1 || !g_nnue || !nnue_loaded) return;
    m_accumulator_stack[m_idx + 1] = m_accumulator_stack[m_idx];
    ++m_idx;
    m_curr = &m_accumulator_stack[m_idx];
  }

  inline int evaluate(int color) {
    if (!g_nnue || !nnue_loaded) return 0;
    const auto &us = (color == Colors::White) ? m_curr->white : m_curr->black;
    const auto &them = (color == Colors::White) ? m_curr->black : m_curr->white;
    const int32_t output = screlu_flatten(us, them, g_nnue->output_v);
    return (output + g_nnue->output_bias) * SCALE / QAB;
  }

  inline void reset_nnue(const BoardState &position) {
    m_idx = 0;
    m_curr = &m_accumulator_stack[0];
    if (!g_nnue || !nnue_loaded) return;

    m_curr->init(g_nnue->feature_bias.data());

    for (int sq = 0; sq < 64; sq++) {
      int piece = position.board[sq];
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

  inline void add_sub(int from_piece, int from, int to_piece, int to) {
    if (m_idx >= MaxSearchDepth - 1 || !g_nnue || !nnue_loaded) return;
    const auto [wf, bf] = feature_indices(from_piece, from);
    const auto [wt, bt] = feature_indices(to_piece, to);

    auto &curr = m_accumulator_stack[m_idx];
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

  inline void add_sub_sub(int from_piece, int from, int to_piece, int to, int captured, int captured_sq) {
    if (m_idx >= MaxSearchDepth - 1 || !g_nnue || !nnue_loaded) return;
    const auto [wf, bf] = feature_indices(from_piece, from);
    const auto [wt, bt] = feature_indices(to_piece, to);
    const auto [wc, bc] = feature_indices(captured, captured_sq);

    auto &curr = m_accumulator_stack[m_idx];
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

  inline void add_add_sub_sub(int p1, int from1, int to1, int p2, int from2, int to2) {
    if (m_idx >= MaxSearchDepth - 1 || !g_nnue || !nnue_loaded) return;
    const auto [w1f, b1f] = feature_indices(p1, from1);
    const auto [w1t, b1t] = feature_indices(p1, to1);
    const auto [w2f, b2f] = feature_indices(p2, from2);
    const auto [w2t, b2t] = feature_indices(p2, to2);

    auto &curr = m_accumulator_stack[m_idx];
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

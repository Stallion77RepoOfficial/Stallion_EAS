#pragma once
#include "defs.h"
#include "simd.h"
#include <algorithm>
#include <array>
#include <cstdint>
#include <cstdlib>
#include <cstring>
 
#include <vector>
#ifdef _MSC_VER
#define W_MSVC
#pragma push_macro("_MSC_VER")
#undef _MSC_VER
#endif

#define INCBIN_PREFIX g_
#include "incbin.h"

#ifdef W_MSVC
#pragma pop_macro("_MSC_VER")
#undef W_MSVC
#endif

constexpr size_t INPUT_SIZE = 768;
constexpr size_t LAYER1_SIZE = 768;

constexpr int SCRELU_MIN = 0;
constexpr int SCRELU_MAX = 255;

constexpr int SCALE = 400;

constexpr int QA = 255;
constexpr int QB = 64;

const auto SCRELU_MIN_VEC = get_int16_vec(SCRELU_MIN);
const auto QA_VEC = get_int16_vec(QA);

constexpr int QAB = QA * QB;

struct alignas(64) NNUE_Params {
  std::array<int16_t, INPUT_SIZE * LAYER1_SIZE> feature_v;
  std::array<int16_t, LAYER1_SIZE> feature_bias;
  std::array<int16_t, LAYER1_SIZE * 2> output_v;
  int16_t output_bias;
};

INCBIN(nnue, "nets/gold.nnue");
INCBIN(nnue2, "nets/base.nnue");
INCBIN(nnue3, "nets/gold.nnue");
INCBIN(nnue4, "nets/gold.nnue");
INCBIN(nnue5, "nets/gold.nnue");

 
inline const NNUE_Params *make_nnue_safe(const unsigned char *data, size_t size) {
  if (!data || size < sizeof(NNUE_Params)) return nullptr;
   
  void *buf = nullptr;
   
  if (posix_memalign(&buf, 64, sizeof(NNUE_Params)) != 0) return nullptr;
  std::memcpy(buf, data, sizeof(NNUE_Params));
  return reinterpret_cast<const NNUE_Params *>(buf);
}

static const NNUE_Params *g_nnue = nullptr;
static const NNUE_Params *g_nnue2 = nullptr;
static const NNUE_Params *g_nnue3 = nullptr;
static const NNUE_Params *g_nnue4 = nullptr;
static const NNUE_Params *g_nnue5 = nullptr;

 
struct NNUEInitializer {
  NNUEInitializer() {
    g_nnue = make_nnue_safe(g_nnueData, g_nnueSize);
    g_nnue2 = make_nnue_safe(g_nnue2Data, g_nnue2Size);
    g_nnue3 = make_nnue_safe(g_nnue3Data, g_nnue3Size);
    g_nnue4 = make_nnue_safe(g_nnue4Data, g_nnue4Size);
    g_nnue5 = make_nnue_safe(g_nnue5Data, g_nnue5Size);
     
    if (!g_nnue) g_nnue = g_nnue2 ? g_nnue2 : nullptr;
    if (!g_nnue2) g_nnue2 = g_nnue ? g_nnue : nullptr;
    if (!g_nnue3) g_nnue3 = g_nnue2 ? g_nnue2 : g_nnue;
    if (!g_nnue4) g_nnue4 = g_nnue3 ? g_nnue3 : g_nnue2;
    if (!g_nnue5) g_nnue5 = g_nnue4 ? g_nnue4 : g_nnue3;
  }
};

static NNUEInitializer nnue_initializer_instance;

template <size_t HiddenSize> struct alignas(64) Accumulator {
  alignas(64) std::array<int16_t, HiddenSize> white;
  alignas(64) std::array<int16_t, HiddenSize> black;

  inline void init(const int16_t *bias_ptr) {
     
    std::memcpy(white.data(), bias_ptr, sizeof(int16_t) * HiddenSize);
    std::memcpy(black.data(), bias_ptr, sizeof(int16_t) * HiddenSize);
  }
  
   
  Accumulator(const Accumulator& other) {
    std::memcpy(white.data(), other.white.data(), sizeof(white));
    std::memcpy(black.data(), other.black.data(), sizeof(black));
  }
  
   
  Accumulator& operator=(const Accumulator& other) {
    if (this != &other) {
      std::memcpy(white.data(), other.white.data(), sizeof(white));
      std::memcpy(black.data(), other.black.data(), sizeof(black));
    }
    return *this;
  }
  
  Accumulator() = default;
};

constexpr int32_t screlu(int16_t x) {
  const auto clipped =
      std::clamp(static_cast<int32_t>(x), SCRELU_MIN, SCRELU_MAX);
  return clipped * clipped;
}

template <size_t size, size_t v>
inline void add_to_all(std::array<int16_t, size> &output,
                       std::array<int16_t, size> &input,
                       const std::array<int16_t, v> &delta, size_t offset) {
   
  if (offset + size > v) return;
  for (size_t i = 0; i < size; ++i) {
    output[i] = input[i] + delta[offset + i];
  }
}

template <size_t size, size_t v>
inline void subtract_from_all(std::array<int16_t, size> &output,
                              std::array<int16_t, size> &input,
                              const std::array<int16_t, v> &delta,
                              size_t offset) {
   
  if (offset + size > v) return;

  for (size_t i = 0; i < size; ++i) {
    output[i] = input[i] - delta[offset + i];
  }
}

std::pair<size_t, size_t> feature_indices(int piece, int sq) {
   
  if (piece == Pieces::Blank || piece < Pieces::WPawn || piece > Pieces::BKing) {
    return {0, 0};  
  }
  if (!is_valid_square(sq)) {
    return {0, 0};  
  }

  constexpr size_t color_stride = 64 * 6;
  constexpr size_t piece_stride = 64;

  const auto base = static_cast<int>(piece / 2 - 1);
  const size_t color = piece & 1;

  const auto whiteIdx =
      color * color_stride + base * piece_stride + static_cast<size_t>(sq);
  const auto blackIdx = (color ^ 1) * color_stride + base * piece_stride +
                        (static_cast<size_t>(sq ^ 56));

  return {whiteIdx, blackIdx};
}

inline int32_t
screlu_flatten(const std::array<int16_t, LAYER1_SIZE> &us,
               const std::array<int16_t, LAYER1_SIZE> &them,
               const std::array<int16_t, LAYER1_SIZE * 2> &weights) {

#if defined(__AVX512F__) || defined(__AVX2__)

  auto sum = vec_int32_zero();

  for (size_t i = 0; i < LAYER1_SIZE; i += REGISTER_SIZE) {

    auto v_us = int16_load(&us[i]);
    auto w_us = int16_load(&weights[i]);

    v_us = vec_int16_clamp(v_us, SCRELU_MIN_VEC, QA_VEC);

    auto our_product = vec_int16_multiply(v_us, w_us);

    auto our_result = vec_int16_madd_int32(our_product, v_us);

    sum = vec_int32_add(sum, our_result);

    auto v_them = int16_load(&them[i]);
    auto w_them = int16_load(&weights[LAYER1_SIZE + i]);

    v_them = vec_int16_clamp(v_them, SCRELU_MIN_VEC, QA_VEC);

    auto their_product = vec_int16_multiply(v_them, w_them);

    auto their_result = vec_int16_madd_int32(their_product, v_them);

    sum = vec_int32_add(sum, their_result);
  }

  return vec_int32_hadd(sum) / QA;

#else

  int32_t sum = 0;

  for (size_t i = 0; i < LAYER1_SIZE; ++i) {
    sum += screlu(us[i]) * weights[i];
    sum += screlu(them[i]) * weights[LAYER1_SIZE + i];
  }

  return sum / QA;

#endif
}

class alignas(64) NNUE_State {
public:
  alignas(64) Accumulator<LAYER1_SIZE> m_accumulator_stack[MaxSearchDepth];
  Accumulator<LAYER1_SIZE> *m_curr;
   
   
  int m_idx;

  void add_sub(int from_piece, int from, int to_piece, int to, int phase);
  void add_sub_sub(int from_piece, int from, int to_piece, int to, int captured,
                   int captured_pos, int phase);
  void add_add_sub_sub(int piece1, int from1, int to1, int piece2, int from2,
                       int to2, int phase);
  void pop();
  int evaluate(int color, int phase);
  void reset_nnue(const Position &position, int phase);
  void change_phases(const Position &position, int phase);
  void reset_and_add_sub_sub(const Position &position, int from_piece, int from,
                             int to_piece, int to, int captured,
                             int captured_sq, int phase);

  template <bool Activate>
  inline void update_feature(int piece, int square, int phase);
  
   
  template <size_t N>
  inline void update_features_batch(const std::array<std::pair<int, int>, N>& features, 
                                   bool activate, int phase);
  
   
  void reset_nnue_optimized(const Position &position, int phase);

  NNUE_State() {
    m_curr = &m_accumulator_stack[0];
    m_idx = 0;
  }

#if REGISTER_SIZE == 0
 
int evaluate_fallback(const Position &position, int color, uint8_t phase) const {
     
    int score = 0;
    for (int piece_type = 1; piece_type <= 6; ++piece_type) {
        uint64_t white_pieces = position.pieces_bb[piece_type] & position.colors_bb[0];
        uint64_t black_pieces = position.pieces_bb[piece_type] & position.colors_bb[1];
        int piece_value = MaterialValues[piece_type];
        score += pop_count(white_pieces) * piece_value;
        score -= pop_count(black_pieces) * piece_value;
    }
    return color ? -score : score;
}
#endif

};

void NNUE_State::add_sub(int from_piece, int from, int to_piece, int to,
                         int phase) {
   
  if (m_idx < 0 || m_idx >= MaxSearchDepth - 1) {
     
    return;
  }

   
  if (m_curr != &m_accumulator_stack[m_idx]) {
     
    m_curr = &m_accumulator_stack[m_idx];
     
    if (m_curr != &m_accumulator_stack[m_idx]) return;
  }

   
  if (from_piece == Pieces::Blank || to_piece == Pieces::Blank ||
      !is_valid_square(from) || !is_valid_square(to)) {
    return;  
  }

  const auto [white_from, black_from] = feature_indices(from_piece, from);
  const auto [white_to, black_to] = feature_indices(to_piece, to);

  const NNUE_Params *ptr = nullptr;
  switch (phase) {
    case PhaseTypes::Opening: ptr = g_nnue; break;
    case PhaseTypes::MiddleGame: ptr = g_nnue2; break;
    case PhaseTypes::LateMiddleGame: ptr = g_nnue3; break;
    case PhaseTypes::Endgame: ptr = g_nnue4; break;
    case PhaseTypes::Sacrifice: ptr = g_nnue5; break;
    default: ptr = g_nnue2;  
  }

  if (!ptr) return;  

   
  const size_t next_idx = static_cast<size_t>(m_idx + 1);
  if (next_idx >= MaxSearchDepth) return;

  auto &curr = m_accumulator_stack[static_cast<size_t>(m_idx)];
  auto &next = m_accumulator_stack[next_idx];

   
  if (white_to * LAYER1_SIZE + (LAYER1_SIZE - 1) >= ptr->feature_v.size() ||
      white_from * LAYER1_SIZE + (LAYER1_SIZE - 1) >= ptr->feature_v.size() ||
      black_to * LAYER1_SIZE + (LAYER1_SIZE - 1) >= ptr->feature_v.size() ||
      black_from * LAYER1_SIZE + (LAYER1_SIZE - 1) >= ptr->feature_v.size()) {
    return;
  }

  for (size_t i = 0; i < LAYER1_SIZE; ++i) {
    next.white[i] = curr.white[i] +
                    ptr->feature_v[white_to * LAYER1_SIZE + i] -
                    ptr->feature_v[white_from * LAYER1_SIZE + i];
    next.black[i] = curr.black[i] +
                    ptr->feature_v[black_to * LAYER1_SIZE + i] -
                    ptr->feature_v[black_from * LAYER1_SIZE + i];
  }

   
  ++m_idx;
  m_curr = &m_accumulator_stack[m_idx];
}

void NNUE_State::add_sub_sub(int from_piece, int from, int to_piece, int to,
                             int captured, int captured_sq, int phase) {
   
  if (m_idx < 0 || m_idx >= MaxSearchDepth - 1) return;
  if (m_curr != &m_accumulator_stack[m_idx]) m_curr = &m_accumulator_stack[m_idx];

   
  if (from_piece == Pieces::Blank || to_piece == Pieces::Blank) {
    return;  
  }

  if (!is_valid_square(from) || !is_valid_square(to)) {
    return;  
  }

  if (captured != Pieces::Blank && !is_valid_square(captured_sq)) {
    return;  
  }

   
  if (phase < PhaseTypes::Opening || phase > PhaseTypes::Sacrifice) {
    phase = PhaseTypes::MiddleGame;  
  }

   
  const auto [white_from, black_from] = feature_indices(from_piece, from);
  const auto [white_to, black_to] = feature_indices(to_piece, to);
  const auto [white_capt, black_capt] = feature_indices(captured, captured_sq);

   
  if (white_from >= INPUT_SIZE || black_from >= INPUT_SIZE ||
      white_to >= INPUT_SIZE || black_to >= INPUT_SIZE ||
      white_capt >= INPUT_SIZE || black_capt >= INPUT_SIZE) {
    return;  
  }

  const NNUE_Params *ptr = nullptr;
  switch (phase) {
    case PhaseTypes::Opening: ptr = g_nnue; break;
    case PhaseTypes::MiddleGame: ptr = g_nnue2; break;
    case PhaseTypes::LateMiddleGame: ptr = g_nnue3; break;
    case PhaseTypes::Endgame: ptr = g_nnue4; break;
    case PhaseTypes::Sacrifice: ptr = g_nnue5; break;
    default: ptr = g_nnue2;  
  }

   
  if (!ptr || !ptr->feature_v.data() || !ptr->output_v.data()) {
    return;  
  }

   
  const size_t max_feature_offset = LAYER1_SIZE;
  if (white_from * LAYER1_SIZE + max_feature_offset > ptr->feature_v.size() ||
      black_from * LAYER1_SIZE + max_feature_offset > ptr->feature_v.size() ||
      white_to * LAYER1_SIZE + max_feature_offset > ptr->feature_v.size() ||
      black_to * LAYER1_SIZE + max_feature_offset > ptr->feature_v.size() ||
      white_capt * LAYER1_SIZE + max_feature_offset > ptr->feature_v.size() ||
      black_capt * LAYER1_SIZE + max_feature_offset > ptr->feature_v.size()) {
    return;  
  }

   
  const size_t next_idx = static_cast<size_t>(m_idx + 1);
  if (next_idx >= MaxSearchDepth) return;
  auto &curr = m_accumulator_stack[static_cast<size_t>(m_idx)];
  auto &next = m_accumulator_stack[next_idx];

   
  if (white_to * LAYER1_SIZE + (LAYER1_SIZE - 1) >= ptr->feature_v.size() ||
      white_from * LAYER1_SIZE + (LAYER1_SIZE - 1) >= ptr->feature_v.size() ||
      white_capt * LAYER1_SIZE + (LAYER1_SIZE - 1) >= ptr->feature_v.size() ||
      black_to * LAYER1_SIZE + (LAYER1_SIZE - 1) >= ptr->feature_v.size() ||
      black_from * LAYER1_SIZE + (LAYER1_SIZE - 1) >= ptr->feature_v.size() ||
      black_capt * LAYER1_SIZE + (LAYER1_SIZE - 1) >= ptr->feature_v.size()) {
    return;
  }

  for (size_t i = 0; i < LAYER1_SIZE; ++i) {
    next.white[i] = curr.white[i] +
                    ptr->feature_v[white_to * LAYER1_SIZE + i] -
                    ptr->feature_v[white_from * LAYER1_SIZE + i] -
                    ptr->feature_v[white_capt * LAYER1_SIZE + i];
    next.black[i] = curr.black[i] +
                    ptr->feature_v[black_to * LAYER1_SIZE + i] -
                    ptr->feature_v[black_from * LAYER1_SIZE + i] -
                    ptr->feature_v[black_capt * LAYER1_SIZE + i];
  }

  ++m_idx;
  m_curr = &m_accumulator_stack[m_idx];
}

void NNUE_State::add_add_sub_sub(int piece1, int from1, int to1, int piece2,
                                 int from2, int to2, int phase) {
  if (m_idx < 0 || m_idx >= MaxSearchDepth - 1) return;
  if (m_curr != &m_accumulator_stack[m_idx]) m_curr = &m_accumulator_stack[m_idx];
  const auto [white_from1, black_from1] = feature_indices(piece1, from1);
  const auto [white_to1, black_to1] = feature_indices(piece1, to1);
  const auto [white_from2, black_from2] = feature_indices(piece2, from2);
  const auto [white_to2, black_to2] = feature_indices(piece2, to2);

  const NNUE_Params *ptr = nullptr;
  switch (phase) {
    case PhaseTypes::Opening: ptr = g_nnue; break;
    case PhaseTypes::MiddleGame: ptr = g_nnue2; break;
    case PhaseTypes::LateMiddleGame: ptr = g_nnue3; break;
    case PhaseTypes::Endgame: ptr = g_nnue4; break;
    case PhaseTypes::Sacrifice: ptr = g_nnue5; break;
    default: ptr = g_nnue2;  
  }

  const size_t next_idx = static_cast<size_t>(m_idx + 1);
  if (next_idx >= MaxSearchDepth) return;
  auto &curr = m_accumulator_stack[static_cast<size_t>(m_idx)];
  auto &next = m_accumulator_stack[next_idx];

   
  if (white_to1 * LAYER1_SIZE + (LAYER1_SIZE - 1) >= ptr->feature_v.size() ||
      white_from1 * LAYER1_SIZE + (LAYER1_SIZE - 1) >= ptr->feature_v.size() ||
      white_to2 * LAYER1_SIZE + (LAYER1_SIZE - 1) >= ptr->feature_v.size() ||
      white_from2 * LAYER1_SIZE + (LAYER1_SIZE - 1) >= ptr->feature_v.size() ||
      black_to1 * LAYER1_SIZE + (LAYER1_SIZE - 1) >= ptr->feature_v.size() ||
      black_from1 * LAYER1_SIZE + (LAYER1_SIZE - 1) >= ptr->feature_v.size() ||
      black_to2 * LAYER1_SIZE + (LAYER1_SIZE - 1) >= ptr->feature_v.size() ||
      black_from2 * LAYER1_SIZE + (LAYER1_SIZE - 1) >= ptr->feature_v.size()) {
    return;
  }

  for (size_t i = 0; i < LAYER1_SIZE; ++i) {
    next.white[i] = curr.white[i] +
                    ptr->feature_v[white_to1 * LAYER1_SIZE + i] -
                    ptr->feature_v[white_from1 * LAYER1_SIZE + i] +
                    ptr->feature_v[white_to2 * LAYER1_SIZE + i] -
                    ptr->feature_v[white_from2 * LAYER1_SIZE + i];
    next.black[i] = curr.black[i] +
                    ptr->feature_v[black_to1 * LAYER1_SIZE + i] -
                    ptr->feature_v[black_from1 * LAYER1_SIZE + i] +
                    ptr->feature_v[black_to2 * LAYER1_SIZE + i] -
                    ptr->feature_v[black_from2 * LAYER1_SIZE + i];
  }

  ++m_idx;
  m_curr = &m_accumulator_stack[m_idx];
}

void NNUE_State::pop() {
   
   
  if (m_idx > 0) {
    --m_idx;
     
    if (m_idx < 0) m_idx = 0;
    m_curr = &m_accumulator_stack[static_cast<size_t>(m_idx)];
  }
}

int NNUE_State::evaluate(int color, int phase) {
  const NNUE_Params *ptr = nullptr;
  switch (phase) {
  case PhaseTypes::Opening: ptr = g_nnue; break;
  case PhaseTypes::MiddleGame: ptr = g_nnue2; break;
  case PhaseTypes::LateMiddleGame: ptr = g_nnue3; break;
  case PhaseTypes::Endgame: ptr = g_nnue4; break;
  case PhaseTypes::Sacrifice: ptr = g_nnue5; break;
  default: ptr = g_nnue2;  
  }

  if (!ptr) return 0;  

  const auto output =
      color == Colors::White
          ? screlu_flatten(m_curr->white, m_curr->black, ptr->output_v)
          : screlu_flatten(m_curr->black, m_curr->white, ptr->output_v);

  return (output + ptr->output_bias) * SCALE / QAB;
}

template <bool Activate>
inline void NNUE_State::update_feature(int piece, int square, int phase) {
   
  if (piece == Pieces::Blank || !is_valid_square(square)) {
    return;  
  }

  const auto [white_idx, black_idx] = feature_indices(piece, square);
  
  const NNUE_Params *ptr = nullptr;
  switch (phase) {
  case PhaseTypes::Opening: ptr = g_nnue; break;
  case PhaseTypes::MiddleGame: ptr = g_nnue2; break;
  case PhaseTypes::LateMiddleGame: ptr = g_nnue3; break;
  case PhaseTypes::Endgame: ptr = g_nnue4; break;
  case PhaseTypes::Sacrifice: ptr = g_nnue5; break;
  default: ptr = g_nnue2;  
  }

  if constexpr (Activate) {
   
  if (m_idx < 0 || m_idx > MaxSearchDepth - 1) return;

     
    const size_t white_off = white_idx * static_cast<size_t>(LAYER1_SIZE);
    const size_t black_off = black_idx * static_cast<size_t>(LAYER1_SIZE);
    if (white_off + LAYER1_SIZE > ptr->feature_v.size() ||
        black_off + LAYER1_SIZE > ptr->feature_v.size()) {
      return;  
    }

    add_to_all(m_curr->white, m_curr->white, ptr->feature_v, white_off);
    add_to_all(m_curr->black, m_curr->black, ptr->feature_v, black_off);
  } else {
  if (m_idx < 0 || m_idx > MaxSearchDepth - 1) return;
    const size_t white_off = white_idx * static_cast<size_t>(LAYER1_SIZE);
    const size_t black_off = black_idx * static_cast<size_t>(LAYER1_SIZE);
    if (white_off + LAYER1_SIZE > ptr->feature_v.size() ||
        black_off + LAYER1_SIZE > ptr->feature_v.size()) {
      return;
    }
    subtract_from_all(m_curr->white, m_curr->white, ptr->feature_v, white_off);
    subtract_from_all(m_curr->black, m_curr->black, ptr->feature_v, black_off);
  }
}

void NNUE_State::reset_nnue(const Position &position, int phase) {
  m_curr = &m_accumulator_stack[0];
  m_idx = 0;
  m_idx = 0;
  
  const NNUE_Params *ptr = nullptr;
  switch (phase) {
  case PhaseTypes::Opening: ptr = g_nnue; break;
  case PhaseTypes::MiddleGame: ptr = g_nnue2; break;
  case PhaseTypes::LateMiddleGame: ptr = g_nnue3; break;
  case PhaseTypes::Endgame: ptr = g_nnue4; break;
  case PhaseTypes::Sacrifice: ptr = g_nnue5; break;
  default: ptr = g_nnue2;  
  }
  
  if (!ptr) return;

  m_curr->init(ptr->feature_bias.data());

  for (int square = 0; square < 64; square++) {
    if (position.board[square] != Pieces::Blank) {
      update_feature<true>(position.board[square], square, phase);
    }
  }
}

void NNUE_State::change_phases(const Position &position, int phase) {
  if (m_idx < MaxSearchDepth - 1) {
    ++m_curr;
    ++m_idx;
  }
  
  const NNUE_Params *ptr = nullptr;
  switch (phase) {
  case PhaseTypes::Opening: ptr = g_nnue; break;
  case PhaseTypes::MiddleGame: ptr = g_nnue2; break;
  case PhaseTypes::LateMiddleGame: ptr = g_nnue3; break;
  case PhaseTypes::Endgame: ptr = g_nnue4; break;
  case PhaseTypes::Sacrifice: ptr = g_nnue5; break;
  default: ptr = g_nnue2;  
  }
  if (!ptr) return;

  m_curr->init(ptr->feature_bias.data());

  for (int square = 0; square < 64; square++) {
    if (position.board[square] != Pieces::Blank) {
      update_feature<true>(position.board[square], square, phase);
    }
  }
}

void NNUE_State::reset_and_add_sub_sub(const Position &position, int from_piece, int from,
                             int to_piece, int to, int captured,
                             int captured_sq, int phase) {
   
  m_curr = &m_accumulator_stack[0];
  m_idx = 0;
  
  const NNUE_Params *ptr = nullptr;
  switch (phase) {
    case PhaseTypes::Opening: ptr = g_nnue; break;
    case PhaseTypes::MiddleGame: ptr = g_nnue2; break;
    case PhaseTypes::LateMiddleGame: ptr = g_nnue3; break;
    case PhaseTypes::Endgame: ptr = g_nnue4; break;
    case PhaseTypes::Sacrifice: ptr = g_nnue5; break;
    default: ptr = g_nnue2;  
  }
  
  m_curr->init(ptr->feature_bias.data());

  for (int square = 0; square < 64; square++) {
    if (position.board[square] != Pieces::Blank && square != from && square != to &&
        square != captured_sq) {
      update_feature<true>(position.board[square], square, phase);
    }
  }

  if (from != SquareNone && from_piece != Pieces::Blank) {
    update_feature<true>(from_piece, from, phase);
  }

  if (to != SquareNone && to_piece != Pieces::Blank) {
    update_feature<true>(to_piece, to, phase);
  }
}

 
template <size_t N>
inline void NNUE_State::update_features_batch(const std::array<std::pair<int, int>, N>& features, 
                                              bool activate, int phase) {
  const NNUE_Params *ptr = nullptr;
  switch (phase) {
    case PhaseTypes::Opening: ptr = g_nnue; break;
    case PhaseTypes::MiddleGame: ptr = g_nnue2; break;
    case PhaseTypes::LateMiddleGame: ptr = g_nnue3; break;
    case PhaseTypes::Endgame: ptr = g_nnue4; break;
    case PhaseTypes::Sacrifice: ptr = g_nnue5; break;
    default: ptr = g_nnue2;
  }
  
  for (const auto& [piece, square] : features) {
    if (piece != Pieces::Blank && square != SquareNone) {
       
  if (!ptr || !ptr->feature_v.data()) continue;
  if (m_idx < 0 || m_idx > MaxSearchDepth - 1) continue;

      const auto [white_idx, black_idx] = feature_indices(piece, square);
      const size_t white_off = white_idx * static_cast<size_t>(LAYER1_SIZE);
      const size_t black_off = black_idx * static_cast<size_t>(LAYER1_SIZE);

       
      if (white_off + LAYER1_SIZE > ptr->feature_v.size() || black_off + LAYER1_SIZE > ptr->feature_v.size()) continue;

      if (activate) {
        add_to_all(m_curr->white, m_curr->white, ptr->feature_v, white_off);
        add_to_all(m_curr->black, m_curr->black, ptr->feature_v, black_off);
      } else {
        subtract_from_all(m_curr->white, m_curr->white, ptr->feature_v, white_off);
        subtract_from_all(m_curr->black, m_curr->black, ptr->feature_v, black_off);
      }
    }
  }
}

 
void NNUE_State::reset_nnue_optimized(const Position &position, int phase) {
  m_curr = &m_accumulator_stack[0];
  m_idx = 0;
  
  const NNUE_Params *ptr = nullptr;
  switch (phase) {
    case PhaseTypes::Opening: ptr = g_nnue; break;
    case PhaseTypes::MiddleGame: ptr = g_nnue2; break;
    case PhaseTypes::LateMiddleGame: ptr = g_nnue3; break;
    case PhaseTypes::Endgame: ptr = g_nnue4; break;
    case PhaseTypes::Sacrifice: ptr = g_nnue5; break;
    default: ptr = g_nnue2;
  }
  
   
  __builtin_prefetch(ptr->feature_bias.data(), 0, 3);
  m_curr->init(ptr->feature_bias.data());

   
  std::array<std::pair<int, int>, 32> active_features;
  size_t feature_count = 0;
  
  for (int square = 0; square < 64 && feature_count < 32; square++) {
    if (position.board[square] != Pieces::Blank) {
      active_features[feature_count++] = {position.board[square], square};
    }
  }
  
   
  for (size_t i = 0; i < feature_count; ++i) {
    const auto [piece, square] = active_features[i];
    const auto [white_idx, black_idx] = feature_indices(piece, square);
    
     
    if (i + 1 < feature_count) {
      const auto [next_piece, next_square] = active_features[i + 1];
      const auto [next_white_idx, next_black_idx] = feature_indices(next_piece, next_square);
      __builtin_prefetch(&ptr->feature_v[next_white_idx * LAYER1_SIZE], 0, 3);
    }
    
    add_to_all(m_curr->white, m_curr->white, ptr->feature_v, white_idx * LAYER1_SIZE);
    add_to_all(m_curr->black, m_curr->black, ptr->feature_v, black_idx * LAYER1_SIZE);
  }
}

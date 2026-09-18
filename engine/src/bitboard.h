#pragma once
#include "defs.h"
#include <algorithm>
#include <array>
#include <atomic>
#include <cstdint>
#include <thread>

void init_bbs() noexcept;

enum Square : int {

  a1,
  b1,
  c1,
  d1,
  e1,
  f1,
  g1,
  h1,
  a2,
  b2,
  c2,
  d2,
  e2,
  f2,
  g2,
  h2,
  a3,
  b3,
  c3,
  d3,
  e3,
  f3,
  g3,
  h3,
  a4,
  b4,
  c4,
  d4,
  e4,
  f4,
  g4,
  h4,
  a5,
  b5,
  c5,
  d5,
  e5,
  f5,
  g5,
  h5,
  a6,
  b6,
  c6,
  d6,
  e6,
  f6,
  g6,
  h6,
  a7,
  b7,
  c7,
  d7,
  e7,
  f7,
  g7,
  h7,
  a8,
  b8,
  c8,
  d8,
  e8,
  f8,
  g8,
  h8,
  SqNone
};

constexpr std::array<uint64_t, 8> Ranks = {
    0xFFull,       0xFFull << 8,  0xFFull << 16, 0xFFull << 24,
    0xFFull << 32, 0xFFull << 40, 0xFFull << 48, 0xFFull << 56};

constexpr std::array<uint64_t, 8> Files = {
    0x101010101010101ull,      0x101010101010101ull << 1,
    0x101010101010101ull << 2, 0x101010101010101ull << 3,
    0x101010101010101ull << 4, 0x101010101010101ull << 5,
    0x101010101010101ull << 6, 0x101010101010101ull << 7};

inline MultiArray<uint64_t, 64, 64> BetweenBBs{};

inline std::array<uint64_t, 64> RookMasks{};
inline std::array<uint64_t, 64> BishopMasks{};
inline MultiArray<uint64_t, 64, 512> BishopAttacks{};
inline MultiArray<uint64_t, 64, 4096> RookAttacks{};
inline MultiArray<uint64_t, 2, 64> PawnAttacks{};
inline std::array<uint64_t, 64> KingAttacks{};
inline std::array<uint64_t, 64> KnightAttacks{};

inline std::atomic<bool> BBS_INITIALIZED{false};
inline std::atomic<bool> BBS_INITIALIZING{false};
inline thread_local bool BBS_INIT_IN_THIS_THREAD = false;

inline void ensure_bbs_initialized() noexcept {
  if (BBS_INITIALIZED.load(std::memory_order_acquire)) [[likely]]
    return;
  if (BBS_INIT_IN_THIS_THREAD) [[unlikely]]
    return;

  bool expected = false;
  if (BBS_INITIALIZING.compare_exchange_strong(expected, true,
                                               std::memory_order_acq_rel)) {

    BBS_INIT_IN_THIS_THREAD = true;
    init_bbs();
    BBS_INITIALIZED.store(true, std::memory_order_release);
    BBS_INIT_IN_THIS_THREAD = false;
    BBS_INITIALIZING.store(false, std::memory_order_release);
  } else {

    while (!BBS_INITIALIZED.load(std::memory_order_acquire))
      std::this_thread::yield();
  }
}

constexpr std::array<uint64_t, 64> BishopMagics = {
    0x2020420401002200, 0x05210A020A002118, 0x1110040454C00484,
    0x1008095104080000, 0xC409104004000000, 0x0002901048080200,
    0x0044040402084301, 0x2002030188040200, 0x0000C8084808004A,
    0x1040040808010028, 0x40040C0114090051, 0x40004820802004C4,
    0x0010042420260012, 0x10024202300C010A, 0x000054013D101000,
    0x0100020482188A0A, 0x0120090421020200, 0x1022204444040C00,
    0x0008000400440288, 0x0008060082004040, 0x0044040081A00800,
    0x021200014308A010, 0x8604040080880809, 0x0000802D46009049,
    0x00500E8040080604, 0x0024030030100320, 0x2004100002002440,
    0x02090C0008440080, 0x0205010000104000, 0x0410820405004A00,
    0x8004140261012100, 0x0A00460000820100, 0x201004A40A101044,
    0x840C024220208440, 0x000C002E00240401, 0x2220A00800010106,
    0x88C0080820060020, 0x0818030B00A81041, 0xC091280200110900,
    0x08A8114088804200, 0x228929109000C001, 0x1230480209205000,
    0x0A43040202000102, 0x1011284010444600, 0x0003041008864400,
    0x0115010901000200, 0x01200402C0840201, 0x001A009400822110,
    0x2002111128410000, 0x8420410288203000, 0x0041210402090081,
    0x8220002442120842, 0x0140004010450000, 0xC0408860086488A0,
    0x0090203E00820002, 0x0820020083090024, 0x1040440210900C05,
    0x0818182101082000, 0x0200800080D80800, 0x32A9220510209801,
    0x0000901010820200, 0x0000014064080180, 0xA001204204080186,
    0xC04010040258C048};
constexpr std::array<uint64_t, 64> RookMagics = {
    0x5080008011400020, 0x0140001000402000, 0x0280091000200480,
    0x0700081001002084, 0x0300024408010030, 0x510004004E480100,
    0x0400044128020090, 0x8080004100012080, 0x0220800480C00124,
    0x0020401001C02000, 0x000A002204428050, 0x004E002040100A00,
    0x0102000A00041020, 0x0A0880040080C200, 0x0002000600018408,
    0x0025001200518100, 0x8900328001400080, 0x0848810020400100,
    0xC001410020010153, 0x4110C90020100101, 0x00A0808004004800,
    0x401080801C000601, 0x0100040028104221, 0x840002000900A054,
    0x1000348280004000, 0x001000404000E008, 0x0424410300200035,
    0x2008C22200085200, 0x0005304D00080100, 0x000C040080120080,
    0x8404058400080210, 0x0001848200010464, 0x6000204001800280,
    0x2410004003C02010, 0x0181200A80801000, 0x000C60400A001200,
    0x0B00040180802800, 0xC00A000280804C00, 0x4040080504005210,
    0x0000208402000041, 0xA200400080628000, 0x0021020240820020,
    0x1020027000848022, 0x0020500018008080, 0x10000D0008010010,
    0x0100020004008080, 0x0008020004010100, 0x12241C0880420003,
    0x4000420024810200, 0x0103004000308100, 0x008C200010410300,
    0x2410008050A80480, 0x0820880080040080, 0x0044220080040080,
    0x2040100805120400, 0x0129000080C20100, 0x0010402010800101,
    0x0648A01040008101, 0x0006084102A00033, 0x0002000870C06006,
    0x0082008820100402, 0x0012008410050806, 0x2009408802100144,
    0x821080440020810A};

inline uint64_t KNIGHT_ATK_SAFE(int sq) noexcept {
  if (!is_valid_square(sq)) [[unlikely]]
    return 0ULL;
  ensure_bbs_initialized();
  return KnightAttacks[static_cast<size_t>(sq)];
}

inline uint64_t KING_ATK_SAFE(int sq) noexcept {
  if (!is_valid_square(sq)) [[unlikely]]
    return 0ULL;
  ensure_bbs_initialized();
  return KingAttacks[static_cast<size_t>(sq)];
}

inline uint64_t PAWN_ATK_SAFE(int color, int sq) noexcept {
  if (!is_valid_square(sq)) [[unlikely]]
    return 0ULL;
  const int c = color & 1;
  ensure_bbs_initialized();
  return PawnAttacks[static_cast<size_t>(c)][static_cast<size_t>(sq)];
}

inline uint64_t BISHOP_ATK_SAFE(int sq, uint64_t occ) noexcept {
  if (!is_valid_square(sq)) [[unlikely]]
    return 0ULL;
  const size_t idx_sq = static_cast<size_t>(sq);
  ensure_bbs_initialized();
  const uint64_t mask = BishopMasks[idx_sq];
  const uint64_t index = ((occ & mask) * BishopMagics[idx_sq]) >> 55;
  const size_t attack_index = static_cast<size_t>(index);
  if (attack_index >= BishopAttacks[idx_sq].size()) [[unlikely]]
    return 0ULL;
  return BishopAttacks[idx_sq][attack_index];
}

inline uint64_t ROOK_ATK_SAFE(int sq, uint64_t occ) noexcept {
  if (!is_valid_square(sq)) [[unlikely]]
    return 0ULL;
  const size_t idx_sq = static_cast<size_t>(sq);
  ensure_bbs_initialized();
  const uint64_t mask = RookMasks[idx_sq];
  const uint64_t index = ((occ & mask) * RookMagics[idx_sq]) >> 52;
  const size_t attack_index = static_cast<size_t>(index);
  if (attack_index >= RookAttacks[idx_sq].size()) [[unlikely]]
    return 0ULL;
  return RookAttacks[idx_sq][attack_index];
}

constexpr inline int get_file(int square) noexcept { return square & 7; }
constexpr inline int get_rank(int square) noexcept { return square >> 3; }

constexpr inline uint64_t file_bb(int square) noexcept { return Files[square & 7]; }
constexpr inline uint64_t rank_bb(int square) noexcept { return Ranks[square >> 3]; }

constexpr inline int pop_count(uint64_t bb) noexcept { return __builtin_popcountll(bb); }

constexpr inline int get_lsb(uint64_t bb) noexcept {
  return bb ? __builtin_ctzll(bb) : SqNone;
}

inline int pop_lsb(uint64_t &bb) noexcept {
  if (!bb) [[unlikely]] return SqNone;
  const int s = __builtin_ctzll(bb);
  bb &= (bb - 1);
  return s;
}

inline uint64_t set_occ(int idx, int size, uint64_t mask) noexcept {
  uint64_t occ = 0;

  for (int i = 0; i < size; i++) {
    const int square = pop_lsb(mask);
    if (idx & (1 << i)) {
      occ |= (1ull << square);
    }
  }
  return occ;
}

inline uint64_t bishop_sliders(int square, uint64_t occ) noexcept {
  uint64_t bb = 0;

  constexpr int dirs_file[4] = {1, -1, 1, -1};
  constexpr int dirs_rank[4] = {1, 1, -1, -1};
  for (int i = 0; i < 4; i++) {
    int temp_file = get_file(square) + dirs_file[i];
    int temp_rank = get_rank(square) + dirs_rank[i];

    while (temp_file >= 0 && temp_file <= 7 && temp_rank >= 0 &&
           temp_rank <= 7) {
      const int temp_sq = temp_file + (temp_rank * 8);
      bb |= (1ull << temp_sq);
      if (occ & (1ull << temp_sq)) {
        break;
      }

      temp_file += dirs_file[i];
      temp_rank += dirs_rank[i];
    }
  }

  return bb;
}

inline uint64_t rook_sliders(int square, uint64_t occ) noexcept {
  uint64_t bb = 0;

  constexpr int dirs_file[4] = {0, 0, 1, -1};
  constexpr int dirs_rank[4] = {-1, 1, 0, 0};
  for (int i = 0; i < 4; i++) {
    int temp_file = get_file(square) + dirs_file[i];
    int temp_rank = get_rank(square) + dirs_rank[i];

    while (temp_file >= 0 && temp_file <= 7 && temp_rank >= 0 &&
           temp_rank <= 7) {
      const int temp_sq = temp_file + (temp_rank * 8);
      bb |= (1ull << temp_sq);
      if (occ & (1ull << temp_sq)) {
        break;
      }

      temp_file += dirs_file[i];
      temp_rank += dirs_rank[i];
    }
  }

  return bb;
}

inline void fill_bishop_attacks() noexcept {
  for (int square = a1; square < SqNone; square++) {
    const int bits = pop_count(BishopMasks[square]);
    const int occ_var = 1 << bits;
    for (int i = 0; i < occ_var; i++) {
      const uint64_t occ = set_occ(i, bits, BishopMasks[square]);
      const uint64_t magic_idx = (occ * BishopMagics[square]) >> 55;
      BishopAttacks[square][magic_idx] = bishop_sliders(square, occ);
    }
  }
}

inline void fill_rook_attacks() noexcept {
  for (int square = a1; square < SqNone; square++) {
    const int bits = pop_count(RookMasks[square]);
    const int occ_var = 1 << bits;
    for (int i = 0; i < occ_var; i++) {
      const uint64_t occ = set_occ(i, bits, RookMasks[square]);
      const uint64_t magic_idx = (occ * RookMagics[square]) >> 52;
      RookAttacks[square][magic_idx] = rook_sliders(square, occ);
    }
  }
}

inline void fill_king_attacks() noexcept {
  for (int square = a1; square < SqNone; square++) {
    uint64_t occ = 0;
    const int left = std::max(0, get_file(square) - 1);
    const int right = std::min(7, get_file(square) + 1);
    const int bottom = std::max(0, get_rank(square) - 1);
    const int top = std::min(7, get_rank(square) + 1);

    for (int file = left; file <= right; file++) {
      for (int rank = bottom; rank <= top; rank++) {
        if (file + rank * 8 == square) {
          continue;
        }
        occ |= (1ull << (file + rank * 8));
      }
    }
    KingAttacks[square] = occ;
  }
}

inline void fill_knight_attacks() noexcept {
  constexpr int knight_moves_file[8] = {-2, -2, -1, 1, 2, 2, 1, -1};
  constexpr int knight_moves_rank[8] = {-1, 1, 2, 2, 1, -1, -2, -2};

  for (int square = a1; square < SqNone; square++) {
    uint64_t occ = 0;
    const int s_file = get_file(square), s_rank = get_rank(square);

    for (int i = 0; i < 8; i++) {
      const int file = s_file + knight_moves_file[i];
      const int rank = s_rank + knight_moves_rank[i];

      if (file >= 0 && file <= 7 && rank >= 0 && rank <= 7) {
        occ |= (1ull << (file + rank * 8));
      }
    }

    KnightAttacks[square] = occ;
  }
}

inline void fill_pawn_attacks() noexcept {
  PawnAttacks.fill({});

  for (int square = a1; square <= h7; square++) {
    if (get_file(square) > 0) {
      PawnAttacks[Colors::White][square] |=
          (1ull << (square + Directions::Northwest));
    }
    if (get_file(square) < 7) {
      PawnAttacks[Colors::White][square] |=
          (1ull << (square + Directions::Northeast));
    }
  }

  for (int square = a2; square <= h8; square++) {
    if (get_file(square) > 0) {
      PawnAttacks[Colors::Black][square] |=
          (1ull << (square + Directions::Southwest));
    }
    if (get_file(square) < 7) {
      PawnAttacks[Colors::Black][square] |=
          (1ull << (square + Directions::Southeast));
    }
  }
}

inline uint64_t get_bishop_attacks(int sq, uint64_t occ) noexcept {
  return BISHOP_ATK_SAFE(sq, occ);
}

inline uint64_t get_rook_attacks(int sq, uint64_t occ) noexcept {
  return ROOK_ATK_SAFE(sq, occ);
}

inline uint64_t attackers_to(const Position &position, int sq, int color,
                             uint64_t occupied) noexcept {
  if (!is_valid_square(sq) || (color != Colors::White && color != Colors::Black)) [[unlikely]]
    return 0;
  return position.colors_bb[color] &
      ((PAWN_ATK_SAFE(color ^ 1, sq) & position.pieces_bb[PieceTypes::Pawn]) |
       (KNIGHT_ATK_SAFE(sq) & position.pieces_bb[PieceTypes::Knight]) |
       (get_bishop_attacks(sq, occupied) &
        (position.pieces_bb[PieceTypes::Bishop] | position.pieces_bb[PieceTypes::Queen])) |
       (get_rook_attacks(sq, occupied) &
        (position.pieces_bb[PieceTypes::Rook] | position.pieces_bb[PieceTypes::Queen])) |
       (KING_ATK_SAFE(sq) & position.pieces_bb[PieceTypes::King]));
}

inline void init_bbs() noexcept {
  for (int square = a1; square < SqNone; square++) {
    const uint64_t edges = ((Ranks[0] | Ranks[7]) & ~rank_bb(square)) |
                           ((Files[0] | Files[7]) & ~file_bb(square));

    BishopMasks[square] = bishop_sliders(square, 0) & ~edges;
    RookMasks[square] = rook_sliders(square, 0) & ~edges;
  }

  fill_bishop_attacks();
  fill_rook_attacks();
  fill_king_attacks();
  fill_knight_attacks();
  fill_pawn_attacks();

  for (int square1 = a1; square1 < SqNone; square1++) {
    for (int square2 = a1; square2 < SqNone; square2++) {
      const uint64_t occ = (1ull << square1) | (1ull << square2);

      if (get_bishop_attacks(square1, 0) & (1ull << square2)) {
        BetweenBBs[square1][square2] =
            get_bishop_attacks(square1, occ) & get_bishop_attacks(square2, occ);
      } else if (get_rook_attacks(square1, 0) & (1ull << square2)) {
        BetweenBBs[square1][square2] =
            get_rook_attacks(square1, occ) & get_rook_attacks(square2, occ);
      }

      BetweenBBs[square1][square2] |= (1ull << square2);
    }
  }
}

inline void update_bb(Position &pos, int from_piece, int from, int to_piece, int to,
                      int captured_piece, int capture_sq) noexcept {
  const int color = get_color(from_piece);
  const int from_type = get_piece_type(from_piece);
  const int to_type = get_piece_type(to_piece);
  const int capt_type = get_piece_type(captured_piece);

  pos.colors_bb[color] ^= (1ull << from) | (1ull << to);
  pos.pieces_bb[from_type] ^= (1ull << from);
  pos.pieces_bb[to_type] ^= (1ull << to);

  if (capture_sq != SquareNone) {
    pos.colors_bb[color ^ 1] ^= (1ull << capture_sq);
    pos.pieces_bb[capt_type] ^= (1ull << capture_sq);
  }
}

constexpr inline uint64_t shift_pawns(uint64_t bb, int dir) noexcept {
  if (dir >= 0) {
    return bb << dir;
  } else {
    return bb >> -dir;
  }
}

// Collect the active extra-feature indices for one accumulator perspective.
// flip=false: White view (as-is). flip=true: Black view, i.e. the same rules
// applied to the mirrored board (squares ^56, colors swapped).
// Indices are emitted in ascending order. Returns the count, or -1 when a
// king is missing or out[] (capacity cap) would overflow.
inline int collect_extra_features(const uint8_t board[64],
                                  const uint64_t colors_bb[2],
                                  const uint64_t pieces_bb[7],
                                  bool flip, int *out, int cap) noexcept {
  if (!board || !colors_bb || !pieces_bb || !out || cap < NNUE_EXTRA_SLOTS) return -1;
  ensure_bbs_initialized();
  const uint64_t wbb = colors_bb[0] & pieces_bb[PieceTypes::King];
  const uint64_t bbb = colors_bb[1] & pieces_bb[PieceTypes::King];
  if (!wbb || !bbb) return -1;
  const int kings[2] = {get_lsb(wbb), get_lsb(bbb)};

  int n = 0;
  auto push = [&](size_t idx) -> bool {
    if (n >= cap) return false;
    out[n++] = static_cast<int>(idx);
    return true;
  };

  int mat_count[2][5] = {};
  int complex_count[2][2] = {};
  for (int sq = 0; sq < 64; ++sq) {
    const int piece = board[sq];
    if (piece < 2 || piece > 13) continue;
    const int color = piece & 1;
    const int base = (piece >> 1) - 1;
    if (base <= 4) mat_count[color][base]++;
    if (base == 0) complex_count[color][(get_file(sq) + get_rank(sq)) & 1]++;
  }
  for (int slot_side = 0; slot_side < 2; ++slot_side)
    for (int type = 0; type < 5; ++type) {
      const int real_side = flip ? (slot_side ^ 1) : slot_side;
      if (!push(nnue_material_index(slot_side, type, mat_count[real_side][type]))) return -1;
    }

  for (int slot_king = 0; slot_king < 2; ++slot_king) {
    const int real_king = flip ? (slot_king ^ 1) : slot_king;
    const int king_view = flip ? (kings[real_king] ^ 56) : kings[real_king];
    const int kf = get_file(king_view), kr = get_rank(king_view);
    for (int off = 0; off < 9; ++off) {
      const int tf = kf + (off % 3) - 1, tr = kr + (off / 3) - 1;
      if (tf < 0 || tf > 7 || tr < 0 || tr > 7) continue;
      const int target_real = flip ? ((tr * 8 + tf) ^ 56) : (tr * 8 + tf);
      const int piece = board[target_real];
      const int occ = (piece < 2 || piece > 13) ? 0 : ((flip ? (piece ^ 1) : piece) - 1);
      if (!push(nnue_zone_occ_index(slot_king, off, occ))) return -1;
    }
  }

  Position tmp{};
  tmp.colors_bb[0] = colors_bb[0];
  tmp.colors_bb[1] = colors_bb[1];
  for (int i = 0; i < 7; ++i) tmp.pieces_bb[i] = pieces_bb[i];
  const uint64_t occupied = colors_bb[0] | colors_bb[1];
  for (int slot_king = 0; slot_king < 2; ++slot_king) {
    const int real_king = flip ? (slot_king ^ 1) : slot_king;
    const int king_view = flip ? (kings[real_king] ^ 56) : kings[real_king];
    const int kf = get_file(king_view), kr = get_rank(king_view);
    const int enemy_real = flip ? slot_king : (slot_king ^ 1);
    for (int off = 0; off < 9; ++off) {
      const int tf = kf + (off % 3) - 1, tr = kr + (off / 3) - 1;
      if (tf < 0 || tf > 7 || tr < 0 || tr > 7) continue;
      const int target_real = flip ? ((tr * 8 + tf) ^ 56) : (tr * 8 + tf);
      if (attackers_to(tmp, target_real, enemy_real, occupied))
        if (!push(nnue_zone_atk_index(slot_king, off))) return -1;
    }
  }

  const uint64_t all_pawns = pieces_bb[PieceTypes::Pawn];
  for (int slot_color = 0; slot_color < 2; ++slot_color) {
    const int real_color = flip ? (slot_color ^ 1) : slot_color;
    const uint64_t own_pawns = all_pawns & colors_bb[real_color];
    const uint64_t enemy_pawns = all_pawns & colors_bb[real_color ^ 1];
    int pawn_sq[16];
    int np = 0;
    uint64_t pb = own_pawns;
    while (pb && np < 16) pawn_sq[np++] = pop_lsb(pb);
    if (pb) return -1;
    for (int a = 1; a < np; ++a) {
      const int key = pawn_sq[a];
      const int key_slot = flip ? (key ^ 56) : key;
      int b = a - 1;
      while (b >= 0 && (flip ? (pawn_sq[b] ^ 56) : pawn_sq[b]) > key_slot) {
        pawn_sq[b + 1] = pawn_sq[b];
        --b;
      }
      pawn_sq[b + 1] = key;
    }
    for (int state = 0; state < 3; ++state) {
      for (int i = 0; i < np; ++i) {
        const int real = pawn_sq[i];
        const int sq = flip ? (real ^ 56) : real;
        const int f = get_file(real), r = get_rank(real);
        bool has = false;
        if (state == 0) {
          const uint64_t adj = Files[f] | (f > 0 ? Files[f - 1] : 0ULL) | (f < 7 ? Files[f + 1] : 0ULL);
          uint64_t ahead;
          if (real_color == 0)
            ahead = r >= 7 ? 0ULL : (~0ULL << ((r + 1) * 8));
          else
            ahead = r <= 0 ? 0ULL : ((1ULL << (r * 8)) - 1ULL);
          has = !(enemy_pawns & adj & ahead);
        } else if (state == 1) {
          const uint64_t adj = (f > 0 ? Files[f - 1] : 0ULL) | (f < 7 ? Files[f + 1] : 0ULL);
          has = !(own_pawns & adj);
        } else {
          has = (own_pawns & Files[f] & ~(1ULL << real)) != 0ULL;
        }
        if (has && !push(nnue_pawn_index(slot_color, state, sq))) return -1;
      }
    }
  }

  for (int slot_color = 0; slot_color < 2; ++slot_color) {
    const int real_color = flip ? (slot_color ^ 1) : slot_color;
    const uint64_t own_pawns = all_pawns & colors_bb[real_color];
    int rook_sq[16];
    int nr = 0;
    uint64_t rb = pieces_bb[PieceTypes::Rook] & colors_bb[real_color];
    while (rb && nr < 16) rook_sq[nr++] = pop_lsb(rb);
    if (rb) return -1;
    for (int a = 1; a < nr; ++a) {
      const int key = rook_sq[a];
      const int key_slot = flip ? (key ^ 56) : key;
      int b = a - 1;
      while (b >= 0 && (flip ? (rook_sq[b] ^ 56) : rook_sq[b]) > key_slot) {
        rook_sq[b + 1] = rook_sq[b];
        --b;
      }
      rook_sq[b + 1] = key;
    }
    for (int kind = 0; kind < 2; ++kind) {
      for (int i = 0; i < nr; ++i) {
        const int real = rook_sq[i];
        const int sq = flip ? (real ^ 56) : real;
        const int f = get_file(real);
        const bool ok = kind == 0 ? !(all_pawns & Files[f]) : !(own_pawns & Files[f]);
        if (ok && !push(nnue_rookfile_index(slot_color, kind, sq))) return -1;
      }
    }
  }

  for (int slot_side = 0; slot_side < 2; ++slot_side)
    for (int sc = 0; sc < 2; ++sc) {
      const int real_side = flip ? (slot_side ^ 1) : slot_side;
      const int real_sc = flip ? (sc ^ 1) : sc;
      if (!push(nnue_complex_index(slot_side, sc, complex_count[real_side][real_sc]))) return -1;
    }

  return n;
}

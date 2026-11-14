#pragma once

#include "defs.h"
#include <algorithm>
#include <array>
#include <atomic>
#include <cstdint>
#include <thread>
#include <type_traits>
#include <utility>

void init_bbs() noexcept;

enum Square : int {
  a1, b1, c1, d1, e1, f1, g1, h1,
  a2, b2, c2, d2, e2, f2, g2, h2,
  a3, b3, c3, d3, e3, f3, g3, h3,
  a4, b4, c4, d4, e4, f4, g4, h4,
  a5, b5, c5, d5, e5, f5, g5, h5,
  a6, b6, c6, d6, e6, f6, g6, h6,
  a7, b7, c7, d7, e7, f7, g7, h7,
  a8, b8, c8, d8, e8, f8, g8, h8,
  SqNone
};

inline constexpr std::array<uint64_t, 8> Ranks{
    0x00000000000000FFULL, 0x000000000000FF00ULL,
    0x0000000000FF0000ULL, 0x00000000FF000000ULL,
    0x000000FF00000000ULL, 0x0000FF0000000000ULL,
    0x00FF000000000000ULL, 0xFF00000000000000ULL
};

inline constexpr std::array<uint64_t, 8> Files{
    0x0101010101010101ULL, 0x0202020202020202ULL,
    0x0404040404040404ULL, 0x0808080808080808ULL,
    0x1010101010101010ULL, 0x2020202020202020ULL,
    0x4040404040404040ULL, 0x8080808080808080ULL
};

inline MultiArray<uint64_t, 64, 64> BetweenBBs{};
inline std::array<uint64_t, 64> RookMasks{};
inline std::array<uint64_t, 64> BishopMasks{};
inline MultiArray<uint64_t, 64, 512> BishopAttacks{};
inline MultiArray<uint64_t, 64, 4096> RookAttacks{};
inline MultiArray<uint64_t, 2, 64> PawnAttacks{};
inline std::array<uint64_t, 64> KingAttacks{};
inline std::array<uint64_t, 64> KnightAttacks{};

inline constexpr std::array<uint64_t, 64> BishopMagics{
    0x2020420401002200ull, 0x5210a020a002118ull, 0x1110040454c00484ull, 0x1008095104080000ull,
    0xc409104004000000ull, 0x2901048080200ull, 0x44040402084301ull, 0x2002030188040200ull,
    0xc8084808004aull, 0x1040040808010028ull, 0x40040c0114090051ull, 0x40004820802004c4ull,
    0x10042420260012ull, 0x10024202300c010aull, 0x54013d101000ull, 0x100020482188a0aull,
    0x120090421020200ull, 0x1022204444040c00ull, 0x8000400440288ull, 0x8060082004040ull,
    0x44040081a00800ull, 0x21200014308a010ull, 0x8604040080880809ull, 0x802d46009049ull,
    0x500e8040080604ull, 0x24030030100320ull, 0x2004100002002440ull, 0x2090c0008440080ull,
    0x205010000104000ull, 0x410820405004a00ull, 0x8004140261012100ull, 0xa00460000820100ull,
    0x201004a40a101044ull, 0x840c024220208440ull, 0xc002e00240401ull, 0x2220a00800010106ull,
    0x88c0080820060020ull, 0x818030b00a81041ull, 0xc091280200110900ull, 0x8a8114088804200ull,
    0x228929109000c001ull, 0x1230480209205000ull, 0xa43040202000102ull, 0x1011284010444600ull,
    0x3041008864400ull, 0x115010901000200ull, 0x1200402c0840201ull, 0x1a009400822110ull,
    0x2002111128410000ull, 0x8420410288203000ull, 0x41210402090081ull, 0x8220002442120842ull,
    0x140004010450000ull, 0xc0408860086488a0ull, 0x90203e00820002ull, 0x820020083090024ull,
    0x1040440210900c05ull, 0x818182101082000ull, 0x200800080d80800ull, 0x32a9220510209801ull,
    0x901010820200ull, 0x14064080180ull, 0xa001204204080186ull, 0xc04010040258c048ull
};

inline constexpr std::array<uint64_t, 64> RookMagics{
    0x5080008011400020ull, 0x140001000402000ull, 0x280091000200480ull, 0x700081001002084ull,
    0x300024408010030ull, 0x510004004e480100ull, 0x400044128020090ull, 0x8080004100012080ull,
    0x220800480c00124ull, 0x20401001c02000ull, 0xa002204428050ull, 0x4e002040100a00ull,
    0x102000a00041020ull, 0xa0880040080c200ull, 0x2000600018408ull, 0x25001200518100ull,
    0x8900328001400080ull, 0x848810020400100ull, 0xc001410020010153ull, 0x4110c90020100101ull,
    0xa0808004004800ull, 0x401080801c000601ull, 0x100040028104221ull, 0x840002000900a054ull,
    0x1000348280004000ull, 0x1000404000e008ull, 0x424410300200035ull, 0x2008c22200085200ull,
    0x5304d00080100ull, 0xc040080120080ull, 0x8404058400080210ull, 0x1848200010464ull,
    0x6000204001800280ull, 0x2410004003c02010ull, 0x181200a80801000ull, 0xc60400a001200ull,
    0xb00040180802800ull, 0xc00a000280804c00ull, 0x4040080504005210ull, 0x208402000041ull,
    0xa200400080628000ull, 0x21020240820020ull, 0x1020027000848022ull, 0x20500018008080ull,
    0x10000d0008010010ull, 0x100020004008080ull, 0x8020004010100ull, 0x12241c0880420003ull,
    0x4000420024810200ull, 0x103004000308100ull, 0x8c200010410300ull, 0x2410008050a80480ull,
    0x820880080040080ull, 0x44220080040080ull, 0x2040100805120400ull, 0x129000080c20100ull,
    0x10402010800101ull, 0x648a01040008101ull, 0x6084102a00033ull, 0x2000870c06006ull,
    0x82008820100402ull, 0x12008410050806ull, 0x2009408802100144ull, 0x821080440020810aull
};

[[nodiscard]] constexpr int get_file(int sq) noexcept { return sq & 7; }
[[nodiscard]] constexpr int get_rank(int sq) noexcept { return sq >> 3; }

[[nodiscard]] constexpr uint64_t file_bb(int sq) noexcept {
  return Files[static_cast<size_t>(get_file(sq))];
}

[[nodiscard]] constexpr uint64_t rank_bb(int sq) noexcept {
  return Ranks[static_cast<size_t>(get_rank(sq))];
}

[[nodiscard]] constexpr int pop_count(uint64_t mask) noexcept {
  return __builtin_popcountll(mask);
}

[[nodiscard]] constexpr int get_lsb(uint64_t mask) noexcept {
  return mask != 0ULL ? __builtin_ctzll(mask) : SqNone;
}

inline int pop_lsb(uint64_t &mask) noexcept {
  if (mask == 0ULL) return SqNone;
  const int index = __builtin_ctzll(mask);
  mask &= mask - 1ULL;
  return index;
}

[[nodiscard]] inline uint64_t KNIGHT_ATK_SAFE(int sq) noexcept {
  return is_valid_square(sq) ? KnightAttacks[static_cast<size_t>(sq)] : 0ULL;
}

[[nodiscard]] inline uint64_t KING_ATK_SAFE(int sq) noexcept {
  return is_valid_square(sq) ? KingAttacks[static_cast<size_t>(sq)] : 0ULL;
}

[[nodiscard]] inline uint64_t PAWN_ATK_SAFE(int side, int sq) noexcept {
  if (!is_valid_square(sq)) [[unlikely]] return 0ULL;
  return PawnAttacks[static_cast<size_t>(side & 1)][static_cast<size_t>(sq)];
}

[[nodiscard]] inline uint64_t BISHOP_ATK_SAFE(int sq, uint64_t occ) noexcept {
  if (!is_valid_square(sq)) [[unlikely]] return 0ULL;
  const size_t s = static_cast<size_t>(sq);
  // A 64-bit product shifted by 55 is below 512, the table width.
  static_assert(std::tuple_size_v<std::remove_reference_t<decltype(BishopAttacks[0])>> == (1u << (64 - 55)));
  return BishopAttacks[s][((occ & BishopMasks[s]) * BishopMagics[s]) >> 55U];
}

[[nodiscard]] inline uint64_t ROOK_ATK_SAFE(int sq, uint64_t occ) noexcept {
  if (!is_valid_square(sq)) [[unlikely]] return 0ULL;
  const size_t s = static_cast<size_t>(sq);
  // A 64-bit product shifted by 52 is below 4096, the table width.
  static_assert(std::tuple_size_v<std::remove_reference_t<decltype(RookAttacks[0])>> == (1u << (64 - 52)));
  return RookAttacks[s][((occ & RookMasks[s]) * RookMagics[s]) >> 52U];
}

[[nodiscard]] inline uint64_t get_bishop_attacks(int sq, uint64_t occ) noexcept {
  return BISHOP_ATK_SAFE(sq, occ);
}

[[nodiscard]] inline uint64_t get_rook_attacks(int sq, uint64_t occ) noexcept {
  return ROOK_ATK_SAFE(sq, occ);
}

namespace detail_bb {

[[nodiscard]] inline uint64_t unpack_mask_bits(int subset_idx, int bit_count,
                                               uint64_t mask) noexcept {
  uint64_t occupancy = 0ULL;
  for (int bit = 0; bit < bit_count; ++bit) {
    const int sq = pop_lsb(mask);
    if ((subset_idx & (1 << bit)) != 0) {
      occupancy |= 1ULL << sq;
    }
  }
  return occupancy;
}

[[nodiscard]] inline uint64_t trace_slider_rays(int origin, uint64_t blockers,
                                                const int (&df)[4],
                                                const int (&dr)[4]) noexcept {
  uint64_t rays = 0ULL;
  const int f0 = get_file(origin);
  const int r0 = get_rank(origin);
  for (int d = 0; d < 4; ++d) {
    int f = f0 + df[d];
    int r = r0 + dr[d];
    while (static_cast<unsigned>(f) < 8U && static_cast<unsigned>(r) < 8U) {
      const uint64_t step_bit = 1ULL << (r * 8 + f);
      rays |= step_bit;
      if ((blockers & step_bit) != 0ULL) break;
      f += df[d];
      r += dr[d];
    }
  }
  return rays;
}

inline constexpr int kDiagFile[4]{1, -1, 1, -1};
inline constexpr int kDiagRank[4]{1, 1, -1, -1};
inline constexpr int kOrthFile[4]{0, 0, 1, -1};
inline constexpr int kOrthRank[4]{-1, 1, 0, 0};

template <typename Table>
inline void populate_magic_table(const std::array<uint64_t, 64> &masks,
                                 const std::array<uint64_t, 64> &magics,
                                 unsigned shift_bits, Table &table,
                                 const int (&df)[4],
                                 const int (&dr)[4]) noexcept {
  for (int sq = a1; sq < SqNone; ++sq) {
    const uint64_t relevant_mask = masks[static_cast<size_t>(sq)];
    const int bit_count = pop_count(relevant_mask);
    const int subset_limit = 1 << bit_count;
    for (int subset = 0; subset < subset_limit; ++subset) {
      const uint64_t occ = unpack_mask_bits(subset, bit_count, relevant_mask);
      const uint64_t idx = (occ * magics[static_cast<size_t>(sq)]) >> shift_bits;
      table[static_cast<size_t>(sq)][static_cast<size_t>(idx)] =
          trace_slider_rays(sq, occ, df, dr);
    }
  }
}

inline void populate_leaper_tables() noexcept {
  struct Offset {
    int df;
    int dr;
  };
  static constexpr std::array<Offset, 8> kKnightOffsets{{
      {-2, -1}, {-2, 1}, {-1, 2}, {1, 2},
      {2, 1},   {2, -1}, {1, -2}, {-1, -2}
  }};

  PawnAttacks.fill({});
  for (int sq = a1; sq < SqNone; ++sq) {
    const int f0 = get_file(sq);
    const int r0 = get_rank(sq);

    uint64_t king_mask = 0ULL;
    for (int dr = -1; dr <= 1; ++dr) {
      for (int df = -1; df <= 1; ++df) {
        if (df == 0 && dr == 0) continue;
        const int nf = f0 + df;
        const int nr = r0 + dr;
        if (static_cast<unsigned>(nf) < 8U && static_cast<unsigned>(nr) < 8U) {
          king_mask |= 1ULL << (nr * 8 + nf);
        }
      }
    }
    KingAttacks[static_cast<size_t>(sq)] = king_mask;

    uint64_t knight_mask = 0ULL;
    for (const auto [df, dr] : kKnightOffsets) {
      const int nf = f0 + df;
      const int nr = r0 + dr;
      if (static_cast<unsigned>(nf) < 8U && static_cast<unsigned>(nr) < 8U) {
        knight_mask |= 1ULL << (nr * 8 + nf);
      }
    }
    KnightAttacks[static_cast<size_t>(sq)] = knight_mask;

    if (sq <= h7) {
      if (f0 > 0) PawnAttacks[Colors::White][static_cast<size_t>(sq)] |= 1ULL << (sq + Directions::Northwest);
      if (f0 < 7) PawnAttacks[Colors::White][static_cast<size_t>(sq)] |= 1ULL << (sq + Directions::Northeast);
    }
    if (sq >= a2) {
      if (f0 > 0) PawnAttacks[Colors::Black][static_cast<size_t>(sq)] |= 1ULL << (sq + Directions::Southwest);
      if (f0 < 7) PawnAttacks[Colors::Black][static_cast<size_t>(sq)] |= 1ULL << (sq + Directions::Southeast);
    }
  }
}

}  // namespace detail_bb

[[nodiscard]] inline uint64_t attackers_to(const Position &pos, int target_sq,
                                           int attacker_side,
                                           uint64_t occupied_mask) noexcept {
  if (!is_valid_square(target_sq) ||
      (attacker_side != Colors::White && attacker_side != Colors::Black)) [[unlikely]] {
    return 0ULL;
  }
  const uint64_t diag_pieces =
      pos.pieces_bb[PieceTypes::Bishop] | pos.pieces_bb[PieceTypes::Queen];
  const uint64_t orth_pieces =
      pos.pieces_bb[PieceTypes::Rook] | pos.pieces_bb[PieceTypes::Queen];
  const uint64_t hit_mask =
      (PAWN_ATK_SAFE(attacker_side ^ 1, target_sq) & pos.pieces_bb[PieceTypes::Pawn]) |
      (KNIGHT_ATK_SAFE(target_sq) & pos.pieces_bb[PieceTypes::Knight]) |
      (get_bishop_attacks(target_sq, occupied_mask) & diag_pieces) |
      (get_rook_attacks(target_sq, occupied_mask) & orth_pieces) |
      (KING_ATK_SAFE(target_sq) & pos.pieces_bb[PieceTypes::King]);
  return pos.colors_bb[static_cast<size_t>(attacker_side)] & hit_mask;
}

// All attackers of both colors on sq for the given occupancy.
[[nodiscard]] inline uint64_t attackers_all(const Position &pos, int target_sq,
                                            uint64_t occupied_mask) noexcept {
  if (!is_valid_square(target_sq)) return 0ULL;
  const uint64_t diag_pieces =
      pos.pieces_bb[PieceTypes::Bishop] | pos.pieces_bb[PieceTypes::Queen];
  const uint64_t orth_pieces =
      pos.pieces_bb[PieceTypes::Rook] | pos.pieces_bb[PieceTypes::Queen];
  const uint64_t pawn_hits =
      (PAWN_ATK_SAFE(Colors::White, target_sq) & pos.colors_bb[Colors::Black] &
       pos.pieces_bb[PieceTypes::Pawn]) |
      (PAWN_ATK_SAFE(Colors::Black, target_sq) & pos.colors_bb[Colors::White] &
       pos.pieces_bb[PieceTypes::Pawn]);
  return pawn_hits |
         (KNIGHT_ATK_SAFE(target_sq) & pos.pieces_bb[PieceTypes::Knight]) |
         (get_bishop_attacks(target_sq, occupied_mask) & diag_pieces) |
         (get_rook_attacks(target_sq, occupied_mask) & orth_pieces) |
         (KING_ATK_SAFE(target_sq) & pos.pieces_bb[PieceTypes::King]);
}

[[nodiscard]] inline bool SEE(const Position &pos, Move encoded_move,
                              int threshold) noexcept {
  const int origin = extract_from(encoded_move);
  const int target = extract_to(encoded_move);
  if (!is_valid_square(origin) || !is_valid_square(target)) return false;

  const int mover_code = pos.board[static_cast<size_t>(origin)];
  if (mover_code == Pieces::Blank || get_color(mover_code) != pos.color) return false;

  const int kind = extract_type(encoded_move);
  if (kind == MoveTypes::Castling) return threshold <= 0;

  const bool is_ep = (kind == MoveTypes::EnPassant);
  const bool is_promo = (kind == MoveTypes::Promotion);
  const int placed_type = is_promo
      ? (extract_promo(encoded_move) + PieceTypes::Knight)
      : get_piece_type(mover_code);
  const int captured_type = is_ep
      ? static_cast<int>(PieceTypes::Pawn)
      : get_piece_type(pos.board[static_cast<size_t>(target)]);

  int balance = SeeValues[static_cast<size_t>(captured_type)] - threshold;
  if (is_promo) {
    balance += SeeValues[static_cast<size_t>(placed_type)] - SeeValues[PieceTypes::Pawn];
  }
  if (balance < 0) return false;

  balance -= SeeValues[static_cast<size_t>(placed_type)];
  if (balance >= 0) return true;

  const uint64_t diag_pieces =
      pos.pieces_bb[PieceTypes::Bishop] | pos.pieces_bb[PieceTypes::Queen];
  const uint64_t orth_pieces =
      pos.pieces_bb[PieceTypes::Rook] | pos.pieces_bb[PieceTypes::Queen];

  uint64_t occ =
      (pos.colors_bb[Colors::White] | pos.colors_bb[Colors::Black]) ^ (1ULL << origin);
  if (is_ep) {
    const int ep_victim = target + (pos.color != 0 ? Directions::North : Directions::South);
    occ &= ~(1ULL << ep_victim);
  }

  uint64_t pending_attackers = attackers_all(pos, target, occ);
  int side = pos.color;

  for (;;) {
    side ^= 1;
    pending_attackers &= occ;

    const uint64_t side_attackers = pending_attackers & pos.colors_bb[static_cast<size_t>(side)];
    if (side_attackers == 0ULL) return side != pos.color;

    int least_type = PieceTypes::PieceNone;
    for (int pt = PieceTypes::Pawn; pt <= PieceTypes::King; ++pt) {
      const uint64_t candidates = side_attackers & pos.pieces_bb[static_cast<size_t>(pt)];
      if (candidates == 0ULL) continue;
      const int sq = get_lsb(candidates);
      if (!is_valid_square(sq)) return false;
      occ ^= 1ULL << sq;
      least_type = pt;
      break;
    }
    if (least_type == PieceTypes::PieceNone) return false;

    if (least_type == PieceTypes::Pawn ||
        least_type == PieceTypes::Bishop ||
        least_type == PieceTypes::Queen) {
      pending_attackers |= get_bishop_attacks(target, occ) & diag_pieces;
    }
    if (least_type == PieceTypes::Rook || least_type == PieceTypes::Queen) {
      pending_attackers |= get_rook_attacks(target, occ) & orth_pieces;
    }

    if (least_type == PieceTypes::King &&
        (attackers_all(pos, target, occ) & occ &
         pos.colors_bb[static_cast<size_t>(side ^ 1)]) != 0ULL) {
      return side != pos.color;
    }

    balance = -balance - SeeValues[static_cast<size_t>(least_type)] - 1;
    if (balance >= 0) return side == pos.color;
  }
}

inline void init_bbs() noexcept {
  for (int sq = a1; sq < SqNone; ++sq) {
    const uint64_t perimeter = ((Ranks[0] | Ranks[7]) & ~rank_bb(sq)) |
                               ((Files[0] | Files[7]) & ~file_bb(sq));
    BishopMasks[static_cast<size_t>(sq)] =
        detail_bb::trace_slider_rays(sq, 0ULL, detail_bb::kDiagFile, detail_bb::kDiagRank) & ~perimeter;
    RookMasks[static_cast<size_t>(sq)] =
        detail_bb::trace_slider_rays(sq, 0ULL, detail_bb::kOrthFile, detail_bb::kOrthRank) & ~perimeter;
  }

  detail_bb::populate_magic_table(BishopMasks, BishopMagics, 55U, BishopAttacks,
                                  detail_bb::kDiagFile, detail_bb::kDiagRank);
  detail_bb::populate_magic_table(RookMasks, RookMagics, 52U, RookAttacks,
                                  detail_bb::kOrthFile, detail_bb::kOrthRank);
  detail_bb::populate_leaper_tables();

  for (int s1 = a1; s1 < SqNone; ++s1) {
    for (int s2 = a1; s2 < SqNone; ++s2) {
      const uint64_t endpoints = (1ULL << s1) | (1ULL << s2);
      uint64_t span = 0ULL;
      if ((get_bishop_attacks(s1, 0ULL) & (1ULL << s2)) != 0ULL) {
        span = get_bishop_attacks(s1, endpoints) & get_bishop_attacks(s2, endpoints);
      } else if ((get_rook_attacks(s1, 0ULL) & (1ULL << s2)) != 0ULL) {
        span = get_rook_attacks(s1, endpoints) & get_rook_attacks(s2, endpoints);
      }
      BetweenBBs[static_cast<size_t>(s1)][static_cast<size_t>(s2)] = span | (1ULL << s2);
    }
  }
}

// Attack tables are filled once during static initialization of any program
// or library that includes this header, before any lookup can run.
inline const bool BitboardsReady = (init_bbs(), true);

inline void update_bb(Position &pos, int mover_code, int origin, int placed_code,
                      int target, int captured_code, int victim_sq) noexcept {
  const size_t side = get_color(static_cast<uint8_t>(mover_code));
  const uint64_t toggle = (1ULL << origin) | (1ULL << target);
  pos.colors_bb[side] ^= toggle;
  pos.pieces_bb[static_cast<size_t>(get_piece_type(mover_code))] ^= (1ULL << origin);
  pos.pieces_bb[static_cast<size_t>(get_piece_type(placed_code))] ^= (1ULL << target);

  if (victim_sq != SquareNone) {
    const uint64_t cap_bit = 1ULL << victim_sq;
    pos.colors_bb[side ^ 1U] ^= cap_bit;
    pos.pieces_bb[static_cast<size_t>(get_piece_type(captured_code))] ^= cap_bit;
  }
}

[[nodiscard]] constexpr uint64_t shift_pawns(uint64_t pawns, int offset) noexcept {
  return offset >= 0 ? (pawns << offset) : (pawns >> (-offset));
}

#pragma once
#include "defs.h"
#include "position.h"
#include <algorithm>
#include <array>
#include <cstdint>

namespace Generate {
constexpr uint8_t GenQuiets = 0;
constexpr uint8_t GenCaptures = 1;
constexpr uint8_t GenAll = 2;
}

constexpr int QueenPromoScore = 5000000;
constexpr int GoodCaptureBaseScore = 2000000;
constexpr int KillerMoveScore = 100000;

inline void pawn_moves(const Position &position, uint64_t check_filter,
                       Move *move_list, int &key, int gen_type) noexcept {

  const uint8_t color = position.color;
  const uint64_t third_rank = color ? Ranks[5] : Ranks[2];
  const uint64_t seventh_rank = color ? Ranks[1] : Ranks[6];
  const int8_t dir = color ? Directions::South : Directions::North;
  const int8_t left = color ? Directions::Southwest : Directions::Northwest;
  const int8_t right = color ? Directions::Southeast : Directions::Northeast;

  const uint64_t empty_squares = ~(position.colors_bb[0] | position.colors_bb[1]);
  const uint64_t our_promos = position.pieces_bb[PieceTypes::Pawn] &
                              position.colors_bb[color] & seventh_rank;
  const uint64_t our_non_promos = position.pieces_bb[PieceTypes::Pawn] &
                                  position.colors_bb[color] & (~seventh_rank);

  if (gen_type != Generate::GenCaptures) {
    uint64_t move_1 = shift_pawns(our_non_promos, dir) & empty_squares;
    uint64_t move_2 =
        shift_pawns(move_1 & third_rank, dir) & empty_squares & check_filter;
    move_1 &= check_filter;

    while (move_1) {
      const int to = pop_lsb(move_1);
      if (key < ListSize) {
        move_list[key++] = pack_move(to - dir, to, MoveTypes::Normal);
      }
    }
    while (move_2) {
      const int to = pop_lsb(move_2);
      if (key < ListSize) {
        move_list[key++] = pack_move(to - (2 * dir), to, MoveTypes::Normal);
      }
    }
  }

  if (gen_type != Generate::GenQuiets) {
    uint64_t cap_left = shift_pawns(our_non_promos & ~Files[0], left) &
                        position.colors_bb[color ^ 1] & check_filter;
    uint64_t cap_right = shift_pawns(our_non_promos & ~Files[7], right) &
                         position.colors_bb[color ^ 1] & check_filter;

    while (cap_left) {
      const int to = pop_lsb(cap_left);
      if (key < ListSize) {
        move_list[key++] = pack_move(to - left, to, MoveTypes::Normal);
      }
    }
    while (cap_right) {
      const int to = pop_lsb(cap_right);
      if (key < ListSize) {
        move_list[key++] = pack_move(to - right, to, MoveTypes::Normal);
      }
    }

    if (position.ep_square != SquareNone) {
      const uint64_t ep_targets = PAWN_ATK_SAFE(color ^ 1, position.ep_square);
      uint64_t ep_captures = our_non_promos & ep_targets;
      while (ep_captures) {
        const int from = pop_lsb(ep_captures);
        if (key < ListSize) {
          move_list[key++] =
              pack_move(from, position.ep_square, MoveTypes::EnPassant);
        }
      }
    }
  }

  const uint64_t move_promo =
      shift_pawns(our_promos, dir) & empty_squares & check_filter;
  uint64_t cap_left_promo = shift_pawns(our_promos & ~Files[0], left) &
                            position.colors_bb[color ^ 1] & check_filter;
  uint64_t cap_right_promo = shift_pawns(our_promos & ~Files[7], right) &
                             position.colors_bb[color ^ 1] & check_filter;

  auto safe_push = [&](Move m) noexcept {
    if (key < ListSize)
      move_list[key++] = m;
  };

  if (gen_type != Generate::GenQuiets) {
    uint64_t promo_pushes = move_promo;
    while (promo_pushes) {
      const int to = pop_lsb(promo_pushes);
      for (int i = 0; i < 4; i++) {
        safe_push(pack_move_promo(to - dir, to, i));
      }
    }
    while (cap_left_promo) {
      const int to = pop_lsb(cap_left_promo);
      for (int i = 0; i < 4; i++) {
        safe_push(pack_move_promo(to - left, to, i));
      }
    }
    while (cap_right_promo) {
      const int to = pop_lsb(cap_right_promo);
      for (int i = 0; i < 4; i++) {
        safe_push(pack_move_promo(to - right, to, i));
      }
    }
  }
}

inline int movegen(const Position &position, Move *move_list, uint64_t checkers,
                   int gen_type) noexcept {

  const uint8_t color = position.color;
  const int king_pos = get_king_pos(position, color);
  int idx = 0;
  const uint64_t stm_pieces = position.colors_bb[color];
  const uint64_t opp_pieces = position.colors_bb[color ^ 1];

  uint64_t targets = 0;
  if (gen_type != Generate::GenCaptures) {
    targets |= ~opp_pieces;
  }
  if (gen_type != Generate::GenQuiets) {
    targets |= opp_pieces;
  }
  targets &= ~stm_pieces;

  const uint64_t occ = position.colors_bb[0] | position.colors_bb[1];
  uint64_t check_filter = ~0ULL;

  if (!is_valid_square(king_pos)) {
    return idx;
  }
  uint64_t king_attacks = KING_ATK_SAFE(king_pos) & targets;
  while (king_attacks) {
    const int to = pop_lsb(king_attacks);
    if (idx < ListSize)
      move_list[idx++] = pack_move(king_pos, to, MoveTypes::Normal);
  }

  if (checkers) {
    if (checkers & (checkers - 1)) {
      return idx;
    }

    const int checker_sq = get_lsb(checkers);
    if (!is_valid_square(checker_sq)) {
      return idx;
    }
    check_filter = BetweenBBs[king_pos][checker_sq];
  }

  pawn_moves(position, check_filter, move_list, idx, gen_type);

  uint64_t knights = position.pieces_bb[PieceTypes::Knight] & stm_pieces;
  while (knights) {
    const int from = pop_lsb(knights);
    uint64_t to = KNIGHT_ATK_SAFE(from) & targets & check_filter;
    while (to) {
      if (idx < ListSize)
        move_list[idx++] = pack_move(from, pop_lsb(to), MoveTypes::Normal);
      else
        pop_lsb(to);
    }
  }

  uint64_t diagonals = (position.pieces_bb[PieceTypes::Bishop] |
                        position.pieces_bb[PieceTypes::Queen]) &
                       stm_pieces;
  while (diagonals) {
    const int from = pop_lsb(diagonals);
    uint64_t to = get_bishop_attacks(from, occ) & targets & check_filter;
    while (to) {
      if (idx < ListSize)
        move_list[idx++] = pack_move(from, pop_lsb(to), MoveTypes::Normal);
      else
        pop_lsb(to);
    }
  }

  uint64_t orthogonals = (position.pieces_bb[PieceTypes::Rook] |
                          position.pieces_bb[PieceTypes::Queen]) &
                         stm_pieces;
  while (orthogonals) {
    const int from = pop_lsb(orthogonals);
    uint64_t to = get_rook_attacks(from, occ) & targets & check_filter;
    while (to) {
      if (idx < ListSize)
        move_list[idx++] = pack_move(from, pop_lsb(to), MoveTypes::Normal);
      else
        pop_lsb(to);
    }
  }

  if (checkers || gen_type == Generate::GenCaptures) {
    return idx;
  }

  for (int side : {Sides::Queenside, Sides::Kingside}) {
    const int castling_sq = position.castling_squares[color][side];
    if (castling_sq == SquareNone || !is_valid_square(castling_sq)) {
      continue;
    }

    if (can_castle(position, king_pos, castling_sq) && idx < ListSize)
      move_list[idx++] = pack_move(king_pos, castling_sq, MoveTypes::Castling);
  }

  return idx;
}

inline int legal_movegen(const Position &position, Move *move_list) {
  const uint64_t checkers = attacks_square(
      position, get_king_pos(position, position.color), position.color ^ 1);
  std::array<Move, ListSize> pseudo_list;
  const int pseudo_nmoves =
      movegen(position, pseudo_list.data(), checkers, Generate::GenAll);

  int legal_nmoves = 0;
  for (int i = 0; i < pseudo_nmoves; i++) {
    if (is_legal(position, pseudo_list[i]))
      move_list[legal_nmoves++] = pseudo_list[i];
  }

  return legal_nmoves;
}

inline Move get_next_move(Move *moves, int *scores, int start_idx, int len) noexcept {
  int best_idx = start_idx, best_score = scores[start_idx];
  for (int i = start_idx + 1; i < len; i++) {
    if (scores[i] > best_score) {
      best_score = scores[i];
      best_idx = i;
    }
  }
  std::swap(moves[start_idx], moves[best_idx]);
  std::swap(scores[start_idx], scores[best_idx]);

  return moves[start_idx];
}

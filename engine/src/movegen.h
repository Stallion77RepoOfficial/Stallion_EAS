#pragma once

#include "defs.h"
#include "position.h"
#include <algorithm>
#include <array>
#include <cstdint>

namespace Generate {
enum : uint8_t { GenQuiets = 0, GenCaptures = 1, GenAll = 2 };
}  // namespace Generate

inline constexpr int QueenPromoScore = 5000000;
inline constexpr int GoodCaptureBaseScore = 2000000;
inline constexpr int KillerMoveScore = 100000;

inline void pawn_moves(const Position &pos, uint64_t evasion_mask,
                       Move *out_moves, int &cursor, int gen_mode) noexcept {
  const uint8_t us = pos.color;
  const uint8_t them = us ^ 1U;
  const uint64_t third_rank = us ? Ranks[5] : Ranks[2];
  const uint64_t seventh_rank = us ? Ranks[1] : Ranks[6];
  const int8_t step_fwd = us ? Directions::South : Directions::North;
  const int8_t diag_left = us ? Directions::Southwest : Directions::Northwest;
  const int8_t diag_right = us ? Directions::Southeast : Directions::Northeast;

  const uint64_t occupied = pos.colors_bb[0] | pos.colors_bb[1];
  const uint64_t vacant = ~occupied;
  const uint64_t friendly_pawns =
      pos.pieces_bb[PieceTypes::Pawn] & pos.colors_bb[us];
  const uint64_t promo_pawns = friendly_pawns & seventh_rank;
  const uint64_t regular_pawns = friendly_pawns & ~seventh_rank;

  if (gen_mode != Generate::GenCaptures) {
    uint64_t single_push = shift_pawns(regular_pawns, step_fwd) & vacant;
    uint64_t double_push =
        shift_pawns(single_push & third_rank, step_fwd) & vacant & evasion_mask;
    single_push &= evasion_mask;

    while (single_push != 0ULL) {
      const int dst = pop_lsb(single_push);
      out_moves[cursor++] = pack_move(dst - step_fwd, dst, MoveTypes::Normal);
    }
    while (double_push != 0ULL) {
      const int dst = pop_lsb(double_push);
      out_moves[cursor++] = pack_move(dst - 2 * step_fwd, dst, MoveTypes::Normal);
    }
  }

  if (gen_mode != Generate::GenQuiets) {
    const uint64_t enemy_mask = pos.colors_bb[them] & evasion_mask;
    auto emit_pawn_captures = [&](uint64_t sources, int8_t delta) noexcept {
      uint64_t hits = shift_pawns(sources, delta) & enemy_mask;
      while (hits != 0ULL) {
        const int dst = pop_lsb(hits);
        out_moves[cursor++] = pack_move(dst - delta, dst, MoveTypes::Normal);
      }
    };
    emit_pawn_captures(regular_pawns & ~Files[0], diag_left);
    emit_pawn_captures(regular_pawns & ~Files[7], diag_right);

    if (pos.ep_square != SquareNone) {
      uint64_t ep_sources =
          regular_pawns & PAWN_ATK_SAFE(them, pos.ep_square);
      while (ep_sources != 0ULL) {
        const int src = pop_lsb(ep_sources);
        out_moves[cursor++] =
            pack_move(src, pos.ep_square, MoveTypes::EnPassant);
      }
    }

    auto emit_promotions = [&](uint64_t targets, int8_t delta) noexcept {
      while (targets != 0ULL) {
        const int dst = pop_lsb(targets);
        for (int promo = 0; promo < 4; ++promo) {
          out_moves[cursor++] = pack_move_promo(dst - delta, dst, promo);
        }
      }
    };
    emit_promotions(shift_pawns(promo_pawns, step_fwd) & vacant & evasion_mask,
                    step_fwd);
    emit_promotions(shift_pawns(promo_pawns & ~Files[0], diag_left) & enemy_mask,
                    diag_left);
    emit_promotions(shift_pawns(promo_pawns & ~Files[7], diag_right) & enemy_mask,
                    diag_right);
  }
}

inline int movegen(const Position &pos, Move *out_moves, uint64_t check_bb,
                   int gen_mode) noexcept {
  const uint8_t us = pos.color;
  const int king_sq = get_king_pos(pos, us);
  if (!is_valid_square(king_sq)) return 0;

  const uint64_t friendly = pos.colors_bb[us];
  const uint64_t enemy = pos.colors_bb[us ^ 1U];
  uint64_t target_mask = 0ULL;
  if (gen_mode != Generate::GenCaptures) target_mask |= ~enemy;
  if (gen_mode != Generate::GenQuiets) target_mask |= enemy;
  target_mask &= ~friendly;

  int cursor = 0;
  uint64_t king_dests = KING_ATK_SAFE(king_sq) & target_mask;
  while (king_dests != 0ULL) {
    out_moves[cursor++] =
        pack_move(king_sq, pop_lsb(king_dests), MoveTypes::Normal);
  }

  uint64_t evasion_mask = ~0ULL;
  if (check_bb != 0ULL) {
    if ((check_bb & (check_bb - 1ULL)) != 0ULL) return cursor;
    const int checker_sq = get_lsb(check_bb);
    if (!is_valid_square(checker_sq)) return cursor;
    evasion_mask = BetweenBBs[static_cast<size_t>(king_sq)][static_cast<size_t>(checker_sq)];
  }

  pawn_moves(pos, evasion_mask, out_moves, cursor, gen_mode);

  const uint64_t occ = friendly | enemy;
  const uint64_t filtered_targets = target_mask & evasion_mask;

  auto emit_piece_moves = [&](uint64_t pieces, auto &&attack_fn) noexcept {
    while (pieces != 0ULL) {
      const int src = pop_lsb(pieces);
      uint64_t dsts = attack_fn(src) & filtered_targets;
      while (dsts != 0ULL) {
        out_moves[cursor++] =
            pack_move(src, pop_lsb(dsts), MoveTypes::Normal);
      }
    }
  };

  emit_piece_moves(pos.pieces_bb[PieceTypes::Knight] & friendly,
                   [](int sq) noexcept { return KNIGHT_ATK_SAFE(sq); });
  emit_piece_moves((pos.pieces_bb[PieceTypes::Bishop] |
                    pos.pieces_bb[PieceTypes::Queen]) &
                       friendly,
                   [occ](int sq) noexcept { return get_bishop_attacks(sq, occ); });
  emit_piece_moves((pos.pieces_bb[PieceTypes::Rook] |
                    pos.pieces_bb[PieceTypes::Queen]) &
                       friendly,
                   [occ](int sq) noexcept { return get_rook_attacks(sq, occ); });

  if (check_bb != 0ULL || gen_mode == Generate::GenCaptures) return cursor;

  for (int side : {Sides::Queenside, Sides::Kingside}) {
    const int rook_sq = pos.castling_squares[us][static_cast<size_t>(side)];
    if (rook_sq != SquareNone && is_valid_square(rook_sq) &&
        can_castle(pos, king_sq, rook_sq)) {
      out_moves[cursor++] = pack_move(king_sq, rook_sq, MoveTypes::Castling);
    }
  }

  return cursor;
}

inline int legal_movegen(const Position &pos, Move *out_moves) {
  const uint64_t check_bb = attacks_square(
      pos, get_king_pos(pos, pos.color), pos.color ^ 1);
  std::array<Move, ListSize> pseudo_buf;
  const int pseudo_count =
      movegen(pos, pseudo_buf.data(), check_bb, Generate::GenAll);

  int legal_count = 0;
  for (int i = 0; i < pseudo_count; ++i) {
    if (is_legal(pos, pseudo_buf[static_cast<size_t>(i)], check_bb)) {
      out_moves[legal_count++] = pseudo_buf[static_cast<size_t>(i)];
    }
  }
  return legal_count;
}

// Same answer as legal_movegen(...) != 0, stopping at the first legal move.
inline bool has_legal_move(const Position &pos) {
  const int us = pos.color;
  const int king_sq = get_king_pos(pos, us);
  if (!is_valid_square(king_sq)) return false;

  uint64_t king_dests = KING_ATK_SAFE(king_sq) & ~pos.colors_bb[us];
  while (king_dests != 0ULL) {
    const Move m = pack_move(king_sq, pop_lsb(king_dests), MoveTypes::Normal);
    if (is_legal(pos, m)) return true;
  }

  const uint64_t check_bb = attacks_square(pos, king_sq, us ^ 1);
  if ((check_bb & (check_bb - 1ULL)) != 0ULL) return false;

  // A legal king move or single pawn push is enough to rule out mate/stalemate.
  // Keep the complete generator for captures, blocks, castling and en passant.
  const uint64_t evasion_mask =
      check_bb != 0ULL ? BetweenBBs[static_cast<size_t>(king_sq)][static_cast<size_t>(get_lsb(check_bb))] : ~0ULL;
  const int step_fwd = us ? Directions::South : Directions::North;
  const uint64_t friendly_pawns =
      pos.pieces_bb[PieceTypes::Pawn] & pos.colors_bb[us];
  const uint64_t vacant = ~(pos.colors_bb[0] | pos.colors_bb[1]);
  uint64_t push_dests = shift_pawns(friendly_pawns, step_fwd) & vacant & evasion_mask;
  const int promo_rank = us ? 0 : 7;

  while (push_dests != 0ULL) {
    const int dst = pop_lsb(push_dests);
    const Action m = get_rank(dst) == promo_rank
                         ? pack_move_promo(dst - step_fwd, dst, Promos::Queen)
                         : pack_move(dst - step_fwd, dst, MoveTypes::Normal);
    if (is_legal(pos, m, check_bb)) return true;
  }

  std::array<Move, ListSize> pseudo_buf;
  const int pseudo_count =
      movegen(pos, pseudo_buf.data(), check_bb, Generate::GenAll);
  for (int i = 0; i < pseudo_count; ++i) {
    if (is_legal(pos, pseudo_buf[static_cast<size_t>(i)], check_bb)) return true;
  }
  return false;
}

inline Move get_next_move(Move *move_buf, int *score_buf, int start_pos,
                          int count) noexcept {
  int best_pos = start_pos;
  int top_score = score_buf[start_pos];
  for (int i = start_pos + 1; i < count; ++i) {
    if (score_buf[i] > top_score) {
      top_score = score_buf[i];
      best_pos = i;
    }
  }
  std::swap(move_buf[start_pos], move_buf[best_pos]);
  std::swap(score_buf[start_pos], score_buf[best_pos]);
  return move_buf[start_pos];
}

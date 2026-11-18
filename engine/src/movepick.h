#pragma once

#include "movegen.h"

namespace Stages {
enum : uint8_t {
  TT = 0,
  GenCaptures = 1,
  Captures = 2,
  GenQuiets = 3,
  Quiets = 4,
  BadCaptures = 5
};
}  // namespace Stages

struct MovePicker {
  int see_threshold{0};
  int stage{Stages::TT};
  uint64_t checkers{0};
  int idx{0};

  // Lists are read only up to their len; the buffers are not cleared.
  MoveInfo &captures;
  MoveInfo &quiets;
  MoveInfo &bad_captures;

  explicit MovePicker(MoveBuffers &buffers)
      : captures(buffers.captures),
        quiets(buffers.quiets),
        bad_captures(buffers.bad_captures) {}
};

static_assert(sizeof(MovePicker) <= 128,
              "Move lists must stay outside the thread stack");

inline void init_picker(MovePicker &picker, int threshold,
                        uint64_t check_bb) noexcept {
  picker.see_threshold = threshold;
  picker.stage = Stages::TT;
  picker.checkers = check_bb;
  picker.idx = 0;
  picker.captures.len = 0;
  picker.quiets.len = 0;
  picker.bad_captures.len = 0;
}

inline void score_capture_list(MovePicker &picker, const Position &pos,
                               const ThreadInfo &ti) noexcept {
  for (int i = 0; i < picker.captures.len; ++i) {
    const Action candidate = picker.captures.moves[static_cast<size_t>(i)];
    const int src = extract_from(candidate);
    const int dst = extract_to(candidate);
    if (!is_valid_square(src) || !is_valid_square(dst)) continue;

    const int mover_code = pos.board[static_cast<size_t>(src)];
    const int kind = extract_type(candidate);
    const int cap_pt = captured_piece_type(pos, candidate);

    if (kind == MoveTypes::Promotion) {
      const int promo = extract_promo(candidate);
      if (promo == Promos::Queen) {
        picker.captures.scores[static_cast<size_t>(i)] = QueenPromoScore;
      } else {
        int val = GoodCaptureBaseScore +
                  SeeValues[PromoPieceTypes[static_cast<size_t>(promo)]];
        if (mover_code >= Pieces::WPawn && mover_code <= Pieces::BKing) {
          val += ti.CapHistScores[static_cast<size_t>(mover_code)]
                                 [static_cast<size_t>(dst)]
                                 [static_cast<size_t>(cap_pt)];
        }
        picker.captures.scores[static_cast<size_t>(i)] = val;
      }
      continue;
    }

    const int victim_pt = (kind == MoveTypes::EnPassant)
                              ? static_cast<int>(PieceTypes::Pawn)
                              : cap_pt;
    const int mvv_lva = SeeValues[static_cast<size_t>(victim_pt)] * 100 -
                        SeeValues[static_cast<size_t>(get_piece_type(mover_code))] / 100;
    picker.captures.scores[static_cast<size_t>(i)] =
        GoodCaptureBaseScore + mvv_lva +
        ti.CapHistScores[static_cast<size_t>(mover_code)]
                        [static_cast<size_t>(dst)]
                        [static_cast<size_t>(cap_pt)];
  }
}

inline void score_quiet_list(MovePicker &picker, const Position &pos,
                             const ThreadInfo &ti) noexcept {
  auto lookup_history = [&](int ply_back, int &dst_out, int &piece_out) noexcept {
    dst_out = SquareNone;
    piece_out = Pieces::Blank;
    if (ti.game_ply >= ply_back && (ti.game_ply - ply_back) < MaxGameLen) {
      const StateRecord &rec = ti.game_hist[static_cast<size_t>(ti.game_ply - ply_back)];
      if (rec.played_move != MoveNone) {
        dst_out = extract_to(rec.played_move);
        piece_out = rec.piece_moved;
      }
    }
  };

  int ply1_dst, ply1_piece, ply2_dst, ply2_piece, ply4_dst, ply4_piece;
  lookup_history(1, ply1_dst, ply1_piece);
  lookup_history(2, ply2_dst, ply2_piece);
  lookup_history(4, ply4_dst, ply4_piece);

  const bool ply_valid = ti.search_ply < MaxSearchPly;
  const Action k0 = ply_valid ? ti.KillerMoves[ti.search_ply][0] : MoveNone;
  const Action k1 = ply_valid ? ti.KillerMoves[ti.search_ply][1] : MoveNone;
  const Action counter =
      (ply1_piece != Pieces::Blank && ply1_dst != SquareNone)
          ? ti.CounterMoves[static_cast<size_t>(ply1_piece)][static_cast<size_t>(ply1_dst)]
          : MoveNone;

  const auto *ch1 = (ply1_dst != SquareNone)
                        ? &ti.ContHistScores[static_cast<size_t>(ply1_piece)][static_cast<size_t>(ply1_dst)]
                        : nullptr;
  const auto *ch2 = (ply2_dst != SquareNone)
                        ? &ti.ContHistScores[static_cast<size_t>(ply2_piece)][static_cast<size_t>(ply2_dst)]
                        : nullptr;
  const auto *ch4 = (ply4_dst != SquareNone)
                        ? &ti.ContHistScores[static_cast<size_t>(ply4_piece)][static_cast<size_t>(ply4_dst)]
                        : nullptr;

  for (int i = 0; i < picker.quiets.len; ++i) {
    const Action candidate = picker.quiets.moves[static_cast<size_t>(i)];
    const int src = extract_from(candidate);
    const int dst = extract_to(candidate);
    if (!is_valid_square(src) || !is_valid_square(dst)) continue;

    if (candidate == k0) {
      picker.quiets.scores[static_cast<size_t>(i)] = KillerMoveScore;
      continue;
    }
    if (candidate == k1) {
      picker.quiets.scores[static_cast<size_t>(i)] = KillerMoveScore - 1000;
      continue;
    }

    const int mover_code = pos.board[static_cast<size_t>(src)];
    int val = ti.HistoryScores[static_cast<size_t>(mover_code)][static_cast<size_t>(dst)];
    if (ti.search_ply < LowPlyHistDepth) {
      val += 2 * ti.LowPlyHistory[ti.search_ply][static_cast<size_t>(mover_code)]
                                 [static_cast<size_t>(dst)];
    }
    if (ch1 != nullptr) val += (*ch1)[static_cast<size_t>(mover_code)][static_cast<size_t>(dst)];
    if (ch2 != nullptr) val += (*ch2)[static_cast<size_t>(mover_code)][static_cast<size_t>(dst)];
    if (ch4 != nullptr) val += (*ch4)[static_cast<size_t>(mover_code)][static_cast<size_t>(dst)];
    if (counter != MoveNone && candidate == counter) val += 8000;

    if (get_piece_type(mover_code) == PieceTypes::Pawn) {
      const int rel_rank =
          (pos.color == Colors::White) ? get_rank(dst) : (7 - get_rank(dst));
      if (rel_rank >= 6) {
        val += 14000;
      } else if (rel_rank == 5) {
        val += 7000;
      }
    }
    picker.quiets.scores[static_cast<size_t>(i)] = val;
  }
}

inline Action next_move(MovePicker &picker, const Position &pos,
                        ThreadInfo &ti, Action tt_move,
                        bool skip_quiets) noexcept {
  switch (picker.stage) {
    case Stages::TT:
      ++picker.stage;
      if (tt_move != MoveNone && is_pseudo_legal(pos, tt_move, picker.checkers)) {
        return tt_move;
      }
      [[fallthrough]];

    case Stages::GenCaptures:
      picker.captures.len = static_cast<uint16_t>(movegen(
          pos, picker.captures.moves.data(), picker.checkers, Generate::GenCaptures));
      score_capture_list(picker, pos, ti);
      ++picker.stage;
      [[fallthrough]];

    case Stages::Captures:
      while (picker.idx < picker.captures.len) {
        const Action candidate = get_next_move(
            picker.captures.moves.data(), picker.captures.scores.data(),
            picker.idx++, picker.captures.len);
        if (candidate == tt_move) continue;
        if (SEE(pos, candidate, picker.see_threshold)) return candidate;
        picker.bad_captures.moves[picker.bad_captures.len++] = candidate;
      }
      picker.idx = 0;
      ++picker.stage;
      [[fallthrough]];

    case Stages::GenQuiets:
      if (skip_quiets) {
        picker.idx = 0;
        picker.stage = Stages::BadCaptures;
        return picker.idx < picker.bad_captures.len
                   ? picker.bad_captures.moves[picker.idx++]
                   : MoveNone;
      }
      picker.quiets.len = static_cast<uint16_t>(movegen(
          pos, picker.quiets.moves.data(), picker.checkers, Generate::GenQuiets));
      score_quiet_list(picker, pos, ti);
      ++picker.stage;
      [[fallthrough]];

    case Stages::Quiets:
      while (!skip_quiets && picker.idx < picker.quiets.len) {
        const Action candidate = get_next_move(
            picker.quiets.moves.data(), picker.quiets.scores.data(),
            picker.idx++, picker.quiets.len);
        if (candidate != tt_move) return candidate;
      }
      picker.idx = 0;
      ++picker.stage;
      [[fallthrough]];

    case Stages::BadCaptures:
      return picker.idx < picker.bad_captures.len
                 ? picker.bad_captures.moves[picker.idx++]
                 : MoveNone;

    default:
      return MoveNone;
  }
}

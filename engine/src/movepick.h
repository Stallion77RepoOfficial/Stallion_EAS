#pragma once
#include "movegen.h"

namespace Stages {
constexpr uint8_t TT = 0;
constexpr uint8_t GenCaptures = 1;
constexpr uint8_t Captures = 2;
constexpr uint8_t GenQuiets = 3;
constexpr uint8_t Quiets = 4;
constexpr uint8_t BadCaptures = 5;
}

struct MovePicker {
  int see_threshold = 0;
  int stage = Stages::TT;
  uint64_t checkers = 0;
  int idx = 0;

  MoveInfo captures{};
  MoveInfo quiets{};
  MoveInfo bad_captures{};
};

inline void init_picker(MovePicker &picker, [[maybe_unused]] const Position &position,
                        int threshold, uint64_t checkers) noexcept {
  picker.see_threshold = threshold;
  picker.stage = Stages::TT;
  picker.checkers = checkers;
  picker.idx = 0;
  picker.captures.len = 0;
  picker.quiets.len = 0;
  picker.bad_captures.len = 0;
}

inline Action next_move(MovePicker &picker, const Position &position,
                        ThreadInfo &thread_info, Action tt_move, bool skip_quiets) noexcept {

  if (picker.stage == Stages::TT) {
    picker.stage++;
    if (tt_move != MoveNone &&
        is_pseudo_legal(position, tt_move, picker.checkers)) {
      return tt_move;
    }
  }

  if (picker.stage == Stages::GenCaptures) {
    picker.captures.len = movegen(position, picker.captures.moves.data(),
                                  picker.checkers, Generate::GenCaptures);

    for (int i = 0; i < picker.captures.len; i++) {
      const Action move = picker.captures.moves[i];

      const int from = extract_from(move), to = extract_to(move);
      if (!is_valid_square(from) || !is_valid_square(to))
        continue;

      const int from_piece = position.board[from];
      if (extract_type(move) == MoveTypes::Promotion) {
        if (extract_promo(move) == Promos::Queen) {
          picker.captures.scores[i] = QueenPromoScore;
        } else {
          picker.captures.scores[i] =
              GoodCaptureBaseScore + SeeValues[PromoPieceTypes[extract_promo(move)]];
          if (from_piece >= Pieces::WPawn && from_piece <= Pieces::BKing) {
            picker.captures.scores[i] +=
                thread_info.CapHistScores[from_piece][to];
          }
        }
      } else {
        const int to_piece = (extract_type(move) == MoveTypes::EnPassant)
                                 ? (position.color == Colors::White ? Pieces::BPawn : Pieces::WPawn)
                                 : position.board[to];
        picker.captures.scores[i] = GoodCaptureBaseScore +
                                    SeeValues[get_piece_type(to_piece)] * 100 -
                                    SeeValues[get_piece_type(from_piece)] / 100;
        picker.captures.scores[i] += thread_info.CapHistScores[from_piece][to];
      }
    }

    picker.stage++;
  }

  if (picker.stage == Stages::Captures) {
    while (picker.idx < picker.captures.len) {
      const Action move = get_next_move(picker.captures.moves.data(),
                                        picker.captures.scores.data(), picker.idx++,
                                        picker.captures.len);
      if (move == tt_move) continue;
      if (SEE(position, move, picker.see_threshold)) {
        return move;
      } else {
        picker.bad_captures.moves[picker.bad_captures.len++] = move;
      }
    }
    picker.idx = 0;
    picker.stage++;
  }

  if (picker.stage == Stages::GenQuiets) {
    if (skip_quiets) {
      picker.idx = 0;
      picker.stage = Stages::BadCaptures;
    } else {
      picker.quiets.len = movegen(position, picker.quiets.moves.data(),
                                  picker.checkers, Generate::GenQuiets);

      int their_last = SquareNone;
      int their_piece = Pieces::Blank;
      int our_last = SquareNone;
      int our_piece = Pieces::Blank;
      int ply4last = SquareNone;
      int ply4piece = Pieces::Blank;

      auto get_hist_move = [&](int offset, int &last_sq, int &piece) noexcept {
        if (thread_info.game_ply >= offset && (thread_info.game_ply - offset) < MaxGameLen) {
          const StateRecord &h = thread_info.game_hist[thread_info.game_ply - offset];
          if (h.played_move != MoveNone) {
            last_sq = extract_to(h.played_move);
            piece = h.piece_moved;
          }
        }
      };

      get_hist_move(1, their_last, their_piece);
      get_hist_move(2, our_last, our_piece);
      get_hist_move(4, ply4last, ply4piece);

      const Action killer0 = (thread_info.search_ply < MaxSearchPly)
                                 ? thread_info.KillerMoves[thread_info.search_ply][0]
                                 : MoveNone;
      const Action killer1 = (thread_info.search_ply < MaxSearchPly)
                                 ? thread_info.KillerMoves[thread_info.search_ply][1]
                                 : MoveNone;
      const Action counter_move = (their_piece != Pieces::Blank && their_last != SquareNone)
                                      ? thread_info.CounterMoves[their_piece][their_last]
                                      : MoveNone;

      for (int i = 0; i < picker.quiets.len; i++) {
        const Action move = picker.quiets.moves[i];

        const int from = extract_from(move), to = extract_to(move);
        if (!is_valid_square(from) || !is_valid_square(to))
          continue;

        if (move == killer0) {
          picker.quiets.scores[i] = KillerMoveScore;
        } else if (move == killer1) {
          picker.quiets.scores[i] = KillerMoveScore - 1000;
        } else {
          const int piece = position.board[from];
          picker.quiets.scores[i] = thread_info.HistoryScores[piece][to];

          if (their_last != SquareNone) {
            picker.quiets.scores[i] +=
                thread_info.ContHistScores[their_piece][their_last][piece][to];
          }
          if (our_last != SquareNone) {
            picker.quiets.scores[i] +=
                thread_info.ContHistScores[our_piece][our_last][piece][to];
          }
          if (ply4last != SquareNone) {
            picker.quiets.scores[i] +=
                thread_info.ContHistScores[ply4piece][ply4last][piece][to];
          }

          if (counter_move != MoveNone && move == counter_move) {
            picker.quiets.scores[i] += 8000;
          }

          const int pt = get_piece_type(piece);
          if (pt == PieceTypes::Pawn) {
            const int rel_rank = (position.color == Colors::White) ? get_rank(to) : (7 - get_rank(to));
            if (rel_rank >= 6) {
              picker.quiets.scores[i] += 14000;
            } else if (rel_rank == 5) {
              picker.quiets.scores[i] += 7000;
            }
          }
        }
      }
      picker.stage++;
    }
  }

  if (picker.stage == Stages::Quiets) {
    while (!skip_quiets && picker.idx < picker.quiets.len) {
      const Action move = get_next_move(picker.quiets.moves.data(),
                                        picker.quiets.scores.data(), picker.idx++,
                                        picker.quiets.len);
      if (move != tt_move) return move;
    }
    if (skip_quiets || picker.idx >= picker.quiets.len) {
      picker.idx = 0;
      picker.stage++;
    }
  }

  if (picker.stage == Stages::BadCaptures) {
    if (picker.idx >= picker.bad_captures.len) {
      return MoveNone;
    }
    return picker.bad_captures.moves[picker.idx++];
  }

  return MoveNone;
}

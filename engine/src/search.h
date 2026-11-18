#pragma once

#include "../fathom/src/tbprobe.h"
#include "movepick.h"
#include "position.h"
#include "utils.h"

#include <algorithm>
#include <array>
#include <cassert>
#include <cmath>
#include <memory>
#include <random>
#include <string>
#include <utility>
#include <vector>

extern bool tb_initialized;

struct QTraceRow {
  std::vector<Action> line;
  std::string leaf_fen;
  const char *kind = "unknown";
  int leaf_raw = ScoreNone;
  int leaf_corrected = ScoreNone;
  bool uses_nnue = false;
};

struct QSearchTrace {
  std::array<QTraceRow, MaxSearchPly + 1> rows;
  uint64_t searched_moves = 0;
  uint64_t delta_pruned = 0;
  bool truncated = false;
};

inline thread_local QSearchTrace *active_qtrace = nullptr;

inline Action uci_to_internal(const BoardState &pos, const std::string &uci) {
  std::array<Action, MaxActions> legal{};
  const int count = legal_movegen(pos, legal.data());
  for (int i = 0; i < count; ++i) {
    if (internal_to_uci(pos, legal[i]) == uci) return legal[i];
  }
  return MoveNone;
}

inline int64_t
safe_elapsed(const std::chrono::steady_clock::time_point &start) noexcept {
  const int64_t elapsed_ms = time_elapsed(start);
  return elapsed_ms ? elapsed_ms : 1;
}

inline unsigned tb_ep_square(const BoardState &pos) noexcept {
  const int ep = pos.ep_square;
  if (!is_valid_square(ep)) return 0U;

  const int stm = pos.color;
  const int victim_sq = ep + (stm ? Directions::North : Directions::South);
  if (!is_valid_square(victim_sq) ||
      pos.board[victim_sq] != Pieces::WPawn + (stm ^ 1)) {
    return 0U;
  }

  const uint64_t takers = PAWN_ATK_SAFE(stm ^ 1, ep) & pos.colors_bb[stm] &
                          pos.pieces_bb[PieceTypes::Pawn];
  return takers ? static_cast<unsigned>(ep) : 0U;
}

inline int probe_wdl_tb(const BoardState &pos, const ThreadInfo &ti) noexcept {
  if (!tb_initialized || !ti.use_syzygy) return ScoreNone;

  const int piece_count = pop_count(pos.colors_bb[0] | pos.colors_bb[1]);
  if (piece_count > std::min<int>(TB_LARGEST, ti.syzygy_probe_limit)) {
    return ScoreNone;
  }

  unsigned castle_flags = 0U;
  if (pos.castling_squares[Colors::White][Sides::Kingside] != SquareNone)
    castle_flags |= TB_CASTLING_K;
  if (pos.castling_squares[Colors::White][Sides::Queenside] != SquareNone)
    castle_flags |= TB_CASTLING_Q;
  if (pos.castling_squares[Colors::Black][Sides::Kingside] != SquareNone)
    castle_flags |= TB_CASTLING_k;
  if (pos.castling_squares[Colors::Black][Sides::Queenside] != SquareNone)
    castle_flags |= TB_CASTLING_q;
  if (castle_flags != 0U) return ScoreNone;

  if (ti.syzygy_50_move_rule && pos.halfmoves != 0) return ScoreNone;

  const unsigned probe_res = tb_probe_wdl(
      pos.colors_bb[0], pos.colors_bb[1], pos.pieces_bb[PieceTypes::King],
      pos.pieces_bb[PieceTypes::Queen], pos.pieces_bb[PieceTypes::Rook],
      pos.pieces_bb[PieceTypes::Bishop], pos.pieces_bb[PieceTypes::Knight],
      pos.pieces_bb[PieceTypes::Pawn], 0U, castle_flags, tb_ep_square(pos),
      pos.color == Colors::White);
  if (probe_res == TB_RESULT_FAILED) {
    ++thread_data.tb_fails;
    return ScoreNone;
  }

  ++thread_data.tb_hits;
  switch (TB_GET_WDL(probe_res)) {
    case TB_WIN:
      return TB_WIN_SCORE;
    case TB_CURSED_WIN:
      return ti.syzygy_50_move_rule ? 0 : TB_WIN_SCORE;
    case TB_DRAW:
      return 0;
    case TB_BLESSED_LOSS:
      return ti.syzygy_50_move_rule ? 0 : -TB_WIN_SCORE;
    case TB_LOSS:
      return -TB_WIN_SCORE;
    default:
      return ScoreNone;
  }
}

inline void update_history(int16_t &entry, int delta) noexcept {
  entry += static_cast<int16_t>(delta - entry * std::abs(delta) / HistGravityMax);
}

inline void update_corrhist(int16_t &entry, int delta) noexcept {
  entry += static_cast<int16_t>(delta - entry * std::abs(delta) / CorrHistMax);
}

inline void update_continuation_histories(ThreadInfo &ti, int piece, int sq,
                                          int bonus, int their_last,
                                          int their_piece, int our_last,
                                          int our_piece, int ply4_last,
                                          int ply4_piece) noexcept {
  update_history(ti.HistoryScores[piece][sq], bonus);
  if (their_last != SquareNone) {
    update_history(ti.ContHistScores[their_piece][their_last][piece][sq], bonus);
  }
  if (our_last != SquareNone) {
    update_history(ti.ContHistScores[our_piece][our_last][piece][sq], bonus);
  }
  if (ply4_last != SquareNone) {
    update_history(ti.ContHistScores[ply4_piece][ply4_last][piece][sq],
                   bonus * HistCont4UpdPct / 100);
  }
}

namespace detail_search {

inline uint64_t aggregate_nodes(const ThreadInfo &main_ti) noexcept {
  uint64_t total = main_ti.nodes.load(std::memory_order_relaxed);
  for (const ThreadInfo &helper : thread_data.thread_infos) {
    total += helper.nodes.load(std::memory_order_relaxed);
  }
  return total;
}

inline void handle_ponder_hit(ThreadInfo &ti) noexcept {
  if (thread_data.ponder_hit_time.load(std::memory_order_relaxed) < 0) return;
  const int64_t hit_ms =
      thread_data.ponder_hit_time.exchange(-1, std::memory_order_relaxed);
  if (hit_ms < 0) return;

  const auto hit_tp =
      std::chrono::steady_clock::time_point(std::chrono::milliseconds(hit_ms));
  const int64_t ponder_elapsed = std::max<int64_t>(
      0, std::chrono::duration_cast<std::chrono::milliseconds>(hit_tp - ti.start_time)
             .count());
  ti.start_time = hit_tp;
  ti.opt_time = std::min(
      ti.max_time,
      ti.opt_time + static_cast<uint64_t>(ponder_elapsed) * ti.ponder_time_factor / 100);
  ti.time_manager.soft_limit = ti.opt_time;
  ti.pondering = false;
  ti.ponder_hit = true;
}

}  // namespace detail_search

inline bool out_of_time(ThreadInfo &ti) noexcept {
  if (thread_data.stop) return true;
  if (ti.thread_id != 0) return false;

  detail_search::handle_ponder_hit(ti);

  ++ti.time_checks;
  const uint16_t check_period = (ti.max_nodes_searched < 1024) ? 16 : 256;
  if (ti.time_checks < check_period) return false;
  ti.time_checks = 0;

  // Aggregating helper node counts only on check ticks avoids cross-thread reads per node.
  const uint64_t total_nodes = detail_search::aggregate_nodes(ti);
  if (!thread_data.pondering && total_nodes >= ti.max_nodes_searched) {
    thread_data.stop = true;
    return true;
  }

  if (!ti.infinite_search && !thread_data.pondering) {
    const uint64_t elapsed = time_elapsed(ti.start_time);
    if (ti.time_manager.should_stop(elapsed, ti.best_move_stable,
                                    ti.eval_dropping, ti.is_movetime) ||
        elapsed > ti.max_time) {
      thread_data.stop = true;
      return true;
    }
  }
  return false;
}

inline bool has_non_pawn_material(const BoardState &pos, int color) noexcept {
  const int base = 2 + color;
  return pos.material_count[base] || pos.material_count[base + 2] ||
         pos.material_count[base + 4] || pos.material_count[base + 6];
}

inline int eval(BoardState &pos, ThreadInfo &ti) {
  const int raw = ti.nnue_state.evaluate(pos);
  return (NnueEvalScale == 100) ? raw : std::clamp(raw * NnueEvalScale / 100, -MaxEval, MaxEval);
}

inline int correct_eval(const BoardState &pos, const ThreadInfo &ti,
                        int raw_eval) noexcept {
  const int scaled =
      raw_eval * std::max(0, HalfmoveScaleMax - pos.halfmoves) / HalfmoveScaleMax;
  const int stm = pos.color;
  int corr = ti.PawnCorrHist[stm][get_corrhist_index(pos.pawn_key)];
  corr += ti.NonPawnCorrHist[stm][Colors::White]
                            [get_corrhist_index(pos.non_pawn_key[Colors::White])];
  corr += ti.NonPawnCorrHist[stm][Colors::Black]
                            [get_corrhist_index(pos.non_pawn_key[Colors::Black])];
  if (ti.game_ply >= 1 && ti.game_ply - 1 < MaxGameLen) {
    const StateRecord &r1 = ti.game_hist[ti.game_ply - 1];
    if (r1.played_move != MoveNone && r1.piece_moved >= Pieces::WPawn &&
        r1.piece_moved <= Pieces::BKing) {
      const int dst1 = extract_to(r1.played_move);
      if (is_valid_square(dst1)) {
        corr += (ti.Cont1CorrHist[stm][r1.piece_moved][dst1] * CorrCont1EvalPct) / 100;
      }
    }
  }
  if (ti.game_ply >= 2 && ti.game_ply - 2 < MaxGameLen) {
    const StateRecord &r2 = ti.game_hist[ti.game_ply - 2];
    if (r2.played_move != MoveNone && r2.piece_moved >= Pieces::WPawn &&
        r2.piece_moved <= Pieces::BKing) {
      const int dst2 = extract_to(r2.played_move);
      if (is_valid_square(dst2)) {
        corr += (ti.Cont2CorrHist[stm][r2.piece_moved][dst2] * CorrCont2EvalPct) / 100;
      }
    }
  }
  return std::clamp(scaled + (CorrWeight * corr / 640), -MaxEval, MaxEval);
}

inline void ss_push(const BoardState &pos, ThreadInfo &ti, Action move) noexcept {
  assert(ti.search_ply + 1 < MaxSearchPly);
  assert(ti.game_ply + 1 < MaxGameLen);
  StateRecord &slot = ti.game_hist[ti.game_ply];
  slot.position_key = pos.zobrist_key;
  slot.played_move = move;
  slot.piece_moved =
      (move == MoveNone) ? Pieces::Blank : pos.board[extract_from(move)];
  ++ti.search_ply;
  ++ti.game_ply;
}

inline void ss_pop(ThreadInfo &ti) noexcept {
  assert(ti.search_ply > 0 && ti.game_ply > 0);
  --ti.search_ply;
  --ti.game_ply;
  ti.nnue_state.pop();
}

inline bool material_draw(const BoardState &pos) noexcept {
  if (pos.pieces_bb[PieceTypes::Pawn] || pos.pieces_bb[PieceTypes::Rook] ||
      pos.pieces_bb[PieceTypes::Queen]) {
    return false;
  }
  const uint64_t knights = pos.pieces_bb[PieceTypes::Knight];
  const uint64_t bishops = pos.pieces_bb[PieceTypes::Bishop];
  if (pop_count(knights | bishops) <= 1) return true;
  constexpr uint64_t kDarkSquares = 0xAA55AA55AA55AA55ULL;
  return !knights && (!(bishops & kDarkSquares) || !(bishops & ~kDarkSquares));
}

inline bool is_draw(const BoardState &pos, ThreadInfo &ti) noexcept {
  if (pos.halfmoves >= 100) {
    if (attacks_square(pos, get_king_pos(pos, pos.color), pos.color ^ 1)) {
      std::array<Action, MaxActions> legal{};
      if (!legal_movegen(pos, legal.data())) return false;
    }
    return true;
  }
  if (material_draw(pos)) return true;

  int seen_count = 0;
  const int floor_ply = std::max(0, static_cast<int>(ti.game_ply) - pos.halfmoves);
  const int root_ply = static_cast<int>(ti.game_ply) - ti.search_ply;
  for (int idx = static_cast<int>(ti.game_ply) - 1; idx >= floor_ply; --idx) {
    const StateRecord &rec = ti.game_hist[idx];
    if (rec.played_move == MoveNone) break;
    if (rec.position_key == pos.zobrist_key && (++seen_count == 2 || idx >= root_ply)) {
      return true;
    }
  }
  return false;
}

inline int draw_score(const BoardState &pos, ThreadInfo &ti) noexcept {
  const int base = 1 - static_cast<int>(ti.nodes.load(std::memory_order_relaxed) & 3ULL);
  if (Contempt == 0) return base;
  return base + (pos.color == ti.root_color ? -Contempt : Contempt);
}

namespace detail_search {

struct HistoryContext {
  int prev1_sq = SquareNone;
  int prev1_pc = Pieces::Blank;
  int prev2_sq = SquareNone;
  int prev2_pc = Pieces::Blank;
  int prev4_sq = SquareNone;
  int prev4_pc = Pieces::Blank;
  const MultiArray<int16_t, 14, 64> *c1 = nullptr;
  const MultiArray<int16_t, 14, 64> *c2 = nullptr;
  const MultiArray<int16_t, 14, 64> *c4 = nullptr;
};

inline HistoryContext collect_history_context(const ThreadInfo &ti,
                                              const StateRecord *ss) noexcept {
  HistoryContext ctx;
  if (ti.game_ply >= 1 && (ss - 1)->played_move != MoveNone) {
    ctx.prev1_sq = extract_to((ss - 1)->played_move);
    ctx.prev1_pc = (ss - 1)->piece_moved;
  }
  if (ti.game_ply >= 2 && (ss - 2)->played_move != MoveNone) {
    ctx.prev2_sq = extract_to((ss - 2)->played_move);
    ctx.prev2_pc = (ss - 2)->piece_moved;
  }
  if (ti.game_ply >= 4 && (ss - 4)->played_move != MoveNone) {
    ctx.prev4_sq = extract_to((ss - 4)->played_move);
    ctx.prev4_pc = (ss - 4)->piece_moved;
  }
  ctx.c1 = (ctx.prev1_sq != SquareNone)
               ? &ti.ContHistScores[ctx.prev1_pc][ctx.prev1_sq]
               : nullptr;
  ctx.c2 = (ctx.prev2_sq != SquareNone)
               ? &ti.ContHistScores[ctx.prev2_pc][ctx.prev2_sq]
               : nullptr;
  ctx.c4 = (ctx.prev4_sq != SquareNone)
               ? &ti.ContHistScores[ctx.prev4_pc][ctx.prev4_sq]
               : nullptr;
  return ctx;
}

inline int evaluate_corrected(BoardState &pos, ThreadInfo &ti) {
  return correct_eval(pos, ti, eval(pos, ti));
}

constexpr inline bool tt_cutoff_usable(int bound, int tt_score, int alpha,
                                       int beta) noexcept {
  if (tt_score == ScoreNone) return false;
  return bound == EntryTypes::Exact ||
         (bound == EntryTypes::LBound && tt_score >= beta) ||
         (bound == EntryTypes::UBound && tt_score <= alpha);
}

constexpr inline int tighten_eval_from_tt(int bound, int tt_score,
                                          int base_eval) noexcept {
  if (tt_score == ScoreNone) return base_eval;
  if (bound == EntryTypes::Exact ||
      (bound == EntryTypes::UBound && tt_score < base_eval) ||
      (bound == EntryTypes::LBound && tt_score > base_eval)) {
    return tt_score;
  }
  return base_eval;
}

inline void update_fail_high_tables(const BoardState &pos, ThreadInfo &ti,
                                    const HistoryContext &ctx, int ply,
                                    int depth, int best_score, int beta,
                                    Action best_move, const Action *quiets,
                                    int quiet_count, const Action *captures,
                                    int cap_count) noexcept {
  const int d = depth + (best_score > beta + HistDepthBoostMargin);
  const int bonus = std::min(HistBonus * d * (d + 1) / 4 + HistBonus / 2, HistMax);
  const int best_pc = pos.board[extract_from(best_move)];
  const int best_dst = extract_to(best_move);

  if (is_cap(pos, best_move)) {
    update_history(
        ti.CapHistScores[best_pc][best_dst][captured_piece_type(pos, best_move)],
        bonus);
  } else {
    for (int i = 0; i < quiet_count; ++i) {
      const Action m = quiets[i];
      update_continuation_histories(ti, pos.board[extract_from(m)],
                                    extract_to(m), -bonus, ctx.prev1_sq,
                                    ctx.prev1_pc, ctx.prev2_sq, ctx.prev2_pc,
                                    ctx.prev4_sq, ctx.prev4_pc);
    }
    update_continuation_histories(ti, best_pc, best_dst, bonus, ctx.prev1_sq,
                                  ctx.prev1_pc, ctx.prev2_sq, ctx.prev2_pc,
                                  ctx.prev4_sq, ctx.prev4_pc);
    if (ply < LowPlyHistDepth && ply < MaxLowPlyHistDepth) {
      const int lp_bonus = std::min(bonus * (LowPlyHistDepth - ply) / 2, HistMax);
      for (int i = 0; i < quiet_count; ++i) {
        const Action m = quiets[i];
        update_history(
            ti.LowPlyHistory[ply][pos.board[extract_from(m)]][extract_to(m)],
            -lp_bonus);
      }
      update_history(ti.LowPlyHistory[ply][best_pc][best_dst], lp_bonus);
    }
    if (best_move != ti.KillerMoves[ply][0]) {
      ti.KillerMoves[ply][1] = ti.KillerMoves[ply][0];
      ti.KillerMoves[ply][0] = best_move;
    }
    if (ctx.prev1_pc != Pieces::Blank && ctx.prev1_sq != SquareNone) {
      ti.CounterMoves[ctx.prev1_pc][ctx.prev1_sq] = best_move;
    }
  }

  for (int i = 0; i < cap_count; ++i) {
    const Action m = captures[i];
    update_history(
        ti.CapHistScores[pos.board[extract_from(m)]][extract_to(m)]
                        [captured_piece_type(pos, m)],
        -bonus);
  }
}

inline bool move_gives_check(const BoardState &pos, Action move) noexcept {
  const int stm = pos.color;
  const int opp = stm ^ 1;
  const int kind = extract_type(move);
  if (kind != MoveTypes::Normal) {
    BoardState tmp = pos;
    make_move(tmp, move);
    return attacks_square(tmp, get_king_pos(tmp, opp), stm) != 0ULL;
  }

  const int opp_king = get_king_pos(pos, opp);
  const int src = extract_from(move);
  const int dst = extract_to(move);
  if (!is_valid_square(opp_king) || !is_valid_square(src) || !is_valid_square(dst)) {
    return false;
  }

  const int ptype = get_piece_type(pos.board[static_cast<size_t>(src)]);
  const uint64_t occ =
      ((pos.colors_bb[0] | pos.colors_bb[1]) & ~(1ULL << src)) | (1ULL << dst);
  const uint64_t dst_bb = 1ULL << dst;

  switch (ptype) {
    case PieceTypes::Pawn:
      if ((PAWN_ATK_SAFE(opp, opp_king) & dst_bb) != 0ULL) return true;
      break;
    case PieceTypes::Knight:
      if ((KNIGHT_ATK_SAFE(opp_king) & dst_bb) != 0ULL) return true;
      break;
    case PieceTypes::Bishop:
      if ((get_bishop_attacks(opp_king, occ) & dst_bb) != 0ULL) return true;
      break;
    case PieceTypes::Rook:
      if ((get_rook_attacks(opp_king, occ) & dst_bb) != 0ULL) return true;
      break;
    case PieceTypes::Queen:
      if (((get_bishop_attacks(opp_king, occ) | get_rook_attacks(opp_king, occ)) &
           dst_bb) != 0ULL) {
        return true;
      }
      break;
    default:
      break;
  }

  if ((BishopAttacks[static_cast<size_t>(opp_king)][0] |
       RookAttacks[static_cast<size_t>(opp_king)][0]) &
      (1ULL << src)) {
    const uint64_t our_rem = pos.colors_bb[stm] & ~(1ULL << src);
    const uint64_t diag = our_rem & (pos.pieces_bb[PieceTypes::Bishop] |
                                     pos.pieces_bb[PieceTypes::Queen]);
    const uint64_t orth = our_rem & (pos.pieces_bb[PieceTypes::Rook] |
                                     pos.pieces_bb[PieceTypes::Queen]);
    return ((get_bishop_attacks(opp_king, occ) & diag) |
            (get_rook_attacks(opp_king, occ) & orth)) != 0ULL;
  }
  return false;
}

inline void update_all_corrhist(const BoardState &pos, ThreadInfo &ti, int depth,
                                int best_score, int static_eval) noexcept {
  const int delta = std::clamp((best_score - static_eval) * depth / CorrDeltaDiv,
                               -CorrDeltaMax, CorrDeltaMax);
  const int stm = pos.color;
  update_corrhist(ti.PawnCorrHist[stm][get_corrhist_index(pos.pawn_key)], delta);
  update_corrhist(
      ti.NonPawnCorrHist[stm][Colors::White]
                        [get_corrhist_index(pos.non_pawn_key[Colors::White])],
      delta);
  update_corrhist(
      ti.NonPawnCorrHist[stm][Colors::Black]
                        [get_corrhist_index(pos.non_pawn_key[Colors::Black])],
      delta);
  if (ti.game_ply >= 1 && ti.game_ply - 1 < MaxGameLen) {
    const StateRecord &r1 = ti.game_hist[ti.game_ply - 1];
    if (r1.played_move != MoveNone && r1.piece_moved >= Pieces::WPawn &&
        r1.piece_moved <= Pieces::BKing) {
      const int dst1 = extract_to(r1.played_move);
      if (is_valid_square(dst1)) {
        update_corrhist(ti.Cont1CorrHist[stm][r1.piece_moved][dst1], delta);
      }
    }
  }
  if (ti.game_ply >= 2 && ti.game_ply - 2 < MaxGameLen) {
    const StateRecord &r2 = ti.game_hist[ti.game_ply - 2];
    if (r2.played_move != MoveNone && r2.piece_moved >= Pieces::WPawn &&
        r2.piece_moved <= Pieces::BKing) {
      const int dst2 = extract_to(r2.played_move);
      if (is_valid_square(dst2)) {
        update_corrhist(ti.Cont2CorrHist[stm][r2.piece_moved][dst2],
                        delta * CorrCont2UpdPct / 100);
      }
    }
  }
}

inline std::string format_uci_eval(int score) {
  if (std::abs(score) < MateScore - MaxSearchPly) {
    return "cp " + std::to_string(score * 100 / NormalizationFactor);
  }
  if (score > 0) {
    return "mate " + std::to_string(std::max(1, (MateScore - score + 1) / 2));
  }
  return "mate -" + std::to_string(std::max(1, (MateScore + score + 1) / 2));
}

inline void apply_elo_move_selection(ThreadInfo &ti, int real_multi_pv,
                                     const std::array<Action, MaxActions> &cand_moves,
                                     const std::array<int, MaxActions> &cand_scores) {
  int n_cands = 0;
  while (n_cands < real_multi_pv && cand_moves[n_cands] != MoveNone &&
         cand_scores[n_cands] != ScoreNone) {
    ++n_cands;
  }
  if (n_cands <= 1) return;

  const int elo = std::clamp(ti.uci_elo, MinUciElo, MaxUciElo);
  const double weakness =
      (MaxUciElo - elo) / static_cast<double>(MaxUciElo - MinUciElo);
  const double temperature =
      std::max(1.0, weakness * weakness * 2.5 * NormalizationFactor);
  const int peak_score =
      *std::max_element(cand_scores.begin(), cand_scores.begin() + n_cands);

  std::array<double, MaxActions> weights{};
  double total_weight = 0.0;
  for (int i = 0; i < n_cands; ++i) {
    const double gap = static_cast<double>(cand_scores[i] - peak_score);
    weights[i] = std::exp(std::max(-20.0, gap / temperature));
    total_weight += weights[i];
  }
  if (total_weight <= 0.0) return;

  static thread_local std::mt19937_64 prng(Random::rd());
  double draw = std::uniform_real_distribution<double>(0.0, total_weight)(prng);
  int picked = 0;
  for (int i = 0; i < n_cands; ++i) {
    draw -= weights[i];
    if (draw <= 0.0) {
      picked = i;
      break;
    }
  }
  ti.best_moves[0] = cand_moves[picked];
  ti.best_scores[0] = cand_scores[picked];
}

}  // namespace detail_search

// Non-tracing instantiations compile away all QSearchTrace bookkeeping via if constexpr.
template <bool Trace = false>
inline int qsearch(int alpha, int beta, BoardState &pos, ThreadInfo &ti,
                   std::vector<TTBucket> &table) {
  auto mark_leaf = [&](const char *kind, int raw = ScoreNone,
                       int corrected = ScoreNone, bool uses_nnue = false) {
    if constexpr (Trace) {
      QTraceRow &row = active_qtrace->rows[ti.search_ply];
      row = {};
      row.leaf_fen = export_fen(pos, ti);
      row.kind = kind;
      row.leaf_raw = raw;
      row.leaf_corrected = corrected;
      row.uses_nnue = uses_nnue;
    }
  };
  mark_leaf("unknown");

  const int ply = ti.search_ply;
  if (ply == 0 && !has_legal_move(pos)) {
    mark_leaf("terminal");
    return attacks_square(pos, get_king_pos(pos, pos.color), pos.color ^ 1)
               ? (-MateScore + ply)
               : 0;
  }

  if (ply >= MaxSearchPly - 4) {
    if constexpr (Trace) active_qtrace->truncated = true;
    mark_leaf("ply_limit");
    return detail_search::evaluate_corrected(pos, ti);
  }

  if (ti.game_ply >= MaxGameLen - 2) {
    if constexpr (Trace) active_qtrace->truncated = true;
    mark_leaf("history_limit");
    return detail_search::evaluate_corrected(pos, ti);
  }

  if (ply && is_draw(pos, ti)) {
    mark_leaf("draw");
    return draw_score(pos, ti);
  }

  if (out_of_time(ti)) {
    mark_leaf("budget");
    return detail_search::evaluate_corrected(pos, ti);
  }

  const int tb_score = probe_wdl_tb(pos, ti);
  if (tb_score != ScoreNone) {
    mark_leaf("tablebase");
    return tb_score;
  }

  StateRecord *ss = &ti.game_hist[ti.game_ply];
  ++ti.nodes;
  if (ply > ti.seldepth) ti.seldepth = ply;

  const uint64_t hash = pos.zobrist_key;
  bool tt_hit = false;
  const TTEntry tt_entry = probe_entry(hash, tt_hit, ti.searches, table);

  int entry_type = EntryTypes::None;
  int tt_static_eval = ScoreNone;
  int tt_score = ScoreNone;
  Action tt_move = MoveNone;

  if (tt_hit) {
    entry_type = tt_entry.get_type();
    tt_static_eval = tt_entry.static_eval;
    tt_score = (pos.halfmoves < TtHalfmoveLimit) ? score_from_tt(tt_entry.score, ply) : ScoreNone;
    tt_move = tt_entry.best_move;
  }

  if (detail_search::tt_cutoff_usable(entry_type, tt_score, alpha, beta)) {
    mark_leaf("transposition");
    return tt_score;
  }

  const uint64_t in_check =
      attacks_square(pos, get_king_pos(pos, pos.color), pos.color ^ 1);

  int raw_eval = ScoreNone;
  int static_eval = ScoreNone;
  int stand_pat = ScoreNone;
  int best_score = ScoreNone;
  Action best_move = MoveNone;
  bool raised_alpha = false;

  if (!in_check) {
    raw_eval = (tt_static_eval == ScoreNone) ? eval(pos, ti) : tt_static_eval;
    static_eval = correct_eval(pos, ti, raw_eval);
    ss->static_eval = static_eval;

    stand_pat = detail_search::tighten_eval_from_tt(entry_type, tt_score, static_eval);
    best_score = stand_pat;
    const bool stand_from_nnue = (stand_pat == static_eval) &&
                                 !(tt_score != ScoreNone &&
                                   (entry_type == EntryTypes::Exact ||
                                    (entry_type == EntryTypes::UBound && tt_score < static_eval) ||
                                    (entry_type == EntryTypes::LBound && tt_score > static_eval)));

    mark_leaf(stand_from_nnue ? "stand_pat" : "transposition_bound", raw_eval,
              static_eval, stand_from_nnue);

    if (stand_pat >= beta) {
      insert_entry(hash, 0, MoveNone, raw_eval, score_to_tt(stand_pat, ply),
                   EntryTypes::LBound, ti.searches);
      return stand_pat;
    }
    if (stand_pat > alpha) {
      alpha = stand_pat;
      raised_alpha = true;
    }
  } else {
    ss->static_eval = ScoreNone;
  }

  MoveBufferLease lease(ti.move_buffers);
  MovePicker picker(lease.buffers);
  init_picker(picker, in_check ? QSearchCheckSeeMargin : QSearchCapSeeMargin, in_check);

  if (!in_check && tt_move != MoveNone) {
    if (!is_cap(pos, tt_move) && extract_type(tt_move) != MoveTypes::Promotion) {
      tt_move = MoveNone;
    }
  }

  while (Action move = next_move(picker, pos, ti, tt_move, !in_check)) {
    if (thread_data.stop) break;
    if (!in_check && picker.stage > Stages::Captures) break;
    if (!is_legal(pos, move, in_check)) continue;

    if (!in_check && stand_pat != ScoreNone) {
      const int to_sq = extract_to(move);
      int cap_pc = pos.board[to_sq];
      if (!cap_pc && extract_type(move) == MoveTypes::EnPassant) {
        cap_pc = Pieces::WPawn + (pos.color ^ 1);
      }
      int promo_gain = 0;
      if (extract_type(move) == MoveTypes::Promotion) {
        promo_gain = MaterialValues[PromoPieceTypes[extract_promo(move)]] -
                     MaterialValues[PieceTypes::Pawn];
      }
      const int delta_bound =
          MaterialValues[get_piece_type(cap_pc)] + promo_gain + DeltaMarginBase;
      if (stand_pat + delta_bound < alpha && !detail_search::move_gives_check(pos, move)) {
        if constexpr (Trace) ++active_qtrace->delta_pruned;
        continue;
      }
    }

    BoardState child_pos = pos;
    make_move(child_pos, move);

    if constexpr (Trace) ++active_qtrace->searched_moves;
    ti.nnue_state.push(child_pos);

    int score = ScoreNone;
    const bool can_recurse =
        (ti.search_ply + 1 < MaxSearchPly - 4) && (ti.game_ply < MaxGameLen - 2);
    if (can_recurse) {
      ss_push(pos, ti, move);
      score = -qsearch<Trace>(-beta, -alpha, child_pos, ti, table);
      ss_pop(ti);
    } else {
      if constexpr (Trace) active_qtrace->truncated = true;
      score = -detail_search::evaluate_corrected(child_pos, ti);
      ti.nnue_state.pop();
    }

    if (thread_data.stop) {
      return (best_score != ScoreNone)
                 ? best_score
                 : ((stand_pat != ScoreNone) ? stand_pat : alpha);
    }

    if (best_score == ScoreNone || score > best_score) {
      if constexpr (Trace) {
        if (can_recurse) {
          QTraceRow child_row = active_qtrace->rows[ply + 1];
          child_row.line.insert(child_row.line.begin(), move);
          active_qtrace->rows[ply] = std::move(child_row);
        } else {
          mark_leaf("ply_limit");
        }
      }
      best_score = score;
    }

    if (score > alpha) {
      alpha = score;
      best_move = move;
      raised_alpha = true;
      if (score >= beta) break;
    }
  }

  if (thread_data.stop) {
    return (best_score != ScoreNone)
               ? best_score
               : ((stand_pat != ScoreNone) ? stand_pat : alpha);
  }

  if (best_score == ScoreNone) {
    if (in_check) {
      mark_leaf("mate");
      return -MateScore + ply;
    }
    best_score = (stand_pat != ScoreNone)
                     ? stand_pat
                     : detail_search::evaluate_corrected(pos, ti);
  }

  const uint8_t bound =
      (best_score >= beta) ? EntryTypes::LBound
                           : (raised_alpha ? EntryTypes::Exact : EntryTypes::UBound);
  insert_entry(hash, 0, best_move, raw_eval, score_to_tt(best_score, ply), bound,
               ti.searches);
  return best_score;
}

template <bool is_pv>
inline int search(int alpha, int beta, int depth, bool cutnode, BoardState &pos,
                  ThreadInfo &ti, std::vector<TTBucket> &table) {
  if (ti.game_ply >= MaxGameLen - 2) {
    return detail_search::evaluate_corrected(pos, ti);
  }
  StateRecord *ss = &ti.game_hist[ti.game_ply];

  if (ti.search_ply == 0) {
    ti.current_iter = depth;
    ti.seldepth = 0;
    ti.pv.fill(MoveNone);
  }

  const int ply = ti.search_ply;
  const int pv_base = ply * MaxSearchPly;
  const Action excluded_move = ti.excluded_move;
  const bool singular_search = (excluded_move != MoveNone);
  if (!singular_search) ti.pv[pv_base] = MoveNone;

  if (ply > ti.seldepth) ti.seldepth = ply;
  if (ply >= MaxRootDepth) return detail_search::evaluate_corrected(pos, ti);

  depth = std::min(depth, MaxRootDepth - ply);
  if (out_of_time(ti) || ply >= MaxSearchPly - 1) {
    return detail_search::evaluate_corrected(pos, ti);
  }
  if (ply && is_draw(pos, ti)) return draw_score(pos, ti);
  if (depth <= 0) return qsearch(alpha, beta, pos, ti, table);

  ++ti.nodes;

  const bool at_root = (ply == 0);
  const int stm = pos.color;
  bool raised_alpha = false;
  Action best_move = MoveNone;
  ti.excluded_move = MoveNone;

  const int mate_bound = MateScore - 1 - ply;
  if (mate_bound < beta) {
    beta = mate_bound;
    if (alpha >= beta) return beta;
  }

  const uint64_t hash = pos.zobrist_key;
  bool tt_hit = false;
  const TTEntry tt_entry = probe_entry(hash, tt_hit, ti.searches, table);

  int entry_type = EntryTypes::None;
  int tt_static_eval = ScoreNone;
  int tt_score = ScoreNone;
  Action tt_move = MoveNone;

  if (tt_hit && !singular_search) {
    entry_type = tt_entry.get_type();
    tt_static_eval = tt_entry.static_eval;
    tt_score = (pos.halfmoves < TtHalfmoveLimit) ? score_from_tt(tt_entry.score, ply) : ScoreNone;
    tt_move = tt_entry.best_move;
  }

  const bool tt_pv = is_pv || (tt_hit && tt_entry.get_pv());
  const bool tt_capture = (tt_move != MoveNone && is_cap(pos, tt_move));

  if (!is_pv && tt_entry.depth >= depth &&
      detail_search::tt_cutoff_usable(entry_type, tt_score, alpha, beta)) {
    return tt_score;
  }

  const uint64_t in_check = attacks_square(pos, get_king_pos(pos, stm), stm ^ 1);
  int32_t raw_eval = ScoreNone;
  int32_t static_eval = ScoreNone;

  if (in_check) {
    static_eval = raw_eval = ScoreNone;
  } else if (singular_search) {
    static_eval = raw_eval = ss->static_eval;
  } else {
    raw_eval = (tt_static_eval == ScoreNone) ? eval(pos, ti) : tt_static_eval;
    static_eval = correct_eval(pos, ti, raw_eval);
  }

  ss->static_eval = static_eval;

  bool improving = false;
  if (!in_check) {
    if (ply > 1 && ti.game_ply >= 2 && (ss - 2)->static_eval != ScoreNone) {
      improving = static_eval > (ss - 2)->static_eval;
    } else if (ply > 3 && ti.game_ply >= 4 && (ss - 4)->static_eval != ScoreNone) {
      improving = static_eval > (ss - 4)->static_eval;
    }
  }

  static_eval = detail_search::tighten_eval_from_tt(entry_type, tt_score, static_eval);

  if (!is_pv && !in_check && !singular_search) {
    if (depth >= ti.syzygy_probe_depth) {
      const int tb_val = probe_wdl_tb(pos, ti);
      if (tb_val != ScoreNone && (tb_val >= beta || tb_val <= alpha)) return tb_val;
    }

    if (ti.mate_search == 0) {
      if (depth <= RFPMaxDepth &&
          static_eval - RFPMargin * (depth - improving) - RFPTtPvMargin * tt_pv >= beta) {
        return (static_eval + beta) / 2;
      }

      if (depth <= RazorMaxDepth && static_eval + RazorMargin * depth < alpha) {
        const int razor_val = qsearch(alpha, beta, pos, ti, table);
        if (razor_val <= alpha) return razor_val;
      }

      if (static_eval >= beta && depth >= NMPMinDepth && ply >= ti.nmp_min_ply &&
          has_non_pawn_material(pos, stm) && ti.game_ply > 0 &&
          (ss - 1)->played_move != MoveNone) {
        BoardState null_pos = pos;
        make_move(null_pos, MoveNone);
        ti.nnue_state.push(null_pos);
        ss_push(pos, ti, MoveNone);

        const int null_red =
            NMPBase + depth / NMPDepthDiv + std::min(NMPEvalMaxRed, (static_eval - beta) / NMPEvalDiv);
        int null_score = -search<false>(-beta, -beta + 1, depth - null_red,
                                        !cutnode, null_pos, ti, table);
        ss_pop(ti);

        if (null_score >= beta) {
          if (null_score >= MateThreshold) null_score = beta;
          if (ti.nmp_min_ply > 0 || depth < NMPVerifyDepth) {
            return null_score;
          }
          const int ver_depth = std::max(1, depth - null_red);
          ti.nmp_min_ply = ply + (NMPVerifyPct * ver_depth) / 100;
          const int ver_score =
              search<false>(beta - 1, beta, ver_depth, false, pos, ti, table);
          ti.nmp_min_ply = 0;
          if (ver_score >= beta) return null_score;
        }
      }
    }
  }

  MoveBufferLease lease(ti.move_buffers);
  if ((is_pv || cutnode) && tt_move == MoveNone && depth > IIRMinDepth) --depth;

  const int pc_beta = beta + ProbCutMargin;
  if (!at_root && !is_pv && !in_check && !singular_search && ti.mate_search == 0 &&
      depth >= ProbCutMinDepth && std::abs(beta) < MateThreshold &&
      (!tt_hit || tt_entry.depth + ProbCutTtDepthMargin <= depth || tt_score >= pc_beta)) {
    const int pc_threshold = pc_beta - static_eval;
    MovePicker pc_picker(lease.buffers);
    init_picker(pc_picker, pc_threshold, in_check);
    const Action pc_tt =
        (tt_move != MoveNone && SEE(pos, tt_move, pc_threshold)) ? tt_move : MoveNone;

    while (Action move = next_move(pc_picker, pos, ti, pc_tt, true)) {
      if (thread_data.stop || pc_picker.stage > Stages::Captures) break;
      if (move == excluded_move || !is_legal(pos, move, in_check)) continue;

      BoardState child_pos = pos;
      make_move(child_pos, move);
      ti.nnue_state.push(child_pos);
      ss_push(pos, ti, move);

      int pc_score = -qsearch(-pc_beta, -pc_beta + 1, child_pos, ti, table);
      if (pc_score >= pc_beta) {
        pc_score = -search<is_pv>(-pc_beta, -pc_beta + 1, depth - ProbCutReduction, false,
                                  child_pos, ti, table);
      }
      ss_pop(ti);
      if (pc_score >= pc_beta) return pc_score;
    }
  }

  Action *const quiets = lease.buffers.quiet_history.data();
  Action *const captures = lease.buffers.capture_history.data();
  int quiet_count = 0;
  int cap_count = 0;
  ti.KillerMoves[ply + 1][0] = MoveNone;
  ti.KillerMoves[ply + 1][1] = MoveNone;

  MovePicker picker(lease.buffers);
  init_picker(picker, MainCapSeeMargin, in_check);

  const detail_search::HistoryContext hist_ctx =
      detail_search::collect_history_context(ti, ss);

  int best_score = ScoreNone;
  int moves_played = 0;
  bool skip_quiets = false;

  while (Action move = next_move(picker, pos, ti, tt_move, skip_quiets)) {
    if (thread_data.stop) break;

    RootAction *root_slot = nullptr;
    if (at_root) {
      root_slot = find_root_move(ti, move);
      if (!root_slot) continue;
      bool already_in_pv = false;
      for (int idx = 0; idx < ti.multipv_index; ++idx) {
        if (ti.best_moves[idx] == move) {
          already_in_pv = true;
          break;
        }
      }
      if (already_in_pv) continue;
    }

    if (move == excluded_move || !is_legal(pos, move, in_check)) continue;

    const uint64_t nodes_before = ti.nodes.load();
    const int src_sq = extract_from(move);
    const int dst_sq = extract_to(move);
    const int moved_pc = is_valid_square(src_sq) ? pos.board[src_sq] : Pieces::Blank;
    const bool is_pawn = (get_piece_type(moved_pc) == PieceTypes::Pawn);
    const int rel_rank =
        is_pawn ? ((stm == Colors::White) ? get_rank(dst_sq) : (7 - get_rank(dst_sq)))
                : 0;
    const bool advanced_pawn = is_pawn && (rel_rank >= 5);
    const bool move_is_cap = is_cap(pos, move);

    int hist_score = 0;
    int full_hist = 0;
    int prune_hist = 0;
    int cap_hist = 0;
    if (!move_is_cap) {
      const int main_hist = ti.HistoryScores[moved_pc][dst_sq];
      const int cont_sum = (hist_ctx.c1 ? (*hist_ctx.c1)[moved_pc][dst_sq] : 0) +
                           (hist_ctx.c2 ? (*hist_ctx.c2)[moved_pc][dst_sq] : 0) +
                           (hist_ctx.c4 ? (*hist_ctx.c4)[moved_pc][dst_sq] : 0);
      full_hist = main_hist + cont_sum;
      hist_score = full_hist / 2;
      prune_hist = std::max(main_hist, hist_score);
    } else {
      cap_hist =
          ti.CapHistScores[moved_pc][dst_sq][captured_piece_type(pos, move)];
    }

    const bool gives_check = detail_search::move_gives_check(pos, move);

    if (!move_is_cap && !is_pv && best_score > -MateScore && ti.mate_search == 0) {
      if (!advanced_pawn && !in_check && !gives_check && depth < LMPDepth &&
          moves_played >= LMPBase + depth * depth / (improving ? LmpImprDiv : LmpNoImprDiv)) {
        skip_quiets = true;
      }
      if (!advanced_pawn && !in_check && !gives_check && depth < FPDepth &&
          picker.stage > Stages::Captures &&
          static_eval + (FPMargin1 + FPMargin2 * depth) < alpha) {
        continue;
      }
      if (!advanced_pawn && !in_check && !gives_check && depth < HistPruneDepth &&
          prune_hist < -HistPruneThreshold * depth) {
        continue;
      }
    }

    if (!at_root && !in_check && !gives_check && !(is_pawn && rel_rank >= 6) &&
        best_score > -MateThreshold && depth < SeePruningDepth) {
      const int see_margin =
          move_is_cap ? (depth * SeePruningNoisyMargin) : SeePruningQuietMargin;
      if (!SEE(pos, move, depth * see_margin)) continue;
    }

    const bool bad_capture = (picker.stage == Stages::BadCaptures);

    BoardState child_pos = pos;
    make_move(child_pos, move);

    int extension = 0;
    if (!at_root && ply < ti.current_iter * 2 && !singular_search &&
        depth >= SEDepth && move == tt_move && tt_hit && tt_score != ScoreNone &&
        std::abs(tt_score) < MateThreshold && tt_entry.depth >= depth - SETtDepthMargin &&
        (entry_type == EntryTypes::LBound || entry_type == EntryTypes::Exact)) {
      const int s_beta = tt_score - (depth * SEBetaDepthFactor) / 100;
      ti.excluded_move = move;
      const int s_score = search<false>(s_beta - 1, s_beta, (depth - 1) / 2, cutnode,
                                        pos, ti, table);
      if (s_score < s_beta) {
        if (!is_pv && s_score + SEDoubleExtMargin < s_beta && ply < ti.current_iter) {
          extension = 2 + (!move_is_cap && s_score < s_beta - SETripleExtMargin);
        } else {
          extension = 1;
        }
      } else if (s_beta >= beta) {
        return s_beta;
      } else if (cutnode || (!is_pv && tt_score >= beta)) {
        extension = -1;
      }
    }

    if (extension == 0 && !move_is_cap && depth >= HistExtMinDepth && full_hist > HistExtThreshold) {
      extension = 1;
    }
    if (extension == 0 && !move_is_cap && rel_rank >= 6 && depth >= 2 &&
        SEE(pos, move, 0)) {
      extension = 1;
    }

    if (extension > 0) {
      const int avail_budget = ti.current_iter + MaxExtBudget - (ply + depth);
      extension = (avail_budget <= 0) ? 0 : std::min(extension, avail_budget);
    }

    ti.nnue_state.push(child_pos);
    ss_push(pos, ti, move);

    auto clamp_child_depth = [&](int d) noexcept {
      return std::clamp(d, 0, std::max(0, depth - 1 + extension));
    };
    int next_depth = clamp_child_depth(std::min(depth - 1 + extension, MaxRootDepth - 1));

    int child_score = ScoreNone;
    bool run_zero_window = false;
    if (next_depth > 0 && depth >= LMRMinDepth && moves_played > is_pv &&
        ti.mate_search == 0) {
      int r = LMRTable[depth][moves_played];
      r = move_is_cap ? ((bad_capture ? r : (r / 2)) - cap_hist / LMRCapHistoryDiv)
                      : (r - full_hist / LMRHistoryDiv);
      r -= tt_pv;
      if (!move_is_cap && tt_capture) ++r;
      r -= (tt_hit && tt_entry.depth >= depth);
      r += !improving;
      r += cutnode;
      r -= gives_check;
      if (gives_check && depth >= 6) --r;
      if (advanced_pawn && r > 0) r = (rel_rank >= 6) ? 0 : std::max(0, r - LMRAdvPawnRed);
      r = std::clamp(r, 0, next_depth - 1);

      child_score = -search<false>(-alpha - 1, -alpha, next_depth - r,
                                   (r > 0) ? true : !cutnode, child_pos, ti, table);
      if (child_score > alpha) {
        run_zero_window = (r > 0);
        if (r >= 2 && child_score < best_score + next_depth && !at_root) {
          --next_depth;
        }
        next_depth = clamp_child_depth(next_depth);
      }
    } else {
      run_zero_window = moves_played || !is_pv;
    }

    if (run_zero_window) {
      child_score = -search<false>(-alpha - 1, -alpha, next_depth, !cutnode,
                                   child_pos, ti, table);
    }
    if ((child_score > alpha || !moves_played) && is_pv) {
      child_score =
          -search<true>(-beta, -alpha, next_depth, false, child_pos, ti, table);
    }

    ss_pop(ti);
    if (thread_data.stop) return (best_score != ScoreNone) ? best_score : alpha;

    if (at_root && root_slot) {
      root_slot->nodes += (ti.nodes.load() - nodes_before);
    }

    if (child_score > best_score) {
      best_score = child_score;
      if (child_score > alpha) {
        best_move = move;
        raised_alpha = true;
        alpha = child_score;
        ti.pv[pv_base] = best_move;
        if (child_score >= beta) break;

        for (int n = 0; n < MaxSearchPly - ply - 1; ++n) {
          ti.pv[pv_base + 1 + n] = ti.pv[pv_base + MaxSearchPly + n];
        }
      }
    }

    if (move_is_cap) {
      captures[cap_count++] = move;
    } else {
      quiets[quiet_count++] = move;
    }
    ++moves_played;
  }

  if (thread_data.stop) return (best_score != ScoreNone) ? best_score : alpha;

  if (at_root) {
    if (best_move != MoveNone) ti.best_moves[ti.multipv_index] = best_move;
    ti.best_scores[ti.multipv_index] = best_score;
  }

  if (best_score >= beta) {
    detail_search::update_fail_high_tables(
        pos, ti, hist_ctx, ply, depth, best_score, beta, best_move, quiets,
        quiet_count, captures, cap_count);
  }

  if (best_score == ScoreNone) {
    return singular_search ? alpha : (in_check ? (-MateScore + ply) : 0);
  }

  const uint8_t out_bound =
      (best_score >= beta) ? EntryTypes::LBound
                           : (raised_alpha ? EntryTypes::Exact : EntryTypes::UBound);
  if (!in_check && (!best_move || !is_cap(pos, best_move)) &&
      !(best_score >= beta && best_score <= ss->static_eval) &&
      !(!best_move && best_score >= ss->static_eval)) {
    detail_search::update_all_corrhist(pos, ti, depth, best_score, ss->static_eval);
  }

  if (!singular_search &&
      !(at_root && (ti.root_moves_limited || ti.multipv_index))) {
    insert_entry(hash, depth, best_move, raw_eval, score_to_tt(best_score, ply),
                 out_bound, ti.searches, tt_pv);
  }

  return best_score;
}

inline std::string format_pv(const BoardState &pos, const ThreadInfo &ti) {
  BoardState cursor = pos;
  std::string line;
  std::array<Action, MaxActions> legal{};

  for (int idx = 0; idx < MaxSearchPly && ti.pv[idx] != MoveNone; ++idx) {
    const Action move = ti.pv[idx];
    const int n_legal = legal_movegen(cursor, legal.data());
    bool is_ok = false;
    for (int i = 0; i < n_legal; ++i) {
      if (legal[i] == move) {
        is_ok = true;
        break;
      }
    }
    if (!is_ok) break;

    if (!line.empty()) line.push_back(' ');
    line += internal_to_uci(cursor, move);
    make_move(cursor, move);
  }
  return line;
}

inline void iterative_deepen(BoardState &pos, ThreadInfo &ti,
                             std::vector<TTBucket> &table) {
  ti.original_opt = ti.opt_time;
  ti.root_color = pos.color;
  calculate(pos);
  ti.nodes.store(0);
  ti.time_checks = 0;
  ti.search_ply = 0;
  ti.nnue_state.reset_nnue(pos);
  ti.excluded_move = MoveNone;
  ti.nmp_min_ply = 0;
  ti.best_moves.fill(MoveNone);
  ti.best_scores.fill(ScoreNone);
  for (auto &row : ti.KillerMoves) row.fill(MoveNone);
  for (auto &tbl : ti.LowPlyHistory) {
    for (auto &row : tbl) {
      for (int16_t &v : row) v /= LowPlyDecayDiv;
    }
  }

  ti.root_moves.reserve(MaxActions);
  std::array<Action, MaxActions> all_legal{};
  const int legal_count = legal_movegen(pos, all_legal.data());

  if (ti.root_moves_limited) {
    std::vector<RootAction> filtered;
    filtered.reserve(ti.root_moves.size());
    for (const RootAction &entry : ti.root_moves) {
      const Action cand = entry.move;
      bool valid = false;
      for (int i = 0; i < legal_count; ++i) {
        if (all_legal[i] == cand) {
          valid = true;
          break;
        }
      }
      if (!valid) continue;
      const bool dup = std::any_of(filtered.begin(), filtered.end(),
                                   [cand](const RootAction &r) { return r.move == cand; });
      if (!dup) filtered.push_back(RootAction{cand, 0});
    }
    ti.root_moves = std::move(filtered);
  } else {
    ti.root_moves.clear();
    for (int i = 0; i < legal_count; ++i) {
      ti.root_moves.push_back(RootAction{all_legal[i], 0});
    }
  }
  ti.best_moves[0] = ti.root_moves.empty() ? MoveNone : ti.root_moves[0].move;

  Action prev_best = MoveNone;
  int prev_score = ScoreNone;
  int alpha = ScoreNone;
  int beta = -ScoreNone;
  int bm_stability = 0;

  const int target_depth = std::clamp(ti.max_iter_depth, 1, MaxRootDepth);
  std::array<Action, MaxActions> completed_moves{};
  std::array<int, MaxActions> completed_scores{};
  completed_scores.fill(ScoreNone);
  std::array<Action, MaxSearchPly> completed_pv{};

  const int user_multipv = ti.multipv;
  const int req_multipv =
      (ti.thread_id == 0 && ti.uci_limit_strength && ti.uci_elo < MaxUciElo)
          ? std::max<int>(user_multipv, 4)
          : user_multipv;
  int active_multipv =
      std::min<int>(req_multipv, static_cast<int>(ti.root_moves.size()));

  const int start_depth =
      (ti.thread_id == 0) ? 1 : 1 + std::min<int>(ti.thread_id, 4);
  bool aborted = false;

  for (int depth = start_depth; !ti.root_moves.empty() && !aborted; ++depth) {
    if (thread_data.stop || depth > target_depth || depth > MaxRootDepth) break;

    active_multipv =
        std::min<int>(req_multipv, static_cast<int>(ti.root_moves.size()));

    for (ti.multipv_index = 0; ti.multipv_index < active_multipv; ++ti.multipv_index) {
      int window_depth = depth;
      int delta = std::max(AspMinDelta, AspStartWindow * AspDeltaPct / 100);
      bool extended_fail_low = false;
      bool saw_fail_low = false;
      int score = search<true>(alpha, beta, depth, false, pos, ti, table);

      while (score <= alpha || score >= beta || thread_data.stop) {
        if (thread_data.stop) {
          aborted = true;
          break;
        }

        if (ti.thread_id == 0 && search_output_enabled &&
            ti.multipv_index < user_multipv) {
          const char *bound_str = (score >= beta) ? "lowerbound" : "upperbound";
          const uint64_t total_nodes = detail_search::aggregate_nodes(ti);
          const int64_t elapsed_ms = time_elapsed(ti.start_time);
          const int64_t nps = elapsed_ms
                                  ? static_cast<int64_t>(total_nodes) * 1000 / elapsed_ms
                                  : static_cast<int64_t>(total_nodes) * 1000;
          const Action bound_move =
              (score <= alpha) ? prev_best : ti.best_moves[ti.multipv_index];
          std::string pv_tail;
          if (bound_move != MoveNone) {
            const std::string mv_str = internal_to_uci(pos, bound_move);
            if (mv_str != "0000") pv_tail = " pv " + mv_str;
          }
          const std::string score_text = detail_search::format_uci_eval(score);
          safe_printf("info multipv %i depth %i seldepth %i score %s %s nodes %" PRIu64
                      " nps %" PRIi64 " time %" PRIi64 "%s\n",
                      ti.multipv_index + 1, depth, ti.seldepth, score_text.c_str(),
                      bound_str, total_nodes, nps, elapsed_ms, pv_tail.c_str());
        }

        if (score <= alpha) {
          beta = (alpha + beta) / 2;
          alpha -= delta;
          if (score <= -MateScore + MaxSearchPly) alpha = ScoreNone;
          window_depth = depth;
          if (ti.thread_id == 0 && !extended_fail_low && depth > AspMinDepth &&
              !ti.is_movetime && !ti.infinite_search && !thread_data.pondering) {
            extended_fail_low = true;
            saw_fail_low = true;
            ti.eval_dropping = true;
            ti.opt_time = std::min<uint64_t>(
                ti.max_time,
                ti.opt_time + ti.original_opt * static_cast<uint64_t>(FailLowTimePct) / 100ULL);
            ti.time_manager.soft_limit = ti.opt_time;
          }
        } else if (score >= beta) {
          beta += delta;
          if (score >= MateScore - MaxSearchPly) beta = -ScoreNone;
          window_depth = std::max(window_depth - 1, std::max(1, depth - 2));
        }
        delta += delta * AspWidenPct / 100;
        score = search<true>(alpha, beta, window_depth, false, pos, ti, table);
      }

      if (aborted || score == ScoreNone) break;

      const std::string score_text = detail_search::format_uci_eval(score);
      ti.best_moves[ti.multipv_index] = ti.pv[0];
      completed_moves[ti.multipv_index] = ti.pv[0];
      completed_scores[ti.multipv_index] = score;
      if (ti.multipv_index == 0) {
        std::copy_n(ti.pv.begin(), MaxSearchPly, completed_pv.begin());
      }

      if (ti.thread_id == 0) {
        const uint64_t total_nodes = detail_search::aggregate_nodes(ti);
        const int64_t elapsed_ms = time_elapsed(ti.start_time);
        const int64_t nps =
            static_cast<int64_t>(total_nodes) * 1000 / std::max<int64_t>(1, elapsed_ms);

        if (search_output_enabled && ti.multipv_index < user_multipv) {
          safe_printf("info multipv %i depth %i seldepth %i score %s nodes %" PRIu64
                      " nps %" PRIi64 " time %" PRIi64 " tbhits %" PRIu64 " pv %s\n",
                      ti.multipv_index + 1, depth, ti.seldepth, score_text.c_str(),
                      total_nodes, nps, elapsed_ms, thread_data.tb_hits.load(),
                      format_pv(pos, ti).c_str());
        }

        if (!ti.is_movetime && !ti.infinite_search && !thread_data.pondering &&
            ti.multipv == 1 && ti.multipv_index == 0 && depth > 6) {
          const bool best_move_changed = (prev_best != MoveNone && ti.best_moves[0] != prev_best);
          if (!best_move_changed && prev_best != MoveNone) {
            bm_stability = std::min(bm_stability + 1, 8);
            ++ti.stability_counter;
          } else {
            bm_stability = 0;
            ti.stability_counter = 0;
          }
          const int eval_drop =
              (prev_score != ScoreNone && std::abs(score) < MateThreshold &&
               std::abs(prev_score) < MateThreshold)
                  ? (prev_score - score)
                  : 0;
          ti.eval_dropping = saw_fail_low || (eval_drop > EvalDropThreshold) || best_move_changed;
          ti.best_move_stable = (ti.stability_counter >= BmStableCount && !ti.eval_dropping);
          double eval_scale = std::clamp(
              1.0 + eval_drop * (EvalScaleSlope / 1000.0),
              EvalScaleMin / 100.0, EvalScaleMax / 100.0);
          if (saw_fail_low) eval_scale = std::max(eval_scale, FailLowEvalScale / 100.0);
          if (best_move_changed) eval_scale = std::max(eval_scale, BmChangeTimeScale / 100.0);
          RootAction *best_entry = find_root_move(ti, ti.best_moves[0]);
          adjust_soft_limit(ti, best_entry ? best_entry->nodes : 0ULL, bm_stability,
                            eval_scale);
        }

        const bool budget_exhausted =
            !ti.infinite_search && !thread_data.pondering &&
            (static_cast<uint64_t>(elapsed_ms) > ti.opt_time ||
             (ti.uci_limit_strength && ti.multipv_index + 1 >= active_multipv &&
              total_nodes >= ti.max_nodes_searched));
        if (budget_exhausted) {
          thread_data.stop = true;
        }
      }

      if (thread_data.stop) {
        aborted = true;
        break;
      }

      if (ti.multipv_index == 0) {
        prev_best = ti.best_moves[0];
        prev_score = score;
      }

      if (depth > AspMinDepth && ti.multipv_index == 0 && active_multipv == 1) {
        // Stagger helper aspiration windows around the score to vary worker trees.
        const int half_win =
            std::max(AspMinHalfWin, AspStartWindow + AspHalfWinOffset) +
            (ti.thread_id % 4) * AspThreadSpread;
        alpha = score - half_win;
        beta = score + half_win;
      } else {
        alpha = ScoreNone;
        beta = -ScoreNone;
      }
    }

    if (aborted) break;

    if (ti.mate_search > 0 && completed_scores[0] >= MateScore - MaxSearchPly) {
      const int mate_dist = (MateScore - completed_scores[0] + 1) / 2;
      if (mate_dist <= ti.mate_search) {
        if (ti.thread_id == 0) thread_data.stop = true;
        break;
      }
    }
    if (std::abs(completed_scores[0]) >= MateScore - MaxSearchPly) {
      const int plies_to_mate = MateScore - std::abs(completed_scores[0]);
      if (depth >= plies_to_mate + 2) {
        if (ti.thread_id == 0 && !ti.infinite_search && !thread_data.pondering) {
          thread_data.stop = true;
        }
        break;
      }
    }
  }

  if (completed_moves[0] != MoveNone) {
    ti.best_moves = completed_moves;
    ti.best_scores = completed_scores;
    std::copy(completed_pv.begin(), completed_pv.end(), ti.pv.begin());
  }

  if (ti.thread_id == 0 && ti.uci_limit_strength && ti.uci_elo < MaxUciElo) {
    detail_search::apply_elo_move_selection(ti, active_multipv, completed_moves,
                                            completed_scores);
  }

  if (ti.thread_id == 0) {
    if (!thread_data.stop && (ti.infinite_search || thread_data.pondering)) {
      std::unique_lock lock(thread_data.control_mutex);
      thread_data.control_cv.wait(lock, [&] {
        return thread_data.stop || (!ti.infinite_search && !thread_data.pondering);
      });
    }
    thread_data.stop = true;
  }

  if (ti.thread_id == 0 && thread_data.emit_bestmove.load() &&
      (!ti.infinite_search || thread_data.stop)) {
    ti.ponder_move =
        (completed_moves[0] != MoveNone && ti.pv[0] == ti.best_moves[0]) ? ti.pv[1]
                                                                         : MoveNone;
    if (ti.best_moves[0] == MoveNone) {
      safe_printf("bestmove 0000\n");
    } else {
      const std::string best_str = internal_to_uci(pos, ti.best_moves[0]);
      if (ti.use_ponder && ti.ponder_move != MoveNone) {
        BoardState after_pos = pos;
        make_move(after_pos, ti.best_moves[0]);
        safe_printf("bestmove %s ponder %s\n", best_str.c_str(),
                    internal_to_uci(after_pos, ti.ponder_move).c_str());
      } else {
        safe_printf("bestmove %s\n", best_str.c_str());
      }
    }
  }
}

inline void filter_root_tablebase(const BoardState &pos, ThreadInfo &ti) {
  if (!tb_initialized || !ti.use_syzygy ||
      pop_count(pos.colors_bb[0] | pos.colors_bb[1]) >
          std::min<int>(TB_LARGEST, ti.syzygy_probe_limit)) {
    return;
  }

  for (const auto &wing_rooks : pos.castling_squares) {
    for (int sq : wing_rooks) {
      if (sq != SquareNone) return;
    }
  }

  bool seen_before = false;
  for (int i = 0; i < ti.game_ply; ++i) {
    if (ti.game_hist[i].position_key == pos.zobrist_key) {
      seen_before = true;
      break;
    }
  }

  auto root_tb = std::make_unique<TbRootMoves>();
  const unsigned ep = tb_ep_square(pos);
  const unsigned rule50 =
      ti.syzygy_50_move_rule ? static_cast<unsigned>(std::min<int>(pos.halfmoves, 100)) : 0U;
  auto query_root = [&](bool use_dtz) {
    const auto &pbb = pos.pieces_bb;
    if (use_dtz) {
      return tb_probe_root_dtz(
          pos.colors_bb[0], pos.colors_bb[1], pbb[PieceTypes::King],
          pbb[PieceTypes::Queen], pbb[PieceTypes::Rook], pbb[PieceTypes::Bishop],
          pbb[PieceTypes::Knight], pbb[PieceTypes::Pawn], rule50, 0U, ep,
          pos.color == Colors::White, seen_before, ti.syzygy_50_move_rule,
          root_tb.get());
    }
    return tb_probe_root_wdl(
        pos.colors_bb[0], pos.colors_bb[1], pbb[PieceTypes::King],
        pbb[PieceTypes::Queen], pbb[PieceTypes::Rook], pbb[PieceTypes::Bishop],
        pbb[PieceTypes::Knight], pbb[PieceTypes::Pawn], rule50, 0U, ep,
        pos.color == Colors::White, ti.syzygy_50_move_rule, root_tb.get());
  };

  if (!query_root(true) && (rule50 || !query_root(false))) {
    ++thread_data.tb_fails;
    return;
  }
  ++thread_data.tb_hits;

  std::vector<RootAction> filtered;
  int top_rank = INT32_MIN;
  for (unsigned i = 0; i < root_tb->size; ++i) {
    const auto &tb_entry = root_tb->moves[i];
    const unsigned tb_promo = TB_MOVE_PROMOTES(tb_entry.move);
    for (const RootAction &legal : ti.root_moves) {
      const Action m = legal.move;
      if (extract_from(m) != TB_MOVE_FROM(tb_entry.move) ||
          extract_to(m) != TB_MOVE_TO(tb_entry.move)) {
        continue;
      }
      const bool promo_match =
          tb_promo ? (extract_type(m) == MoveTypes::Promotion &&
                      extract_promo(m) == static_cast<int>(4U - tb_promo))
                   : (extract_type(m) != MoveTypes::Promotion);
      if (!promo_match) continue;

      if (tb_entry.tbRank > top_rank) {
        top_rank = tb_entry.tbRank;
        filtered.clear();
      }
      if (tb_entry.tbRank == top_rank) filtered.push_back(legal);
      break;
    }
  }

  if (!filtered.empty()) {
    ti.root_moves = std::move(filtered);
    ti.root_moves_limited = true;
  }
}

inline void search_position(BoardState &pos, ThreadInfo &ti,
                            std::vector<TTBucket> &table) {
  ti.position = pos;
  ti.thread_id = 0;
  ti.nodes.store(0);
  thread_data.tb_hits = 0;
  thread_data.tb_fails = 0;

  if (!ti.root_moves_limited) {
    ti.root_moves.clear();
    std::array<Action, MaxActions> legal{};
    const int n_legal = legal_movegen(pos, legal.data());
    for (int i = 0; i < n_legal; ++i) ti.root_moves.push_back(RootAction{legal[i], 0});
  }

  filter_root_tablebase(pos, ti);

  for (size_t i = 0; i < thread_data.thread_infos.size(); ++i) {
    thread_data.thread_infos[i] = ti;
    thread_data.thread_infos[i].thread_id = static_cast<uint16_t>(i + 1);
    thread_data.thread_infos[i].multipv = 1;
  }

  std::atomic<bool> workers_ready{false};
  bool launch_error = false;
  for (size_t i = 0; i < thread_data.thread_infos.size(); ++i) {
    try {
      thread_data.threads.emplace_back([i, &table, &workers_ready] {
        workers_ready.wait(false);
        ThreadInfo &worker = thread_data.thread_infos[i];
        iterative_deepen(worker.position, worker, table);
      });
    } catch (const std::system_error &) {
      launch_error = true;
      break;
    }
  }

  if (launch_error) {
    thread_data.stop = true;
    workers_ready.store(true);
    workers_ready.notify_all();
    for (std::thread &t : thread_data.threads) t.join();
    thread_data.threads.clear();
    std::exit(EXIT_FAILURE);
  }

  workers_ready.store(true);
  workers_ready.notify_all();
  iterative_deepen(pos, ti, table);

  thread_data.stop = true;
  for (std::thread &t : thread_data.threads) t.join();
  thread_data.threads.clear();
  ti.searches = static_cast<uint8_t>((ti.searches + 1) % MaxAge);
}

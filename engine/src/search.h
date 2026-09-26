#pragma once
#include "movepick.h"
#include "params.h"
#include "position.h"

#include "../fathom/src/tbprobe.h"
#include "utils.h"
#include <cassert>
#include <memory>

inline Action uci_to_internal(const BoardState &position, const std::string &uci) {
  std::array<Action, MaxActions> list{};
  const int nmoves = legal_movegen(position, list.data());

  for (int i = 0; i < nmoves; i++) {
    if (internal_to_uci(position, list[i]) == uci)
      return list[i];
  }

  return MoveNone;
}

extern bool tb_initialized;

inline int64_t
safe_elapsed(const std::chrono::steady_clock::time_point &start) noexcept {
  const auto ms = time_elapsed(start);
  return ms ? ms : 1;
}


inline unsigned tb_ep_square(const BoardState &position) noexcept {
  const int ep = position.ep_square;
  if (!is_valid_square(ep)) return 0;
  const int color = position.color;
  const int captured = ep + (color ? Directions::North : Directions::South);
  if (!is_valid_square(captured) ||
      position.board[captured] != Pieces::WPawn + (color ^ 1)) return 0;
  const uint64_t our_pawns =
      position.colors_bb[color] & position.pieces_bb[PieceTypes::Pawn];
  if (!(PAWN_ATK_SAFE(color ^ 1, ep) & our_pawns)) return 0;
  return static_cast<unsigned>(ep);
}

inline int probe_wdl_tb(const BoardState &position, const ThreadInfo &thread_info) noexcept {

  if (!tb_initialized || !thread_info.use_syzygy)
    return ScoreNone;

  const int material_count = pop_count(position.colors_bb[0] | position.colors_bb[1]);
  if (material_count > std::min<int>(TB_LARGEST, thread_info.syzygy_probe_limit))
    return ScoreNone;

  unsigned castling = 0;
  if (position.castling_squares[Colors::White][Sides::Kingside] != SquareNone)
    castling |= TB_CASTLING_K;
  if (position.castling_squares[Colors::White][Sides::Queenside] != SquareNone)
    castling |= TB_CASTLING_Q;
  if (position.castling_squares[Colors::Black][Sides::Kingside] != SquareNone)
    castling |= TB_CASTLING_k;
  if (position.castling_squares[Colors::Black][Sides::Queenside] != SquareNone)
    castling |= TB_CASTLING_q;
  if (castling)
    return ScoreNone;

  if (thread_info.syzygy_50_move_rule && position.halfmoves != 0) return ScoreNone;
  const unsigned ep = tb_ep_square(position);

  const unsigned result = tb_probe_wdl(position.colors_bb[0], position.colors_bb[1],
                                       position.pieces_bb[PieceTypes::King],
                                       position.pieces_bb[PieceTypes::Queen],
                                       position.pieces_bb[PieceTypes::Rook],
                                       position.pieces_bb[PieceTypes::Bishop],
                                       position.pieces_bb[PieceTypes::Knight],
                                       position.pieces_bb[PieceTypes::Pawn], 0,
                                       castling, ep,
                                       position.color == Colors::White);
  if (result == TB_RESULT_FAILED) {
    thread_data.tb_fails++;
    return ScoreNone;
  }
  thread_data.tb_hits++;
  const int wdl = TB_GET_WDL(result);
  switch (wdl) {
  case TB_WIN:
    return TB_WIN_SCORE;
  case TB_CURSED_WIN:
    return thread_info.syzygy_50_move_rule ? 0 : TB_WIN_SCORE;
  case TB_DRAW:
    return 0;
  case TB_BLESSED_LOSS:
    return thread_info.syzygy_50_move_rule ? 0 : -TB_WIN_SCORE;
  case TB_LOSS:
    return -TB_WIN_SCORE;
  default:
    return ScoreNone;
  }
}

inline void update_history(int16_t &entry, int score) noexcept {
  entry += score - entry * std::abs(score) / 16384;
}
inline void update_corrhist(int16_t &entry, int score) noexcept {
  entry += score - entry * std::abs(score) / 1024;
}

inline void update_continuation_histories(ThreadInfo &thread_info, int piece,
                                          int sq, int bonus, int their_last,
                                          int their_piece, int our_last,
                                          int our_piece, int ply4_last,
                                          int ply4_piece) noexcept {

  update_history(thread_info.HistoryScores[piece][sq], bonus);

  if (their_last != SquareNone) {
    update_history(
        thread_info.ContHistScores[their_piece][their_last][piece][sq], bonus);
  }
  if (our_last != SquareNone) {
    update_history(thread_info.ContHistScores[our_piece][our_last][piece][sq],
                   bonus);
  }
  if (ply4_last != SquareNone) {
    update_history(thread_info.ContHistScores[ply4_piece][ply4_last][piece][sq],
                   bonus / 2);
  }
}

inline bool out_of_time(ThreadInfo &thread_info) noexcept {
  if (thread_data.stop)
    return true;

  if (thread_info.thread_id != 0)
    return false;

  const int64_t hit_time =
      thread_data.ponder_hit_time.load(std::memory_order_relaxed) >= 0
          ? thread_data.ponder_hit_time.exchange(-1, std::memory_order_relaxed)
          : -1;
  if (hit_time >= 0) {
    const auto hit = std::chrono::steady_clock::time_point(std::chrono::milliseconds(hit_time));
    const auto ponder_ms = std::max<int64_t>(0, std::chrono::duration_cast<std::chrono::milliseconds>(hit - thread_info.start_time).count());
    thread_info.start_time = hit;
    thread_info.opt_time = std::min(thread_info.max_time, thread_info.opt_time +
        static_cast<uint64_t>(ponder_ms) * thread_info.ponder_time_factor / 100);
    thread_info.time_manager.soft_limit = thread_info.opt_time;
    thread_info.pondering = false;
    thread_info.ponder_hit = true;
  }

  thread_info.time_checks++;
  constexpr uint16_t check_interval = 256;
  if (thread_info.time_checks >= check_interval) {
    thread_info.time_checks = 0;
    // Summing every helper thread's nodes is O(#threads); doing it once per
    // interval instead of once per node keeps high thread counts scalable.
    uint64_t total_nodes = thread_info.nodes.load(std::memory_order_relaxed);
    for (auto &ti : thread_data.thread_infos)
      total_nodes += ti.nodes.load(std::memory_order_relaxed);
    if (!thread_data.pondering && total_nodes >= thread_info.max_nodes_searched) {
      thread_data.stop = true;
      return true;
    }
    if (!thread_info.infinite_search && !thread_data.pondering) {
      const uint64_t elapsed = time_elapsed(thread_info.start_time);
      if (thread_info.time_manager.should_stop(
              elapsed, thread_info.best_move_stable,
              thread_info.is_movetime) ||
          elapsed > thread_info.max_time) {
        thread_data.stop = true;
        return true;
      }
    }
  }
  return false;
}

inline bool has_non_pawn_material(const BoardState &position, int color) noexcept {
  const int s_indx = 2 + color;
  return (position.material_count[s_indx] ||
          position.material_count[s_indx + 2] ||
          position.material_count[s_indx + 4] ||
          position.material_count[s_indx + 6]);
}

inline int eval(BoardState &position, ThreadInfo &thread_info) {
  const int piece_count = pop_count(position.colors_bb[0] | position.colors_bb[1]);
  return thread_info.nnue_state.evaluate(position.color, piece_count);
}

inline int correct_eval(const BoardState &position, const ThreadInfo &thread_info,
                        int eval) noexcept {

  eval = eval * std::max(0, HalfmoveScaleMax - position.halfmoves) / HalfmoveScaleMax;

  int corr =
      thread_info
          .PawnCorrHist[position.color][get_corrhist_index(position.pawn_key)];

  corr +=
      thread_info
          .NonPawnCorrHist[position.color][Colors::White][get_corrhist_index(
              position.non_pawn_key[Colors::White])];
  corr +=
      thread_info
          .NonPawnCorrHist[position.color][Colors::Black][get_corrhist_index(
              position.non_pawn_key[Colors::Black])];

  return std::clamp(eval + (CorrWeight * corr / 512), -MaxEval, MaxEval);
}

inline void ss_push(const BoardState &position, ThreadInfo &thread_info, Action move) noexcept {
  assert(thread_info.search_ply + 1 < MaxSearchPly);
  assert(thread_info.game_ply + 1 < MaxGameLen);
  auto &record = thread_info.game_hist[thread_info.game_ply];
  record.position_key = position.zobrist_key;
  record.played_move = move;
  record.piece_moved = move == MoveNone ? Pieces::Blank : position.board[extract_from(move)];
  ++thread_info.search_ply;
  ++thread_info.game_ply;
}

inline void ss_pop(ThreadInfo &thread_info) noexcept {
  assert(thread_info.search_ply > 0 && thread_info.game_ply > 0);
  --thread_info.search_ply;
  --thread_info.game_ply;
  thread_info.nnue_state.pop();
}

inline bool material_draw(const BoardState &position) noexcept {
  if (position.pieces_bb[PieceTypes::Pawn] || position.pieces_bb[PieceTypes::Rook] ||
      position.pieces_bb[PieceTypes::Queen]) return false;
  const uint64_t knights = position.pieces_bb[PieceTypes::Knight];
  const uint64_t bishops = position.pieces_bb[PieceTypes::Bishop];
  if (pop_count(knights | bishops) <= 1) return true;
  constexpr uint64_t dark = 0xAA55AA55AA55AA55ULL;
  return !knights && (!(bishops & dark) || !(bishops & ~dark));
}

inline bool is_draw(const BoardState &position, ThreadInfo &thread_info) noexcept {
  if (position.halfmoves >= 100) {
    if (attacks_square(position, get_king_pos(position, position.color), position.color ^ 1)) {
      std::array<Action, MaxActions> moves{};
      if (!legal_movegen(position, moves.data())) return false;
    }
    return true;
  }
  if (material_draw(position)) return true;
  int repetitions = 0;
  const int earliest = std::max(0, int(thread_info.game_ply) - position.halfmoves);
  const int root_ply = int(thread_info.game_ply) - thread_info.search_ply;
  for (int i = int(thread_info.game_ply) - 1; i >= earliest; --i) {
    const auto &record = thread_info.game_hist[i];
    if (record.played_move == MoveNone) break;
    if (record.position_key == position.zobrist_key &&
        (++repetitions == 2 || i >= root_ply)) return true;
  }
  return false;
}

inline int draw_score([[maybe_unused]] const BoardState &position, ThreadInfo &thread_info) noexcept {
  return 1 - (thread_info.nodes.load(std::memory_order_relaxed) & 3);
}

inline int qsearch(int alpha, int beta, BoardState &position, ThreadInfo &thread_info,
            std::vector<TTBucket> &table, int qdepth = 0) {

  constexpr int MAX_QDEPTH = 16;

  auto eval_now = [&](BoardState &pos) {
    return correct_eval(pos, thread_info, eval(pos, thread_info));
  };

  std::array<Action, MaxActions> terminal_moves{};
  if (!legal_movegen(position, terminal_moves.data())) {
    return attacks_square(position, get_king_pos(position, position.color), position.color ^ 1)
               ? -MateScore + thread_info.search_ply : 0;
  }

  if (qdepth >= MAX_QDEPTH) {
    return eval_now(position);
  }

  int ply = thread_info.search_ply;

  if (ply >= MaxSearchPly - 4) {
    return eval_now(position);
  }

  if (thread_info.game_ply >= MaxGameLen - 2) {
    return eval_now(position);
  }

  if (ply && is_draw(position, thread_info)) {
    return draw_score(position, thread_info);
  }

  if (out_of_time(thread_info)) {
    return correct_eval(position, thread_info, eval(position, thread_info));
  }

  const int tb_score = probe_wdl_tb(position, thread_info);
  if (tb_score != ScoreNone)
    return tb_score;

  StateRecord *ss = &(thread_info.game_hist[thread_info.game_ply]);

  ++thread_info.nodes;
  if (ply > thread_info.seldepth)
    thread_info.seldepth = ply;

  uint64_t hash = position.zobrist_key;

  bool tt_hit;
  TTEntry entry = probe_entry(hash, tt_hit, thread_info.searches, table);

  int entry_type = EntryTypes::None;
  int tt_static_eval = ScoreNone;
  int tt_score = ScoreNone;
  Action tt_move = MoveNone;

  if (tt_hit) {
    entry_type = entry.get_type();
    tt_static_eval = entry.static_eval;
    tt_score = position.halfmoves < 90 ? score_from_tt(entry.score, ply) : ScoreNone;
    tt_move = entry.best_move;
  }

  if (tt_score != ScoreNone &&
      ((entry_type == EntryTypes::Exact) ||
       (entry_type == EntryTypes::LBound && tt_score >= beta) ||
       (entry_type == EntryTypes::UBound && tt_score <= alpha))) {
    return tt_score;
  }

  uint64_t in_check = attacks_square(
      position, get_king_pos(position, position.color), position.color ^ 1);

  int raw_eval = ScoreNone;
  int static_eval = ScoreNone;
  int stand_pat = ScoreNone;
  int best_score = ScoreNone;
  Action best_move = MoveNone;
  bool raised_alpha = false;

  if (!in_check) {
    raw_eval = (tt_static_eval == ScoreNone) ? eval(position, thread_info)
                                             : tt_static_eval;
    static_eval = correct_eval(position, thread_info, raw_eval);
    ss->static_eval = static_eval;

    stand_pat = static_eval;
    best_score = stand_pat;

    if (tt_score != ScoreNone) {
      if (entry_type == EntryTypes::Exact ||
          (entry_type == EntryTypes::UBound && tt_score < stand_pat) ||
          (entry_type == EntryTypes::LBound && tt_score > stand_pat)) {
        stand_pat = best_score = tt_score;
      }
    }

    if (stand_pat >= beta) {
      insert_entry(hash, 0, MoveNone, raw_eval,
                   score_to_tt(stand_pat, ply), EntryTypes::LBound,
                   thread_info.searches);
      return stand_pat;
    }

    if (stand_pat > alpha) {
      alpha = stand_pat;
      raised_alpha = true;
    }
  } else {
    ss->static_eval = ScoreNone;
  }

  MovePicker picker;
  init_picker(picker, position, -107, in_check);

  if (!in_check && tt_move != MoveNone) {
    bool tt_is_cap = is_cap(position, tt_move);
    bool tt_is_promo = extract_type(tt_move) == MoveTypes::Promotion;
    if (!tt_is_cap && !tt_is_promo)
      tt_move = MoveNone;
  }

  while (Action move =
             next_move(picker, position, thread_info, tt_move, !in_check)) {
    if (thread_data.stop)
      break;
    if (!in_check && picker.stage > Stages::Captures)
      break;
    if (!is_legal(position, move))
      continue;

    const int to_sq = extract_to(move);

    if (!in_check && stand_pat != ScoreNone) {
      int captured_piece = position.board[to_sq];
      if (!captured_piece && extract_type(move) == MoveTypes::EnPassant) {
        captured_piece = Pieces::WPawn + (position.color ^ 1);
      }
      int capture_value = MaterialValues[get_piece_type(captured_piece)];
      int promotion_gain = 0;
      if (extract_type(move) == MoveTypes::Promotion) {
        promotion_gain = MaterialValues[PromoPieceTypes[extract_promo(move)]] -
                         MaterialValues[PieceTypes::Pawn];
      }
      int delta_margin = capture_value + promotion_gain + DeltaMarginBase;
      if (stand_pat + delta_margin < alpha)
        continue;
    }

    BoardState moved_position = position;
    make_move(moved_position, move);
    update_nnue_state(thread_info, move, position, moved_position);

    int score = ScoreNone;
    bool can_recurse = (thread_info.search_ply + 1 < MaxSearchPly - 4) &&
                       (thread_info.game_ply < MaxGameLen - 2);

    if (can_recurse) {
      ss_push(position, thread_info, move);
      score = -qsearch(-beta, -alpha, moved_position, thread_info, table,
                       qdepth + 1);
      ss_pop(thread_info);
    } else {
      const int leaf_eval = eval_now(moved_position);
      score = -leaf_eval;
      thread_info.nnue_state.pop();
    }

    if (thread_data.stop) {
      // The caller discards interrupted results; do not invent a new score.
      return best_score;
    }

    if (best_score == ScoreNone || score > best_score) {
      best_score = score;
    }

    if (score > alpha) {
      alpha = score;
      best_move = move;
      raised_alpha = true;
      if (score >= beta)
        break;
    }
  }

  if (best_score == ScoreNone) {
    if (in_check) {
      return -MateScore + ply;
    }
    best_score = (stand_pat != ScoreNone) ? stand_pat : eval_now(position);
  }

  uint8_t store_type = EntryTypes::UBound;
  if (best_score >= beta)
    store_type = EntryTypes::LBound;
  else if (raised_alpha)
    store_type = EntryTypes::Exact;

  insert_entry(hash, 0, best_move, raw_eval,
               score_to_tt(best_score, ply), store_type, thread_info.searches);
  return best_score;
}

template <bool is_pv>
inline int search(int alpha, int beta, int depth, bool cutnode, BoardState &position,
           ThreadInfo &thread_info, std::vector<TTBucket> &table) {

  if (thread_info.game_ply >= MaxGameLen - 2)
    return correct_eval(position, thread_info, eval(position, thread_info));
  StateRecord *ss = &(thread_info.game_hist[thread_info.game_ply]);

  if (!thread_info.search_ply) {
    thread_info.current_iter = depth;
    thread_info.seldepth = 0;
    thread_info.pv.fill(MoveNone);
  }

  int ply = thread_info.search_ply, pv_index = ply * MaxSearchPly;
  Action excluded_move = thread_info.excluded_move;
  bool singular_search = (excluded_move != MoveNone);
  // Leaf children return through qsearch; their PV row must not keep moves
  // from an earlier line, or the parent copies them into its PV.
  if (!singular_search) {
    thread_info.pv[pv_index] = MoveNone;
  }

  if (ply > thread_info.seldepth) {
    thread_info.seldepth = ply;
  }

  if (ply >= MaxRootDepth) {
    return correct_eval(position, thread_info, eval(position, thread_info));
  }
  depth = std::min(depth, MaxRootDepth - ply);

  if (out_of_time(thread_info) || ply >= MaxSearchPly - 1) {

    return correct_eval(position, thread_info, eval(position, thread_info));
  }

  if (ply && is_draw(position, thread_info)) {
    return draw_score(position, thread_info);
  }

  if (depth <= 0) {
    return qsearch(alpha, beta, position, thread_info, table);
  }
  ++thread_info.nodes;

  bool root = !ply, color = position.color, raised_alpha = false;

  Action best_move = MoveNone;

  thread_info.excluded_move = MoveNone;

  int score = ScoreNone;

  uint64_t hash = position.zobrist_key;

  int mate_distance = MateScore - 1 - ply;
  if (mate_distance < beta)

  {
    beta = mate_distance;
    if (alpha >= beta) {
      return beta;
    }
  }

  bool tt_hit;
  TTEntry entry = probe_entry(hash, tt_hit, thread_info.searches, table);

  int entry_type = EntryTypes::None, tt_static_eval = ScoreNone,
      tt_score = ScoreNone, tt_move = MoveNone;

  if (tt_hit && !singular_search) {
    entry_type = entry.get_type();
    tt_static_eval = entry.static_eval;
    tt_score = position.halfmoves < 90 ? score_from_tt(entry.score, ply) : ScoreNone;
    tt_move = entry.best_move;
  }

  if (tt_score != ScoreNone && !is_pv && entry.depth >= depth) {

    if ((entry_type == EntryTypes::Exact) ||
        (entry_type == EntryTypes::LBound && tt_score >= beta) ||
        (entry_type == EntryTypes::UBound && tt_score <= alpha)) {
      return tt_score;
    }
  }

  uint64_t in_check =
      attacks_square(position, get_king_pos(position, color), color ^ 1);

  int32_t static_eval;
  int32_t raw_eval;

  if (in_check) {
    static_eval = raw_eval = ScoreNone;
  } else if (singular_search) {
    static_eval = raw_eval = ss->static_eval;
  } else {
    raw_eval = (tt_static_eval == ScoreNone) ? eval(position, thread_info) : tt_static_eval;
    static_eval = correct_eval(position, thread_info, raw_eval);

    if (!tt_hit) {
      insert_entry(hash, 0, MoveNone, raw_eval, ScoreNone,
                   EntryTypes::None, thread_info.searches);
    }
  }

  ss->static_eval = static_eval;

  bool improving = false;

  if (ply > 1 && thread_info.game_ply >= 2 && !in_check &&
      static_eval > (ss - 2)->static_eval) {
    improving = true;
  }

  if (tt_score != ScoreNone) {
    if (entry_type == EntryTypes::Exact ||
        (entry_type == EntryTypes::UBound && tt_score < static_eval) ||
        (entry_type == EntryTypes::LBound && tt_score > static_eval)) {

      static_eval = tt_score;
    }
  }

  if (!is_pv && !in_check && !singular_search) {

    if (depth >= thread_info.syzygy_probe_depth) {
      const int tb_score = probe_wdl_tb(position, thread_info);
      if (tb_score != ScoreNone && (tb_score >= beta || tb_score <= alpha))
        return tb_score;
    }

    if (thread_info.mate_search == 0) {
      if (depth <= RFPMaxDepth &&
          static_eval - RFPMargin * (depth - improving) >= beta) {
        return (static_eval + beta) / 2;
      }

      if (!is_pv && depth <= 3 &&
          static_eval + RazorMargin * depth < alpha) {
        int razor_score = qsearch(alpha, beta, position, thread_info, table);
        if (razor_score <= alpha)
          return razor_score;
      }

      if (static_eval >= beta && depth >= NMPMinDepth &&
          has_non_pawn_material(position, color) && thread_info.game_ply > 0 &&
          (ss - 1)->played_move != MoveNone) {

        BoardState temp_pos = position;

        make_move(temp_pos, MoveNone);

        update_nnue_state(thread_info, MoveNone, position, temp_pos);
        ss_push(position, thread_info, MoveNone);

        int R = NMPBase + depth / NMPDepthDiv +
                std::min(3, (static_eval - beta) / NMPEvalDiv);
        score = -search<false>(-beta, -beta + 1, depth - R, !cutnode, temp_pos,
                               thread_info, table);

        ss_pop(thread_info);

        if (score >= beta) {
          if (score >= MateThreshold) {
            score = beta;
          }
          return score;
        }
      }
    }
  }

  if (!is_pv && !in_check && !singular_search && cutnode && thread_info.mate_search == 0 &&
      depth >= MultiCutDepth && tt_move != MoveNone) {
    int mc_cuts = 0;
    int mc_moves = 0;

    MovePicker mc_picker;
    init_picker(mc_picker, position, 0, in_check);

    while (Action move =
               next_move(mc_picker, position, thread_info, tt_move, false)) {
      if (thread_data.stop)
        break;
      if (mc_moves >= MultiCutMoves)
        break;
      if (!is_legal(position, move))
        continue;

      mc_moves++;

      BoardState mc_pos = position;
      make_move(mc_pos, move);

      update_nnue_state(thread_info, move, position, mc_pos);
      ss_push(position, thread_info, move);
      int mc_score = -search<false>(-beta, -beta + 1, depth - 4, false, mc_pos,
                                    thread_info, table);
      ss_pop(thread_info);

      if (mc_score >= beta) {
        mc_cuts++;
        if (mc_cuts >= MultiCutCuts) {
          return beta;
        }
      }
    }
  }

  if ((is_pv || cutnode) && tt_move == MoveNone && depth > IIRMinDepth) {

    depth--;
  }

  int p_beta = beta + ProbCutMargin;
  if (!root && !is_pv && !in_check && !singular_search && thread_info.mate_search == 0 && depth >= 5 && abs(beta) < MateThreshold &&
      (!tt_hit || entry.depth + 4 <= depth || tt_score >= p_beta)) {

    int threshold = p_beta - static_eval;
    MovePicker probcut_p;
    init_picker(probcut_p, position, threshold, in_check);
    Action p_tt_move =
        (tt_move != MoveNone && SEE(position, tt_move, threshold) ? tt_move
                                                                  : MoveNone);

    while (Action move =
               next_move(probcut_p, position, thread_info, p_tt_move, true)) {
      if (thread_data.stop)
        break;

      if (probcut_p.stage > Stages::Captures) {
        break;
      }
      if (move == excluded_move || !is_legal(position, move)) {
        continue;
      }

      BoardState moved_position = position;

      make_move(moved_position, move);

      update_nnue_state(thread_info, move, position, moved_position);
      ss_push(position, thread_info, move);

      int probcut_score =
          -qsearch(-p_beta, -p_beta + 1, moved_position, thread_info, table);
      if (probcut_score >= p_beta) {
        probcut_score = -search<is_pv>(-p_beta, -p_beta + 1, depth - 4, false,
                                moved_position, thread_info, table);
      }

      ss_pop(thread_info);

      if (probcut_score >= p_beta) {
        return probcut_score;
      }
    }
  }

  Action quiets[64];
  int num_quiets = 0;
  Action captures[64];
  int num_captures = 0;
  thread_info.KillerMoves[ply + 1][0] = MoveNone;
  thread_info.KillerMoves[ply + 1][1] = MoveNone;

  MovePicker picker;
  init_picker(picker, position, -107, in_check);

  int best_score = ScoreNone, moves_played = 0;
  bool is_capture = false, skip = false;

  while (Action move =
             next_move(picker, position, thread_info, tt_move, skip)) {
    if (thread_data.stop) break;

    RootAction *root_move_entry = nullptr;
    if (root) {
      root_move_entry = find_root_move(thread_info, move);
      if (!root_move_entry) {
        continue;
      }
      bool pv_skip = false;
      for (int i = 0; i < thread_info.multipv_index; i++) {
        if (thread_info.best_moves[i] == move) {
          pv_skip = true;
          break;
        }
      }
      if (pv_skip) {
        continue;
      }
    }

    if (move == excluded_move) {
      continue;
    }
    if (!is_legal(position, move)) {
      continue;
    }

    uint64_t curr_nodes = thread_info.nodes.load();

    const int from_sq = extract_from(move);
    const int to_sq = extract_to(move);
    const int moving_piece = is_valid_square(from_sq) ? position.board[from_sq] : Pieces::Blank;
    const bool is_pawn = (get_piece_type(moving_piece) == PieceTypes::Pawn);
    const int rel_rank = is_pawn ? ((position.color == Colors::White) ? get_rank(to_sq) : (7 - get_rank(to_sq))) : 0;
    const bool is_advanced_pawn = is_pawn && (rel_rank >= 5);

    int hist_score =
        thread_info.HistoryScores[moving_piece]
                                 [to_sq];

    is_capture = is_cap(position, move);
    if (!is_capture && !is_pv && best_score > -MateScore && thread_info.mate_search == 0) {

      if (!is_advanced_pawn && !in_check && depth < LMPDepth &&
          moves_played >= LMPBase + depth * depth / (2 - improving)) {
        skip = true;
      }

      if (!is_advanced_pawn && !in_check && depth < FPDepth &&
          picker.stage > Stages::Captures) {
        int fp_margin = FPMargin1 + FPMargin2 * depth;
        if (static_eval + fp_margin < alpha) {
          skip = true;
        }
      }

      if (!is_advanced_pawn && !in_check && !is_pv && !is_capture && depth < HistPruneDepth &&
          hist_score < -HistPruneThreshold * depth) {
        continue;
      }
    }

    if (!root && !in_check && !is_advanced_pawn && best_score > -MateThreshold && depth < SeePruningDepth) {

      int margin =
          is_capture ? (depth * SeePruningNoisyMargin) : SeePruningQuietMargin;

      if (!SEE(position, move, depth * margin)) {

        continue;
      }
    }

    int extension = 0;

    if (!root && ply < thread_info.current_iter * 2) {
      if (!singular_search && depth >= SEDepth && move == tt_move &&
          tt_hit && tt_score != ScoreNone && abs(tt_score) < MateThreshold && entry.depth >= depth - 3 &&
          (entry_type == EntryTypes::LBound || entry_type == EntryTypes::Exact)) {

        int sBeta = tt_score - depth;
        thread_info.excluded_move = move;
        int sScore = search<false>(sBeta - 1, sBeta, (depth - 1) / 2, cutnode,
                                   position, thread_info, table);

        if (sScore < sBeta) {
          if (!is_pv && sScore + SEDoubleExtMargin < sBeta &&
              ply < thread_info.current_iter) {

            extension = 2 + (!is_capture && sScore < sBeta - 125);
          } else {
            extension = 1;
          }
        } else if (sBeta >= beta) {

          return sBeta;
        } else if (cutnode) {
          extension = -1;
        }
      }
    }

    if (extension == 0 && !is_capture && depth >= 4) {
      if (hist_score > HistExtThreshold) {
        extension = 1;
      }
    }

    if (extension == 0 && !is_capture && is_advanced_pawn && depth >= 2) {
      extension = (rel_rank >= 6) ? 2 : 1;
    }

    BoardState moved_position = position;
    make_move(moved_position, move);

    update_nnue_state(thread_info, move, position, moved_position);
    ss_push(position, thread_info, move);

    bool full_search = false;
    if (extension > 0) {
      constexpr int MaxExtensionBudget = 10;
      int remaining_extension_budget =
          thread_info.current_iter + MaxExtensionBudget - (ply + depth);
      if (remaining_extension_budget <= 0) {
        extension = 0;
      } else {
        extension = std::min(extension, remaining_extension_budget);
      }
    }
    auto clamp_child_depth = [&](int child_depth) {
      int max_child_depth = std::max(0, depth - 1 + extension);
      return std::clamp(child_depth, 0, max_child_depth);
    };
    // Nominal child depth honors the full root range; depth-indexed
    // tables (LMRTable) are sized for MaxSearchDepth.
    int newdepth = clamp_child_depth(std::min(depth - 1 + extension, MaxRootDepth - 1));

    if (newdepth > 0 && depth >= LMRMinDepth && moves_played > is_pv && thread_info.mate_search == 0) {
      int R = LMRTable[depth][moves_played];
      if (is_capture) {
        R /= 2;
      } else {
        R -= hist_score / 1024;
      }

      R -= is_pv;

      R -= (tt_hit && entry.depth >= depth);

      R += !improving;

      R += cutnode;

      bool gives_check =
          (attacks_square(moved_position, get_king_pos(position, color ^ 1),
                          color) != 0);
      R -= gives_check;

      if (gives_check && depth >= 6)
        R -= 1;

      if (is_advanced_pawn && R > 0) {
        R = (rel_rank >= 6) ? 0 : std::max(0, R - 2);
      }

      R = std::clamp(R, 0, newdepth - 1);

      score = -search<false>(-alpha - 1, -alpha, newdepth - R, true,
                             moved_position, thread_info, table);
      if (score > alpha) {
        full_search = R > 0;
        newdepth += (score > (best_score + 60 + newdepth * 2));
        newdepth -= (score < best_score + newdepth && !root);
        newdepth = clamp_child_depth(newdepth);
      }
    } else {
      full_search = moves_played || !is_pv;
    }
    if (full_search) {

      score = -search<false>(-alpha - 1, -alpha, newdepth, !cutnode,
                             moved_position, thread_info, table);
    }
    if ((score > alpha || !moves_played) && is_pv) {

      score = -search<true>(-beta, -alpha, newdepth, false, moved_position,
                            thread_info, table);
    }

    ss_pop(thread_info);

    if (thread_data.stop) {

      return best_score;
    }

    if (root && root_move_entry) {
      root_move_entry->nodes += (thread_info.nodes.load() - curr_nodes);
    }

    if (score > best_score) {
      best_score = score;

      if (score > alpha) {
        best_move = move;
        raised_alpha = true;
        alpha = score;

        if (score >= beta) {
          thread_info.pv[pv_index] = best_move;
          break;
        }

        else {

          thread_info.pv[pv_index] = best_move;
          for (int n = 0; n < MaxSearchPly - ply - 1; n++) {
            thread_info.pv[pv_index + 1 + n] =
                thread_info.pv[pv_index + MaxSearchPly + n];
          }
        }
      }
    }

    if (is_capture) {
      if (num_captures < 64)
        captures[num_captures++] = move;
    } else {
      if (num_quiets < 64)
        quiets[num_quiets++] = move;
    }

    moves_played++;
  }

  if (root) {
    if (best_move != MoveNone) {
      thread_info.best_moves[thread_info.multipv_index] = best_move;
    }
    thread_info.best_scores[thread_info.multipv_index] = best_score;
  }

  if (best_score >= beta) {

    int piece = position.board[extract_from(best_move)],
        sq = extract_to(best_move);

    int bonus = std::min(
        (int)HistBonus * (depth - 1 + (best_score > beta + 125)), (int)HistMax);

    const bool best_is_capture = is_cap(position, best_move);
    if (best_is_capture) {
      int capture_bonus = bonus / 2;
      update_history(thread_info.CapHistScores[piece][sq], capture_bonus);

    } else {

      int their_last = SquareNone;
      int their_piece = Pieces::Blank;
      int our_last = SquareNone;
      int our_piece = Pieces::Blank;
      int ply4_last = SquareNone;
      int ply4_piece = Pieces::Blank;

      if (thread_info.game_ply >= 1 && (ss - 1)->played_move != MoveNone) {
        their_last = extract_to((ss - 1)->played_move);
        their_piece = (ss - 1)->piece_moved;
      }
      if (thread_info.game_ply >= 2 && (ss - 2)->played_move != MoveNone) {
        our_last = extract_to((ss - 2)->played_move);
        our_piece = (ss - 2)->piece_moved;
      }
      if (thread_info.game_ply >= 4 && (ss - 4)->played_move != MoveNone) {
        ply4_last = extract_to((ss - 4)->played_move);
        ply4_piece = (ss - 4)->piece_moved;
      }

      for (int i = 0; i < num_quiets; i++) {

        Action move = quiets[i];

        int piece_m = position.board[extract_from(move)],
            sq_m = extract_to(move);

        update_continuation_histories(thread_info, piece_m, sq_m, -bonus,
                                      their_last, their_piece, our_last,
                                      our_piece, ply4_last, ply4_piece);
      }

      update_continuation_histories(thread_info, piece, sq, bonus, their_last,
                                    their_piece, our_last, our_piece, ply4_last,
                                    ply4_piece);

      if (best_move != thread_info.KillerMoves[ply][0]) {
        thread_info.KillerMoves[ply][1] = thread_info.KillerMoves[ply][0];
        thread_info.KillerMoves[ply][0] = best_move;
      }

      if (their_piece != Pieces::Blank && their_last != SquareNone) {
        thread_info.CounterMoves[their_piece][their_last] = best_move;
      }
    }

    for (int i = 0; i < num_captures; i++) {
      Action move = captures[i];

      int piece_m = position.board[extract_from(move)], sq_m = extract_to(move);

      int capture_penalty = bonus * 2;
      update_history(thread_info.CapHistScores[piece_m][sq_m],
                     -capture_penalty);
    }
  }

  if (best_score == ScoreNone) {
    return singular_search ? alpha : in_check ? (-MateScore + ply) : 0;
  }

  entry_type = best_score >= beta ? EntryTypes::LBound
               : raised_alpha     ? EntryTypes::Exact
                                  : EntryTypes::UBound;

  bool best_capture = is_cap(position, best_move);

  if (!in_check && (!best_move || !best_capture) &&
      !(best_score >= beta && best_score <= ss->static_eval) &&
      !(!best_move && best_score >= ss->static_eval)) {

    int bonus =
        std::clamp((best_score - ss->static_eval) * depth / 8, -256, 256);

    update_corrhist(
        thread_info.PawnCorrHist[color][get_corrhist_index(position.pawn_key)],
        bonus);
    update_corrhist(
        thread_info.NonPawnCorrHist[color][Colors::White][get_corrhist_index(
            position.non_pawn_key[Colors::White])],
        bonus);
    update_corrhist(
        thread_info.NonPawnCorrHist[color][Colors::Black][get_corrhist_index(
            position.non_pawn_key[Colors::Black])],
        bonus);
  }

  if (!singular_search && !(root && (thread_info.root_moves_limited || thread_info.multipv_index))) {
    insert_entry(hash, depth, best_move, raw_eval,
                 score_to_tt(best_score, ply), entry_type,
                 thread_info.searches);
  }

  return best_score;
}

inline std::string format_pv(const BoardState &position, const ThreadInfo &thread_info) {
  BoardState temp_pos = position;
  std::string result;
  int indx = 0;

  while (indx < MaxSearchPly && thread_info.pv[indx] != MoveNone) {

    const Action best_move = thread_info.pv[indx];

    std::array<Action, MaxActions> legal_moves{};
    const int movelen = legal_movegen(temp_pos, legal_moves.data());

    bool found_move = false;

    for (int i = 0; i < movelen; i++) {
      if (legal_moves[i] == best_move) {
        found_move = true;
        break;
      }
    }

    if (!found_move) {
      break;
    }

    {
      const std::string mv = internal_to_uci(temp_pos, best_move);
      if (!result.empty()) result += ' ';
      result += mv;
    }

    make_move(temp_pos, best_move);

    indx++;
  }

  return result;
}

inline void iterative_deepen(BoardState &position, ThreadInfo &thread_info,
                      std::vector<TTBucket> &table) {

  thread_info.original_opt = thread_info.opt_time;

  calculate(position);
  thread_info.nodes.store(0);
  thread_info.time_checks = 0;
  thread_info.search_ply = 0;
  thread_info.nnue_state.reset_nnue(position);
  thread_info.excluded_move = MoveNone;
  thread_info.best_moves = {0};
  thread_info.best_scores.fill(ScoreNone);
  for (auto &k : thread_info.KillerMoves) {
    k[0] = MoveNone;
    k[1] = MoveNone;
  }

  thread_info.root_moves.reserve(MaxActions);
  std::array<Action, MaxActions> raw_root_moves{};
  int nmoves = legal_movegen(position, raw_root_moves.data());
  if (thread_info.root_moves_limited) {
    std::vector<RootAction> filtered;
    filtered.reserve(thread_info.root_moves.size());
    for (const auto &rm : thread_info.root_moves) {
      Action candidate = rm.move;
      bool legal = false;
      for (int i = 0; i < nmoves; i++) {
        if (raw_root_moves[i] == candidate) {
          legal = true;
          break;
        }
      }
      if (!legal)
        continue;
      bool duplicate =
          std::any_of(filtered.begin(), filtered.end(),
                      [&](const RootAction &r) { return r.move == candidate; });
      if (!duplicate) {
        filtered.push_back({candidate, 0});
      }
    }
    thread_info.root_moves = std::move(filtered);
  } else {
    thread_info.root_moves.clear();
    for (int i = 0; i < nmoves; i++) {
      thread_info.root_moves.push_back({raw_root_moves[i], 0});
    }
  }
  // Reported best moves always come from this legal root move list.
  thread_info.best_moves[0] =
      thread_info.root_moves.empty() ? MoveNone : thread_info.root_moves[0].move;

  Action prev_best = MoveNone;
  int alpha = ScoreNone, beta = -ScoreNone;
  int bm_stability = 0;

  int target_depth = std::clamp(thread_info.max_iter_depth, 1, MaxRootDepth);
  std::array<Action, MaxActions> completed_moves{};
  std::array<int, MaxActions> completed_scores{};
  completed_scores.fill(ScoreNone);
  std::array<Action, MaxSearchPly> completed_pv{};

  int real_multi_pv =
      std::min<int>(thread_info.multipv, (int)thread_info.root_moves.size());

  // Helpers skip the cheap early iterations so each thread explores different
  // depths instead of duplicating the main thread's work. Results from helpers
  // are discarded; they only contribute transposition table entries.
  const int start_depth = thread_info.thread_id == 0
                              ? 1
                              : 1 + std::min<int>(thread_info.thread_id, 4);
  for (int depth = start_depth; !thread_info.root_moves.empty(); ++depth) {
    if (thread_data.stop) {
      break;
    }
    if (depth > target_depth || depth > MaxRootDepth) {
      break;
    }

    real_multi_pv =
        std::min<int>(thread_info.multipv, (int)thread_info.root_moves.size());

    for (thread_info.multipv_index = 0;
         thread_info.multipv_index < real_multi_pv;
         thread_info.multipv_index++) {

      int temp_depth = depth;

      int score, delta = AspStartWindow;

      score =
          search<true>(alpha, beta, depth, false, position, thread_info, table);

      while (score <= alpha || score >= beta || thread_data.stop) {

        if (thread_data.stop) {
          goto finish;
        }

        if (thread_info.thread_id == 0) {
          std::string bound_string;
          if (score >= beta) {
            bound_string = "lowerbound";
          } else {
            bound_string = "upperbound";
          }

          uint64_t nodes = thread_info.nodes.load();
          for (auto &td : thread_data.thread_infos) {
            nodes += td.nodes.load();
          }
          int64_t search_time = time_elapsed(thread_info.start_time);
          int64_t nps = search_time
                            ? static_cast<int64_t>(nodes) * 1000 / search_time
                            : static_cast<int64_t>(nodes) * 1000;

          Action move = score <= alpha
                            ? prev_best
                            : thread_info.best_moves[thread_info.multipv_index];

          std::string pv_suffix = "";
          if (move != MoveNone) {
            std::string pv_str = internal_to_uci(position, move);
            if (pv_str != "0000") {
              pv_suffix = " pv " + pv_str;
            }
          }

          if (abs(score) < MateScore - MaxSearchPly) {
            safe_printf(
                "info multipv %i depth %i seldepth %i score cp %i %s nodes "
                "%" PRIu64 " nps %" PRIi64 " time %" PRIi64 "%s\n",
                thread_info.multipv_index + 1, depth, thread_info.seldepth,
                score * 100 / NormalizationFactor, bound_string.c_str(),
                nodes, nps, search_time, pv_suffix.c_str());
          } else if (score > 0) {
            int dist = std::max(1, (MateScore - score + 1) / 2);
            safe_printf("info multipv %i depth %i seldepth %i score mate %i "
                        "%s nodes %" PRIu64 " nps %" PRIi64 " time %" PRIi64
                        "%s\n",
                        thread_info.multipv_index + 1, depth,
                        thread_info.seldepth, dist, bound_string.c_str(),
                        nodes, nps, search_time, pv_suffix.c_str());
          } else {
            int dist = std::max(1, (MateScore + score + 1) / 2);
            safe_printf("info multipv %i depth %i seldepth %i score mate %i "
                        "%s nodes %" PRIu64 " nps %" PRIi64 " time %" PRIi64
                        "%s\n",
                        thread_info.multipv_index + 1, depth,
                        thread_info.seldepth, -dist, bound_string.c_str(),
                        nodes, nps, search_time, pv_suffix.c_str());
          }
        }

        if (score <= alpha) {
          beta = (alpha + beta) / 2;
          alpha -= delta;
          if (score <= -MateScore + MaxSearchPly) alpha = ScoreNone;
          temp_depth = depth;
        } else if (score >= beta) {
          beta += delta;
          if (score >= MateScore - MaxSearchPly) beta = -ScoreNone;
          temp_depth = std::max(temp_depth - 1, 1);
        }
        delta += delta / 3;

        score = search<true>(alpha, beta, temp_depth, false, position,
                             thread_info, table);
      }

      if (score == ScoreNone) {
        break;
      }

      std::string eval_string;

      if (abs(score) < MateScore - MaxSearchPly) {
        eval_string = "cp " + std::to_string(score * 100 / NormalizationFactor);
      } else if (score > 0) {
        int dist = (MateScore - score + 1) / 2;
        eval_string = "mate " + std::to_string(std::max(1, dist));
      } else {
        int dist = (MateScore + score + 1) / 2;
        eval_string = "mate -" + std::to_string(std::max(1, dist));
      }

      thread_info.best_moves[thread_info.multipv_index] = thread_info.pv[0];
      completed_moves[thread_info.multipv_index] = thread_info.pv[0];
      completed_scores[thread_info.multipv_index] = score;
      if (thread_info.multipv_index == 0) {
        std::copy_n(thread_info.pv.begin(), MaxSearchPly, completed_pv.begin());
      }

      if (thread_info.thread_id == 0) {

        uint64_t nodes = thread_info.nodes.load();

        for (auto &td : thread_data.thread_infos) {
          nodes += td.nodes.load();
        }

        int64_t search_time = time_elapsed(thread_info.start_time);
        int64_t nps = static_cast<int64_t>(nodes) * 1000 / std::max<int64_t>(1, search_time);

        safe_printf(
            "info multipv %i depth %i seldepth %i score %s nodes %" PRIu64
            " nps %" PRIi64 " time %" PRIi64 " tbhits %" PRIu64 " pv %s\n",
            thread_info.multipv_index + 1, depth, thread_info.seldepth,
            eval_string.c_str(), nodes, nps, search_time, thread_data.tb_hits.load(),
            format_pv(position, thread_info).c_str());

        if (!thread_info.infinite_search && !thread_data.pondering &&
            static_cast<uint64_t>(search_time) > thread_info.opt_time) {
          thread_data.stop = true;
        }

        else if (!thread_info.is_movetime && thread_info.multipv == 1 && depth > 6) {
          if (thread_info.best_moves[0] == prev_best) {
            bm_stability = std::min(bm_stability + 1, 8);
            thread_info.stability_counter++;
            thread_info.best_move_stable = (thread_info.stability_counter >= 3);
          } else {
            bm_stability = 0;
            thread_info.stability_counter = 0;
            thread_info.best_move_stable = false;
          }

          RootAction *ra = find_root_move(thread_info, thread_info.best_moves[0]);
          adjust_soft_limit(
              thread_info,
              ra ? ra->nodes : 0,
              bm_stability);
        }
      }

      if (thread_data.stop) {
        goto finish;
      }

      prev_best = thread_info.best_moves[0];

      if (depth > 6 && thread_info.multipv_index == 0) {
        // Helpers use varied aspiration windows so they don't duplicate the
        // main thread's search; the main thread keeps the standard window.
        const int asp = 20 + (thread_info.thread_id % 4) * 8;
        alpha = score - asp, beta = score + asp;
      } else {
        alpha = ScoreNone, beta = -ScoreNone;
      }
    }

    if (thread_info.mate_search > 0 && completed_scores[0] >= MateScore - MaxSearchPly) {
      int dist = (MateScore - completed_scores[0] + 1) / 2;
      if (dist <= thread_info.mate_search) {
        if (thread_info.thread_id == 0) thread_data.stop = true;
        break;
      }
    }

    if (abs(completed_scores[0]) >= MateScore - MaxSearchPly) {
      int mate_plies = MateScore - abs(completed_scores[0]);
      if (depth >= mate_plies + 2) {
        if (thread_info.thread_id == 0 && !thread_info.infinite_search && !thread_data.pondering) {
          thread_data.stop = true;
        }
        break;
      }
    }
  }

finish:

  if (completed_moves[0] != MoveNone) {
    thread_info.best_moves = completed_moves;
    thread_info.best_scores = completed_scores;
    std::copy(completed_pv.begin(), completed_pv.end(), thread_info.pv.begin());
  }

  if (thread_info.thread_id == 0) {
    if (!thread_data.stop && (thread_info.infinite_search || thread_data.pondering)) {
      std::unique_lock lock(thread_data.control_mutex);
      thread_data.control_cv.wait(lock, [&] {
        return thread_data.stop || (!thread_info.infinite_search && !thread_data.pondering);
      });
    }
    thread_data.stop = true;
  }

  if (thread_info.thread_id == 0 && thread_data.emit_bestmove.load() &&
      (!thread_info.infinite_search || thread_data.stop)) {
    // The reply comes from the last completed principal variation only.
    thread_info.ponder_move =
        completed_moves[0] != MoveNone && thread_info.pv[0] == thread_info.best_moves[0]
            ? thread_info.pv[1]
            : MoveNone;
    if (thread_info.best_moves[0] == MoveNone) {
      safe_printf("bestmove 0000\n");
    } else {
      const std::string bm = internal_to_uci(position, thread_info.best_moves[0]);
      if (thread_info.use_ponder && thread_info.ponder_move != MoveNone) {
        BoardState ponder_pos = position;
        make_move(ponder_pos, thread_info.best_moves[0]);
        safe_printf("bestmove %s ponder %s\n", bm.c_str(),
                    internal_to_uci(ponder_pos, thread_info.ponder_move).c_str());
      } else {
        safe_printf("bestmove %s\n", bm.c_str());
      }
    }
  }
}

inline void filter_root_tablebase(const BoardState &position, ThreadInfo &thread_info) {
  if (!tb_initialized || !thread_info.use_syzygy ||
      pop_count(position.colors_bb[0] | position.colors_bb[1]) >
          std::min<int>(TB_LARGEST, thread_info.syzygy_probe_limit)) return;
  for (const auto &rights : position.castling_squares)
    for (int rook : rights) if (rook != SquareNone) return;
  bool repeated = false;
  for (int i = 0; i < thread_info.game_ply; ++i) {
    if (thread_info.game_hist[i].position_key == position.zobrist_key) {
      repeated = true;
      break;
    }
  }
  auto results = std::make_unique<TbRootMoves>();
  const unsigned ep = tb_ep_square(position);
  const unsigned rule50 = thread_info.syzygy_50_move_rule ? std::min<int>(position.halfmoves, 100) : 0;
  auto probe = [&](bool dtz) {
    const auto &bb = position.pieces_bb;
    if (dtz) return tb_probe_root_dtz(position.colors_bb[0], position.colors_bb[1],
        bb[PieceTypes::King], bb[PieceTypes::Queen], bb[PieceTypes::Rook], bb[PieceTypes::Bishop],
        bb[PieceTypes::Knight], bb[PieceTypes::Pawn], rule50, 0, ep,
        position.color == Colors::White, repeated, thread_info.syzygy_50_move_rule, results.get());
    return tb_probe_root_wdl(position.colors_bb[0], position.colors_bb[1],
        bb[PieceTypes::King], bb[PieceTypes::Queen], bb[PieceTypes::Rook], bb[PieceTypes::Bishop],
        bb[PieceTypes::Knight], bb[PieceTypes::Pawn], rule50, 0, ep,
        position.color == Colors::White, thread_info.syzygy_50_move_rule, results.get());
  };

  if (!probe(true) && (rule50 || !probe(false))) { ++thread_data.tb_fails; return; }
  ++thread_data.tb_hits;
  std::vector<RootAction> best;
  int best_rank = INT32_MIN;
  for (unsigned i = 0; i < results->size; ++i) {
    const auto &result = results->moves[i];
    for (const auto &legal : thread_info.root_moves) {
      const Action move = legal.move;
      const unsigned promo = TB_MOVE_PROMOTES(result.move);
      if (extract_from(move) != TB_MOVE_FROM(result.move) || extract_to(move) != TB_MOVE_TO(result.move)) continue;
      if (promo ? (extract_type(move) != MoveTypes::Promotion || extract_promo(move) != 4 - promo)
                : extract_type(move) == MoveTypes::Promotion) continue;

      if (result.tbRank > best_rank) { best_rank = result.tbRank; best.clear(); }
      if (result.tbRank == best_rank) best.push_back(legal);
      break;
    }
  }
  if (!best.empty()) {
    thread_info.root_moves = std::move(best);
    thread_info.root_moves_limited = true;
  }
}

inline void search_position(BoardState &position, ThreadInfo &thread_info,
                            std::vector<TTBucket> &table) {
  thread_info.position = position;
  thread_info.thread_id = 0;
  thread_info.nodes.store(0);
  thread_data.tb_hits = 0;
  thread_data.tb_fails = 0;
  if (!thread_info.root_moves_limited) {
    thread_info.root_moves.clear();
    std::array<Action, MaxActions> moves{};
    const int count = legal_movegen(position, moves.data());
    for (int i = 0; i < count; ++i) thread_info.root_moves.push_back({moves[i], 0});
  }
  filter_root_tablebase(position, thread_info);
  for (size_t i = 0; i < thread_data.thread_infos.size(); ++i) {
    thread_data.thread_infos[i] = thread_info;
    thread_data.thread_infos[i].thread_id = static_cast<uint16_t>(i + 1);
    // Helper results are discarded, so helpers only ever search the first line.
    thread_data.thread_infos[i].multipv = 1;
  }
  std::atomic<bool> start_workers{false};
  bool spawn_failed = false;
  for (size_t i = 0; i < thread_data.thread_infos.size(); ++i) {
    try {
      thread_data.threads.emplace_back([i, &table, &start_workers] {
        start_workers.wait(false);
        auto &worker = thread_data.thread_infos[i];
        iterative_deepen(worker.position, worker, table);
      });
    } catch (const std::system_error &) {
      spawn_failed = true;
      break;
    }
  }
  if (spawn_failed) {
    thread_data.stop = true;
    start_workers.store(true);
    start_workers.notify_all();
    for (auto &worker : thread_data.threads) worker.join();
    thread_data.threads.clear();
    std::exit(EXIT_FAILURE);
  }
  start_workers.store(true);
  start_workers.notify_all();
  iterative_deepen(position, thread_info, table);
  thread_data.stop = true;
  for (auto &worker : thread_data.threads) worker.join();
  thread_data.threads.clear();
  thread_info.searches = (thread_info.searches + 1) % MaxAge;
}

#pragma once
#include "search.h"

inline std::atomic<bool> evaltrace_running{false};
inline std::atomic<bool> evaltrace_cancelled{false};

// Diagnostic-only fresh search state, with the position's repetition history kept.
inline void evaltrace(BoardState position, ThreadInfo &ti, uint64_t node_limit) {
  const auto history = ti.game_hist;
  const auto game_ply = ti.game_ply;
  auto prepare = [&] {
    new_game(ti, TT);
    ti.game_hist = history;
    ti.game_ply = game_ply;
    ti.search_ply = 0;
    ti.seldepth = 0;
    ti.time_checks = 0;
    ti.max_nodes_searched = node_limit;
    ti.max_time = ti.opt_time = UINT64_MAX / 2;
    ti.infinite_search = true;
    ti.pondering = false;
    ti.start_time = std::chrono::steady_clock::now();
    ti.nnue_state.reset_nnue(position);
    for (auto &helper : thread_data.thread_infos) helper.nodes = 0;
    thread_data.stop = false;
    if (evaltrace_cancelled) thread_data.stop = true;
    thread_data.pondering = false;
  };
  prepare();

  const bool in_check = attacks_square(position, get_king_pos(position, position.color), position.color ^ 1);
  const int raw = in_check ? ScoreNone : eval(position, ti);
  const int corrected = in_check ? ScoreNone : correct_eval(position, ti, raw);
  auto trace = std::make_unique<QSearchTrace>();
  active_qtrace = trace.get();
  const int value = qsearch<true>(-MateScore, MateScore, position, ti, TT);
  active_qtrace = nullptr;
  const bool stopped = thread_data.stop.load();
  const auto nodes = ti.nodes.load();
  const int seldepth = ti.seldepth;
  int reference = ScoreNone;
  if (!evaltrace_cancelled) {
    prepare();
    reference = qsearch(-MateScore, MateScore, position, ti, TT);
  }
  const bool verified = !evaltrace_cancelled && reference == value && nodes == ti.nodes.load() && stopped == thread_data.stop.load();
  const bool complete = verified && !stopped && !trace->truncated;
  const auto &row = trace->rows[0];

  auto json_int = [](int v) { return v == ScoreNone ? std::string("null") : std::to_string(v); };
  auto cp = [&](int v) {
    return v == ScoreNone || std::abs(v) >= MateThreshold
        ? std::string("null") : std::to_string(v * 100 / NormalizationFactor);
  };
  std::string line = "[";
  BoardState replay = position;
  for (Action move : row.line) {
    if (line.size() > 1) line += ',';
    line += '"' + internal_to_uci(replay, move) + '"';
    make_move(replay, move);
  }
  line += ']';
  int captures = 0, safe_captures = 0, checking_captures = 0;
  if (row.uses_nnue) {
    std::array<Action, MaxActions> legal{};
    const int n = legal_movegen(replay, legal.data());
    for (int i = 0; i < n; ++i) {
      const Action move = legal[i];
      if (!is_cap(replay, move) && extract_type(move) != MoveTypes::Promotion) continue;
      ++captures;
      safe_captures += SEE(replay, move, 0);
      BoardState child = replay;
      make_move(child, move);
      checking_captures += bool(attacks_square(child, get_king_pos(child, child.color), child.color ^ 1));
    }
  }
  safe_printf("info string evaltrace {\"version\":1,\"static_raw\":%s,\"static_cp\":%s,"
              "\"corrected_raw\":%s,\"corrected_cp\":%s,\"q_raw\":%s,\"q_cp\":%s,"
              "\"reference_q_raw\":%s,\"trace_verified\":%s,\"cancelled\":%s,"
              "\"in_check\":%s,\"complete\":%s,\"truncated\":%s,\"nodes\":%" PRIu64 ","
              "\"seldepth\":%d,\"searched_moves\":%" PRIu64 ",\"delta_pruned\":%" PRIu64 ","
              "\"pv\":%s,\"leaf\":{\"fen\":\"%s\",\"kind\":\"%s\",\"uses_nnue\":%s,"
              "\"raw\":%s,\"corrected_raw\":%s,\"legal_captures\":%d,"
              "\"non_losing_captures\":%d,\"checking_captures\":%d}}\n",
              json_int(raw).c_str(), cp(raw).c_str(), json_int(corrected).c_str(), cp(corrected).c_str(),
              json_int(value).c_str(), cp(value).c_str(),
              json_int(reference).c_str(), verified ? "true" : "false", evaltrace_cancelled ? "true" : "false", in_check ? "true" : "false",
              complete ? "true" : "false", trace->truncated ? "true" : "false",
              nodes, seldepth, trace->searched_moves, trace->delta_pruned, line.c_str(),
              row.leaf_fen.c_str(), row.kind, row.uses_nnue ? "true" : "false",
              json_int(row.leaf_raw).c_str(), json_int(row.leaf_corrected).c_str(),
              captures, safe_captures, checking_captures);
  ti.infinite_search = false;
  thread_data.stop = true;
}

#pragma once

#include "../fathom/src/tbprobe.h"
#include "evaltrace.h"
#include "search.h"

#include <algorithm>
#include <array>
#include <cctype>
#include <charconv>
#include <chrono>
#include <cinttypes>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <functional>
#include <iostream>
#include <random>
#include <sstream>
#include <string>
#include <string_view>
#include <thread>
#include <vector>

inline bool tb_initialized = false;

inline bool parse_int64(const std::string &text, int64_t &out) noexcept {
  if (text.empty()) return false;
  const char *begin = text.data();
  const char *end = begin + text.size();
  const auto [ptr, ec] = std::from_chars(begin, end, out);
  return ec == std::errc() && ptr == end;
}

inline bool parse_check(std::string text, bool &out) {
  for (char &ch : text) {
    ch = static_cast<char>(std::tolower(static_cast<unsigned char>(ch)));
  }
  if (text == "true" || text == "1" || text == "yes" || text == "on") {
    out = true;
    return true;
  }
  if (text == "false" || text == "0" || text == "no" || text == "off") {
    out = false;
    return true;
  }
  return false;
}

inline void run_thread(BoardState &pos, ThreadInfo &ti, std::thread &worker) {
  if (worker.joinable()) worker.join();
  {
    std::lock_guard<std::mutex> guard(thread_data.control_mutex);
    thread_data.emit_bestmove = true;
    thread_data.stop = false;
  }
  worker = std::thread(search_position, std::ref(pos), std::ref(ti), std::ref(TT));
}

namespace detail_uci {

constexpr std::string_view kStartFen =
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

enum class OptionType { Toggle, Integer, Text };

struct OptionEntry {
  std::string label;
  OptionType type;
  int64_t lo = 0;
  int64_t hi = 0;
  std::string initial;
  std::function<void(int64_t)> on_int;
  std::function<void(bool)> on_bool;
  std::function<void(const std::string &)> on_text;

  std::string uci_declaration() const {
    std::string line = "option name " + label + " type ";
    switch (type) {
      case OptionType::Toggle:
        line += "check default " + initial;
        break;
      case OptionType::Integer:
        line += "spin default " + initial + " min " + std::to_string(lo) +
                " max " + std::to_string(hi);
        break;
      case OptionType::Text:
        line += "string default " + (initial.empty() ? "<empty>" : initial);
        break;
    }
    return line;
  }
};

inline OptionEntry make_spin(std::string label, int64_t lo, int64_t hi,
                             int64_t init_val, std::function<void(int64_t)> cb) {
  return OptionEntry{std::move(label), OptionType::Integer, lo, hi,
                     std::to_string(init_val), std::move(cb), {}, {}};
}

inline OptionEntry make_check(std::string label, bool init_val,
                              std::function<void(bool)> cb) {
  return OptionEntry{std::move(label), OptionType::Toggle, 0, 0,
                     init_val ? "true" : "false", {}, std::move(cb), {}};
}

inline OptionEntry make_text(std::string label, std::string init_val,
                             std::function<void(const std::string &)> cb) {
  return OptionEntry{std::move(label), OptionType::Text, 0, 0,
                     std::move(init_val), {}, {}, std::move(cb)};
}

inline std::string ascii_lower(std::string s) {
  for (char &ch : s) {
    ch = static_cast<char>(std::tolower(static_cast<unsigned char>(ch)));
  }
  return s;
}

inline std::string trim_whitespace(std::string_view sv) {
  const size_t first = sv.find_first_not_of(" \t\r");
  if (first == std::string_view::npos) return {};
  const size_t last = sv.find_last_not_of(" \t\r");
  return std::string(sv.substr(first, last - first + 1));
}

inline bool is_uci_coord_move(std::string_view token) noexcept {
  if (token.size() != 4 && token.size() != 5) return false;
  const bool coords_ok = token[0] >= 'a' && token[0] <= 'h' &&
                         token[1] >= '1' && token[1] <= '8' &&
                         token[2] >= 'a' && token[2] <= 'h' &&
                         token[3] >= '1' && token[3] <= '8';
  if (!coords_ok) return false;
  if (token.size() == 5) {
    const char p = static_cast<char>(std::tolower(static_cast<unsigned char>(token[4])));
    return p == 'q' || p == 'r' || p == 'b' || p == 'n';
  }
  return true;
}

inline void reload_tablebases(const ThreadInfo &ti) noexcept {
  if (tb_initialized) tb_free();
  tb_initialized = false;
  if (ti.use_syzygy && !ti.syzygy_path.empty()) {
    tb_initialized = tb_init(ti.syzygy_path.c_str()) && TB_LARGEST > 0;
    if (!tb_initialized) std::exit(EXIT_FAILURE);
  }
}

inline void reload_opening_book(ThreadInfo &ti) {
  ti.opening_book.clear_book();
  if (ti.use_opening_book && !ti.book_path.empty()) {
    if (!ti.opening_book.load_book(ti.book_path)) std::exit(EXIT_FAILURE);
  }
}

inline void configure_worker_threads(int64_t thread_count) {
  try {
    std::vector<ThreadInfo> helpers(static_cast<size_t>(thread_count - 1));
    thread_data.threads.reserve(static_cast<size_t>(thread_count - 1));
    thread_data.thread_infos.swap(helpers);
  } catch (const std::bad_alloc &) {
    std::exit(EXIT_FAILURE);
  }
}

inline std::vector<OptionEntry> build_option_table(ThreadInfo &ti) {
  std::vector<OptionEntry> entries;
  entries.reserve(24 + params.size());

  entries.push_back(make_spin("Hash", 1, 131072, DefaultHashMB,
                              [](int64_t mb) { resize_TT(static_cast<int>(mb)); }));
  entries.push_back(make_spin("Threads", 1, 1024, 1, configure_worker_threads));
  entries.push_back(make_spin("MultiPV", 1, MaxActions, ti.multipv,
                              [&ti](int64_t v) { ti.multipv = static_cast<uint16_t>(v); }));
  entries.push_back(make_check("UCI_Chess960", thread_data.is_frc,
                               [](bool v) { thread_data.is_frc = v; }));
  entries.push_back(make_check("UCI_LimitStrength", ti.uci_limit_strength,
                               [&ti](bool v) { ti.uci_limit_strength = v; }));
  entries.push_back(make_spin("UCI_Elo", MinUciElo, MaxUciElo, ti.uci_elo,
                              [&ti](int64_t v) { ti.uci_elo = static_cast<int>(v); }));
  entries.push_back(make_spin("MaxMoveTime", 0, 10000,
                              static_cast<int64_t>(ti.max_move_time),
                              [&ti](int64_t v) { ti.max_move_time = static_cast<uint64_t>(v); }));
  entries.push_back(make_spin("MoveOverhead", 0, 1000,
                              static_cast<int64_t>(ti.move_overhead),
                              [&ti](int64_t v) { ti.move_overhead = static_cast<uint64_t>(v); }));
  entries.push_back(make_check("Ponder", ti.use_ponder, [&ti](bool v) {
    ti.use_ponder = v;
    if (!v) {
      ti.pondering = false;
      ti.ponder_hit = false;
      ti.ponder_move = MoveNone;
    }
  }));
  entries.push_back(make_spin("MaxDepth", 0, MaxRootDepth, ti.max_depth,
                              [&ti](int64_t v) { ti.max_depth = static_cast<uint16_t>(v); }));
  entries.push_back(make_spin("MaxNodes", 0, INT32_MAX,
                              static_cast<int64_t>(ti.max_nodes),
                              [&ti](int64_t v) { ti.max_nodes = static_cast<uint64_t>(v); }));
  entries.push_back(make_check("UseSyzygy", ti.use_syzygy, [&ti](bool v) {
    ti.use_syzygy = v;
    reload_tablebases(ti);
  }));
  entries.push_back(make_text("SyzygyPath", ti.syzygy_path, [&ti](const std::string &v) {
    ti.syzygy_path = v;
    reload_tablebases(ti);
  }));
  entries.push_back(make_spin("SyzygyProbeDepth", 1, 64, ti.syzygy_probe_depth,
                              [&ti](int64_t v) { ti.syzygy_probe_depth = static_cast<int>(v); }));
  entries.push_back(make_spin("SyzygyProbeLimit", 1, 7, ti.syzygy_probe_limit,
                              [&ti](int64_t v) { ti.syzygy_probe_limit = static_cast<int>(v); }));
  entries.push_back(make_check("Syzygy50MoveRule", ti.syzygy_50_move_rule,
                               [&ti](bool v) { ti.syzygy_50_move_rule = v; }));
  entries.push_back(make_check("UseOpeningBook", ti.use_opening_book, [&ti](bool v) {
    ti.use_opening_book = v;
    reload_opening_book(ti);
  }));
  entries.push_back(make_text("BookPath", ti.book_path, [&ti](const std::string &v) {
    ti.book_path = v;
    reload_opening_book(ti);
  }));
  entries.push_back(make_spin("BookDepthLimit", 0, 50, ti.book_depth_limit,
                              [&ti](int64_t v) { ti.book_depth_limit = static_cast<int>(v); }));
  entries.push_back(make_spin("BookMinWeight", 0, 1000, ti.book_min_weight,
                              [&ti](int64_t v) { ti.book_min_weight = static_cast<int>(v); }));
  entries.push_back(make_spin("PonderTimeFactor", 0, 200, ti.ponder_time_factor,
                              [&ti](int64_t v) { ti.ponder_time_factor = static_cast<int>(v); }));
  entries.push_back(make_spin("NormalizationFactor", 50, 500, NormalizationFactor,
                              [](int64_t v) { NormalizationFactor = static_cast<int>(v); }));

  for (Parameter &param : params) {
    entries.push_back(make_spin(param.name, param.min, param.max, param.value,
                                [&param](int64_t v) {
                                  param.value = static_cast<int>(v);
                                  if (param.name == "LMRBase" || param.name == "LMRRatio") {
                                    init_LMR();
                                  }
                                }));
  }
  return entries;
}

inline void handle_setoption(std::istringstream &iss,
                             const std::vector<OptionEntry> &registry) {
  std::string token, target_name, raw_val;
  bool reading_name = false;
  while (iss >> token) {
    if (token == "name") {
      reading_name = true;
    } else if (token == "value") {
      std::getline(iss, raw_val);
      break;
    } else if (reading_name) {
      if (!target_name.empty()) target_name.push_back(' ');
      target_name += token;
    }
  }

  std::string cleaned_val = trim_whitespace(raw_val);
  if (cleaned_val == "<empty>") cleaned_val.clear();

  const std::string key = ascii_lower(target_name);
  for (const OptionEntry &opt : registry) {
    if (ascii_lower(opt.label) != key) continue;
    int64_t num = 0;
    bool flag = false;
    if (opt.type == OptionType::Integer && parse_int64(cleaned_val, num)) {
      opt.on_int(std::clamp(num, opt.lo, opt.hi));
    } else if (opt.type == OptionType::Toggle && parse_check(cleaned_val, flag)) {
      opt.on_bool(flag);
    } else if (opt.type == OptionType::Text) {
      opt.on_text(cleaned_val);
    }
    break;
  }
}

inline void handle_position(std::istringstream &iss, BoardState &pos,
                            ThreadInfo &ti) {
  const BoardState saved_pos = pos;
  const auto saved_hist = ti.game_hist;
  const uint16_t saved_gply = ti.game_ply;
  const uint16_t saved_sply = ti.search_ply;

  std::string kind;
  if (!(iss >> kind)) return;

  bool has_moves = false;
  if (kind == "fen") {
    std::string fen_str, tok;
    while (iss >> tok) {
      if (tok == "moves") {
        has_moves = true;
        break;
      }
      if (!fen_str.empty()) fen_str.push_back(' ');
      fen_str += tok;
    }
    if (!set_board(pos, ti, fen_str)) return;
  } else if (kind == "startpos") {
    std::string tok;
    if (iss >> tok) {
      if (tok != "moves") return;
      has_moves = true;
    }
    set_board(pos, ti, std::string(kStartFen));
  } else {
    return;
  }

  if (!has_moves) return;

  std::string move_tok;
  while (iss >> move_tok) {
    const Action move = uci_to_internal(pos, move_tok);
    if (move == MoveNone) {
      pos = saved_pos;
      ti.game_hist = saved_hist;
      ti.game_ply = saved_gply;
      ti.search_ply = saved_sply;
      return;
    }

    if (ti.game_ply >= MaxGameLen - MaxSearchPly - 2) {
      std::move(ti.game_hist.begin() + MaxGameLen / 2, ti.game_hist.end(),
                ti.game_hist.begin());
      ti.game_ply -= MaxGameLen / 2;
    }

    StateRecord &rec = ti.game_hist[ti.game_ply];
    rec.position_key = pos.zobrist_key;
    rec.played_move = move;
    rec.piece_moved = pos.board[extract_from(move)];
    if (ti.game_ply + 1 < MaxGameLen) ++ti.game_ply;
    make_move(pos, move);
  }
  ti.search_ply = 0;
}

inline Action pick_opening_book_move(const BoardState &pos, ThreadInfo &ti) {
  if (ti.infinite_search || ti.pondering || !ti.use_opening_book ||
      !ti.opening_book.is_loaded()) {
    return MoveNone;
  }
  const uint64_t played_halfmoves =
      static_cast<uint64_t>(pos.fullmove - 1U) * 2ULL + pos.color;
  if (ti.book_depth_limit != 0 &&
      played_halfmoves >= static_cast<uint64_t>(ti.book_depth_limit)) {
    return MoveNone;
  }

  const uint64_t book_key = ti.opening_book.polyglot_key(pos);
  Action chosen = ti.opening_book.probe_book(pos, ti.book_min_weight);
  if (chosen == MoveNone) return MoveNone;

  const bool recently_played =
      std::find(ti.recent_book_keys.begin(), ti.recent_book_keys.end(), book_key) !=
      ti.recent_book_keys.end();
  if (recently_played) {
    static thread_local std::mt19937 book_rng(Random::rd());
    if (std::uniform_int_distribution<int>(0, 99)(book_rng) < 50) return MoveNone;
  }

  ti.recent_book_keys[ti.recent_book_head++ % ti.recent_book_keys.size()] = book_key;

  std::array<Action, MaxActions> legal{};
  const int n_legal = legal_movegen(pos, legal.data());
  if (std::find(legal.begin(), legal.begin() + n_legal, chosen) ==
      legal.begin() + n_legal) {
    return MoveNone;
  }
  return chosen;
}

inline void handle_go(std::istringstream &iss, BoardState &pos, ThreadInfo &ti,
                      std::thread &worker, bool interactive) {
  if (ti.use_syzygy && !tb_initialized) std::exit(EXIT_FAILURE);
  if (ti.use_opening_book && !ti.opening_book.is_loaded()) std::exit(EXIT_FAILURE);

  ti.start_time = std::chrono::steady_clock::now();
  ti.infinite_search = false;
  ti.ponder_hit = false;
  ti.pondering = false;
  ti.ponder_move = MoveNone;
  thread_data.ponder_hit_time = -1;
  ti.best_move_stable = false;
  ti.stability_counter = 0;
  ti.time_checks = 0;
  ti.max_nodes_searched = UINT64_MAX / 2;
  ti.max_iter_depth = MaxRootDepth;
  ti.mate_search = 0;

  std::vector<std::string> tokens;
  for (std::string tok; iss >> tok;) tokens.push_back(std::move(tok));

  int side_time = -1;
  int side_inc = 0;
  int fixed_movetime = -1;
  int moves_to_go = 0;
  std::vector<Action> forced_root_moves;
  bool restrict_root = false;

  const int stm = pos.color;
  auto parse_nonneg = [](const std::string &s, int64_t &v) noexcept {
    return parse_int64(s, v) && v >= 0;
  };

  for (size_t i = 0; i < tokens.size(); ++i) {
    const std::string &tok = tokens[i];
    if (tok == "ponder") {
      ti.pondering = ti.use_ponder;
    } else if (tok == "infinite") {
      ti.max_iter_depth = MaxRootDepth;
      ti.max_time = UINT64_MAX;
      ti.opt_time = UINT64_MAX;
      ti.infinite_search = true;
    } else if (tok == "nodes") {
      if (i + 1 >= tokens.size()) continue;
      int64_t n = 0;
      if (parse_nonneg(tokens[++i], n)) {
        ti.max_nodes_searched = std::max<uint64_t>(1ULL, static_cast<uint64_t>(n));
      }
    } else if (tok == "searchmoves") {
      restrict_root = true;
      forced_root_moves.clear();
      for (++i; i < tokens.size(); ++i) {
        if (!is_uci_coord_move(tokens[i])) {
          --i;
          break;
        }
        const Action mv = uci_to_internal(pos, tokens[i]);
        if (mv != MoveNone) forced_root_moves.push_back(mv);
      }
    } else if (tok == "wtime" || tok == "btime" || tok == "winc" || tok == "binc" ||
               tok == "movestogo" || tok == "mate" || tok == "depth" ||
               tok == "movetime") {
      if (i + 1 >= tokens.size()) continue;
      int64_t raw_val = 0;
      const bool is_clock = (tok == "wtime" || tok == "btime");
      if (!(is_clock ? parse_int64(tokens[++i], raw_val)
                     : parse_nonneg(tokens[++i], raw_val))) {
        continue;
      }
      if (is_clock) raw_val = std::max<int64_t>(0, raw_val);
      const int clamped = static_cast<int>(std::min<int64_t>(raw_val, INT32_MAX));

      if (tok == "wtime" && stm == Colors::White) side_time = clamped;
      else if (tok == "btime" && stm == Colors::Black) side_time = clamped;
      else if (tok == "winc" && stm == Colors::White) side_inc = clamped;
      else if (tok == "binc" && stm == Colors::Black) side_inc = clamped;
      else if (tok == "movestogo") moves_to_go = clamped;
      else if (tok == "mate") {
        ti.mate_search = std::clamp(clamped, 1, MaxRootDepth / 2);
        ti.max_iter_depth = ti.mate_search * 2;
      } else if (tok == "depth") {
        ti.max_iter_depth = std::clamp(clamped, 1, MaxRootDepth);
      } else if (tok == "movetime") {
        fixed_movetime = std::max(1, clamped);
      }
    }
  }

  if (!interactive) {
    const bool has_bound = ti.max_iter_depth != MaxRootDepth || ti.mate_search > 0 ||
                           fixed_movetime >= 0 || side_time >= 0 ||
                           ti.max_nodes_searched != UINT64_MAX / 2;
    if (ti.infinite_search || ti.pondering.load() || !has_bound) {
      std::exit(EXIT_FAILURE);
    }
  }

  constexpr uint64_t kUnlimitedMs = UINT64_MAX / 2;
  ti.max_time = kUnlimitedMs;
  ti.opt_time = kUnlimitedMs;
  ti.is_movetime = (fixed_movetime >= 0);

  if (!ti.infinite_search) {
    if (fixed_movetime >= 0) {
      const uint64_t overhead =
          std::min<uint64_t>(ti.move_overhead, static_cast<uint64_t>(fixed_movetime / 10));
      ti.max_time = ti.opt_time =
          std::max<uint64_t>(1ULL, static_cast<uint64_t>(fixed_movetime) - overhead);
    } else if (side_time >= 0) {
      const uint64_t overhead =
          std::min<uint64_t>(ti.move_overhead, static_cast<uint64_t>(side_time / 10));
      const uint64_t usable =
          std::max<uint64_t>(1ULL, static_cast<uint64_t>(side_time) - overhead);
      ti.time_manager.initialize(usable, side_inc, moves_to_go, pos.fullmove);
      ti.max_time = ti.time_manager.hard_limit;
      ti.opt_time = ti.time_manager.soft_limit;
    }
    if (ti.max_move_time > 0) {
      ti.max_time = std::min(ti.max_time, ti.max_move_time);
      ti.opt_time = std::min(ti.opt_time, ti.max_time);
    }
  }

  ti.time_manager.allocated_time = ti.opt_time;
  ti.time_manager.soft_limit = ti.opt_time;
  ti.time_manager.max_time = ti.max_time;
  ti.time_manager.hard_limit = ti.max_time;
  thread_data.pondering = ti.pondering.load();

  if (!restrict_root) {
    const Action book_move = pick_opening_book_move(pos, ti);
    if (book_move != MoveNone) {
      safe_printf("bestmove %s\n", internal_to_uci(pos, book_move).c_str());
      return;
    }
  }

  if (ti.max_depth > 0) {
    ti.max_iter_depth = std::min(ti.max_iter_depth, static_cast<int>(ti.max_depth));
  }
  if (ti.max_nodes > 0) {
    ti.max_nodes_searched = std::min(ti.max_nodes_searched, ti.max_nodes);
  }
  // Keep infinite search waitable even when UCI_LimitStrength is active.
  if (ti.uci_limit_strength && !ti.infinite_search) {
    ti.max_nodes_searched = std::min(ti.max_nodes_searched, uci_elo_max_nodes(ti.uci_elo));
  }

  ti.root_moves.clear();
  if (restrict_root) {
    ti.root_moves.reserve(forced_root_moves.size());
    for (Action mv : forced_root_moves) ti.root_moves.push_back(RootAction{mv, 0});
    ti.root_moves_limited = true;
  } else {
    ti.root_moves_limited = false;
  }

  run_thread(pos, ti, worker);
}

inline void handle_hashfull() {
  int occupied_buckets = 0;
  int sample_len = static_cast<int>(std::min<uint64_t>(safe_TT_size(), 1000ULL));
  if (sample_len <= 0) sample_len = 0;

  {
    std::lock_guard<std::mutex> guard(thread_data.data_mutex);
    for (int i = 0; i < sample_len; ++i) {
      const auto &entries = TT[static_cast<size_t>(i)].entries;
      const bool non_empty = std::any_of(entries.begin(), entries.end(), [](const TTEntry &e) {
        return e.score != 0 || e.get_type() != EntryTypes::None;
      });
      if (non_empty) ++occupied_buckets;
    }
  }

  const int denom = sample_len ? sample_len : 1;
  safe_printf("info hashfull %d\n", (occupied_buckets * 1000) / denom);
}

inline bool stops_active_search(std::string_view cmd) noexcept {
  return cmd == "setoption" || cmd == "ucinewgame" || cmd == "position" ||
         cmd == "go" || cmd == "eval" || cmd == "evaltrace" || cmd == "flip" ||
         cmd == "hashfull" || cmd == "d" || cmd == "printparams";
}

}  // namespace detail_uci

using UciOption = detail_uci::OptionEntry;

inline void uci(ThreadInfo &thread_info, BoardState &position,
                std::istream &in_stream = std::cin, bool interactive = true) {
  std::setvbuf(stdin, nullptr, _IONBF, 0);
  std::setvbuf(stdout, nullptr, _IONBF, 0);

  new_game(thread_info, TT);
  set_board(position, thread_info, std::string(detail_uci::kStartFen));
  if (!load_embedded_nnue()) std::exit(EXIT_FAILURE);

  std::thread search_worker;
  auto join_worker = [](std::thread &t) {
    if (t.joinable()) t.join();
  };
  auto halt_search = [&](bool emit_bm) {
    if (evaltrace_running) evaltrace_cancelled = true;
    {
      std::lock_guard<std::mutex> guard(thread_data.control_mutex);
      thread_data.emit_bestmove = emit_bm;
      thread_data.stop = true;
    }
    thread_data.control_cv.notify_all();
    join_worker(search_worker);
  };

  const std::vector<UciOption> option_table =
      detail_uci::build_option_table(thread_info);

  std::string line;
  while (std::getline(in_stream, line)) {
    if (line.empty()) continue;

    std::istringstream iss(line);
    std::string cmd;
    if (!(iss >> std::skipws >> cmd)) continue;

    if (detail_uci::stops_active_search(cmd)) halt_search(false);

    if (cmd == "quit") {
      halt_search(false);
      break;
    } else if (cmd == "d") {
      print_board(position);
      safe_printf("Fen: %s\n", export_fen(position, thread_info).c_str());
    } else if (cmd == "uci") {
      std::string banner = "id name Stallion EAS NNUE\nid author LegendOfCompiling\n";
      for (const UciOption &opt : option_table) banner += opt.uci_declaration() + "\n";
      safe_printf("%suciok\n", banner.c_str());
    } else if (cmd == "printparams") {
      print_params_for_ob();
    } else if (cmd == "isready") {
      safe_printf("readyok\n");
    } else if (cmd == "setoption") {
      detail_uci::handle_setoption(iss, option_table);
    } else if (cmd == "stop") {
      halt_search(true);
    } else if (cmd == "ucinewgame") {
      new_game(thread_info, TT);
      thread_info.time_manager = TimeManager();
      thread_info.is_movetime = false;
      thread_info.best_move_stable = false;
      thread_info.stability_counter = 0;
      thread_info.root_moves.clear();
      thread_info.root_moves_limited = false;
      set_board(position, thread_info, std::string(detail_uci::kStartFen));
    } else if (cmd == "position") {
      detail_uci::handle_position(iss, position, thread_info);
    } else if (cmd == "go") {
      detail_uci::handle_go(iss, position, thread_info, search_worker, interactive);
    } else if (cmd == "ponderhit") {
      if (thread_data.pondering) {
        thread_data.ponder_hit_time =
            std::chrono::duration_cast<std::chrono::milliseconds>(
                std::chrono::steady_clock::now().time_since_epoch())
                .count();
        {
          std::lock_guard<std::mutex> guard(thread_data.control_mutex);
          thread_data.pondering = false;
        }
        thread_data.control_cv.notify_all();
      }
    } else if (cmd == "evaltrace") {
      uint64_t node_cap = 32768;
      std::string key, val;
      bool ok = true;
      while (iss >> key) {
        int64_t parsed = 0;
        if (key != "nodes" || !(iss >> val) || !parse_int64(val, parsed) || parsed <= 0) {
          ok = false;
          break;
        }
        node_cap = static_cast<uint64_t>(parsed);
      }
      if (ok) {
        evaltrace_cancelled = false;
        evaltrace_running = true;
        search_worker = std::thread([&position, &thread_info, node_cap] {
          evaltrace(position, thread_info, node_cap);
          evaltrace_running = false;
        });
      } else {
        safe_printf("info string evaltrace {\"error\":\"positive nodes required\"}\n");
      }
    } else if (cmd == "eval") {
      thread_info.nnue_state.reset_nnue(position);
      const int raw = thread_info.nnue_state.evaluate(position);
      safe_printf("NNUE raw: %d (eval: %d cp)\n", raw, raw * 100 / NormalizationFactor);
    } else if (cmd == "flip") {
      position.color ^= 1;
      position.ep_square = SquareNone;
      calculate(position);
    } else if (cmd == "hashfull") {
      detail_uci::handle_hashfull();
    }
  }

  if (interactive) {
    halt_search(false);
  } else {
    join_worker(search_worker);
  }
  thread_data.stop = true;
  if (tb_initialized) {
    tb_free();
    tb_initialized = false;
  }
}

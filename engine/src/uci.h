#pragma once
#include <algorithm>
#include <array>
#include <cctype>
#include <charconv>
#include <functional>
#include <cinttypes>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <memory>
#include <random>
#include <sstream>
#include <string>
#include <string_view>
#include <thread>
#include <vector>

#include "../fathom/src/tbprobe.h"
#include "search.h"

inline bool tb_initialized = false;

inline void run_thread(BoardState &position, ThreadInfo &thread_info,
                       std::thread &s) {
  if (s.joinable())
    s.join();
  {
    std::lock_guard lock(thread_data.control_mutex);
    thread_data.emit_bestmove = true;
    thread_data.stop = false;
  }
  s = std::thread(search_position, std::ref(position), std::ref(thread_info),
                  std::ref(TT));
}

inline uint64_t perft(int depth, BoardState &position, bool first,
                      ThreadInfo &thread_info) {
  if (depth == 0)
    return 1;
  uint64_t total_nodes = 0;
  const uint64_t checkers = attacks_square(
      position, get_king_pos(position, position.color), position.color ^ 1);

  if (depth <= 1) {
    std::array<Action, MaxActions> list{};
    const int nmoves = legal_movegen(position, list.data());
    if (first) {
      for (int i = 0; i < nmoves; i++) {
        const std::string move_uci = internal_to_uci(position, list[i]);
        safe_printf("%s: 1\n", move_uci.c_str());
      }
    }
    return nmoves;
  }

  MovePicker picker{};
  init_picker(picker, position, -107, checkers);

  while (Action move =
             next_move(picker, position, thread_info, MoveNone, false))

  {
    if (!is_legal(position, move)) {
      continue;
    }

    BoardState new_position = position;
    make_move(new_position, move);

    const uint64_t nodes = perft(depth - 1, new_position, false, thread_info);

    if (first) {
      const std::string move_uci = internal_to_uci(position, move);
      safe_printf("%s: %" PRIu64 "\n", move_uci.c_str(), nodes);
    }
    total_nodes += nodes;
  }

  return total_nodes;
}

inline void bench(BoardState &position, ThreadInfo &thread_info,
                  int depth = 12) {
  static const std::array<std::string, 49> fens = {
      "2r2k2/8/4P1R1/1p6/8/P4K1N/7b/2B5 b - - 0 55",
      "2r4r/1p4k1/1Pnp4/3Qb1pq/8/4BpPp/5P2/2RR1BK1 w - - 0 42",
      "6k1/5pp1/8/2bKP2P/2P5/p4PNb/B7/8 b - - 1 44",
      "6r1/5k2/p1b1r2p/1pB1p1p1/1Pp3PP/2P1R1K1/2P2P2/3R4 w - - 1 36",
      "4rrk1/2p1b1p1/p1p3q1/4p3/2P2n1p/1P1NR2P/PB3PP1/3R1QK1 b - - 2 24",
      "3br1k1/p1pn3p/1p3n2/5pNq/2P1p3/1PN3PP/P2Q1PB1/4R1K1 w - - 0 23",
      "r3k2r/2pb1ppp/2pp1q2/p7/1nP1B3/1P2P3/P2N1PPP/R2QK2R w KQkq a6 0 14",
      "r3qbrk/6p1/2b2pPp/p3pP1Q/PpPpP2P/3P1B2/2PB3K/R5R1 w - - 16 42",
      "6k1/1R3p2/6p1/2Bp3p/3P2q1/P7/1P2rQ1K/5R2 b - - 4 44",
      "8/8/1p2k1p1/3p3p/1p1P1P1P/1P2PK2/8/8 w - - 3 54",
      "7r/2p3k1/1p1p1qp1/1P1Bp3/p1P2r1P/P7/4R3/Q4RK1 w - - 0 36",
      "r1bq1rk1/pp2b1pp/n1pp1n2/3P1p2/2P1p3/2N1P2N/PP2BPPP/R1BQ1RK1 b - - 2 10",
      "3r3k/2r4p/1p1b3q/p4P2/P2Pp3/1B2P3/3BQ1RP/6K1 w - - 3 87",
      "4q1bk/6b1/7p/p1p4p/PNPpP2P/KN4P1/3Q4/4R3 b - - 0 37",
      "2q3r1/1r2pk2/pp3pp1/2pP3p/P1Pb1BbP/1P4Q1/R3NPP1/4R1K1 w - - 2 34",
      "1r2r2k/1b4q1/pp5p/2pPp1p1/P3Pn2/1P1B1Q1P/2R3P1/4BR1K b - - 1 37",
      "r3kbbr/pp1n1p1P/3ppnp1/q5N1/1P1pP3/P1N1B3/2P1QP2/R3KB1R b KQkq b3 0 17",
      "8/6pk/2b1Rp2/3r4/1R1B2PP/P5K1/8/2r5 b - - 16 42",
      "1r4k1/4ppb1/2n1b1qp/pB4p1/1n1BP1P1/7P/2PNQPK1/3RN3 w - - 8 29",
      "8/p2B4/PkP5/4p1pK/4Pb1p/5P2/8/8 w - - 29 68",
      "3r4/ppq1ppkp/4bnp1/2pN4/2P1P3/1P4P1/PQ3PBP/R4K2 b - - 2 20",
      "5rr1/4n2k/4q2P/P1P2n2/3B1p2/4pP2/2N1P3/1RR1K2Q w - - 1 49",
      "q5k1/5ppp/1r3bn1/1B6/P1N2P2/BQ2P1P1/5K1P/8 b - - 2 34",
      "r1b2k1r/5n2/p4q2/1ppn1Pp1/3pp1p1/NP2P3/P1PPBK2/1RQN2R1 w - - 0 22",
      "r1bqk2r/pppp1ppp/5n2/4b3/4P3/P1N5/1PP2PPP/R1BQKB1R w KQkq - 0 5",
      "r1bqr1k1/pp1p1ppp/2p5/8/3N1Q2/P2BB3/1PP2PPP/R3K2n b Q - 1 12",
      "r1bq2k1/p4r1p/1pp2pp1/3p4/1P1B3Q/P2B1N2/2P3PP/4R1K1 b - - 2 19",
      "r4qk1/6r1/1p4p1/2ppBbN1/1p5Q/P7/2P3PP/5RK1 w - - 2 25",
      "r7/6k1/1p6/2pp1p2/7Q/8/p1P2K1P/8 w - - 0 32",
      "r3k2r/ppp1pp1p/2nqb1pn/3p4/4P3/2PP4/PP1NBPPP/R2QK1NR w KQkq - 1 5",
      "3r1rk1/1pp1pn1p/p1n1q1p1/3p4/Q3P3/2P5/PP1NBPPP/4RRK1 w - - 0 12",
      "5rk1/1pp1pn1p/p3Brp1/8/1n6/5N2/PP3PPP/2R2RK1 w - - 2 20",
      "8/1p2pk1p/p1p1r1p1/3n4/8/5R2/PP3PPP/4R1K1 b - - 3 27",
      "8/4pk2/1p1r2p1/p1p4p/Pn5P/3R4/1P3PP1/4RK2 w - - 1 33",
      "8/5k2/1pnrp1p1/p1p4p/P6P/4R1PK/1P3P2/4R3 b - - 1 38",
      "8/8/1p1kp1p1/p1pr1n1p/P6P/1R4P1/1P3PK1/1R6 b - - 15 45",
      "8/8/1p1k2p1/p1prp2p/P2n3P/6P1/1P1R1PK1/4R3 b - - 5 49",
      "8/8/1p4p1/p1p2k1p/P2npP1P/4K1P1/1P6/3R4 w - - 6 54",
      "8/8/1p4p1/p1p2k1p/P2n1P1P/4K1P1/1P6/6R1 b - - 6 59",
      "8/5k2/1p4p1/p1pK3p/P2n1P1P/6P1/1P6/4R3 b - - 14 63",
      "8/1R6/1p1K1kp1/p6p/P1p2P1P/6P1/1Pn5/8 w - - 0 67",
      "1rb1rn1k/p3q1bp/2p3p1/2p1p3/2P1P2N/PP1RQNP1/1B3P2/4R1K1 b - - 4 23",
      "4rrk1/pp1n1pp1/q5p1/P1pP4/2n3P1/7P/1P3PB1/R1BQ1RK1 w - - 3 22",
      "r2qr1k1/pb1nbppp/1pn1p3/2ppP3/3P4/2PB1NN1/PP3PPP/R1BQR1K1 w - - 4 12",
      "2rqr1k1/1p3p1p/p2p2p1/P1nPb3/2B1P3/5P2/1PQ2NPP/R1R4K w - - 3 25",
      "r1b2rk1/p1q1ppbp/6p1/2Q5/8/4BP2/PPP3PP/2KR1B1R b - - 2 14",
      "rnbqkb1r/pppppppp/5n2/8/2PP4/8/PP2PPPP/RNBQKBNR b KQkq c3 0 2",
      "2rr2k1/1p4bp/p1q1p1p1/4Pp1n/2PB4/1PN3P1/P3Q2P/2RR2K1 w - f6 0 20",
      "2r2b2/5p2/5k2/p1r1pP2/P2pB3/1P3P2/K1P3R1/7R w - - 23 93"};

  thread_info.max_iter_depth = std::clamp(depth, 1, MaxRootDepth);
  thread_info.max_nodes_searched = UINT64_MAX / 2;
  thread_info.is_movetime = false;
  thread_info.pondering = false;
  thread_data.pondering = false;
  uint64_t total_nodes = 0;

  const auto start = std::chrono::steady_clock::now();

  for (const std::string &fen : fens) {
    new_game(thread_info, TT);
    set_board(position, thread_info, fen);
    // Depth-only search: time limits restart per position, so the node count
    // does not depend on how fast earlier positions were searched.
    thread_info.max_time = thread_info.opt_time = UINT64_MAX / 2;
    thread_info.time_manager.hard_limit = thread_info.max_time;
    thread_info.time_manager.soft_limit = thread_info.opt_time;
    thread_info.start_time = std::chrono::steady_clock::now();
    thread_info.infinite_search = false;
    thread_info.root_moves_limited = false;
    thread_info.root_moves.clear();
    thread_data.stop = false;
    search_position(position, thread_info, TT);
    total_nodes += thread_info.nodes.load(std::memory_order_relaxed);
    for (const auto &helper : thread_data.thread_infos)
      total_nodes += helper.nodes.load(std::memory_order_relaxed);
  }

  safe_printf("Bench: %" PRIu64 " nodes %" PRIi64 " nps\n", total_nodes,
              static_cast<int64_t>(total_nodes * 1000 / safe_elapsed(start)));
}

inline bool parse_int64(const std::string &text, int64_t &out) noexcept {
  const char *first = text.data(), *last = text.data() + text.size();
  const auto [end, error] = std::from_chars(first, last, out);
  return !text.empty() && error == std::errc() && end == last;
}

inline bool parse_check(std::string text, bool &out) {
  std::transform(text.begin(), text.end(), text.begin(),
                 [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
  if (text == "true" || text == "1" || text == "yes" || text == "on") out = true;
  else if (text == "false" || text == "0" || text == "no" || text == "off") out = false;
  else return false;
  return true;
}

// One UCI option: printed by "uci" and applied by "setoption" from the same entry.
struct UciOption {
  enum class Kind { Check, Spin, String };
  std::string name;
  Kind kind;
  int64_t min = 0, max = 0;
  std::string default_value;
  std::function<void(int64_t)> set_spin;
  std::function<void(bool)> set_check;
  std::function<void(const std::string &)> set_string;

  std::string describe() const {
    switch (kind) {
    case Kind::Check:
      return "option name " + name + " type check default " + default_value;
    case Kind::Spin:
      return "option name " + name + " type spin default " + default_value +
             " min " + std::to_string(min) + " max " + std::to_string(max);
    case Kind::String:
      return "option name " + name + " type string default " +
             (default_value.empty() ? "<empty>" : default_value);
    }
    return {};
  }
};

inline UciOption spin_option(std::string name, int64_t min, int64_t max, int64_t value,
                             std::function<void(int64_t)> set) {
  return {std::move(name), UciOption::Kind::Spin, min, max, std::to_string(value), std::move(set), {}, {}};
}

inline UciOption check_option(std::string name, bool value, std::function<void(bool)> set) {
  return {std::move(name), UciOption::Kind::Check, 0, 0, value ? "true" : "false", {}, std::move(set), {}};
}

inline UciOption string_option(std::string name, std::string value,
                               std::function<void(const std::string &)> set) {
  return {std::move(name), UciOption::Kind::String, 0, 0, std::move(value), {}, {}, std::move(set)};
}

inline void uci(ThreadInfo &thread_info, BoardState &position,
                std::istream &in_stream = std::cin, bool interactive = true) {
  setvbuf(stdin, NULL, _IONBF, 0);
  setvbuf(stdout, NULL, _IONBF, 0);

  new_game(thread_info, TT);
  set_board(position, thread_info,
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
  if (!load_embedded_nnue()) {
    std::exit(EXIT_FAILURE);
  }

  std::string input;

  std::thread s;

  auto safe_join = [](std::thread &t) {
    if (t.joinable())
      t.join();
  };
  auto stop_search = [&](bool emit_bm = false) {
    {
      std::lock_guard lock(thread_data.control_mutex);
      thread_data.emit_bestmove = emit_bm;
      thread_data.stop = true;
    }
    thread_data.control_cv.notify_all();
    safe_join(s);
  };
  auto init_tablebases = [&] {
    if (tb_initialized)
      tb_free();
    tb_initialized = false;
    if (thread_info.use_syzygy && !thread_info.syzygy_path.empty()) {
      tb_initialized =
          tb_init(thread_info.syzygy_path.c_str()) && TB_LARGEST > 0;
      if (!tb_initialized)
        std::exit(EXIT_FAILURE);
    }
  };
  auto init_book = [&] {
    thread_info.opening_book.clear_book();
    if (thread_info.use_opening_book && !thread_info.book_path.empty()) {
      const bool loaded =
          thread_info.opening_book.load_book(thread_info.book_path);
      if (!loaded)
        std::exit(EXIT_FAILURE);
    }
  };
  auto require_search_assets = [&] {
    if (thread_info.use_syzygy && !tb_initialized)
      std::exit(EXIT_FAILURE);
    if (thread_info.use_opening_book && !thread_info.opening_book.is_loaded())
      std::exit(EXIT_FAILURE);
  };

  auto &ti = thread_info;
  std::vector<UciOption> options = {
      spin_option("Hash", 1, 131072, DefaultHashMB, [](int64_t v) { resize_TT(static_cast<int>(v)); }),
      spin_option("Threads", 1, 1024, 1, [](int64_t v) {
        try {
          std::vector<ThreadInfo> workers(static_cast<size_t>(v - 1));
          thread_data.threads.reserve(static_cast<size_t>(v - 1));
          thread_data.thread_infos.swap(workers);
        } catch (const std::bad_alloc &) {
          std::exit(EXIT_FAILURE);
        }
      }),
      spin_option("MultiPV", 1, 256, ti.multipv, [&ti](int64_t v) { ti.multipv = static_cast<uint16_t>(v); }),
      check_option("UCI_Chess960", thread_data.is_frc, [](bool v) { thread_data.is_frc = v; }),
      spin_option("MaxMoveTime", 0, 10000, static_cast<int64_t>(ti.max_move_time),
                  [&ti](int64_t v) { ti.max_move_time = static_cast<uint64_t>(v); }),
      spin_option("MoveOverhead", 0, 1000, static_cast<int64_t>(ti.move_overhead),
                  [&ti](int64_t v) { ti.move_overhead = static_cast<uint64_t>(v); }),
      check_option("Ponder", ti.use_ponder, [&ti](bool v) {
        ti.use_ponder = v;
        if (!v) {
          ti.pondering = false;
          ti.ponder_hit = false;
          ti.ponder_move = MoveNone;
        }
      }),
      spin_option("MaxDepth", 0, MaxRootDepth, ti.max_depth,
                  [&ti](int64_t v) { ti.max_depth = static_cast<uint16_t>(v); }),
      spin_option("MaxNodes", 0, INT32_MAX, static_cast<int64_t>(ti.max_nodes),
                  [&ti](int64_t v) { ti.max_nodes = static_cast<uint64_t>(v); }),
      check_option("UseSyzygy", ti.use_syzygy, [&](bool v) {
        ti.use_syzygy = v;
        init_tablebases();
      }),
      string_option("SyzygyPath", ti.syzygy_path, [&](const std::string &v) {
        ti.syzygy_path = v;
        init_tablebases();
      }),
      spin_option("SyzygyProbeDepth", 1, 64, ti.syzygy_probe_depth,
                  [&ti](int64_t v) { ti.syzygy_probe_depth = static_cast<int>(v); }),
      spin_option("SyzygyProbeLimit", 1, 7, ti.syzygy_probe_limit,
                  [&ti](int64_t v) { ti.syzygy_probe_limit = static_cast<int>(v); }),
      check_option("Syzygy50MoveRule", ti.syzygy_50_move_rule, [&ti](bool v) { ti.syzygy_50_move_rule = v; }),
      check_option("UseOpeningBook", ti.use_opening_book, [&](bool v) {
        ti.use_opening_book = v;
        init_book();
      }),
      string_option("BookPath", ti.book_path, [&](const std::string &v) {
        ti.book_path = v;
        init_book();
      }),
      spin_option("BookDepthLimit", 0, 50, ti.book_depth_limit,
                  [&ti](int64_t v) { ti.book_depth_limit = static_cast<int>(v); }),
      spin_option("BookMinWeight", 0, 1000, ti.book_min_weight,
                  [&ti](int64_t v) { ti.book_min_weight = static_cast<int>(v); }),
      spin_option("PonderTimeFactor", 0, 200, ti.ponder_time_factor,
                  [&ti](int64_t v) { ti.ponder_time_factor = static_cast<int>(v); }),
      spin_option("NormalizationFactor", 50, 500, NormalizationFactor,
                  [](int64_t v) { NormalizationFactor = static_cast<int>(v); }),
  };
  for (auto &param : params) {
    options.push_back(spin_option(param.name, param.min, param.max, param.value, [&param](int64_t v) {
      param.value = static_cast<int>(v);
      if (param.name == "LMRBase" || param.name == "LMRRatio")
        init_LMR();
    }));
  }

  while (getline(in_stream, input)) {

    if (input.empty()) {
      continue;
    }

    std::istringstream input_stream(input);

    std::string command;

    input_stream >> std::skipws >> command;

    if (command == "setoption" || command == "ucinewgame" ||
        command == "position" || command == "go" || command == "bench" ||
        command == "perft" || command == "eval" || command == "flip" ||
        command == "hashfull" || command == "d" || command == "printparams")
      stop_search(false);

    if (command == "d") {
      print_board(position);
      safe_printf("Fen: %s\n", export_fen(position, thread_info).c_str());
      continue;
    }

    if (command == "quit") {
      stop_search(false);
      break;
    }

    else if (command == "uci") {
      std::string reply = "id name Stallion EAS NNUE\nid author LegendOfCompiling\n";
      for (const auto &option : options)
        reply += option.describe() + "\n";
      safe_printf("%suciok\n", reply.c_str());
    }

    else if (command == "printparams") {
      print_params_for_ob();
    }

    else if (command == "isready") {
      safe_printf("readyok\n");
    }

    else if (command == "setoption") {
      std::string word, name, value;
      bool in_name = false;
      while (input_stream >> word) {
        if (word == "name") {
          in_name = true;
        } else if (word == "value") {
          std::getline(input_stream, value);
          break;
        } else if (in_name) {
          name += (name.empty() ? "" : " ") + word;
        }
      }
      const auto first = value.find_first_not_of(" \t\r");
      value = first == std::string::npos
                  ? ""
                  : value.substr(first, value.find_last_not_of(" \t\r") - first + 1);
      if (value == "<empty>")
        value.clear();
      auto lowercase = [](std::string text) {
        std::transform(text.begin(), text.end(), text.begin(),
                       [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
        return text;
      };
      const auto option = std::find_if(options.begin(), options.end(), [&](const UciOption &o) {
        return lowercase(o.name) == lowercase(name);
      });
      if (option == options.end())
        continue;
      int64_t number = 0;
      bool flag = false;
      switch (option->kind) {
      case UciOption::Kind::Spin:
        if (parse_int64(value, number))
          option->set_spin(std::clamp(number, option->min, option->max));
        break;
      case UciOption::Kind::Check:
        if (parse_check(value, flag))
          option->set_check(flag);
        break;
      case UciOption::Kind::String:
        option->set_string(value);
        break;
      }
    }

    else if (command == "stop") {
      stop_search(true);
    }

    else if (command == "ucinewgame") {
      new_game(thread_info, TT);
      thread_info.time_manager = TimeManager();
      thread_info.is_movetime = false;
      thread_info.best_move_stable = false;
      thread_info.stability_counter = 0;
      thread_info.root_moves.clear();
      thread_info.root_moves_limited = false;
      set_board(position, thread_info,
                "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
    }

    else if (command == "position") {

      const BoardState previous_position = position;
      const auto previous_game_hist = thread_info.game_hist;
      const uint16_t previous_game_ply = thread_info.game_ply;
      const uint16_t previous_search_ply = thread_info.search_ply;

      std::string setup;
      input_stream >> setup;
      bool has_moves_token = false;

      if (setup == "fen") {
        std::string fen;
        std::string token;

        while (input_stream >> token) {
          if (token == "moves") {
            has_moves_token = true;
            break;
          }
          if (!fen.empty())
            fen += " ";
          fen += token;
        }

        if (!set_board(position, thread_info, fen)) {
          continue;
        }
      } else if (setup == "startpos") {
        std::string token;
        if (input_stream >> token) {
          if (token != "moves") {
            continue;
          }
          has_moves_token = true;
        }
        set_board(position, thread_info,
                  "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
      } else {
        continue;
      }

      if (has_moves_token) {

        std::string moves;
        bool invalid_move = false;
        while (input_stream >> moves) {
          Action move = uci_to_internal(position, moves);
          if (move == MoveNone) {
            invalid_move = true;
            break;
          }
          if (thread_info.game_ply >= MaxGameLen - MaxSearchPly - 2) {
            std::move(thread_info.game_hist.begin() + MaxGameLen / 2,
                      thread_info.game_hist.end(),
                      thread_info.game_hist.begin());
            thread_info.game_ply -= MaxGameLen / 2;
          }

          thread_info.game_hist[thread_info.game_ply].position_key =
              position.zobrist_key;
          thread_info.game_hist[thread_info.game_ply].played_move = move;
          thread_info.game_hist[thread_info.game_ply].piece_moved =
              position.board[extract_from(move)];
          if (thread_info.game_ply + 1 < MaxGameLen)
            thread_info.game_ply++;

          make_move(position, move);
        }

        if (invalid_move) {
          position = previous_position;
          thread_info.game_hist = previous_game_hist;
          thread_info.game_ply = previous_game_ply;
          thread_info.search_ply = previous_search_ply;
          continue;
        }

        thread_info.search_ply = 0;
      }

    }

    else if (command == "go") {
      require_search_assets();
      thread_info.start_time = std::chrono::steady_clock::now();
      thread_info.infinite_search = false;

      thread_info.ponder_hit = false;
      thread_info.pondering = false;
      thread_info.ponder_move = MoveNone;
      thread_data.ponder_hit_time = -1;
      thread_info.best_move_stable = false;
      thread_info.stability_counter = 0;
      thread_info.time_checks = 0;
      thread_info.max_nodes_searched = UINT64_MAX / 2;
      thread_info.max_iter_depth = MaxRootDepth;
      thread_info.mate_search = 0;

      int color = position.color, time = -1, increment = 0;
      int move_time = -1;
      std::string token;
      int movestogo = 0;
      std::vector<Action> searchmoves;
      bool searchmoves_specified = false;
      auto parse_count = [](const std::string &s, int64_t &out) {
        return parse_int64(s, out) && out >= 0;
      };
      auto is_uci_move_token = [](const std::string &s) -> bool {
        auto valid_file = [](char c) { return c >= 'a' && c <= 'h'; };
        auto valid_rank = [](char c) { return c >= '1' && c <= '8'; };
        if (s.size() != 4 && s.size() != 5)
          return false;
        if (!valid_file(s[0]) || !valid_rank(s[1]) || !valid_file(s[2]) ||
            !valid_rank(s[3]))
          return false;
        if (s.size() == 5) {
          char promo =
              static_cast<char>(std::tolower(static_cast<unsigned char>(s[4])));
          if (promo != 'q' && promo != 'r' && promo != 'b' && promo != 'n')
            return false;
        }
        return true;
      };

      std::vector<std::string> go_tokens;
      while (input_stream >> token)
        go_tokens.push_back(token);

      for (size_t i = 0; i < go_tokens.size(); ++i) {
        token = go_tokens[i];

        if (token == "ponder") {
          thread_info.pondering = thread_info.use_ponder;
          continue;
        }

        if (token == "infinite") {
          thread_info.max_iter_depth = MaxRootDepth;
          thread_info.max_time = UINT64_MAX;
          thread_info.opt_time = UINT64_MAX;
          thread_info.infinite_search = true;
        } else if (token == "wtime" || token == "btime" || token == "winc" ||
                   token == "binc" || token == "movestogo" || token == "mate" ||
                   token == "depth" || token == "movetime") {
          if (i + 1 >= go_tokens.size())
            continue;

          int64_t parsed = 0;
          if (!parse_count(go_tokens[i + 1], parsed)) {
            ++i;
            continue;
          }
          const int value = static_cast<int>(std::min<int64_t>(parsed, INT32_MAX));

          if (token == "wtime") {
            if (color == Colors::White)
              time = value;
          } else if (token == "btime") {
            if (color == Colors::Black)
              time = value;
          } else if (token == "winc") {
            if (color == Colors::White)
              increment = value;
          } else if (token == "binc") {
            if (color == Colors::Black)
              increment = value;
          } else if (token == "movestogo") {
            movestogo = value;
          } else if (token == "mate") {
            thread_info.mate_search = std::clamp(value, 1, MaxRootDepth / 2);
            thread_info.max_iter_depth = thread_info.mate_search * 2;
          } else if (token == "depth") {
            thread_info.max_iter_depth = std::clamp(value, 1, MaxRootDepth);
          } else if (token == "movetime") {
            move_time = std::max(1, value);
          }
          ++i;
        } else if (token == "nodes") {
          if (i + 1 >= go_tokens.size())
            continue;
          int64_t nodes = 0;
          if (parse_count(go_tokens[i + 1], nodes))
            thread_info.max_nodes_searched = std::max<uint64_t>(1, static_cast<uint64_t>(nodes));
          ++i;
        } else if (token == "searchmoves") {
          searchmoves_specified = true;
          searchmoves.clear();
          ++i;
          for (; i < go_tokens.size(); ++i) {
            const std::string &move_str = go_tokens[i];
            if (!is_uci_move_token(move_str)) {
              --i;
              break;
            }
            Action move = uci_to_internal(position, move_str);
            if (move != MoveNone)
              searchmoves.push_back(move);
          }
        }
      }

      if (!interactive) {
        const bool bound = thread_info.max_iter_depth != MaxRootDepth ||
                           thread_info.mate_search > 0 || move_time >= 0 || time >= 0 ||
                           thread_info.max_nodes_searched != UINT64_MAX / 2;
        if (thread_info.infinite_search)
          std::exit(EXIT_FAILURE);
        if (thread_info.pondering.load())
          std::exit(EXIT_FAILURE);
        if (!bound)
          std::exit(EXIT_FAILURE);
      }

      constexpr uint64_t unlimited = UINT64_MAX / 2;
      thread_info.max_time = thread_info.opt_time = unlimited;
      thread_info.is_movetime = (move_time >= 0);
      if (!thread_info.infinite_search) {
        if (move_time >= 0) {
          const auto overhead = std::min<uint64_t>(
              thread_info.move_overhead, static_cast<uint64_t>(move_time / 10));
          thread_info.max_time = thread_info.opt_time = std::max<uint64_t>(
              1, static_cast<uint64_t>(move_time) - overhead);
        } else if (time >= 0) {
          const auto overhead = std::min<uint64_t>(
              thread_info.move_overhead, static_cast<uint64_t>(time / 10));
          const uint64_t usable =
              std::max<uint64_t>(1, static_cast<uint64_t>(time) - overhead);
          thread_info.time_manager.initialize(usable, increment, movestogo,
                                              position.fullmove);
          thread_info.max_time = thread_info.time_manager.hard_limit;
          thread_info.opt_time = thread_info.time_manager.soft_limit;
        }
        if (thread_info.max_move_time > 0) {
          thread_info.max_time =
              std::min(thread_info.max_time, thread_info.max_move_time);
          thread_info.opt_time =
              std::min(thread_info.opt_time, thread_info.max_time);
        }
      }
      thread_info.time_manager.allocated_time = thread_info.opt_time;
      thread_info.time_manager.soft_limit = thread_info.opt_time;
      thread_info.time_manager.max_time = thread_info.max_time;
      thread_info.time_manager.hard_limit = thread_info.max_time;
      thread_data.pondering = thread_info.pondering.load();

      if (!thread_info.infinite_search && !searchmoves_specified &&
          !thread_info.pondering && thread_info.use_opening_book &&
          thread_info.opening_book.is_loaded() &&
          (thread_info.book_depth_limit == 0 ||
           uint64_t(position.fullmove - 1) * 2 + position.color <
               static_cast<uint64_t>(thread_info.book_depth_limit))) {

        const uint64_t book_key =
            thread_info.opening_book.polyglot_key(position);
        Action book_move = thread_info.opening_book.probe_book(
            position, thread_info.book_min_weight);

        if (book_move != MoveNone) {
          const bool seen =
              std::find(thread_info.recent_book_keys.begin(),
                        thread_info.recent_book_keys.end(),
                        book_key) != thread_info.recent_book_keys.end();
          if (seen) {
            static thread_local std::mt19937 rng(Random::rd());
            if ((std::uniform_int_distribution<int>(0, 99)(rng)) < 50) {
              book_move = MoveNone;
            }
          }
          if (book_move != MoveNone) {
            thread_info.recent_book_keys[thread_info.recent_book_head++ %
                                         thread_info.recent_book_keys.size()] =
                book_key;
          }
        }

        if (book_move != MoveNone) {
          std::array<Action, MaxActions> legal_moves{};
          const int num_legal = legal_movegen(position, legal_moves.data());
          const bool is_legal =
              std::find(legal_moves.begin(), legal_moves.begin() + num_legal,
                        book_move) != (legal_moves.begin() + num_legal);
          if (is_legal) {
            const std::string bm = internal_to_uci(position, book_move);
            safe_printf("bestmove %s\n", bm.c_str());
            continue;
          }
        }
      }

      if (thread_info.max_depth > 0) {
        thread_info.max_iter_depth =
            std::min(thread_info.max_iter_depth,
                     static_cast<int>(thread_info.max_depth));
      }

      if (thread_info.max_nodes > 0) {
        thread_info.max_nodes_searched =
            std::min(thread_info.max_nodes_searched, thread_info.max_nodes);
      }

      if (searchmoves_specified) {
        thread_info.root_moves.clear();
        thread_info.root_moves.reserve(searchmoves.size());
        for (Action move : searchmoves) {
          thread_info.root_moves.push_back({move, 0});
        }
        thread_info.root_moves_limited = true;
      } else {
        thread_info.root_moves.clear();
        thread_info.root_moves_limited = false;
      }

      run_thread(position, thread_info, s);
    }

    else if (command == "ponderhit") {
      if (thread_data.pondering) {
        thread_data.ponder_hit_time =
            std::chrono::duration_cast<std::chrono::milliseconds>(
                std::chrono::steady_clock::now().time_since_epoch())
                .count();
        {
          std::lock_guard lock(thread_data.control_mutex);
          thread_data.pondering = false;
        }
        thread_data.control_cv.notify_all();
      }
    }

    else if (command == "bench") {
      require_search_assets();
      int depth = 12;
      input_stream >> depth;
      bench(position, thread_info, depth);
    }

    else if (command == "perft") {
      int perft_depth = 0;
      if ((input_stream >> perft_depth) && perft_depth >= 0 &&
          perft_depth <= 10) {
        const auto perft_start = std::chrono::steady_clock::now();
        const uint64_t nodes = perft(perft_depth, position, true, thread_info);
        const uint64_t elapsed_ms = time_elapsed(perft_start);
        const uint64_t nps = (elapsed_ms > 0) ? (nodes * 1000 / elapsed_ms) : 0;
        safe_printf("%" PRIu64 " nodes %" PRIu64 " nps\n", nodes, nps);
      }
    }

    else if (command == "eval") {
      thread_info.nnue_state.reset_nnue(position);
      const int raw = thread_info.nnue_state.evaluate(position);
      safe_printf("NNUE raw: %d (eval: %d cp)\n", raw,
                  raw * 100 / NormalizationFactor);
    }

    else if (command == "flip") {
      position.color ^= 1;
      position.ep_square = SquareNone;
      calculate(position);
    }

    else if (command == "hashfull") {
      int filled = 0;
      int sample_size =
          static_cast<int>(std::min<uint64_t>(safe_TT_size(), 1000));
      if (sample_size <= 0)
        sample_size = 0;

      {
        std::lock_guard<std::mutex> lg(thread_data.data_mutex);
        for (int i = 0; i < sample_size; i++) {
          for (const auto &entry : TT[i].entries) {
            if (entry.score != 0 || entry.get_type() != EntryTypes::None) {
              filled++;
              break;
            }
          }
        }
      }
      if (sample_size == 0)
        sample_size = 1;
      safe_printf("info hashfull %d\n", (filled * 1000) / sample_size);
    }
  }

  if (interactive)
    stop_search(false);
  else
    safe_join(s);
  thread_data.stop = true;
  if (tb_initialized) {
    tb_free();
    tb_initialized = false;
  }
}

#pragma once
#include <array>

#include "../fathom/src/tbprobe.h"
#include "search.h"

#include <algorithm>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <memory>

bool tb_initialized = false;

inline void compute_human_params(ThreadInfo &thread_info) {
  int elo = thread_info.human_elo;
  int delta = 3401 - elo;
  if (delta < 0)
    delta = 0;
  if (delta > 3000)
    delta = 3000;
  thread_info.human_value_margin = 15 + delta / 30;
  if (thread_info.human_value_margin > 120)
    thread_info.human_value_margin = 120;
  thread_info.human_noise_sigma = delta / 25;
  if (thread_info.human_noise_sigma > 120)
    thread_info.human_noise_sigma = 120;
  int depth_cap = 0;
  if (elo < 3300) {
    depth_cap = 8 + (elo - 1200) * 8 / 2100;
  }

  if (elo <= 2000)
    depth_cap = 14;
  if (elo <= 1800)
    depth_cap = 16;
  if (elo <= 1500)
    depth_cap = 14;
  if (elo <= 1300)
    depth_cap = 12;
  thread_info.human_depth_limit = depth_cap;
}

void run_thread(Position &position, ThreadInfo &thread_info, std::thread &s) {

  s = std::thread(search_position, std::ref(position), std::ref(thread_info),
                  std::ref(TT));
}

uint64_t perft(int depth, Position &position, bool first,
               ThreadInfo &thread_info)

{
  uint64_t total_nodes = 0;
  uint64_t checkers = attacks_square(
      position, get_king_pos(position, position.color), position.color ^ 1);

  if (depth <= 1) {
    std::array<Move, ListSize> list;

    int nmoves = legal_movegen(position, list.data());

    for (int i = 0; i < nmoves; i++) {
      total_nodes += is_legal(position, list[i]);
    }

    return total_nodes;
  }

  MovePicker picker;
  init_picker(picker, position, -107, checkers,
              &(thread_info.game_hist[thread_info.game_ply]));

  while (Move move = next_move(picker, position, thread_info, MoveNone, false))

  {
    if (!is_legal(position, move)) {
      continue;
    }

    Position new_position = position;
    make_move(new_position, move);

    uint64_t nodes = perft(depth - 1, new_position, false, thread_info);

    if (first) {
      std::string move_uci = internal_to_uci(position, move);
      safe_printf("%s: %" PRIu64 "\n", move_uci.c_str(), nodes);
    }
    total_nodes += nodes;
  }

  return total_nodes;
}

void bench(Position &position, ThreadInfo &thread_info) {
  std::vector<std::string> fens = {
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

  thread_info.max_time = UINT64_MAX / 2, thread_info.opt_time = UINT64_MAX / 2;
  thread_info.max_iter_depth = 12;
  uint64_t total_nodes = 0;

  auto start = std::chrono::steady_clock::now();

  for (std::string fen : fens) {
    new_game(thread_info, TT);
    set_board(position, thread_info, fen);
    thread_info.start_time = std::chrono::steady_clock::now();
    thread_info.infinite_search = false;
    search_position(position, thread_info, TT);
    total_nodes += thread_info.nodes.load();
  }

  safe_printf("Bench: %" PRIu64 " nodes %" PRIi64 " nps\n", total_nodes,
              (int64_t)(total_nodes * 1000 / time_elapsed(start)));
}

void uci(ThreadInfo &thread_info, Position &position) noexcept {
  setvbuf(stdin, NULL, _IONBF, 0);
  setvbuf(stdout, NULL, _IONBF, 0);

  safe_printf("Stallion, written by LegendOfCompiling\n\n\n");

  new_game(thread_info, TT);
  set_board(position, thread_info,
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");

  std::string input;

  std::thread s;

  auto safe_join = [](std::thread &t) {
    try {
      if (t.joinable())
        t.join();
    } catch (...) {
    }
  };

  while (getline(std::cin, input)) {

    if (std::cin.eof() || std::cin.fail()) {
      break;
    }

    if (input.empty()) {
      continue;
    }

    std::istringstream input_stream(input);

    std::string command;

    input_stream >> std::skipws >> command;

    if (command == "d") {
      input_stream.clear();
      input_stream.str("position fen "
                       "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/"
                       "R3K2R w KQkq - 0 1");
      input_stream >> std::skipws >> command;
    }

    if (command == "quit") {
      thread_data.terminate = true;

      safe_join(s);

      if (thread_data.num_threads > 0 && !thread_data.threads.empty()) {

        try {
          reset_barrier.cancel();
          idle_barrier.cancel();
        } catch (...) {
        }

        {
          std::lock_guard<std::mutex> lg(thread_data.data_mutex);
          for (size_t i = 0; i < thread_data.threads.size(); i++) {
            safe_join(thread_data.threads[i]);
          }
          thread_data.thread_infos.clear();
          thread_data.threads.clear();
        }

        try {
          reset_barrier.clear_cancel();
          idle_barrier.clear_cancel();
        } catch (...) {
        }
      }
      std::exit(0);
    }

    else if (command == "uci") {
      safe_printf(
          "id name Stallion GB Edition\n"
          "id author LegendOfCompiling\n"

          "option name Hash type spin default 256 min 1 max 131072\n"
          "option name Threads type spin default 1 min 1 max 1024\n"
          "option name MultiPV type spin default 1 min 1 max 256\n"

          "option name Variety type spin default 150 min 0 max 150\n"
          "option name UCI_LimitStrength type check default false\n"

          "option name UCI_Elo type spin default 3401 min 500 max 3401\n"
          "option name UCI_Chess960 type check default false\n"
          "option name OpeningAggressiveness type spin default 150 min 50 max "
          "150\n"
          "option name MiddlegameAggressiveness type spin default 150 min 50 "
          "max 150\n"
          "option name LateMiddlegameAggressiveness type spin default 150 min "
          "50 max 150\n"
          "option name EndgameAggressiveness type spin default 150 min 50 max "
          "150\n"
          "option name SacrificeLookAhead type spin default 1 min 0 max 1\n"
          "option name SacrificeLookAheadTimeMultiplier type spin default 200 "
          "min 50 max 200\n"
          "option name SacrificeLookAheadAggressiveness type spin default 150 "
          "min 50 max 150\n"
          "option name MaxMoveTime type spin default 0 min 0 max 10000\n"
          "option name MoveOverhead type spin default 30 min 0 max 1000\n"
          "option name Ponder type check default false\n"
          "option name UseSyzygy type check default false\n"
          "option name SyzygyPath type string default \"\"\n"
          "option name MaxDepth type spin default 0 min 0 max 256\n"
          "option name MaxNodes type spin default 0 min 0 max 500000\n"

          "option name UseOpeningBook type check default false\n"
          "option name BookPath type string default \"\"\n"
          "option name BookDepthLimit type spin default 0 min 0 max 50\n"
          "option name SyzygyProbeDepth type spin default 6 min 1 max 64\n"
          "option name SyzygyProbeLimit type spin default 6 min 1 max 7\n"
          "option name Syzygy50MoveRule type check default true\n"
          "option name BookMinWeight type spin default 0 min 0 max 1000\n"
          "option name PonderTimeFactor type spin default 200 min 0 max 200\n");

      safe_printf("uciok\n");
    }

    else if (command == "printparams") {
      print_params_for_ob();
    }

    else if (command == "isready") {
      safe_printf("readyok\n");
    }

    else if (command == "setoption") {

      std::string word;
      std::string optName;
      std::string valueStr;
      bool gotName = false;
      bool afterValue = false;

      while (input_stream >> word) {
        if (word == "name") {
          continue;
        } else if (!gotName) {
          optName = word;
          gotName = true;
        } else if (word == "value") {
          afterValue = true;

          std::string rest;
          std::getline(input_stream, rest);
          if (!rest.empty() && (rest[0] == ' ' || rest[0] == '\t'))
            rest.erase(0, rest.find_first_not_of(" \t"));
          valueStr = rest;
          break;
        } else if (!afterValue) {

          valueStr = word;
          afterValue = true;
          std::string rest;
          std::getline(input_stream, rest);
          if (!rest.empty())
            valueStr += rest;
          if (!valueStr.empty() && (valueStr[0] == ' ' || valueStr[0] == '\t'))
            valueStr.erase(0, valueStr.find_first_not_of(" \t"));
          break;
        }
      }

      auto parse_int = [](const std::string &s, bool &ok) {
        try {
          int v = std::stoi(s);
          ok = true;
          return v;
        } catch (...) {
          ok = false;
          return 0;
        }
      };
      auto parse_uint64 = [](const std::string &s, bool &ok) -> uint64_t {
        try {
          unsigned long long tmp = std::stoull(s);
          ok = true;
          return static_cast<uint64_t>(tmp);
        } catch (...) {
          ok = false;
          return static_cast<uint64_t>(0);
        }
      };
      auto to_bool = [](std::string s) {
        std::transform(s.begin(), s.end(), s.begin(), ::tolower);
        return s == "true" || s == "1" || s == "yes" || s == "on";
      };

      if (!gotName) {
        continue;
      }

      if (optName == "Hash") {
        bool ok = false;
        int mb = parse_int(valueStr, ok);
        if (!ok)
          continue;
        if (mb < 1)
          mb = 1;
        if (mb > 131072)
          mb = 131072;
        resize_TT(mb);
      } else if (optName == "Threads") {
        bool ok = false;
        int thr = parse_int(valueStr, ok);
        if (!ok)
          continue;
        if (thr < 1)
          thr = 1;
        if (thr > 1024)
          thr = 1024;

        thread_data.terminate = true;

        try {
          reset_barrier.cancel();
          idle_barrier.cancel();
        } catch (...) {
        }
        for (auto &t : thread_data.threads) {
          try {
            if (t.joinable())
              t.join();
          } catch (...) {
          }
        }
        try {
          reset_barrier.clear_cancel();
          idle_barrier.clear_cancel();
        } catch (...) {
        }
        thread_data.thread_infos.clear();
        thread_data.threads.clear();
        thread_data.terminate = false;
        thread_data.num_threads = thr;

        reset_barrier.reset(thread_data.num_threads);
        idle_barrier.reset(thread_data.num_threads);
        search_end_barrier.reset(thread_data.num_threads);

        for (int i = 0; i < thr - 1; i++) {
          thread_data.thread_infos.emplace_back();
          thread_data.threads.emplace_back(loop, i);
        }
      } else if (optName == "MultiPV") {
        bool ok = false;
        int mv = parse_int(valueStr, ok);
        if (!ok)
          continue;
        if (mv < 1)
          mv = 1;
        if (mv > 256)
          mv = 256;
        thread_info.multipv = mv;
      } else if (optName == "Variety") {
        bool ok = false;
        int v = parse_int(valueStr, ok);
        if (!ok)
          continue;
        v = std::clamp(v, 0, 150);
        thread_info.variety = v;
      } else if (optName == "UCI_LimitStrength") {
        bool b = to_bool(valueStr);
        thread_info.is_human = b;
        if (!b) {
          thread_info.human_value_margin = 0;
          thread_info.human_noise_sigma = 0;
          thread_info.human_depth_limit = 0;
        } else {
          compute_human_params(thread_info);
        }
      } else if (optName == "UCI_Chess960") {
        bool b = to_bool(valueStr);
        thread_data.is_frc = b;
      } else if (optName == "UCI_Elo") {
        bool ok = false;
        int elo = parse_int(valueStr, ok);
        if (!ok)
          continue;
        elo = std::clamp(elo, 500, 3401);
        thread_info.human_elo = elo;
        if (thread_info.is_human) {
          compute_human_params(thread_info);
        }
      } else if (optName == "OpeningAggressiveness") {
        bool ok = false;
        int v = parse_int(valueStr, ok);
        if (!ok)
          continue;
        v = std::clamp(v, 50, 150);
        thread_info.opening_aggressiveness = v / 100.0f;
      } else if (optName == "MiddlegameAggressiveness") {
        bool ok = false;
        int v = parse_int(valueStr, ok);
        if (!ok)
          continue;
        v = std::clamp(v, 50, 150);
        thread_info.middlegame_aggressiveness = v / 100.0f;
      } else if (optName == "LateMiddlegameAggressiveness") {
        bool ok = false;
        int v = parse_int(valueStr, ok);
        if (!ok)
          continue;
        v = std::clamp(v, 50, 150);
        thread_info.late_middlegame_aggressiveness = v / 100.0f;
      } else if (optName == "EndgameAggressiveness") {
        bool ok = false;
        int v = parse_int(valueStr, ok);
        if (!ok)
          continue;
        v = std::clamp(v, 50, 150);
        thread_info.endgame_aggressiveness = v / 100.0f;
      } else if (optName == "SacrificeLookAhead") {
        bool ok = false;
        int v = parse_int(valueStr, ok);
        if (!ok)
          continue;
        v = std::clamp(v, 0, 1);
        thread_info.sacrifice_lookahead = v;
      } else if (optName == "SacrificeLookAheadTimeMultiplier") {
        bool ok = false;
        int v = parse_int(valueStr, ok);
        if (!ok)
          continue;
        v = std::clamp(v, 50, 200);
        thread_info.sacrifice_lookahead_time_multiplier = v;
      } else if (optName == "SacrificeLookAheadAggressiveness") {
        bool ok = false;
        int v = parse_int(valueStr, ok);
        if (!ok)
          continue;
        v = std::clamp(v, 50, 150);
        thread_info.sacrifice_lookahead_aggressiveness = v;
      } else if (optName == "MaxMoveTime") {
        bool ok = false;
        int v = parse_int(valueStr, ok);
        if (!ok)
          continue;
        thread_info.max_move_time = static_cast<uint64_t>(std::max(0, v));
      } else if (optName == "MoveOverhead") {
        bool ok = false;
        int v = parse_int(valueStr, ok);
        if (!ok)
          continue;
        thread_info.move_overhead =
            static_cast<uint64_t>(std::clamp(v, 0, 1000));
      } else if (optName == "MaxDepth") {
        bool ok = false;
        int v = parse_int(valueStr, ok);
        if (!ok)
          continue;
        v = std::clamp(v, 0, 256);
        thread_info.max_depth = (uint16_t)v;
      } else if (optName == "MaxNodes") {
        bool ok = false;
        uint64_t v = parse_uint64(valueStr, ok);
        if (!ok)
          continue;
        if (v > 500000ULL)
          v = 500000ULL;
        thread_info.max_nodes = v;
      } else if (optName == "UseSyzygy") {
        bool b = to_bool(valueStr);
        if (b && !thread_info.syzygy_path.empty()) {

          if (std::filesystem::exists(thread_info.syzygy_path)) {
            thread_info.use_syzygy = true;
            if (tb_initialized) {
              tb_free();
              tb_initialized = false;
            }
            safe_printf("info string syzygy enabled: %s\n",
                        thread_info.syzygy_path.c_str());
            safe_fflush();
          } else {
            safe_printf("info string failed to enable syzygy: %s\n",
                        thread_info.syzygy_path.c_str());
            safe_fflush();
            thread_info.use_syzygy = false;
          }
        } else if (!b) {
          thread_info.use_syzygy = false;
          if (tb_initialized) {
            tb_free();
            tb_initialized = false;
          }
          safe_printf("info string syzygy disabled\n");
          safe_fflush();
        } else {
          safe_printf("info string failed to enable syzygy: no path set\n");
          safe_fflush();
          thread_info.use_syzygy = false;
        }
      } else if (optName == "SyzygyPath") {
        if (valueStr != thread_info.syzygy_path) {
          thread_info.syzygy_path = valueStr;
          if (tb_initialized) {
            tb_free();
            tb_initialized = false;
          }
          if (thread_info.use_syzygy) {

            if (std::filesystem::exists(valueStr)) {
              safe_printf("info string syzygy path updated: %s\n",
                          valueStr.c_str());
              safe_fflush();
            } else {
              safe_printf("info string failed to set syzygy path: %s\n",
                          valueStr.c_str());
              safe_fflush();
              thread_info.use_syzygy = false;
            }
          }
        }
      } else if (optName == "SyzygyProbeDepth") {
        bool ok = false;
        int v = parse_int(valueStr, ok);
        if (!ok)
          continue;
        v = std::clamp(v, 1, 64);
        thread_info.syzygy_probe_depth = v;
      } else if (optName == "SyzygyProbeLimit") {
        bool ok = false;
        int v = parse_int(valueStr, ok);
        if (!ok)
          continue;
        v = std::clamp(v, 1, 7);
        thread_info.syzygy_probe_limit = v;
      } else if (optName == "UseOpeningBook") {
        bool b = to_bool(valueStr);
        if (b && !thread_info.book_path.empty()) {

          if (std::filesystem::exists(thread_info.book_path)) {
            thread_info.use_opening_book = true;
            safe_printf("info string opening book enabled: %s\n",
                        thread_info.book_path.c_str());
            safe_fflush();
          } else {
            safe_printf("info string failed to enable opening book: %s\n",
                        thread_info.book_path.c_str());
            safe_fflush();
            thread_info.use_opening_book = false;
          }
        } else if (!b) {
          thread_info.use_opening_book = false;
          safe_printf("info string opening book disabled\n");
          safe_fflush();
        } else {
          safe_printf(
              "info string failed to enable opening book: no path set\n");
          safe_fflush();
          thread_info.use_opening_book = false;
        }
      } else if (optName == "BookPath") {
        if (valueStr != thread_info.book_path) {
          thread_info.book_path = valueStr;
          if (thread_info.use_opening_book) {

            if (std::filesystem::exists(valueStr)) {
              safe_printf("info string opening book path updated: %s\n",
                          valueStr.c_str());
              safe_fflush();
            } else {
              safe_printf("info string failed to set opening book path: %s\n",
                          valueStr.c_str());
              safe_fflush();
              thread_info.use_opening_book = false;
            }
          }
        }
      } else if (optName == "BookDepthLimit") {
        bool ok = false;
        int v = parse_int(valueStr, ok);
        if (!ok)
          continue;
        v = std::clamp(v, 0, 50);
        thread_info.book_depth_limit = v;
      } else if (optName == "BookMinWeight") {
        bool ok = false;
        int v = parse_int(valueStr, ok);
        if (!ok)
          continue;
        v = std::clamp(v, 0, 1000);
        thread_info.book_min_weight = v;
      } else if (optName == "PonderTimeFactor") {
        bool ok = false;
        int v = parse_int(valueStr, ok);
        if (!ok)
          continue;
        v = std::clamp(v, 0, 200);
        thread_info.ponder_time_factor = v;
      } else {

        for (auto &param : params) {
          if (optName == param.name) {
            bool ok = false;
            int v = parse_int(valueStr, ok);
            if (!ok)
              break;
            v = std::clamp(v, param.min, param.max);
            param.value = v;
            if (optName == "LMRBase" || optName == "LMRRatio")
              init_LMR();
            break;
          }
        }
      }
    }

    else if (command == "stop") {
      thread_data.stop = true;

      if (s.joinable()) {
        s.join();
      }
    }

    else if (command == "ucinewgame") {
      thread_data.stop = true;

      if (s.joinable()) {
        s.join();
      }

      new_game(thread_info, TT);
      thread_info.game_ply = 0;

      thread_info.time_manager = TimeManager();
      thread_info.best_move_stable = false;
      thread_info.stability_counter = 0;
      thread_info.previous_best_move = MoveNone;

      if (thread_info.use_opening_book && !thread_info.book_path.empty()) {
        if (!thread_info.opening_book.is_loaded()) {
          bool loaded =
              thread_info.opening_book.load_book(thread_info.book_path);
          if (!loaded) {
            safe_print_cerr(
                std::string("Warning: Failed to load opening book: ") +
                thread_info.book_path);
            thread_info.use_opening_book = false;
          }
        }
      }

      if (thread_info.use_syzygy && !tb_init(thread_info.syzygy_path.c_str())) {
        safe_print_cerr(
            std::string("Warning: Syzygy TB initialization failed for path: ") +
            thread_info.syzygy_path);
        thread_info.use_syzygy = false;
      } else if (!thread_info.use_syzygy) {
        tb_free();
      }
      set_board(position, thread_info,
                "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
    }

    else if (command == "position") {

      thread_data.stop = true;
      {
        std::unique_lock<std::mutex> lk(thread_data.search_mutex);
        thread_data.search_cv.wait(lk, [&] {
          return std::none_of(
              thread_data.thread_infos.begin(), thread_data.thread_infos.end(),
              [](const ThreadInfo &ti) { return ti.searching.load(); });
        });
      }
      if (s.joinable()) {
        s.join();
      }

      std::string setup;
      input_stream >> setup;
      if (setup == "fen") {
        thread_info.game_ply = 0;
        std::string fen;

        for (int i = 0; i < 6; i++) {

          std::string substr;
          input_stream >> substr;
          fen += substr + " ";
        }

        set_board(position, thread_info, fen);
      } else {
        thread_info.game_ply = 0;
        set_board(position, thread_info,
                  "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
      }

      calculate(position);
      std::string has_moves;
      if (input_stream >> has_moves) {

        std::string moves;
        Move last_move_played = MoveNone;
        while (input_stream >> moves) {
          Move move = uci_to_internal(position, moves);
          if (move == MoveNone)
            break;
          if (thread_info.game_ply >= GameSize)
            break;

          thread_info.game_hist[thread_info.game_ply].position_key =
              position.zobrist_key;
          thread_info.game_hist[thread_info.game_ply].played_move = move;
          thread_info.game_hist[thread_info.game_ply].piece_moved =
              position.board[extract_from(move)];
          thread_info.game_hist[thread_info.game_ply].is_cap =
              is_cap(position, move);
          thread_info.game_hist[thread_info.game_ply].m_diff =
              material_eval(position);
          if (thread_info.game_ply + 1 < GameSize)
            thread_info.game_ply++;

          make_move(position, move);
          last_move_played = move;
        }

        thread_info.search_ply = 0;

        if (thread_info.pondering && last_move_played != MoveNone &&
            thread_info.ponder_move != MoveNone) {
          if (last_move_played != thread_info.ponder_move) {

            thread_info.pondering = false;
            if (!thread_data.stop) {
              thread_data.stop = true;
              safe_printf("info string ponder mismatch abort\n");
            }
          } else {

            thread_info.ponder_hit = true;
            thread_info.pondering = false;
          }
        }
      }

    }

    else if (command == "go") {
      thread_info.start_time = std::chrono::steady_clock::now();
      thread_info.infinite_search = false;

      if (thread_info.pondering == false) {
        thread_info.ponder_hit = false;
        thread_info.ponder_move = MoveNone;
      }

      if (thread_info.use_syzygy && !tb_initialized) {
        if (tb_init(thread_info.syzygy_path.c_str())) {
          tb_initialized = true;
          safe_printf("info string tablebase initialized: %s\n",
                      thread_info.syzygy_path.c_str());
        } else {
          safe_print_cerr(
              std::string(
                  "Warning: Syzygy TB initialization failed for path: ") +
              thread_info.syzygy_path);
          thread_info.use_syzygy = false;
        }
      } else if (!thread_info.use_syzygy && tb_initialized) {
        tb_free();
        tb_initialized = false;
      }
      {
        std::unique_lock<std::mutex> lk(thread_data.search_mutex);
        thread_data.search_cv.wait(lk, [&] {
          return std::none_of(
              thread_data.thread_infos.begin(), thread_data.thread_infos.end(),
              [](const ThreadInfo &ti) { return ti.searching.load(); });
        });
      }
      if (s.joinable()) {
        s.join();
      }
      thread_info.max_nodes_searched = UINT64_MAX / 2;
      if (thread_info.max_iter_depth != -1)
        thread_info.max_iter_depth = MaxSearchDepth;

      int color = position.color, time = INT32_MAX, increment = 0;
      std::string token;
      int movestogo = 0;
      int mate_in = 0;
      std::vector<Move> searchmoves;

      while (input_stream >> token) {
        if (token == "ponder") {
          thread_info.pondering = true;
          continue;
        }

        if (token == "infinite") {
          thread_info.max_iter_depth = MaxSearchDepth;
          thread_info.max_time = UINT64_MAX;
          thread_info.opt_time = UINT64_MAX;
          thread_info.infinite_search = true;
        } else if (token == "wtime" && color == Colors::White) {
          input_stream >> time;
        } else if (token == "btime" && color == Colors::Black) {
          input_stream >> time;
        } else if (token == "winc" && color == Colors::White) {
          input_stream >> increment;
        } else if (token == "binc" && color == Colors::Black) {
          input_stream >> increment;
        } else if (token == "movestogo") {
          input_stream >> movestogo;
        } else if (token == "mate") {
          input_stream >> mate_in;
          thread_info.max_iter_depth =
              std::min(mate_in * 2 + 1, MaxSearchDepth);
        } else if (token == "searchmoves") {
          std::string move_str;
          while (input_stream >> move_str && move_str.length() >= 4) {
            Move move = uci_to_internal(position, move_str);
            if (move != MoveNone) {
              searchmoves.push_back(move);
            }
          }

          if (!move_str.empty() && move_str.length() < 4) {
            input_stream.str(move_str + " " +
                             input_stream.str().substr(input_stream.tellg()));
            input_stream.seekg(0);
          }
        } else if (token == "nodes") {
          uint64_t nodes;
          input_stream >> nodes;
          thread_info.max_nodes_searched = nodes;
        } else if (token == "depth") {
          int depth;
          input_stream >> depth;
          thread_info.max_iter_depth = std::min(depth, MaxSearchDepth);
        } else if (token == "movetime") {
          int t;
          input_stream >> t;
          thread_info.max_time = static_cast<uint64_t>(t);
          thread_info.opt_time = static_cast<uint64_t>(t);
          thread_info.time_manager.allocated_time = static_cast<uint64_t>(t);
          thread_info.time_manager.max_time = static_cast<uint64_t>(t);
          thread_info.time_manager.panic_time = static_cast<uint64_t>(t);
          thread_info.time_manager.soft_limit = static_cast<uint64_t>(t);
          thread_info.time_manager.hard_limit = static_cast<uint64_t>(t);
          time = INT32_MAX;
          goto run;
        }
      }

      if (movestogo > 0 && time != INT32_MAX) {
        int overhead = std::min(50, time / 10);
        time = std::max(2, time - overhead);
        thread_info.max_time =
            static_cast<uint64_t>(time) / std::max(movestogo, 1);
        thread_info.opt_time = thread_info.max_time * 8 / 10;
      } else {

        int overhead = std::min(50, time / 10);
        time = std::max(2, time - overhead);
        thread_info.max_time = static_cast<uint64_t>(time) * 4 / 5;
        thread_info.opt_time = (static_cast<uint64_t>(time) / 15 +
                                static_cast<uint64_t>(increment) * 9 / 10) *
                               7 / 10;
      }

    run:

      if (!thread_info.infinite_search && time != INT32_MAX) {
        int game_move = (thread_info.game_ply / 2) + 1;
        thread_info.time_manager.initialize(static_cast<uint64_t>(time),
                                            static_cast<uint64_t>(increment),
                                            movestogo, game_move);
        thread_info.max_time = thread_info.time_manager.hard_limit;
        thread_info.opt_time = thread_info.time_manager.soft_limit;
      }

      if (thread_info.max_move_time > 0) {
        thread_info.max_time = thread_info.max_move_time;
        thread_info.opt_time =
            thread_info.max_move_time > thread_info.move_overhead
                ? thread_info.max_move_time - thread_info.move_overhead
                : 0;
      }

      if (!thread_info.pondering && thread_info.use_opening_book &&
          thread_info.opening_book.is_loaded() &&
          (thread_info.book_depth_limit == 0 ||
           thread_info.game_ply <= thread_info.book_depth_limit)) {

        uint64_t book_key = thread_info.opening_book.polyglot_key(position);
        Move book_move = thread_info.opening_book.probe_book(
            book_key, thread_info.book_min_weight);

        if (book_move != MoveNone && thread_info.variety > 0) {

          bool seen = false;
          for (auto k : thread_info.recent_book_keys)
            if (k == book_key) {
              seen = true;
              break;
            }
          if (seen) {

            int v = (int)thread_info.variety;
            int skip_chance = (v * v) / 225;
            if (skip_chance > 100)
              skip_chance = 100;
            if ((Random::dist(Random::rd) % 100) < skip_chance) {
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

          std::array<Move, ListSize> legal_moves;
          int num_legal = legal_movegen(position, legal_moves.data());
          bool is_legal = false;
          for (int i = 0; i < num_legal; i++) {
            if (legal_moves[i] == book_move) {
              is_legal = true;
              break;
            }
          }
          if (is_legal) {
            std::string bm = internal_to_uci(position, book_move);
            safe_printf("bestmove %s\n", bm.c_str());
            continue;
          }
        }

        if (thread_info.game_ply <= 2) {
          safe_printf("info string no book move found for this position\n");
        }
      }

      if (thread_info.max_depth > 0) {
        thread_info.max_iter_depth =
            std::min(thread_info.max_iter_depth,
                     static_cast<int>(thread_info.max_depth));
      }

      if (thread_info.is_human && thread_info.human_depth_limit > 0) {
        thread_info.max_iter_depth =
            std::min(thread_info.max_iter_depth, thread_info.human_depth_limit);
      }

      if (thread_info.max_nodes > 0) {
        thread_info.max_nodes_searched =
            std::min(thread_info.max_nodes_searched, thread_info.max_nodes);
      }

      // Run search
      run_thread(position, thread_info, s);
    }

    else if (command == "ponderhit") {
      if (thread_info.pondering) {

        thread_info.ponder_hit = true;
        thread_info.pondering = false;

        auto ponder_elapsed = time_elapsed(thread_info.ponder_start_time);
        if (ponder_elapsed > 0 && !thread_info.infinite_search) {
          uint64_t bonus =
              ponder_elapsed * thread_info.ponder_time_factor / 100;
          thread_info.opt_time = std::min<uint64_t>(
              thread_info.opt_time + bonus, thread_info.max_time);
        }
        safe_printf("info string ponderhit reuse\n");
      }
    }

    else if (command == "bench") {
      bench(position, thread_info);
    }

    else if (command == "perft") {
      int perft_depth;
      if (input_stream >> perft_depth) {
        auto perft_start = std::chrono::steady_clock::now();
        uint64_t nodes = perft(perft_depth, position, true, thread_info);
        uint64_t elapsed_ms = time_elapsed(perft_start);
        uint64_t nps = (elapsed_ms > 0) ? (nodes * 1000 / elapsed_ms) : 0;
        safe_printf("%" PRIu64 " nodes %" PRIu64 " nps\n", nodes, nps);
      }
    }

    else if (command == "eval") {
      int eval_score = eval(position, thread_info);
      safe_printf("info string evaluation: %d cp\n",
                  eval_score * 100 / NormalizationFactor);
    }

    else if (command == "flip") {
      position.color ^= 1;
      position.zobrist_key ^= zobrist_keys[side_index];
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
          for (auto &entry : TT[i].entries) {
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

  if (std::cin.eof() || std::cin.fail()) {
    thread_data.terminate = true;

    if (s.joinable()) {
      s.join();
    }

    if (thread_data.num_threads > 0 && !thread_data.threads.empty()) {
      try {
        reset_barrier.arrive_and_wait();
        idle_barrier.arrive_and_wait();
      } catch (...) {
      }

      for (size_t i = 0; i < thread_data.threads.size(); i++) {
        try {
          if (thread_data.threads[i].joinable()) {
            thread_data.threads[i].join();
          }
        } catch (...) {
        }
      }
    }
  }
}

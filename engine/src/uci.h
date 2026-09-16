#pragma once
#include <algorithm>
#include <array>
#include <cctype>
#include <cinttypes>
#include <cstdint>
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

inline void compute_human_params(ThreadInfo &thread_info) noexcept {
  const int elo = thread_info.human_elo;
  const int delta = std::clamp(3401 - elo, 0, 3000);
  thread_info.human_value_margin = std::min(15 + delta / 30, 120);
  thread_info.human_noise_sigma = std::min(delta / 25, 120);
  int depth_cap = 0;
  if (elo < 3300) {
    if (elo <= 1300)
      depth_cap = 12;
    else if (elo <= 1500)
      depth_cap = 14;
    else if (elo <= 1800)
      depth_cap = 16;
    else if (elo <= 2000)
      depth_cap = 14;
    else
      depth_cap = 8 + (elo - 1200) * 8 / 2100;
  }
  thread_info.human_depth_limit = depth_cap;
}

inline void run_thread(BoardState &position, ThreadInfo &thread_info, std::thread &s) {
  thread_data.stop = false;
  s = std::thread(search_position, std::ref(position), std::ref(thread_info),
                  std::ref(TT));
}

inline uint64_t perft(int depth, BoardState &position, bool first,
                      ThreadInfo &thread_info) {
  if (depth == 0) return 1;
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
  init_picker(picker, position, -107, checkers,
              &(thread_info.game_hist[thread_info.game_ply]));

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

inline void bench(BoardState &position, ThreadInfo &thread_info, int depth = 12) {
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

  thread_info.max_time = thread_info.opt_time = UINT64_MAX / 2;
  thread_info.max_iter_depth = std::clamp(depth, 1, MaxRootDepth);
  thread_info.time_manager.hard_limit = thread_info.max_time;
  thread_info.time_manager.soft_limit = thread_info.opt_time;
  thread_info.max_nodes_searched = thread_info.opt_nodes_searched = UINT64_MAX / 2;
  thread_info.pondering = false;
  thread_data.pondering = false;
  uint64_t total_nodes = 0;

  const auto start = std::chrono::steady_clock::now();

  for (const std::string &fen : fens) {
    new_game(thread_info, TT);
    set_board(position, thread_info, fen);
    thread_info.start_time = std::chrono::steady_clock::now();
    thread_info.infinite_search = false;
    thread_info.root_moves_limited = false;
    thread_info.root_moves.clear();
    thread_data.stop = false;
    search_position(position, thread_info, TT);
    total_nodes += thread_info.nodes.load();
  }

  safe_printf("Bench: %" PRIu64 " nodes %" PRIi64 " nps\n", total_nodes,
              static_cast<int64_t>(total_nodes * 1000 / safe_elapsed(start)));
}

inline void uci(ThreadInfo &thread_info, BoardState &position,
         std::istream &in_stream = std::cin, bool interactive = true) {
  setvbuf(stdin, NULL, _IONBF, 0);
  setvbuf(stdout, NULL, _IONBF, 0);

  if (interactive) {
    safe_printf("Stallion, written by LegendOfCompiling\n\n\n");
  }

  new_game(thread_info, TT);
  set_board(position, thread_info,
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
  const bool base_loaded = load_nnue_base(resolve_file_path("nets/base.nnue"));
  const bool aggressive_loaded = load_nnue_aggressive(resolve_file_path("nets/aggressive.nnue"));
  if (!base_loaded && !aggressive_loaded)
    safe_printf("info string NNUE files unavailable; using HCE\n");

  std::string input;

  std::thread s;

  auto safe_join = [](std::thread &t) { if (t.joinable()) t.join(); };
  auto stop_search = [&] {
    { std::lock_guard lock(thread_data.control_mutex); thread_data.stop = true; }
    thread_data.control_cv.notify_all();
    safe_join(s);
  };
  auto init_tablebases = [&] {
    if (tb_initialized) tb_free();
    tb_initialized = false;
    if (thread_info.use_syzygy && !thread_info.syzygy_path.empty()) {
      tb_initialized = tb_init(thread_info.syzygy_path.c_str()) && TB_LARGEST > 0;
      safe_printf("info string Syzygy %s (largest %u)\n", tb_initialized ? "loaded" : "unavailable", TB_LARGEST);
    }
  };
  auto init_book = [&] {
    thread_info.opening_book.clear_book();
    if (thread_info.use_opening_book && !thread_info.book_path.empty()) {
      const bool loaded = thread_info.opening_book.load_book(thread_info.book_path);
      safe_printf("info string Opening book %s\n", loaded ? "loaded" : "unavailable");
    }
  };

  while (getline(in_stream, input)) {

    if (input.empty()) {
      continue;
    }

    std::istringstream input_stream(input);

    std::string command;

    input_stream >> std::skipws >> command;

    if (command == "setoption" || command == "ucinewgame" || command == "position" ||
        command == "go" || command == "bench" || command == "perft" || command == "eval" ||
        command == "flip" || command == "hashfull" || command == "d") stop_search();

    if (command == "d") {
      print_board(position);
      safe_printf("Fen: %s\n", export_fen(position, thread_info).c_str());
      continue;
    }

    if (command == "quit") break;

    else if (command == "uci") {
      safe_printf(
          "id name Stallion EAS NNUE\n"
          "id author LegendOfCompiling\n"

          "option name Use NNUE type check default true\n"
          "option name EvalFile type string default nets/base.nnue\n"
          "option name EvalFileAggressive type string default nets/aggressive.nnue\n"
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
          "option name EndgameAggressiveness type spin default 100 min 50 max "
          "150\n"
          "option name SacrificeLookAhead type spin default 1 min 0 max 1\n"
          "option name SacrificeLookAheadTimeMultiplier type spin default 200 "
          "min 50 max 200\n"
          "option name SacrificeLookAheadAggressiveness type spin default 150 "
          "min 50 max 150\n"
          "option name MaxMoveTime type spin default 0 min 0 max 10000\n"
          "option name MoveOverhead type spin default 30 min 0 max 1000\n"
          "option name Ponder type check default true\n"
          "option name UseSyzygy type check default false\n"
          "option name SyzygyPath type string default <empty>\n"
          "option name MaxNodes type spin default 0 min 0 max 500000\n"

          "option name UseOpeningBook type check default false\n"
          "option name BookPath type string default <empty>\n"
          "option name BookDepthLimit type spin default 0 min 0 max 50\n"
          "option name SyzygyProbeDepth type spin default 6 min 1 max 64\n"
          "option name SyzygyProbeLimit type spin default 6 min 1 max 7\n"
          "option name Syzygy50MoveRule type check default true\n"
          "option name BookMinWeight type spin default 0 min 0 max 1000\n"
          "option name PonderTimeFactor type spin default 200 min 0 max 200\n"

          "option name Contempt type spin default -15 min -100 max 100\n"
          "option name TempoBonus type spin default 16 min 0 max 50\n"

          "option name TropismQueenWeight type spin default 6 min 0 max 15\n"
          "option name TropismRookWeight type spin default 4 min 0 max 15\n"
          "option name TropismKnightWeight type spin default 4 min 0 max 15\n"
          "option name TropismBishopWeight type spin default 3 min 0 max 15\n"

          "option name ThreatPawnAttack type spin default 28 min 0 max 80\n"
          "option name ThreatMinorOnHeavy type spin default 40 min 0 max 100\n"
          "option name ThreatRookOnQueen type spin default 35 min 0 max 100\n"
          "option name ThreatRookOnMinor type spin default 15 min 0 max 60\n"
          "option name ThreatHanging type spin default 18 min 0 max 60\n"

          "option name KSPawnShield type spin default 20 min 0 max 60\n"
          "option name KSPawnClose type spin default 15 min 0 max 40\n"
          "option name KSPawnMed type spin default 10 min 0 max 30\n"
          "option name KSNoPawn type spin default -30 min -80 max 0\n"
          "option name KSOpenFile type spin default -25 min -60 max 0\n"
          "option name KSSafeSqLow type spin default -60 min -120 max 0\n"
          "option name KSSafeSqMed type spin default -20 min -60 max 0\n"
          "option name KSCastleBonus type spin default 10 min 0 max 40\n"
          "option name KSCastledFlank type spin default 30 min 0 max 60\n"
          "option name KSCentralKingMajor type spin default -50 min -120 max "
          "0\n"
          "option name KSCentralKingMinor type spin default -25 min -60 max 0\n"
          "option name KSAdvancedKing type spin default -40 min -100 max 0\n"
          "option name KSMovedKingCastle type spin default -60 min -120 max 0\n"
          "option name KSUncastledKing type spin default -40 min -100 max 0\n"

          "option name KZDangerMultiplier type spin default 5 min 1 max 15\n"
          "option name KZMultiAttackerBonus type spin default 2 min 0 max 10\n"
          "option name KZSingleAttackerThreshold type spin default 5 min 1 max "
          "15\n"
          "option name KZSingleAttackerPenalty type spin default 3 min 0 max "
          "10\n"
          "option name KZNoQueenBonus type spin default 30 min 0 max 80\n"

          "option name EGCenterDist type spin default 10 min 0 max 30\n"
          "option name EGKingDist type spin default 5 min 0 max 20\n"

          "option name EGMaterialThreshold type spin default 2000 min 500 max "
          "5000\n"
          "option name EGMaterialAdvantage type spin default 200 min 50 max "
          "500\n"

          "option name BishopPairBonus type spin default 50 min 0 max 100\n"
          "option name RookOpenFile type spin default 20 min 0 max 50\n"
          "option name RookSemiOpenFile type spin default 10 min 0 max 30\n"
          "option name PassedPawnBase type spin default 20 min 0 max 60\n"
          "option name PassedPawnRankMul type spin default 4 min 1 max 10\n"
          "option name PassedPawnBlocked type spin default -12 min -30 max 0\n"
          "option name PassedPawnKingProximity type spin default 25 min 0 max "
          "60\n"
          "option name PassedPawnKingProximityRank type spin default 5 min 3 "
          "max 7\n"
          "option name IsolatedPawnPenalty type spin default -15 min -40 max "
          "0\n"
          "option name DoubledPawnPenalty type spin default -10 min -30 max 0\n"
          "option name OutpostBonus type spin default 35 min 0 max 80\n"

          "option name CenterKnight type spin default 15 min 0 max 40\n"
          "option name CenterBishop type spin default 12 min 0 max 40\n"
          "option name CenterPawn type spin default 10 min 0 max 40\n"

          "option name MobilityKnightBase type spin default 4 min 0 max 8\n"
          "option name MobilityBishopBase type spin default 6 min 0 max 12\n"
          "option name MobilityBishopMul type spin default 3 min 1 max 8\n"
          "option name MobilityBishopDiv type spin default 4 min 1 max 8\n"
          "option name MobilityRookBase type spin default 7 min 0 max 14\n"
          "option name MobilityRookMul type spin default 2 min 1 max 8\n"
          "option name MobilityRookDiv type spin default 3 min 1 max 8\n"
          "option name MobilityQueenBase type spin default 14 min 0 max 28\n"
          "option name MobilityQueenDiv type spin default 3 min 1 max 8\n"
          "option name MobilityEarlyQueenBonus type spin default 8 min 0 max "
          "20\n"

          "option name UndevelopedPenalty type spin default 5 min 0 max 20\n"

          "option name EvalMultBase type spin default 800 min 400 max 1200\n"
          "option name EvalMultMatDiv type spin default 24 min 8 max 64\n"
          "option name EvalMultNorm type spin default 1024 min 512 max 2048\n"
          "option name EvalWinningMul type spin default 120 min 100 max 200\n"
          "option name EvalWinningMatThreshold type spin default 4000 min 2000 "
          "max 8000\n"
          "option name EvalSlightWinMul type spin default 110 min 100 max 150\n"
          "option name EvalSlightWinMatThreshold type spin default 2500 min "
          "1000 max 5000\n"
          "option name EvalLosingMul type spin default 90 min 50 max 100\n"
          "option name EvalLosingThreshold type spin default -150 min -500 max "
          "0\n"
          "option name EvalSlightLoseMul type spin default 95 min 50 max 100\n"
          "option name EvalSlightLoseThreshold type spin default -50 min -200 "
          "max 0\n"

          "option name SacPatternBonus type spin default 55 min 0 max 150\n"
          "option name SacKingFileBonus type spin default 25 min 0 max 80\n"
          "option name SacMultiBonus type spin default 40 min 0 max 100\n"
          "option name SacMaterialThreshold type spin default 3000 min 1000 "
          "max 6000\n"

          "option name DrawContemptMaterial type spin default 60 min 0 max "
          "200\n"
          "option name RazorMargin type spin default 140 min 100 max 500\n"
          "option name HistPruneDepth type spin default 4 min 2 max 8\n"
          "option name HistPruneThreshold type spin default 6196 min 1000 max "
          "8000\n"
          "option name ProbCutMargin type spin default 191 min 100 max 500\n"
          "option name MultiCutDepth type spin default 4 min 3 max 10\n"
          "option name MultiCutMoves type spin default 6 min 2 max 8\n"
          "option name MultiCutCuts type spin default 3 min 1 max 5\n"
          "option name HistExtThreshold type spin default 7000 min 3000 max "
          "15000\n"
          "option name FPAttackModeBonus type spin default 80 min 0 max 200\n"

          "option name AttackModeEnterDepth type spin default 6 min 3 max 12\n"
          "option name AttackModeMaterial type spin default 2800 min 1500 max "
          "5000\n"
          "option name AttackModeEnterRelax type spin default 20 min 0 max "
          "100\n"
          "option name AttackModeExitRelax type spin default 20 min 0 max 100\n"
          "option name AttackModeDropExtra type spin default 30 min 0 max 100\n"
          "option name AttackModeMatExit type spin default 200 min 0 max 500\n"

          "option name PhaseConfirmHits type spin default 2 min 1 max 8\n"
          "option name SacrificeEnterCp type spin default 250 min 100 max 500\n"
          "option name SacrificeExitCp type spin default 170 min 50 max 400\n"
          "option name SacrificeDropThreshold type spin default 120 min 50 max "
          "300\n"
          "option name LatePhaseMaterial type spin default 4200 min 2000 max "
          "6000\n"
          "option name EndgameMaterial type spin default 3000 min 1000 max "
          "5000\n"
          "option name MidRecoverMaterial type spin default 4500 min 2000 max "
          "6000\n"
          "option name EndRecoverMaterial type spin default 3300 min 1500 max "
          "5500\n"
          "option name OpeningMinPly type spin default 20 min 0 max 60\n"

          "option name SpaceWeight type spin default 7 min 0 max 20\n"
          "option name DeltaMarginBase type spin default 180 min 50 max 400\n"

          "option name MaterialBasisPawn type spin default 210 min 100 max "
          "400\n"
          "option name MaterialBasisKnight type spin default 800 min 500 max "
          "1200\n"
          "option name MaterialBasisBishop type spin default 840 min 500 max "
          "1200\n"
          "option name MaterialBasisRook type spin default 1300 min 800 max "
          "2000\n"
          "option name MaterialBasisQueen type spin default 2600 min 1500 max "
          "4000\n"

          "option name PawnStorm1 type spin default 110 min 0 max 200\n"
          "option name PawnStorm2 type spin default 75 min 0 max 150\n"
          "option name PawnStorm3 type spin default 40 min 0 max 100\n"
          "option name PawnStorm4 type spin default 15 min 0 max 60\n"

          "option name KZBishopXray type spin default 1 min 0 max 5\n"
          "option name KZRookXray type spin default 2 min 0 max 8\n"

          "option name NormalizationFactor type spin default 195 min 50 max "
          "500\n"
          "option name HalfmoveScaleMax type spin default 200 min 50 max 500\n"
          "option name PromoBonusDoubleFork type spin default 250 min 0 max "
          "500\n"
          "option name PromoBonusSingleFork type spin default 100 min 0 max "
          "300\n"
          "option name VarietyBaseThreshold type spin default 150 min 50 max "
          "300\n"
          "option name VarietyMultiplier type spin default 2 min 1 max 5\n");

      safe_printf("option name MaxDepth type spin default 0 min 0 max %d\n", MaxRootDepth);
      for (const auto &param : params) {
        if (param.name == "RazorMargin" || param.name == "ProbCutMargin" ||
            param.name == "MultiCutDepth" || param.name == "MultiCutMoves" || param.name == "MultiCutCuts" ||
            param.name == "HistPruneDepth" || param.name == "HistPruneThreshold") continue;
        safe_printf("option name %s type spin default %d min %d max %d\n",
                    param.name.c_str(), param.value, param.min, param.max);
      }
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
      bool in_name = false;

      while (input_stream >> word) {
        if (word == "name") {
          in_name = true;
          continue;
        }
        if (word == "value") {
          std::getline(input_stream, valueStr);
          break;
        }
        if (in_name) {
          if (!optName.empty())
            optName += " ";
          optName += word;
        }
      }

      const auto first = valueStr.find_first_not_of(" \t\r");
      valueStr = first == std::string::npos ? "" : valueStr.substr(first, valueStr.find_last_not_of(" \t\r") - first + 1);
      if (valueStr == "<empty>" || valueStr == "\"\"") valueStr.clear();

      auto parse_int = [](const std::string &s, bool &ok) {
        try {
          size_t end = 0;
          int v = std::stoi(s, &end);
          ok = end == s.size();
          return v;
        } catch (...) {
          ok = false;
          return 0;
        }
      };
      auto parse_uint64 = [](const std::string &s, bool &ok) -> uint64_t {
        try {
          size_t end = 0;
          unsigned long long tmp = std::stoull(s, &end);
          ok = !s.empty() && s[0] != '-' && end == s.size();
          return static_cast<uint64_t>(tmp);
        } catch (...) {
          ok = false;
          return static_cast<uint64_t>(0);
        }
      };
      auto to_bool = [](std::string s) {
        std::transform(s.begin(), s.end(), s.begin(), [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
        return s == "true" || s == "1" || s == "yes" || s == "on";
      };
      auto set_spin = [&](int lo, int hi, int &out) {
        bool ok = false;
        int v = parse_int(valueStr, ok);
        if (ok) out = std::clamp(v, lo, hi);
      };
      auto set_spin_req = [&](int lo, int hi, int &out) -> bool {
        bool ok = false;
        int v = parse_int(valueStr, ok);
        if (!ok) return false;
        out = std::clamp(v, lo, hi);
        return true;
      };
      auto set_aggressiveness = [&](float &out) -> bool {
        bool ok = false;
        int v = parse_int(valueStr, ok);
        if (!ok) return false;
        out = std::clamp(v, 50, 150) / 100.0f;
        return true;
      };

      auto lowercase = [](std::string value) {
        std::transform(value.begin(), value.end(), value.begin(), [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
        return value;
      };
      optName = lowercase(optName);
      if (optName.empty()) {
        continue;
      }

      if (optName == "use nnue" || optName == "usennue" || optName == "use_nnue" ||
          optName == "uci_limitstrength" || optName == "uci_chess960" || optName == "ponder" ||
          optName == "usesyzygy" || optName == "syzygy50moverule" || optName == "useopeningbook") {
        const auto value = lowercase(valueStr);
        if (value != "true" && value != "false" && value != "1" && value != "0" &&
            value != "yes" && value != "no" && value != "on" && value != "off") {
          safe_printf("info string Invalid boolean option value\n");
          continue;
        }
      }

      std::fill(TT.begin(), TT.end(), TTBucket{});
      thread_info.PawnCorrHist.fill({});
      thread_info.NonPawnCorrHist.fill({});
      if (optName == "use nnue" || optName == "usennue" || optName == "use_nnue") {
        use_nnue = to_bool(valueStr);
      } else if (optName == "evalfile") {
        const bool loaded = load_nnue_base(resolve_file_path(valueStr));
        safe_printf("info string EvalFile %s\n", loaded ? "loaded" : "load failed; previous network retained");
      } else if (optName == "evalfileaggressive") {
        const bool loaded = load_nnue_aggressive(resolve_file_path(valueStr));
        safe_printf("info string EvalFileAggressive %s\n", loaded ? "loaded" : "load failed; previous network retained");
      } else if (optName == "hash") {
        bool ok = false;
        int mb = parse_int(valueStr, ok);
        if (!ok)
          continue;
        resize_TT(std::clamp(mb, 1, 131072));
      } else if (optName == "threads") {
        bool ok = false;
        int thr = parse_int(valueStr, ok);
        if (!ok)
          continue;
        thr = std::clamp(thr, 1, 1024);
        try {
          std::vector<ThreadInfo> workers(thr - 1);
          thread_data.threads.reserve(thr - 1);
          thread_data.thread_infos.swap(workers);
          thread_data.num_threads = thr;
        } catch (const std::bad_alloc &) {
          safe_printf("info string Thread allocation failed\n");
        }
      } else if (optName == "multipv") {
        int mv;
        if (!set_spin_req(1, 256, mv)) continue;
        thread_info.multipv = static_cast<uint16_t>(mv);
      } else if (optName == "variety") {
        int v;
        if (!set_spin_req(0, 150, v)) continue;
        thread_info.variety = static_cast<uint16_t>(v);
      } else if (optName == "uci_limitstrength") {
        bool b = to_bool(valueStr);
        thread_info.is_human = b;
        if (!b) {
          thread_info.human_value_margin = 0;
          thread_info.human_noise_sigma = 0;
          thread_info.human_depth_limit = 0;
        } else {
          compute_human_params(thread_info);
        }
      } else if (optName == "uci_chess960") {
        bool b = to_bool(valueStr);
        thread_data.is_frc = b;
      } else if (optName == "uci_elo") {
        if (!set_spin_req(500, 3401, thread_info.human_elo)) continue;
        if (thread_info.is_human) compute_human_params(thread_info);
      } else if (optName == "openingaggressiveness") {
        if (!set_aggressiveness(thread_info.opening_aggressiveness)) continue;
      } else if (optName == "middlegameaggressiveness") {
        if (!set_aggressiveness(thread_info.middlegame_aggressiveness)) continue;
      } else if (optName == "latemiddlegameaggressiveness") {
        if (!set_aggressiveness(thread_info.late_middlegame_aggressiveness)) continue;
      } else if (optName == "endgameaggressiveness") {
        if (!set_aggressiveness(thread_info.endgame_aggressiveness)) continue;
      } else if (optName == "sacrificelookahead") {
        if (!set_spin_req(0, 1, thread_info.sacrifice_lookahead)) continue;
      } else if (optName == "sacrificelookaheadtimemultiplier") {
        if (!set_spin_req(50, 200, thread_info.sacrifice_lookahead_time_multiplier)) continue;
      } else if (optName == "sacrificelookaheadaggressiveness") {
        if (!set_spin_req(50, 150, thread_info.sacrifice_lookahead_aggressiveness)) continue;
      } else if (optName == "maxmovetime") {
        int v;
        if (!set_spin_req(0, 10000, v)) continue;
        thread_info.max_move_time = static_cast<uint64_t>(v);
      } else if (optName == "moveoverhead") {
        int v;
        if (!set_spin_req(0, 1000, v)) continue;
        thread_info.move_overhead = static_cast<uint64_t>(v);
      } else if (optName == "ponder") {
        thread_info.use_ponder = to_bool(valueStr);
        if (!thread_info.use_ponder) {
          thread_info.pondering = false;
          thread_info.ponder_hit = false;
          thread_info.ponder_move = MoveNone;
        }
      } else if (optName == "maxdepth") {
        int v;
        if (!set_spin_req(0, MaxRootDepth, v)) continue;
        thread_info.max_depth = static_cast<uint16_t>(v);
      } else if (optName == "maxnodes") {
        bool ok = false;
        uint64_t v = parse_uint64(valueStr, ok);
        if (!ok) continue;
        thread_info.max_nodes = std::min(v, uint64_t{500000});
      } else if (optName == "usesyzygy") {
        thread_info.use_syzygy = to_bool(valueStr);
        init_tablebases();
      } else if (optName == "syzygypath") {
        thread_info.syzygy_path = valueStr;
        init_tablebases();
      } else if (optName == "syzygyprobedepth") {
        if (!set_spin_req(1, 64, thread_info.syzygy_probe_depth)) continue;
      } else if (optName == "syzygyprobelimit") {
        if (!set_spin_req(1, 7, thread_info.syzygy_probe_limit)) continue;
      } else if (optName == "syzygy50moverule") {
        thread_info.syzygy_50_move_rule = to_bool(valueStr);
      } else if (optName == "useopeningbook") {
        thread_info.use_opening_book = to_bool(valueStr);
        init_book();
      } else if (optName == "bookpath") {
        thread_info.book_path = valueStr;
        init_book();
      } else if (optName == "bookdepthlimit") {
        if (!set_spin_req(0, 50, thread_info.book_depth_limit)) continue;
      } else if (optName == "bookminweight") {
        if (!set_spin_req(0, 1000, thread_info.book_min_weight)) continue;
      } else if (optName == "pondertimefactor") {
        if (!set_spin_req(0, 200, thread_info.ponder_time_factor)) continue;
      } else if (optName == "contempt")
        set_spin(-100, 100, Contempt);
      else if (optName == "tempobonus")
        set_spin(0, 50, TempoBonus);
      else if (optName == "tropismqueenweight")
        set_spin(0, 15, TropismQueenWeight);
      else if (optName == "tropismrookweight")
        set_spin(0, 15, TropismRookWeight);
      else if (optName == "tropismknightweight")
        set_spin(0, 15, TropismKnightWeight);
      else if (optName == "tropismbishopweight")
        set_spin(0, 15, TropismBishopWeight);
      else if (optName == "threatpawnattack")
        set_spin(0, 80, ThreatPawnAttack);
      else if (optName == "threatminoronheavy")
        set_spin(0, 100, ThreatMinorOnHeavy);
      else if (optName == "threatrookonqueen")
        set_spin(0, 100, ThreatRookOnQueen);
      else if (optName == "threatrookonminor")
        set_spin(0, 60, ThreatRookOnMinor);
      else if (optName == "threathanging")
        set_spin(0, 60, ThreatHanging);

      else if (optName == "kspawnshield")
        set_spin(0, 60, KSPawnShield);
      else if (optName == "kspawnclose")
        set_spin(0, 40, KSPawnClose);
      else if (optName == "kspawnmed")
        set_spin(0, 30, KSPawnMed);
      else if (optName == "ksnopawn")
        set_spin(-80, 0, KSNoPawn);
      else if (optName == "ksopenfile")
        set_spin(-60, 0, KSOpenFile);
      else if (optName == "kssafesqlow")
        set_spin(-120, 0, KSSafeSqLow);
      else if (optName == "kssafesqmed")
        set_spin(-60, 0, KSSafeSqMed);
      else if (optName == "kscastlebonus")
        set_spin(0, 40, KSCastleBonus);
      else if (optName == "kscastledflank")
        set_spin(0, 60, KSCastledFlank);
      else if (optName == "kscentralkingmajor")
        set_spin(-120, 0, KSCentralKingMajor);
      else if (optName == "kscentralkingminor")
        set_spin(-60, 0, KSCentralKingMinor);
      else if (optName == "ksadvancedking")
        set_spin(-100, 0, KSAdvancedKing);
      else if (optName == "ksmovedkingcastle")
        set_spin(-120, 0, KSMovedKingCastle);
      else if (optName == "ksuncastledking")
        set_spin(-100, 0, KSUncastledKing);
      else if (optName == "kzdangermultiplier")
        set_spin(1, 15, KZDangerMultiplier);
      else if (optName == "kzmultiattackerbonus")
        set_spin(0, 10, KZMultiAttackerBonus);
      else if (optName == "kzsingleattackerthreshold")
        set_spin(1, 15, KZSingleAttackerThreshold);
      else if (optName == "kzsingleattackerpenalty")
        set_spin(0, 10, KZSingleAttackerPenalty);
      else if (optName == "kznoqueenbonus")
        set_spin(0, 80, KZNoQueenBonus);

      else if (optName == "egcenterdist")
        set_spin(0, 30, EGCenterDist);
      else if (optName == "egkingdist")
        set_spin(0, 20, EGKingDist);
      else if (optName == "egmaterialthreshold")
        set_spin(500, 5000, EGMaterialThreshold);
      else if (optName == "egmaterialadvantage")
        set_spin(50, 500, EGMaterialAdvantage);
      else if (optName == "bishoppairbonus")
        set_spin(0, 100, BishopPairBonus);
      else if (optName == "rookopenfile")
        set_spin(0, 50, RookOpenFile);
      else if (optName == "rooksemiopenfile")
        set_spin(0, 30, RookSemiOpenFile);
      else if (optName == "passedpawnbase")
        set_spin(0, 60, PassedPawnBase);
      else if (optName == "passedpawnrankmul")
        set_spin(1, 10, PassedPawnRankMul);
      else if (optName == "passedpawnblocked")
        set_spin(-30, 0, PassedPawnBlocked);
      else if (optName == "passedpawnkingproximity")
        set_spin(0, 60, PassedPawnKingProximity);
      else if (optName == "passedpawnkingproximityrank")
        set_spin(3, 7, PassedPawnKingProximityRank);
      else if (optName == "isolatedpawnpenalty")
        set_spin(-40, 0, IsolatedPawnPenalty);
      else if (optName == "doubledpawnpenalty")
        set_spin(-30, 0, DoubledPawnPenalty);
      else if (optName == "outpostbonus")
        set_spin(0, 80, OutpostBonus);
      else if (optName == "centerknight")
        set_spin(0, 40, CenterKnight);
      else if (optName == "centerbishop")
        set_spin(0, 40, CenterBishop);
      else if (optName == "centerpawn")
        set_spin(0, 40, CenterPawn);
      else if (optName == "mobilityknightbase")
        set_spin(0, 8, MobilityKnightBase);
      else if (optName == "mobilitybishopbase")
        set_spin(0, 12, MobilityBishopBase);
      else if (optName == "mobilitybishopmul")
        set_spin(1, 8, MobilityBishopMul);
      else if (optName == "mobilitybishopdiv")
        set_spin(1, 8, MobilityBishopDiv);
      else if (optName == "mobilityrookbase")
        set_spin(0, 14, MobilityRookBase);
      else if (optName == "mobilityrookmul")
        set_spin(1, 8, MobilityRookMul);
      else if (optName == "mobilityrookdiv")
        set_spin(1, 8, MobilityRookDiv);
      else if (optName == "mobilityqueenbase")
        set_spin(0, 28, MobilityQueenBase);
      else if (optName == "mobilityqueendiv")
        set_spin(1, 8, MobilityQueenDiv);
      else if (optName == "mobilityearlyqueenbonus")
        set_spin(0, 20, MobilityEarlyQueenBonus);
      else if (optName == "undevelopedpenalty")
        set_spin(0, 20, UndevelopedPenalty);

      else if (optName == "evalmultbase")
        set_spin(400, 1200, EvalMultBase);
      else if (optName == "evalmultmatdiv")
        set_spin(8, 64, EvalMultMatDiv);
      else if (optName == "evalmultnorm")
        set_spin(512, 2048, EvalMultNorm);
      else if (optName == "evalwinningmul")
        set_spin(100, 200, EvalWinningMul);
      else if (optName == "evalwinningmatthreshold")
        set_spin(2000, 8000, EvalWinningMatThreshold);
      else if (optName == "evalslightwinmul")
        set_spin(100, 150, EvalSlightWinMul);
      else if (optName == "evalslightwinmatthreshold")
        set_spin(1000, 5000, EvalSlightWinMatThreshold);
      else if (optName == "evallosingmul")
        set_spin(50, 100, EvalLosingMul);
      else if (optName == "evallosingthreshold")
        set_spin(-500, 0, EvalLosingThreshold);
      else if (optName == "evalslightlosemul")
        set_spin(50, 100, EvalSlightLoseMul);
      else if (optName == "evalslightlosethreshold")
        set_spin(-200, 0, EvalSlightLoseThreshold);
      else if (optName == "sacpatternbonus")
        set_spin(0, 150, SacPatternBonus);
      else if (optName == "sackingfilebonus")
        set_spin(0, 80, SacKingFileBonus);
      else if (optName == "sacmultibonus")
        set_spin(0, 100, SacMultiBonus);
      else if (optName == "sacmaterialthreshold")
        set_spin(1000, 6000, SacMaterialThreshold);
      else if (optName == "drawcontemptmaterial")
        set_spin(0, 200, DrawContemptMaterial);
      else if (optName == "histextthreshold")
        set_spin(3000, 15000, HistExtThreshold);
      else if (optName == "fpattackmodebonus")
        set_spin(0, 200, FPAttackModeBonus);
      else if (optName == "attackmodeenterdepth")
        set_spin(3, 12, AttackModeEnterDepth);
      else if (optName == "attackmodematerial")
        set_spin(1500, 5000, AttackModeMaterial);
      else if (optName == "attackmodeenterrelax")
        set_spin(0, 100, AttackModeEnterRelax);
      else if (optName == "attackmodeexitrelax")
        set_spin(0, 100, AttackModeExitRelax);
      else if (optName == "attackmodedropextra")
        set_spin(0, 100, AttackModeDropExtra);
      else if (optName == "attackmodematexit")
        set_spin(0, 500, AttackModeMatExit);
      else if (optName == "phaseconfirmhits")
        set_spin(1, 8, PhaseConfirmHits);
      else if (optName == "sacrificeentercp")
        set_spin(100, 500, SacrificeEnterCp);
      else if (optName == "sacrificeexitcp")
        set_spin(50, 400, SacrificeExitCp);
      else if (optName == "sacrificedropthreshold")
        set_spin(50, 300, SacrificeDropThreshold);
      else if (optName == "latephasematerial")
        set_spin(2000, 6000, LatePhaseMaterial);
      else if (optName == "endgamematerial")
        set_spin(1000, 5000, EndgameMaterial);
      else if (optName == "midrecovermaterial")
        set_spin(2000, 6000, MidRecoverMaterial);
      else if (optName == "endrecovermaterial")
        set_spin(1500, 5500, EndRecoverMaterial);
      else if (optName == "openingminply")
        set_spin(0, 60, OpeningMinPly);
      else if (optName == "spaceweight")
        set_spin(0, 20, SpaceWeight);
      else if (optName == "deltamarginbase")
        set_spin(50, 400, DELTA_MARGIN_BASE);
      else if (optName == "materialbasispawn")
        set_spin(100, 400, MaterialBasis[1]);
      else if (optName == "materialbasisknight")
        set_spin(500, 1200, MaterialBasis[2]);
      else if (optName == "materialbasisbishop")
        set_spin(500, 1200, MaterialBasis[3]);
      else if (optName == "materialbasisrook")
        set_spin(800, 2000, MaterialBasis[4]);
      else if (optName == "materialbasisqueen")
        set_spin(1500, 4000, MaterialBasis[5]);
      else if (optName == "pawnstorm1")
        set_spin(0, 200, PawnStormConfig[0]);
      else if (optName == "pawnstorm2")
        set_spin(0, 150, PawnStormConfig[1]);
      else if (optName == "pawnstorm3")
        set_spin(0, 100, PawnStormConfig[2]);
      else if (optName == "pawnstorm4")
        set_spin(0, 60, PawnStormConfig[3]);
      else if (optName == "kzbishopxray")
        set_spin(0, 5, KZBishopXray);
      else if (optName == "kzrookxray")
        set_spin(0, 8, KZRookXray);
      else if (optName == "normalizationfactor")
        set_spin(50, 500, NormalizationFactor);
      else if (optName == "halfmovescalemax")
        set_spin(50, 500, HALFMOVE_SCALE_MAX);
      else if (optName == "promobonusdoublefork")
        set_spin(0, 500, PROMO_BONUS_DOUBLE_FORK);
      else if (optName == "promobonussinglefork")
        set_spin(0, 300, PROMO_BONUS_SINGLE_FORK);
      else if (optName == "varietybasethreshold")
        set_spin(50, 300, VARIETY_BASE_THRESHOLD);
      else if (optName == "varietymultiplier")
        set_spin(1, 5, VARIETY_MULTIPLIER);
      else {
        for (auto &param : params) {
          if (optName == lowercase(param.name)) {
            bool ok = false;
            int v = parse_int(valueStr, ok);
            if (!ok)
              break;
            v = std::clamp(v, param.min, param.max);
            param.value = v;
            if (optName == "lmrbase" || optName == "lmrratio")
              init_LMR();
            break;
          }
        }
      }
    }

    else if (command == "stop") {
      stop_search();
    }

    else if (command == "ucinewgame") {
      new_game(thread_info, TT);
      thread_info.time_manager = TimeManager();
      thread_info.best_move_stable = false;
      thread_info.stability_counter = 0;
      thread_info.previous_best_move = MoveNone;
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
          safe_printf("info string Invalid FEN; position unchanged\n");
          continue;
        }
      } else if (setup == "startpos") {
        std::string token;
        if (input_stream >> token) {
          if (token != "moves") {
            safe_printf("info string Invalid position command; position unchanged\n");
            continue;
          }
          has_moves_token = true;
        }
        set_board(position, thread_info,
                  "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
      } else {
        safe_printf("info string Invalid position command\n");
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
            std::move(thread_info.game_hist.begin() + MaxGameLen / 2, thread_info.game_hist.end(), thread_info.game_hist.begin());
            thread_info.game_ply -= MaxGameLen / 2;
          }

          thread_info.game_hist[thread_info.game_ply].position_key =
              position.zobrist_key;
          thread_info.game_hist[thread_info.game_ply].played_move = move;
          thread_info.game_hist[thread_info.game_ply].piece_moved =
              position.board[extract_from(move)];
          thread_info.game_hist[thread_info.game_ply].is_cap =
              is_cap(position, move);
          thread_info.game_hist[thread_info.game_ply].m_diff =
              material_eval(position);
          if (thread_info.game_ply + 1 < MaxGameLen)
            thread_info.game_ply++;

          make_move(position, move);
        }

        if (invalid_move) {
          position = previous_position;
          thread_info.game_hist = previous_game_hist;
          thread_info.game_ply = previous_game_ply;
          thread_info.search_ply = previous_search_ply;
          safe_printf("info string Invalid move; position unchanged\n");
          continue;
        }

        thread_info.search_ply = 0;

      }

    }

    else if (command == "go") {
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
      thread_info.opt_nodes_searched = UINT64_MAX / 2;
      thread_info.max_iter_depth = MaxRootDepth;
      thread_info.mate_search = 0;

      int color = position.color, time = -1, increment = 0;
      int move_time = -1;
      std::string token;
      int movestogo = 0;
      std::vector<Action> searchmoves;
      bool searchmoves_specified = false;
      auto parse_int_token = [](const std::string &s, int &out) -> bool {
        try {
          size_t consumed = 0;
          int v = std::stoi(s, &consumed);
          if (s.empty() || s[0] == '-' || consumed != s.size())
            return false;
          out = v;
          return true;
        } catch (...) {
          return false;
        }
      };
      auto parse_u64_token = [](const std::string &s, uint64_t &out) -> bool {
        try {
          size_t consumed = 0;
          uint64_t v = std::stoull(s, &consumed);
          if (s.empty() || s[0] == '-' || consumed != s.size())
            return false;
          out = v;
          return true;
        } catch (...) {
          return false;
        }
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
          char promo = static_cast<char>(
              std::tolower(static_cast<unsigned char>(s[4])));
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
          if (thread_info.pondering) {
            thread_info.ponder_start_time = std::chrono::steady_clock::now();
          }
          continue;
        }

        if (token == "infinite") {
          thread_info.max_iter_depth = MaxRootDepth;
          thread_info.max_time = UINT64_MAX;
          thread_info.opt_time = UINT64_MAX;
          thread_info.infinite_search = true;
        } else if (token == "wtime" || token == "btime" || token == "winc" ||
                   token == "binc" || token == "movestogo" ||
                   token == "mate" || token == "depth" ||
                   token == "movetime") {
          if (i + 1 >= go_tokens.size())
            continue;

          int value = 0;
          if (!parse_int_token(go_tokens[i + 1], value)) {
            ++i;
            continue;
          }

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
          uint64_t nodes = 0;
          if (parse_u64_token(go_tokens[i + 1], nodes)) {

            nodes = std::max<uint64_t>(1, nodes);
            thread_info.max_nodes_searched = nodes;
            thread_info.opt_nodes_searched = std::max<uint64_t>(1, nodes / 10 * 8);
          }
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

      constexpr uint64_t unlimited = UINT64_MAX / 2;
      thread_info.max_time = thread_info.opt_time = unlimited;
      if (!thread_info.infinite_search) {
        if (move_time >= 0) {
          const auto overhead = std::min<uint64_t>(thread_info.move_overhead, static_cast<uint64_t>(move_time / 10));
          thread_info.max_time = thread_info.opt_time = std::max<uint64_t>(1, static_cast<uint64_t>(move_time) - overhead);
        } else if (time >= 0) {
          const auto overhead = std::min<uint64_t>(thread_info.move_overhead, static_cast<uint64_t>(time / 10));
          const uint64_t usable = std::max<uint64_t>(1, static_cast<uint64_t>(time) - overhead);
          thread_info.time_manager.initialize(usable, increment, movestogo, position.fullmove);
          thread_info.max_time = thread_info.time_manager.hard_limit;
          thread_info.opt_time = thread_info.time_manager.soft_limit;
        }
        if (thread_info.max_move_time > 0) {
          thread_info.max_time = std::min(thread_info.max_time, thread_info.max_move_time);
          thread_info.opt_time = std::min(thread_info.opt_time, thread_info.max_time);
        }
      }
      thread_info.time_manager.allocated_time = thread_info.opt_time;
      thread_info.time_manager.soft_limit = thread_info.opt_time;
      thread_info.time_manager.max_time = thread_info.max_time;
      thread_info.time_manager.hard_limit = thread_info.max_time;
      thread_info.time_manager.panic_time = thread_info.max_time;
      thread_info.time_manager.use_panic_mode = false;
      thread_data.pondering = thread_info.pondering.load();

      if (!thread_info.infinite_search && !searchmoves_specified && !thread_info.pondering &&
          thread_info.use_opening_book &&
          thread_info.opening_book.is_loaded() &&
          (thread_info.book_depth_limit == 0 ||
           uint64_t(position.fullmove - 1) * 2 + position.color < static_cast<uint64_t>(thread_info.book_depth_limit))) {

        const uint64_t book_key = thread_info.opening_book.polyglot_key(position);
        Action book_move = thread_info.opening_book.probe_book(
            position, thread_info.book_min_weight);

        if (book_move != MoveNone && thread_info.variety > 0) {
          const bool seen = std::find(thread_info.recent_book_keys.begin(),
                                      thread_info.recent_book_keys.end(),
                                      book_key) != thread_info.recent_book_keys.end();
          if (seen) {
            const int v = static_cast<int>(thread_info.variety);
            int skip_chance = (v * v) / 225;
            if (skip_chance > 100)
              skip_chance = 100;
            static thread_local std::mt19937 rng(Random::rd());
            if ((std::uniform_int_distribution<int>(0, 99)(rng)) <
                skip_chance) {
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
          const bool is_legal = std::find(legal_moves.begin(),
                                          legal_moves.begin() + num_legal,
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

      if (thread_info.is_human && thread_info.human_depth_limit > 0) {
        thread_info.max_iter_depth =
            std::min(thread_info.max_iter_depth, thread_info.human_depth_limit);
      }

      if (thread_info.max_nodes > 0) {
        thread_info.max_nodes_searched =
            std::min(thread_info.max_nodes_searched, thread_info.max_nodes);
        thread_info.opt_nodes_searched = std::min(
            thread_info.opt_nodes_searched,
            std::max<uint64_t>(1, thread_info.max_nodes_searched * 8 / 10));
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
        thread_data.ponder_hit_time = std::chrono::duration_cast<std::chrono::milliseconds>(
            std::chrono::steady_clock::now().time_since_epoch()).count();
        { std::lock_guard lock(thread_data.control_mutex); thread_data.pondering = false; }
        thread_data.control_cv.notify_all();
      }
    }

    else if (command == "bench") {
      int depth = 12;
      input_stream >> depth;
      bench(position, thread_info, depth);
    }

    else if (command == "perft") {
      int perft_depth = 0;
      if ((input_stream >> perft_depth) && perft_depth >= 0 && perft_depth <= 10) {
        const auto perft_start = std::chrono::steady_clock::now();
        const uint64_t nodes = perft(perft_depth, position, true, thread_info);
        const uint64_t elapsed_ms = time_elapsed(perft_start);
        const uint64_t nps = (elapsed_ms > 0) ? (nodes * 1000 / elapsed_ms) : 0;
        safe_printf("%" PRIu64 " nodes %" PRIu64 " nps\n", nodes, nps);
      }
    }

    else if (command == "eval") {
      const uint8_t saved_phase = thread_info.phase;
      thread_info.phase = root_phase(position);
      if (use_nnue && nnue_loaded) {
        select_active_nnue(thread_info.phase);
        thread_info.nnue_state.reset_nnue(position);
        safe_printf("info string NNUE raw: %d (net: %s, phase: %d)\n",
                    thread_info.nnue_state.evaluate(position.color),
                    (g_nnue == g_nnue_aggressive.get()) ? "aggressive/sacrifice" : "base",
                    thread_info.phase);
      }
      int eval_score = eval(position, thread_info);
      safe_printf("info string evaluation: %d cp (%s)\n",
                  eval_score * 100 / NormalizationFactor,
                  (use_nnue && nnue_loaded) ? "NNUE" : "HCE");
      thread_info.phase = saved_phase;
    }

    else if (command == "flip") {
      position.color ^= 1;
      position.ep_square = SquareNone;
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

  if (interactive) stop_search();
  else safe_join(s);
  thread_data.stop = true;
  if (tb_initialized) { tb_free(); tb_initialized = false; }
}

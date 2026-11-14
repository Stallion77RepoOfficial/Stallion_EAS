#pragma once
#include "search.h"
#include "sbin_format.h"
#include <fstream>
#include <bit>
#include <charconv>
#include <cmath>
#include <climits>
#include <filesystem>
#include <iostream>
#include <unordered_map>
#include <unordered_set>
#include <stdexcept>

static_assert(std::endian::native == std::endian::little, "SBIN requires little endian storage");

// Native generator shares the exact board, NNUE and search used in UCI games.
inline PackedPosition selfplay_pack(const BoardState &p, int cp) {
  PackedPosition r{};
  r.occupied = p.colors_bb[0] | p.colors_bb[1];
  uint64_t occ = r.occupied;
  int index = 0;
  while (occ) {
    const int sq = get_lsb(occ); occ &= occ - 1;
    r.pieces[index / 2] |= uint8_t(p.board[sq] << ((index % 2) * 4));
    ++index;
  }
  r.flags = p.color;
  for (int color = 0; color < 2; ++color)
    for (int side = 0; side < 2; ++side)
      if (p.castling_squares[color][side] != SquareNone)
        r.flags |= int64_t(1) << (1 + color * 2 + (side == Sides::Queenside));
  r.ep_file = p.ep_square == SquareNone ? 0 : get_file(p.ep_square) + 1;
  r.halfmove = p.halfmoves; r.fullmove = p.fullmove; r.eval = cp;
  return r;
}

inline int native_selfplay(const std::vector<std::string_view> &args, ThreadInfo &ti) {
  try {
    uint64_t games = 100, nodes = 1000, seed = 1, stride = 1, max_moves = 160;
    int depth = MaxRootDepth, opening_min = 4, opening_max = 8;
    double random_chance = 0;
    std::string output, book;
    for (size_t i = 2; i < args.size(); i += 2) {
      if (i + 1 == args.size()) throw std::runtime_error("Missing selfplay option value");
      const std::string key(args[i]), value(args[i + 1]);
      if (key == "--output") output = value;
      else if (key == "--book") book = value;
      else if (key == "--random-chance") {
        size_t end = 0; random_chance = std::stod(value, &end);
        if (end != value.size() || !std::isfinite(random_chance) || random_chance < 0 || random_chance > 1)
          throw std::runtime_error("Invalid random chance");
      } else {
        uint64_t n = 0;
        auto [end, error] = std::from_chars(value.data(), value.data() + value.size(), n);
        if (error != std::errc() || end != value.data() + value.size()) throw std::runtime_error("Invalid integer option");
        if (key == "--games") games = n;
        else if (key == "--nodes") nodes = n;
        else if (key == "--seed") seed = n;
        else if (key == "--stride") stride = n;
        else if (key == "--max-moves") max_moves = n;
        else if (key == "--depth" || key == "--opening-min" || key == "--opening-max") {
          if (n > INT_MAX) throw std::runtime_error("Integer option exceeds representation");
          if (key == "--depth") depth = int(n);
          else if (key == "--opening-min") opening_min = int(n);
          else opening_max = int(n);
        } else throw std::runtime_error("Unknown selfplay option: " + key);
      }
    }
    if (!games || !nodes || !stride || !max_moves || output.empty() ||
        depth < 1 || depth > MaxRootDepth || opening_min > opening_max ||
        max_moves > UINT64_MAX / 2 || opening_max > INT_MAX / 2)
      throw std::runtime_error("Invalid selfplay options");
    std::vector<std::string> fens;
    if (!book.empty()) {
      std::ifstream input(book); std::string line;
      if (!input) throw std::runtime_error("Cannot read opening book");
      while (std::getline(input, line)) if (!line.empty()) fens.push_back(line);
      if (fens.empty()) throw std::runtime_error("Empty opening book");
    }
    if (!load_embedded_nnue()) throw std::runtime_error("NNUE load failed");
    resize_TT(16);
    search_output_enabled = false;
    thread_data.emit_bestmove = false;
    ti.use_opening_book = ti.use_syzygy = ti.use_ponder = false;
    ti.max_iter_depth = depth; ti.max_nodes_searched = nodes;
    ti.max_time = ti.opt_time = UINT64_MAX;
    ti.time_manager.hard_limit = ti.time_manager.soft_limit = UINT64_MAX;
    const std::string temporary = output + ".partial";
    if (std::filesystem::exists(output) || std::filesystem::exists(temporary))
      throw std::runtime_error("Output already exists");
    std::ofstream stream(temporary, std::ios::binary);
    std::ofstream provenance(temporary + ".games");
    if (!stream || !provenance) throw std::runtime_error("Cannot create dataset output");
    const char magic[16] = "STALLION_SBIN64";
    const uint64_t version = 3, record_size = sizeof(PackedPosition);
    stream.write(magic, 16); stream.write(reinterpret_cast<const char *>(&version), 8);
    stream.write(reinterpret_cast<const char *>(&record_size), 8);
    uint64_t completed = 0, discarded = 0, positions = 0, searched_nodes = 0, searches = 0;
    const auto start = std::chrono::steady_clock::now();
    for (uint64_t game = 0; game < games; ++game) {
      std::mt19937_64 rng(seed + game * stride);
      new_game(ti, TT);
      BoardState p{};
      const std::string fen = fens.empty() ? "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" : fens[rng() % fens.size()];
      if (!set_board(p, ti, fen)) throw std::runtime_error("Invalid opening position");
      const uint64_t opening_plies = fens.empty() ? uint64_t(std::uniform_int_distribution<int>(opening_min, opening_max)(rng)) * 2 : 0;
      std::unordered_map<uint64_t, unsigned> repetitions;
      std::unordered_set<uint64_t> seen;
      std::vector<PackedPosition> records;
      uint64_t ply = 0;
      int result = -1;
      std::string reason;
      auto play = [&](Action move) {
        if (ti.game_ply >= MaxGameLen - MaxSearchPly - 2) {
          std::move(ti.game_hist.begin() + MaxGameLen / 2, ti.game_hist.end(), ti.game_hist.begin());
          ti.game_ply -= MaxGameLen / 2;
        }
        auto &h = ti.game_hist[ti.game_ply++];
        h = {}; h.position_key = p.zobrist_key; h.played_move = move;
        h.piece_moved = p.board[extract_from(move)];
        make_move(p, move);
      };
      while (true) {
        std::array<Action, MaxActions> moves{};
        const int count = legal_movegen(p, moves.data());
        if (!count) {
          if (attacks_square(p, get_king_pos(p, p.color), p.color ^ 1)) {
            result = p.color == Colors::White ? 0 : 65535; reason = "mate";
          } else { result = 32768; reason = "stalemate"; }
          break;
        }
        if (material_draw(p) || p.halfmoves >= 100 || ++repetitions[p.zobrist_key] >= 3) {
          result = 32768; reason = "draw"; break;
        }
        if (ply >= max_moves * 2) break;
        if (ply++ < opening_plies) { play(moves[rng() % count]); continue; }
        ti.start_time = std::chrono::steady_clock::now();
        ti.root_moves_limited = false; ti.infinite_search = false;
        thread_data.stop = false;
        search_position(p, ti, TT);
        searched_nodes += ti.nodes.load(); ++searches;
        Action move = ti.best_moves[0];
        if (std::find(moves.begin(), moves.begin() + count, move) == moves.begin() + count)
          throw std::runtime_error("Search returned illegal move");
        const int score = ti.best_scores[0];
        if (score != ScoreNone && seen.insert(p.zobrist_key).second) {
          int cp = std::abs(score) >= MateThreshold ? (score > 0 ? 2000 : -2000) : score;
          records.push_back(selfplay_pack(p, p.color == Colors::White ? cp : -cp));
        }
        if (std::generate_canonical<double, 53>(rng) < random_chance) move = moves[rng() % count];
        play(move);
      }
      if (result < 0 || records.empty()) { ++discarded; continue; }
      provenance << seed + game * stride << ' ' << positions << ' ' << records.size() << ' ' << result << ' ' << reason << '\n';
      for (auto &r : records) r.wdl = r.stored_wdl = result;
      stream.write(reinterpret_cast<const char *>(records.data()), std::streamsize(records.size() * sizeof(PackedPosition)));
      positions += records.size(); ++completed;
      if (!stream || !provenance) throw std::runtime_error("Dataset write failed");
    }
    stream.close(); provenance.close();
    if (!stream || !provenance) throw std::runtime_error("Dataset flush failed");
    std::filesystem::rename(temporary, output);
    std::filesystem::rename(temporary + ".games", output + ".games");
    const double seconds = std::chrono::duration<double>(std::chrono::steady_clock::now() - start).count();
    std::cout << "{\"cp_units\":\"nnue_raw\",\"completed\":" << completed << ",\"discarded\":" << discarded
              << ",\"positions\":" << positions << ",\"searches\":" << searches
              << ",\"searched_nodes\":" << searched_nodes << ",\"seconds\":" << seconds
              << ",\"positions_per_second\":" << positions / seconds << "}\n";
    return 0;
  } catch (const std::exception &e) {
    std::cerr << "selfplay: " << e.what() << '\n'; return 2;
  }
}

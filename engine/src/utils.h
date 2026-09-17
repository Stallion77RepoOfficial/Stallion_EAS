#pragma once
#include "bitboard.h"
#include "defs.h"
#include "params.h"
#include "poly_random.h"
#include "nnue.h"
#include <algorithm>
#include <atomic>
#include <cctype>
#include <condition_variable>
#include <cstdarg>
#include <cstdio>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <memory>
#include <mutex>
#include <string>
#include <thread>
#include <unordered_map>
#include <vector>

inline std::filesystem::path engine_directory;

inline std::string resolve_file_path(const std::string &name) {
  namespace fs = std::filesystem;
  const fs::path path(name);
  if (name.empty() || path.is_absolute()) return name;
  std::vector<fs::path> candidates{path};
  if (!engine_directory.empty()) candidates.push_back(engine_directory / path);
  if (!path.has_parent_path() && path.extension() == ".nnue") {
    candidates.push_back(fs::path("nets") / path);
    if (!engine_directory.empty()) candidates.push_back(engine_directory / "nets" / path);
  }
  for (const auto &candidate : candidates) {
    std::error_code ec;
    if (fs::is_regular_file(candidate, ec)) return candidate.string();
  }
  return name;
}

using uint128_t = unsigned __int128;

struct BookEntry {
  Action move;
  uint16_t weight;
};

class OpeningBook {
private:
  using Positions = std::unordered_map<uint64_t, std::vector<BookEntry>>;
  std::shared_ptr<const Positions> book_positions;

public:
  OpeningBook() = default;

  bool load_book(const std::string &path);
  Action probe_book(const BoardState &position, int min_weight = 1);
  bool is_loaded() const noexcept { return book_positions && !book_positions->empty(); }
  void clear_book() noexcept {
    book_positions.reset();
  }

  uint64_t polyglot_key(const BoardState &pos);

private:
  bool load_polyglot_book(const std::string &filename);
};

struct TimeManager {
  uint64_t allocated_time = 0;
  uint64_t max_time = 0;
  uint64_t panic_time = 0;
  uint64_t soft_limit = 0;
  uint64_t hard_limit = 0;
  bool use_panic_mode = false;

  TimeManager() = default;

  void initialize(uint64_t time_left, uint64_t increment, int moves_to_go,
                  uint32_t game_move) noexcept;
  bool should_stop(uint64_t elapsed, bool best_move_stable, bool in_trouble,
                   bool is_movetime = false) noexcept;
};

struct ThreadInfoBase {
  uint16_t thread_id = 0;
  std::array<StateRecord, MaxGameLen> game_hist{};
  uint16_t game_ply = 0;
  uint16_t search_ply = 0;

  std::vector<RootAction> root_moves;

  std::chrono::steady_clock::time_point start_time;

  int seldepth = 0;

  uint64_t max_time = 0;
  uint64_t opt_time = 0;
  uint64_t original_opt = 0;

  uint16_t time_checks = 0;

  NNUE_State nnue_state;

  MultiArray<int16_t, 14, 64> HistoryScores;
  MultiArray<int16_t, 14, 64, 14, 64> ContHistScores;
  MultiArray<int16_t, 14, 64> CapHistScores;
  MultiArray<int16_t, 2, 16384> PawnCorrHist;
  MultiArray<int16_t, 2, 2, 16384> NonPawnCorrHist;
  MultiArray<Action, MaxSearchPly + 1, 2> KillerMoves;
  MultiArray<Action, 14, 64> CounterMoves;

  uint16_t current_iter = 0;

  uint16_t multipv = 1;
  uint16_t multipv_index = 0;

  Action excluded_move = MoveNone;
  std::array<Action, MaxActions> best_moves{};
  std::array<int, MaxActions> best_scores{};

  int max_iter_depth = MaxRootDepth;
  int mate_search = 0;
  uint64_t max_nodes_searched = UINT64_MAX / 2;
  uint64_t opt_nodes_searched = UINT64_MAX / 2;

  std::array<Action, MaxSearchPly * MaxSearchPly> pv;

  BoardState position;

  uint8_t searches = 0;
  const NNUE_Params *cached_eval_network = nullptr;
  bool infinite_search = false;
  bool root_moves_limited = false;

  uint64_t max_move_time = 0;
  uint64_t move_overhead = 30;

  uint16_t max_depth = 0;

  uint64_t max_nodes = 0;

  bool use_ponder = true;

  Action ponder_move = MoveNone;

  std::chrono::steady_clock::time_point ponder_start_time;

  bool use_syzygy = false;
  std::string syzygy_path;

  TimeManager time_manager;
  bool is_movetime = false;
  bool best_move_stable = false;
  int stability_counter = 0;
  Action previous_best_move = MoveNone;

  OpeningBook opening_book;
  bool use_opening_book = false;
  std::string book_path;
  int book_depth_limit = 0;

  int syzygy_probe_depth = 6;
  int syzygy_probe_limit = 6;
  bool syzygy_50_move_rule = true;
  int book_min_weight = 0;
  int ponder_time_factor = 200;

  std::array<uint64_t, 32> recent_book_keys{};
  uint8_t recent_book_head = 0;
};

struct ThreadInfo : ThreadInfoBase {
  std::atomic<uint64_t> nodes{0};
  std::atomic<bool> pondering{false};
  std::atomic<bool> ponder_hit{false};

  ThreadInfo() = default;
  ThreadInfo(const ThreadInfo &other) : ThreadInfoBase(other) {
    nodes.store(other.nodes.load(std::memory_order_relaxed), std::memory_order_relaxed);
    pondering.store(other.pondering.load(std::memory_order_relaxed), std::memory_order_relaxed);
    ponder_hit.store(other.ponder_hit.load(std::memory_order_relaxed), std::memory_order_relaxed);
  }
  ThreadInfo &operator=(const ThreadInfo &other) {
    if (this != &other) {
      ThreadInfoBase::operator=(other);
      nodes.store(other.nodes.load(std::memory_order_relaxed), std::memory_order_relaxed);
      pondering.store(other.pondering.load(std::memory_order_relaxed), std::memory_order_relaxed);
      ponder_hit.store(other.ponder_hit.load(std::memory_order_relaxed), std::memory_order_relaxed);
    }
    return *this;
  }
};

inline RootAction *find_root_move(ThreadInfo &thread_info, Action move) noexcept {
  for (auto &r : thread_info.root_moves)
    if (r.move == move)
      return &r;
  return nullptr;
}

struct ThreadData {
  std::vector<ThreadInfo> thread_infos;
  std::vector<std::thread> threads;
  int num_threads = 1;
  std::atomic<bool> stop{true};

  std::atomic<bool> is_frc{false};
  std::mutex data_mutex;
  std::atomic<uint64_t> tb_hits{0};
  std::atomic<uint64_t> tb_fails{0};
  std::atomic<bool> pondering{false};
  std::atomic<int64_t> ponder_hit_time{-1};
  std::mutex control_mutex;
  std::condition_variable control_cv;
};

inline ThreadData thread_data;

inline std::mutex &get_print_mutex() {
  static std::mutex print_mutex;
  return print_mutex;
}

inline void safe_printf(const char *fmt, ...) {
  std::lock_guard<std::mutex> lg(get_print_mutex());
  va_list ap;
  va_start(ap, fmt);
  vprintf(fmt, ap);
  va_end(ap);
  fflush(stdout);
}

inline void safe_print_cerr(const std::string &s) {
  std::lock_guard<std::mutex> lg(get_print_mutex());
  std::cerr << s << std::endl;
}

inline uint64_t TT_size = (1 << 20);
inline std::vector<TTBucket> TT(TT_size);

inline std::atomic<bool> TT_resizing{false};

inline void new_game(ThreadInfo &thread_info, std::vector<TTBucket> &table) {
  std::lock_guard<std::mutex> lg(thread_data.data_mutex);
  thread_info.game_ply = 0;
  thread_info.thread_id = 0;
  thread_info.HistoryScores.fill({});
  thread_info.ContHistScores.fill({});
  thread_info.CapHistScores.fill({});
  thread_info.PawnCorrHist.fill({});
  thread_info.NonPawnCorrHist.fill({});
  thread_info.game_hist.fill({});
  thread_info.nodes.store(0);
  TT_resizing.store(true);
  table.assign(TT_size, TTBucket{});
  TT_resizing.store(false);
  thread_info.searches = 0;
  thread_info.search_ply = 0;
  thread_info.cached_eval_network = nullptr;
  thread_info.KillerMoves.fill({});
  thread_info.CounterMoves.fill({});
  thread_info.recent_book_keys.fill(0);
  thread_info.recent_book_head = 0;
}

constexpr inline uint32_t get_hash_low_bits(uint64_t hash) noexcept {
  return static_cast<uint32_t>(hash);
}

constexpr inline int32_t score_to_tt(int32_t score, int32_t ply) noexcept {
  if (score == ScoreNone)
    return ScoreNone;
  if (score >= MateThreshold)
    return score + ply;
  if (score <= -MateThreshold)
    return score - ply;
  return score;
}

constexpr inline int32_t score_from_tt(int32_t score, int32_t ply) noexcept {
  if (score == ScoreNone)
    return ScoreNone;
  if (score >= MateThreshold)
    return score - ply;
  if (score <= -MateThreshold)
    return score + ply;
  return score;
}

inline void resize_TT(int size) {
  std::lock_guard<std::mutex> lock(thread_data.data_mutex);

  TT_resizing.store(true, std::memory_order_release);
  const uint64_t requested = static_cast<uint64_t>(std::clamp(size, 1, 131072)) * 1024 * 1024 / sizeof(TTBucket);
  try {
    std::vector<TTBucket> replacement(requested);
    TT.swap(replacement);
    TT_size = TT.size();
  } catch (const std::bad_alloc &) {
    safe_printf("info string Hash allocation failed; keeping current table\n");
  }
  TT_resizing.store(false, std::memory_order_release);
}

inline uint64_t safe_TT_size() noexcept {
  std::lock_guard<std::mutex> lg(thread_data.data_mutex);
  if (TT_resizing.load(std::memory_order_acquire))
    return 0;
  return TT.size();
}

inline void safe_TT_prefetch(uint64_t hash) noexcept {
  if (TT_resizing.load(std::memory_order_acquire))
    return;
  const uint64_t size = TT.size();
  if (size == 0)
    return;
  const uint64_t idx = (uint128_t(hash) * uint128_t(size)) >> 64;
  if (idx >= size)
    return;
  __builtin_prefetch(&TT[static_cast<size_t>(idx)], 0, 1);
}

constexpr inline int entry_quality(const TTEntry &entry, int searches) noexcept {
  const int age_diff = (MaxAge + searches - entry.get_age()) % MaxAge;
  return entry.depth - age_diff * 8;
}

inline std::array<std::mutex, 4096> tt_mutexes;

inline TTEntry probe_entry(uint64_t hash, bool &hit, uint8_t searches,
                           std::vector<TTBucket> &table) {
  static thread_local TTBucket fallback_bucket;
  auto fallback = [&]() {
    hit = false;
    fallback_bucket.entries[0].age_bound =
        (searches << 2) | fallback_bucket.entries[0].get_type();
    return fallback_bucket.entries[0];
  };

  if (TT_resizing.load(std::memory_order_acquire))
    return fallback();
  const uint64_t size = table.size();
  if (size == 0)
    return fallback();
  const uint32_t zobrist_key = get_hash_low_bits(hash);
  const uint64_t idx = (uint128_t(hash) * uint128_t(size)) >> 64;
  if (idx >= size)
    return fallback();

  std::lock_guard<std::mutex> lock(tt_mutexes[idx % tt_mutexes.size()]);
  auto &bucket = table[idx];
  __builtin_prefetch(&bucket, 0, 1);
  auto &entries = bucket.entries;

  for (int i = 0; i < BucketEntries; i++) {
    const bool empty =
        entries[i].score == 0 && entries[i].get_type() == EntryTypes::None;

    if (empty || entries[i].position_key == zobrist_key) {
      hit = !empty;
      entries[i].age_bound = (searches << 2) | entries[i].get_type();
      return entries[i];
    }
  }

  TTEntry *worst = &(entries[0]);
  int worst_quality = entry_quality(*worst, searches);

  for (int i = 1; i < BucketEntries; i++) {
    const int this_quality = entry_quality(entries[i], searches);
    if (this_quality < worst_quality) {
      worst = &(entries[i]);
      worst_quality = this_quality;
    }
  }

  hit = false;
  return *worst;
}

inline void insert_entry(TTEntry & /*entry*/, uint64_t hash, int depth, Action best_move,
                         int32_t static_eval, int32_t score, uint8_t bound_type,
                         uint8_t searches) {
  const uint32_t zobrist_key = get_hash_low_bits(hash);
  auto set_entry = [&](TTEntry &e) {
    e.position_key = zobrist_key;
    e.depth = static_cast<uint8_t>(std::clamp(depth, 0, 255));
    e.static_eval = static_eval;
    e.score = score;
    e.age_bound = (searches << 2) | bound_type;
  };

  if (TT_resizing.load(std::memory_order_acquire)) {
    static thread_local TTBucket fallback_bucket;
    TTEntry &fe = fallback_bucket.entries[0];
    if (best_move != MoveNone || zobrist_key != fe.position_key)
      fe.best_move = best_move;
    if (fe.position_key == zobrist_key && (bound_type != EntryTypes::Exact) &&
        fe.depth > depth + 4)
      return;
    set_entry(fe);
    return;
  }

  const uint64_t size = TT.size();
  if (size == 0)
    return;
  uint64_t idx = (uint128_t(hash) * uint128_t(size)) >> 64;
  if (idx >= size)
    idx = idx % size;
  std::lock_guard<std::mutex> lock(tt_mutexes[idx % tt_mutexes.size()]);
  auto &bucket = TT[idx];
  auto &entries = bucket.entries;

  for (int i = 0; i < BucketEntries; i++) {
    const bool empty =
        entries[i].score == 0 && entries[i].get_type() == EntryTypes::None;
    if (empty || entries[i].position_key == zobrist_key) {
      TTEntry &e = entries[i];
      if (best_move != MoveNone || zobrist_key != e.position_key)
        e.best_move = best_move;
      if (e.position_key == zobrist_key && (bound_type != EntryTypes::Exact) &&
          e.depth > depth + 4)
        return;
      set_entry(e);
      return;
    }
  }

  TTEntry *worst = &entries[0];
  int worst_q = entry_quality(*worst, searches);
  for (int i = 1; i < BucketEntries; i++) {
    const int q = entry_quality(entries[i], searches);
    if (q < worst_q) {
      worst = &entries[i];
      worst_q = q;
    }
  }
  TTEntry &we = *worst;
  set_entry(we);
  we.best_move = best_move;
}

constexpr inline uint64_t mix_key(uint64_t value) noexcept {
  value = (value ^ (value >> 30)) * 0xbf58476d1ce4e5b9ULL;
  value = (value ^ (value >> 27)) * 0x94d049bb133111ebULL;
  return value ^ (value >> 31);
}

inline uint64_t castling_key(int color, int side, int square) noexcept {
  return square == SquareNone ? 0 : mix_key(zobrist_keys[castling_index + color * 2 + side] ^ uint64_t(square));
}

inline uint64_t ep_key(const BoardState &position) noexcept {
  const int ep = position.ep_square, color = position.color;
  if (!is_valid_square(ep)) return 0;
  const int captured = ep + (color ? Directions::North : Directions::South);
  if (!is_valid_square(captured) || position.board[ep] ||
      position.board[captured] != Pieces::WPawn + (color ^ 1)) return 0;
  uint64_t candidates = PAWN_ATK_SAFE(color ^ 1, ep) & position.colors_bb[color] & position.pieces_bb[PieceTypes::Pawn];
  const uint64_t king_bb = position.colors_bb[color] & position.pieces_bb[PieceTypes::King];
  if (!king_bb) return 0;
  const int king = get_lsb(king_bb);
  const uint64_t enemy = position.colors_bb[color ^ 1] & ~(1ULL << captured);
  while (candidates) {
    const int from = pop_lsb(candidates);
    const uint64_t occ = ((position.colors_bb[0] | position.colors_bb[1]) & ~(1ULL << from) & ~(1ULL << captured)) | (1ULL << ep);
    const uint64_t attacks =
        (PAWN_ATK_SAFE(color, king) & position.pieces_bb[PieceTypes::Pawn]) |
        (KNIGHT_ATK_SAFE(king) & position.pieces_bb[PieceTypes::Knight]) |
        (KING_ATK_SAFE(king) & position.pieces_bb[PieceTypes::King]) |
        (get_bishop_attacks(king, occ) & (position.pieces_bb[PieceTypes::Bishop] | position.pieces_bb[PieceTypes::Queen])) |
        (get_rook_attacks(king, occ) & (position.pieces_bb[PieceTypes::Rook] | position.pieces_bb[PieceTypes::Queen]));
    if (!(attacks & enemy)) return mix_key(zobrist_keys[ep_index] ^ uint64_t(get_file(ep)));
  }
  return 0;
}

inline void calculate(BoardState &position) noexcept {
  uint64_t hash = 0;
  uint64_t pawn_hash = 0;
  position.non_pawn_key[Colors::White] = 0;
  position.non_pawn_key[Colors::Black] = 0;

  for (int indx = 0; indx < 64; indx++) {
    const int piece = position.board[indx];
    if (piece) {
      hash ^= zobrist_keys[get_zobrist_key(piece, indx)];
      if (get_piece_type(piece) == PieceTypes::Pawn) {
        pawn_hash ^= zobrist_keys[get_zobrist_key(piece, indx)];
      } else {
        position.non_pawn_key[get_color(piece)] ^=
            zobrist_keys[get_zobrist_key(piece, indx)];
      }
    }
  }
  if (position.color) {
    hash ^= zobrist_keys[side_index];
  }
  hash ^= ep_key(position);
  for (int color = 0; color < 2; ++color)
    for (int side = 0; side < 2; ++side)
      hash ^= castling_key(color, side, position.castling_squares[color][side]);
  position.zobrist_key = hash;
  position.pawn_key = pawn_hash;
}

constexpr inline int get_corrhist_index(uint64_t key) noexcept { return key % 16384; }

inline int64_t time_elapsed(std::chrono::steady_clock::time_point start_time) noexcept {
  const auto now = std::chrono::steady_clock::now();
  return std::chrono::duration_cast<std::chrono::milliseconds>(now - start_time)
      .count();
}

class Barrier {
public:
  explicit Barrier(size_t expected) : total(expected), remaining(expected) {}
  void reset(size_t expected) {
    std::lock_guard lock(mutex);
    total = remaining = expected;
  }
  void arrive_and_wait() {
    std::unique_lock lock(mutex);
    const auto phase = generation;
    if (--remaining == 0) {
      remaining = total;
      ++generation;
      condition.notify_all();
    } else condition.wait(lock, [&] { return generation != phase; });
  }
private:
  size_t total, remaining, generation = 0;
  std::mutex mutex;
  std::condition_variable condition;
};

inline bool OpeningBook::load_book(const std::string &path) {
  clear_book();
  return !path.empty() && load_polyglot_book(resolve_file_path(path));
}

int legal_movegen(const BoardState &position, Action *moves);

inline Action OpeningBook::probe_book(const BoardState &position, int min_weight) {
  if (!is_loaded()) return MoveNone;
  auto it = book_positions->find(polyglot_key(position));
  if (it == book_positions->end()) return MoveNone;
  std::array<Action, MaxActions> legal{};
  const int count = legal_movegen(position, legal.data());
  std::vector<std::pair<Action, uint16_t>> candidates;
  uint64_t total = 0;
  for (const auto &entry : it->second) {
    if (!entry.weight || entry.weight < std::max(0, min_weight)) continue;
    for (int i = 0; i < count; ++i) {
      const Action move = legal[i];
      if (extract_from(move) != extract_from(entry.move) || extract_to(move) != extract_to(entry.move)) continue;
      const bool promo = extract_type(move) == MoveTypes::Promotion;
      if (promo != (extract_type(entry.move) == MoveTypes::Promotion) ||
          (promo && extract_promo(move) != extract_promo(entry.move))) continue;

      candidates.emplace_back(move, entry.weight);
      total += entry.weight;
      break;
    }
  }
  if (!total) return MoveNone;
  uint64_t ticket = std::uniform_int_distribution<uint64_t>(0, total - 1)(Random::rd);
  for (const auto &[move, weight] : candidates) {
    if (ticket < weight) return move;
    ticket -= weight;
  }
  return MoveNone;
}

inline bool OpeningBook::load_polyglot_book(const std::string &filename) {
  std::ifstream file(filename, std::ios::binary | std::ios::ate);
  if (!file || file.tellg() <= 0 || file.tellg() % 16 != 0) return false;
  file.seekg(0);
  auto positions = std::make_shared<Positions>();
  std::array<unsigned char, 16> bytes{};
  while (file.read(reinterpret_cast<char *>(bytes.data()), bytes.size())) {
    uint64_t key = 0;
    for (int i = 0; i < 8; ++i) key = (key << 8) | bytes[i];

    if (key == 0) continue;
    const unsigned encoded = (unsigned(bytes[8]) << 8) | bytes[9];
    const uint16_t weight = (unsigned(bytes[10]) << 8) | bytes[11];
    const int from = (encoded >> 6) & 63, to = encoded & 63, promo = (encoded >> 12) & 7;
    if (encoded & 0x8000 || promo > 4 || from == to) { clear_book(); return false; }
    const Action move = promo ? pack_move_promo(from, to, promo - 1) : pack_move(from, to, MoveTypes::Normal);
    (*positions)[key].push_back({move, weight});
  }
  if (!file.eof()) { clear_book(); return false; }
  book_positions = std::move(positions);
  return is_loaded();
}

inline uint64_t OpeningBook::polyglot_key(const BoardState &pos) {
  uint64_t key = 0ULL;

  for (int sq = 0; sq < 64; ++sq) {
    const int piece = pos.board[sq];
    if (!piece)
      continue;
    const int ptype = get_piece_type(piece);
    if (ptype >= PieceTypes::Pawn && ptype <= PieceTypes::King) {
      const int poly_index =
          (ptype - 1) * 2 + (get_color(piece) == Colors::White ? 1 : 0);
      key ^= poly_random[64 * poly_index + sq];
    }
  }

  if (pos.castling_squares[Colors::White][Sides::Kingside] != SquareNone)
    key ^= poly_random[768];
  if (pos.castling_squares[Colors::White][Sides::Queenside] != SquareNone)
    key ^= poly_random[769];
  if (pos.castling_squares[Colors::Black][Sides::Kingside] != SquareNone)
    key ^= poly_random[770];
  if (pos.castling_squares[Colors::Black][Sides::Queenside] != SquareNone)
    key ^= poly_random[771];

  if (pos.ep_square != SquareNone && pos.ep_square < 64) {
    const int ep_file = get_file(pos.ep_square);
    const int ep_rank = get_rank(pos.ep_square);
    bool ep_valid = false;
    if (pos.color == Colors::White && ep_rank == 5) {
      if (ep_file > 0) {
        const int sq = (ep_rank - 1) * 8 + (ep_file - 1);
        if (pos.board[sq] == Pieces::WPawn)
          ep_valid = true;
      }
      if (!ep_valid && ep_file < 7) {
        const int sq = (ep_rank - 1) * 8 + (ep_file + 1);
        if (pos.board[sq] == Pieces::WPawn)
          ep_valid = true;
      }
    } else if (pos.color == Colors::Black && ep_rank == 2) {
      if (ep_file > 0) {
        const int sq = (ep_rank + 1) * 8 + (ep_file - 1);
        if (pos.board[sq] == Pieces::BPawn)
          ep_valid = true;
      }
      if (!ep_valid && ep_file < 7) {
        const int sq = (ep_rank + 1) * 8 + (ep_file + 1);
        if (pos.board[sq] == Pieces::BPawn)
          ep_valid = true;
      }
    }
    if (ep_valid)
      key ^= poly_random[768 + 4 + ep_file];
  }
  if (pos.color == Colors::White)
    key ^= poly_random[780];
  return key;
}

inline void TimeManager::initialize(uint64_t time_left, uint64_t increment,
                                    int moves_to_go, uint32_t game_move) noexcept {

  const uint64_t reserved = std::min<uint64_t>(50, time_left / 10);
  const uint64_t usable_time = (time_left > reserved) ? (time_left - reserved)
                                                       : std::max<uint64_t>(1, time_left / 2);

  if (moves_to_go > 0) {
    allocated_time =
        (usable_time +
         increment * static_cast<uint64_t>(std::max(moves_to_go - 1, 0))) /
        static_cast<uint64_t>(std::max(moves_to_go, 1));
    allocated_time = std::min(allocated_time, usable_time / 2);
  } else {
    double time_factor = 1.0;
    if (game_move < 20)
      time_factor = 0.8;
    else if (game_move < 40)
      time_factor = 1.1;
    else
      time_factor = 1.0;

    allocated_time = std::max<uint64_t>(
        1, static_cast<uint64_t>((usable_time / 22.0 + increment * 0.75) *
                                 time_factor));
  }

  allocated_time = std::min(allocated_time, usable_time);

  max_time = std::min<uint64_t>(allocated_time * 3, usable_time * 8 / 10);
  max_time = std::max<uint64_t>(allocated_time, max_time);
  panic_time = std::min<uint64_t>(allocated_time * 2, max_time);

  soft_limit = allocated_time;
  hard_limit = max_time;

  if (hard_limit > usable_time)
    hard_limit = usable_time;
  if (soft_limit > hard_limit)
    soft_limit = hard_limit;

  use_panic_mode = false;
}

inline bool TimeManager::should_stop(uint64_t elapsed, bool best_move_stable,
                                     bool in_trouble, bool is_movetime) noexcept {

  if (elapsed >= hard_limit)
    return true;

  if (is_movetime)
    return false;

  if (in_trouble && !use_panic_mode && elapsed < panic_time) {
    use_panic_mode = true;
    soft_limit = panic_time;
  }

  if (elapsed >= soft_limit) {
    if (best_move_stable || use_panic_mode)
      return true;
    soft_limit = std::min(soft_limit + allocated_time / 8, hard_limit);
  }
  return false;
}

inline void adjust_soft_limit(ThreadInfo &thread_info, uint64_t best_move_nodes,
                              int bm_stability, int best_score) noexcept {
  const uint64_t node_count = thread_info.nodes.load(std::memory_order_relaxed);
  if (node_count == 0)
    return;
  const double fract = static_cast<double>(best_move_nodes) / node_count;
  double factor = (static_cast<double>(NodeTmFactor1) / 100.0 - fract) *
                  NodeTmFactor2 / 100.0;
  const double bm_factor = BmFactor1 / 100.0f - (bm_stability * 0.06);

  if (thread_info.time_manager.use_panic_mode) {
    factor *= 1.5;
  }

  constexpr double WdlK = -0.003;
  const double win_rate = 1.0 / (1.0 + std::exp(WdlK * best_score));
  const double closeness = 1.0 - std::abs(win_rate - 0.5) * 2.0;
  const double wdl_factor = 1.0 + closeness * 0.5;

  double node_factor = 1.0;
  if (node_count > 100000) {
    node_factor = std::min(2.0, node_count / 50000.0);
  }

  const uint64_t new_time =
      static_cast<uint64_t>(std::clamp<long double>(
          static_cast<long double>(thread_info.original_opt) * factor * bm_factor * node_factor * wdl_factor,
          1.0L, static_cast<long double>(thread_info.max_time)));
  thread_info.opt_time = std::min<uint64_t>(new_time, thread_info.max_time);
}

#pragma once

#include "bitboard.h"
#include "defs.h"
#include "nnue.h"
#include "params.h"
#include "poly_random.h"

#include <algorithm>
#include <atomic>
#include <cctype>
#include <condition_variable>
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <memory>
#include <mutex>
#include <string>
#include <thread>
#include <unordered_map>
#include <utility>
#include <vector>

inline std::filesystem::path engine_directory;

inline std::string resolve_file_path(const std::string &name) {
  namespace fs = std::filesystem;
  const fs::path input_path(name);
  if (name.empty() || input_path.is_absolute()) return name;

  std::error_code ec;
  if (fs::is_regular_file(input_path, ec)) return input_path.string();
  if (!engine_directory.empty()) {
    const fs::path rel_to_exe = engine_directory / input_path;
    if (fs::is_regular_file(rel_to_exe, ec)) return rel_to_exe.string();
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
  bool is_loaded() const noexcept {
    return book_positions && !book_positions->empty();
  }
  void clear_book() noexcept { book_positions.reset(); }

  uint64_t polyglot_key(const BoardState &pos);

private:
  bool load_polyglot_book(const std::string &filename);
};

struct TimeManager {
  uint64_t allocated_time = 0;
  uint64_t max_time = 0;
  uint64_t soft_limit = 0;
  uint64_t hard_limit = 0;

  TimeManager() = default;

  void initialize(uint64_t time_left, uint64_t increment, int moves_to_go,
                  uint32_t game_move) noexcept;
  bool should_stop(uint64_t elapsed, bool best_move_stable,
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
  MoveBufferPool move_buffers;

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

  std::array<Action, MaxSearchPly * MaxSearchPly> pv;

  BoardState position;

  uint8_t searches = 0;
  bool infinite_search = false;
  bool root_moves_limited = false;

  uint64_t max_move_time = 0;
  uint64_t move_overhead = 30;

  uint16_t max_depth = 0;

  uint64_t max_nodes = 0;

  bool use_ponder = true;

  Action ponder_move = MoveNone;

  bool use_syzygy = false;
  std::string syzygy_path;

  TimeManager time_manager;
  bool is_movetime = false;
  bool best_move_stable = false;
  int stability_counter = 0;

  OpeningBook opening_book;
  bool use_opening_book = false;
  std::string book_path;
  int book_depth_limit = 0;

  int syzygy_probe_depth = 6;
  int syzygy_probe_limit = 6;
  bool syzygy_50_move_rule = true;
  int book_min_weight = 0;
  int ponder_time_factor = 200;

  bool uci_limit_strength = false;
  int uci_elo = 3000;

  std::array<uint64_t, 32> recent_book_keys{};
  uint8_t recent_book_head = 0;
};

constexpr int MinUciElo = 1000;
constexpr int MaxUciElo = 3200;
constexpr int DefaultUciElo = 3000;

inline uint64_t uci_elo_max_nodes(int elo) noexcept {
  const double bounded = static_cast<double>(std::clamp(elo, MinUciElo, MaxUciElo));
  const double norm = (bounded - MinUciElo) / static_cast<double>(MaxUciElo - MinUciElo);
  return static_cast<uint64_t>(std::llround(64.0 * std::pow(10.0, norm * 3.8)));
}

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

inline RootAction *find_root_move(ThreadInfo &ti, Action move) noexcept {
  for (RootAction &item : ti.root_moves) {
    if (item.move == move) return &item;
  }
  return nullptr;
}

struct ThreadData {
  std::vector<ThreadInfo> thread_infos;
  std::vector<std::thread> threads;
  std::atomic<bool> stop{true};
  std::atomic<bool> emit_bestmove{true};

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
  static std::mutex io_mutex;
  return io_mutex;
}

inline bool search_output_enabled = true;

inline void safe_printf(const char *fmt, ...) {
  if (!search_output_enabled) return;
  std::lock_guard<std::mutex> guard(get_print_mutex());
  va_list args;
  va_start(args, fmt);
  std::vprintf(fmt, args);
  va_end(args);
  std::fflush(stdout);
}

constexpr int DefaultHashMB = 256;
inline uint64_t TT_size = (1 << 20);
inline std::vector<TTBucket> TT(TT_size);
inline std::atomic<bool> TT_resizing{false};
inline std::array<std::mutex, 4096> tt_mutexes;

constexpr inline uint32_t get_hash_low_bits(uint64_t hash) noexcept {
  return static_cast<uint32_t>(hash);
}

constexpr inline int32_t score_to_tt(int32_t score, int32_t ply) noexcept {
  if (score == ScoreNone) return ScoreNone;
  if (score >= MateThreshold) return score + ply;
  if (score <= -MateThreshold) return score - ply;
  return score;
}

constexpr inline int32_t score_from_tt(int32_t score, int32_t ply) noexcept {
  if (score == ScoreNone) return ScoreNone;
  if (score >= MateThreshold) return score - ply;
  if (score <= -MateThreshold) return score + ply;
  return score;
}

inline uint64_t require_TT_size(const std::vector<TTBucket> &table) noexcept {
  // Search threads are joined before resizing or clearing the table.
  if (TT_resizing.load(std::memory_order_acquire) || table.empty()) {
    std::exit(EXIT_FAILURE);
  }
  return table.size();
}

inline uint64_t safe_TT_size() noexcept {
  std::lock_guard<std::mutex> guard(thread_data.data_mutex);
  return require_TT_size(TT);
}

constexpr inline int entry_quality(const TTEntry &entry, int searches) noexcept {
  const int elapsed_age = (MaxAge + searches - entry.get_age()) % MaxAge;
  return entry.depth - elapsed_age * 8;
}

namespace detail_util {

constexpr inline uint64_t tt_slot_index(uint64_t hash, uint64_t table_len) noexcept {
  return static_cast<uint64_t>((uint128_t(hash) * uint128_t(table_len)) >> 64);
}

constexpr inline bool is_empty_slot(const TTEntry &entry) noexcept {
  return entry.score == 0 && entry.get_type() == EntryTypes::None;
}

constexpr inline uint8_t pack_age_bound(uint8_t age_gen, uint8_t bound_type) noexcept {
  return static_cast<uint8_t>((age_gen << 2) | bound_type);
}

inline TTEntry &select_victim(std::array<TTEntry, BucketEntries> &slots,
                              uint8_t searches) noexcept {
  int victim_idx = 0;
  int min_quality = entry_quality(slots[0], searches);
  for (int i = 1; i < BucketEntries; ++i) {
    const int q = entry_quality(slots[i], searches);
    if (q < min_quality) {
      min_quality = q;
      victim_idx = i;
    }
  }
  return slots[static_cast<size_t>(victim_idx)];
}

constexpr inline uint64_t read_be64(const unsigned char *p) noexcept {
  uint64_t v = 0;
  for (int i = 0; i < 8; ++i) v = (v << 8) | p[i];
  return v;
}

constexpr inline uint16_t read_be16(const unsigned char *p) noexcept {
  return static_cast<uint16_t>((static_cast<unsigned>(p[0]) << 8) | p[1]);
}

}  // namespace detail_util

inline void new_game(ThreadInfo &ti, std::vector<TTBucket> &table) {
  std::lock_guard<std::mutex> guard(thread_data.data_mutex);
  ti.thread_id = 0;
  ti.game_ply = 0;
  ti.search_ply = 0;
  ti.searches = 0;
  ti.nodes.store(0);

  ti.HistoryScores.fill({});
  ti.ContHistScores.fill({});
  ti.CapHistScores.fill({});
  ti.PawnCorrHist.fill({});
  ti.NonPawnCorrHist.fill({});
  ti.KillerMoves.fill({});
  ti.CounterMoves.fill({});
  ti.game_hist.fill({});
  ti.recent_book_keys.fill(0);
  ti.recent_book_head = 0;

  TT_resizing.store(true);
  table.assign(TT_size, TTBucket{});
  TT_resizing.store(false);
}

inline void resize_TT(int size_mb) {
  std::lock_guard<std::mutex> guard(thread_data.data_mutex);
  const int bounded_mb = std::clamp(size_mb, 1, 131072);
  TT_resizing.store(true, std::memory_order_release);

  const uint64_t bucket_count =
      static_cast<uint64_t>(bounded_mb) * 1024ULL * 1024ULL / sizeof(TTBucket);
  try {
    std::vector<TTBucket> resized(bucket_count);
    TT.swap(resized);
    TT_size = TT.size();
  } catch (const std::bad_alloc &) {
    std::exit(EXIT_FAILURE);
  }
  TT_resizing.store(false, std::memory_order_release);
}

inline void safe_TT_prefetch(uint64_t hash) noexcept {
  const uint64_t len = require_TT_size(TT);
  const uint64_t idx = detail_util::tt_slot_index(hash, len);
  __builtin_prefetch(&TT[static_cast<size_t>(idx)], 0, 1);
}

inline TTEntry probe_entry(uint64_t hash, bool &hit, uint8_t searches,
                           std::vector<TTBucket> &table) {
  const uint64_t len = require_TT_size(table);
  const uint32_t tag = get_hash_low_bits(hash);
  const uint64_t idx = detail_util::tt_slot_index(hash, len);

  std::lock_guard<std::mutex> guard(tt_mutexes[idx % tt_mutexes.size()]);
  TTBucket &bucket = table[static_cast<size_t>(idx)];
  __builtin_prefetch(&bucket, 0, 1);

  for (TTEntry &slot : bucket.entries) {
    const bool empty = detail_util::is_empty_slot(slot);
    if (empty || slot.position_key == tag) {
      hit = !empty;
      slot.age_bound = detail_util::pack_age_bound(searches, slot.get_type());
      return slot;
    }
  }

  hit = false;
  return detail_util::select_victim(bucket.entries, searches);
}

inline void insert_entry(uint64_t hash, int depth, Action best_move,
                         int32_t static_eval, int32_t score, uint8_t bound_type,
                         uint8_t searches) {
  const uint64_t len = require_TT_size(TT);
  const uint32_t tag = get_hash_low_bits(hash);
  const uint64_t idx = detail_util::tt_slot_index(hash, len);

  const uint8_t clamped_depth = static_cast<uint8_t>(std::clamp(depth, 0, 255));
  const uint8_t meta = detail_util::pack_age_bound(searches, bound_type);
  auto write_fields = [&](TTEntry &slot) noexcept {
    slot.position_key = tag;
    slot.depth = clamped_depth;
    slot.static_eval = static_eval;
    slot.score = score;
    slot.age_bound = meta;
  };

  std::lock_guard<std::mutex> guard(tt_mutexes[idx % tt_mutexes.size()]);
  auto &slots = TT[static_cast<size_t>(idx)].entries;

  for (TTEntry &slot : slots) {
    if (!detail_util::is_empty_slot(slot) && slot.position_key != tag) continue;
    if (best_move != MoveNone || tag != slot.position_key) slot.best_move = best_move;
    if (slot.position_key == tag && bound_type != EntryTypes::Exact &&
        slot.depth > depth + 4) {
      return;
    }
    write_fields(slot);
    return;
  }

  TTEntry &victim = detail_util::select_victim(slots, searches);
  write_fields(victim);
  victim.best_move = best_move;
}

constexpr inline uint64_t mix_key(uint64_t val) noexcept {
  val = (val ^ (val >> 30)) * 0xbf58476d1ce4e5b9ULL;
  val = (val ^ (val >> 27)) * 0x94d049bb133111ebULL;
  return val ^ (val >> 31);
}

inline uint64_t castling_key(int color, int side, int square) noexcept {
  if (square == SquareNone) return 0ULL;
  return mix_key(zobrist_keys[castling_index + color * 2 + side] ^
                 static_cast<uint64_t>(square));
}

inline uint64_t ep_key(const BoardState &pos) noexcept {
  const int ep = pos.ep_square;
  const int stm = pos.color;
  if (!is_valid_square(ep)) return 0ULL;

  const int victim_sq = ep + (stm ? Directions::North : Directions::South);
  if (!is_valid_square(victim_sq) || pos.board[ep] != Pieces::Blank ||
      pos.board[victim_sq] != Pieces::WPawn + (stm ^ 1)) {
    return 0ULL;
  }

  const uint64_t king_mask = pos.colors_bb[stm] & pos.pieces_bb[PieceTypes::King];
  if (!king_mask) return 0ULL;
  const int king_sq = get_lsb(king_mask);
  const uint64_t base_occ = pos.colors_bb[0] | pos.colors_bb[1];

  uint64_t takers = PAWN_ATK_SAFE(stm ^ 1, ep) & pos.colors_bb[stm] &
                    pos.pieces_bb[PieceTypes::Pawn];
  while (takers) {
    const int taker_sq = pop_lsb(takers);
    const uint64_t after_occ =
        (base_occ & ~(1ULL << taker_sq) & ~(1ULL << victim_sq)) | (1ULL << ep);
    if (!(attackers_to(pos, king_sq, stm ^ 1, after_occ) & ~(1ULL << victim_sq))) {
      return mix_key(zobrist_keys[ep_index] ^ static_cast<uint64_t>(get_file(ep)));
    }
  }
  return 0ULL;
}

inline void calculate(BoardState &pos) noexcept {
  uint64_t full_hash = 0ULL;
  uint64_t pawn_hash = 0ULL;
  pos.non_pawn_key[Colors::White] = 0ULL;
  pos.non_pawn_key[Colors::Black] = 0ULL;

  for (int sq = 0; sq < 64; ++sq) {
    const int pc = pos.board[sq];
    if (!pc) continue;
    const uint64_t k = zobrist_keys[get_zobrist_key(pc, sq)];
    full_hash ^= k;
    if (get_piece_type(pc) == PieceTypes::Pawn) {
      pawn_hash ^= k;
    } else {
      pos.non_pawn_key[get_color(pc)] ^= k;
    }
  }

  if (pos.color) full_hash ^= zobrist_keys[side_index];
  full_hash ^= ep_key(pos);
  for (int stm = 0; stm < 2; ++stm) {
    for (int wing = 0; wing < 2; ++wing) {
      full_hash ^= castling_key(stm, wing, pos.castling_squares[stm][wing]);
    }
  }

  pos.zobrist_key = full_hash;
  pos.pawn_key = pawn_hash;
}

constexpr inline int get_corrhist_index(uint64_t key) noexcept {
  return static_cast<int>(key % 16384ULL);
}

inline int64_t time_elapsed(std::chrono::steady_clock::time_point start_time) noexcept {
  const auto now = std::chrono::steady_clock::now();
  return std::chrono::duration_cast<std::chrono::milliseconds>(now - start_time).count();
}

inline bool OpeningBook::load_book(const std::string &path) {
  clear_book();
  return !path.empty() && load_polyglot_book(resolve_file_path(path));
}

int legal_movegen(const BoardState &position, Action *moves);

inline Action OpeningBook::probe_book(const BoardState &position, int min_weight) {
  if (!is_loaded()) return MoveNone;
  const auto it = book_positions->find(polyglot_key(position));
  if (it == book_positions->end()) return MoveNone;

  std::array<Action, MaxActions> legal{};
  const int legal_count = legal_movegen(position, legal.data());
  const int floor_weight = std::max(0, min_weight);

  std::vector<BookEntry> pool;
  uint64_t weight_sum = 0;
  for (const BookEntry &raw_entry : it->second) {
    if (raw_entry.weight == 0 || raw_entry.weight < floor_weight) continue;

    const int target_from = extract_from(raw_entry.move);
    const int target_to = extract_to(raw_entry.move);
    const bool target_is_promo = extract_type(raw_entry.move) == MoveTypes::Promotion;
    const int target_promo = extract_promo(raw_entry.move);

    for (int i = 0; i < legal_count; ++i) {
      const Action m = legal[i];
      if (extract_from(m) != target_from || extract_to(m) != target_to) continue;
      const bool m_is_promo = extract_type(m) == MoveTypes::Promotion;
      if (m_is_promo != target_is_promo) continue;
      if (m_is_promo && extract_promo(m) != target_promo) continue;

      pool.push_back(BookEntry{m, raw_entry.weight});
      weight_sum += raw_entry.weight;
      break;
    }
  }

  if (weight_sum == 0) return MoveNone;
  uint64_t pick =
      std::uniform_int_distribution<uint64_t>(0, weight_sum - 1)(Random::rd);
  for (const BookEntry &cand : pool) {
    if (pick < cand.weight) return cand.move;
    pick -= cand.weight;
  }
  return MoveNone;
}

inline bool OpeningBook::load_polyglot_book(const std::string &filename) {
  std::ifstream stream(filename, std::ios::binary | std::ios::ate);
  if (!stream || stream.tellg() <= 0 || stream.tellg() % 16 != 0) return false;
  stream.seekg(0);

  auto loaded = std::make_shared<Positions>();
  std::array<unsigned char, 16> record{};
  while (stream.read(reinterpret_cast<char *>(record.data()), record.size())) {
    const uint64_t key = detail_util::read_be64(record.data());
    if (key == 0ULL) continue;

    const unsigned raw_move = detail_util::read_be16(record.data() + 8);
    const uint16_t weight = detail_util::read_be16(record.data() + 10);
    const int src = (raw_move >> 6) & 63;
    const int dst = raw_move & 63;
    const int promo = (raw_move >> 12) & 7;
    if ((raw_move & 0x8000U) != 0U || promo > 4 || src == dst) {
      clear_book();
      return false;
    }

    const Action move = promo ? pack_move_promo(src, dst, promo - 1)
                              : pack_move(src, dst, MoveTypes::Normal);
    (*loaded)[key].push_back(BookEntry{move, weight});
  }

  if (!stream.eof()) {
    clear_book();
    return false;
  }
  book_positions = std::move(loaded);
  return is_loaded();
}

inline uint64_t OpeningBook::polyglot_key(const BoardState &pos) {
  uint64_t key = 0ULL;

  for (int sq = 0; sq < 64; ++sq) {
    const int pc = pos.board[sq];
    if (!pc) continue;
    const int ptype = get_piece_type(pc);
    if (ptype >= PieceTypes::Pawn && ptype <= PieceTypes::King) {
      const int poly_piece =
          (ptype - 1) * 2 + (get_color(pc) == Colors::White ? 1 : 0);
      key ^= poly_random[64 * poly_piece + sq];
    }
  }

  static constexpr int kCastleOffsets[2][2] = {{769, 768}, {771, 770}};
  for (int c = 0; c < 2; ++c) {
    for (int s = 0; s < 2; ++s) {
      if (pos.castling_squares[c][s] != SquareNone) {
        key ^= poly_random[kCastleOffsets[c][s]];
      }
    }
  }

  if (pos.ep_square != SquareNone && pos.ep_square < 64) {
    const int ep_file = get_file(pos.ep_square);
    const int ep_rank = get_rank(pos.ep_square);
    if (ep_rank == (pos.color == Colors::White ? 5 : 2)) {
      const uint64_t ep_takers =
          PAWN_ATK_SAFE(pos.color ^ 1, pos.ep_square) &
          pos.colors_bb[pos.color] & pos.pieces_bb[PieceTypes::Pawn];
      if (ep_takers != 0ULL) key ^= poly_random[772 + ep_file];
    }
  }

  if (pos.color == Colors::White) key ^= poly_random[780];
  return key;
}

inline void TimeManager::initialize(uint64_t time_left, uint64_t increment,
                                    int moves_to_go, uint32_t game_move) noexcept {
  const uint64_t reserve = std::min<uint64_t>(50ULL, time_left / 10ULL);
  const uint64_t usable =
      (time_left > reserve) ? (time_left - reserve) : std::max<uint64_t>(1ULL, time_left / 2ULL);

  if (moves_to_go > 0) {
    const uint64_t rem = static_cast<uint64_t>(moves_to_go);
    const uint64_t pool = usable + increment * (rem - 1ULL);
    allocated_time = std::min(pool / rem, usable / 2ULL);
  } else {
    const double phase_scale =
        (game_move < 20U) ? 0.8 : ((game_move < 40U) ? 1.1 : 1.0);
    const double base_budget = (usable / 22.0 + increment * 0.75) * phase_scale;
    allocated_time = std::max<uint64_t>(1ULL, static_cast<uint64_t>(base_budget));
  }

  allocated_time = std::min(allocated_time, usable);
  max_time = std::max(allocated_time, std::min(allocated_time * 3ULL, usable * 8ULL / 10ULL));
  hard_limit = std::min(max_time, usable);
  soft_limit = std::min(allocated_time, hard_limit);
}

inline bool TimeManager::should_stop(uint64_t elapsed, bool best_move_stable,
                                     bool is_movetime) noexcept {
  if (elapsed >= hard_limit) return true;
  if (is_movetime) return false;
  if (elapsed >= soft_limit) {
    if (best_move_stable) return true;
    soft_limit = std::min(soft_limit + allocated_time / 8ULL, hard_limit);
  }
  return false;
}

inline void adjust_soft_limit(ThreadInfo &ti, uint64_t best_move_nodes,
                              int bm_stability) noexcept {
  const uint64_t visited = ti.nodes.load(std::memory_order_relaxed);
  if (visited == 0) return;

  const double bm_share = static_cast<double>(best_move_nodes) / visited;
  const double node_scale =
      (static_cast<double>(NodeTmFactor1) / 100.0 - bm_share) * NodeTmFactor2 / 100.0;
  const double stability_scale = BmFactor1 / 100.0f - (bm_stability * 0.06);
  const double volume_scale =
      (visited > 100000ULL) ? std::min(2.0, visited / 50000.0) : 1.0;

  const long double scaled_time =
      static_cast<long double>(ti.original_opt) * node_scale * stability_scale * volume_scale;
  const uint64_t clamped_time = static_cast<uint64_t>(std::clamp<long double>(
      scaled_time, 1.0L, static_cast<long double>(ti.max_time)));
  ti.opt_time = std::min<uint64_t>(clamped_time, ti.max_time);
}

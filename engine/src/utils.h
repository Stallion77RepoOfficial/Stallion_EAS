#pragma once
#include "bitboard.h"
#include "defs.h"
#include "params.h"
#include "poly_random.h"
#include <algorithm>
#include <atomic>
#include <cctype>
#include <condition_variable>
#include <cstdarg>
#include <fstream>
#include <iostream>
#include <mutex>
#include <stdio.h>
#include <string>
#include <thread>
#include <unordered_map>
#include <vector>

using std::array;

typedef unsigned __int128 uint128_t;

struct BookEntry {
  uint64_t key;
  Action move;
  uint16_t weight;
  uint32_t learn;
};

class OpeningBook {
private:
  std::unordered_map<uint64_t, std::vector<BookEntry>> book_positions;
  bool book_loaded;
  std::string book_path;

public:
  OpeningBook() : book_loaded(false) {}

  bool load_book(const std::string &path);
  Action probe_book(uint64_t position_key, int min_weight = 1);
  bool is_loaded() const { return book_loaded; }
  void clear_book() {
    book_positions.clear();
    book_loaded = false;
  }

  uint64_t polyglot_key(const BoardState &pos);

private:
  bool load_polyglot_book(const std::string &filename);
};

struct TimeManager {
  uint64_t allocated_time;
  uint64_t max_time;
  uint64_t panic_time;
  uint64_t soft_limit;
  uint64_t hard_limit;
  bool use_panic_mode;
  int move_number;
  int time_stability;
  uint64_t nodes_searched;

  TimeManager()
      : allocated_time(0), max_time(0), panic_time(0), soft_limit(0),
        hard_limit(0), use_panic_mode(false), move_number(0), time_stability(0),
        nodes_searched(0) {}

  void initialize(uint64_t time_left, uint64_t increment, int moves_to_go,
                  int game_move);
  bool should_stop(uint64_t elapsed, bool best_move_stable, bool in_trouble);
  void update_node_count(uint64_t nodes);
};

struct ThreadInfoBase {
  uint16_t thread_id = 0;
  std::array<StateRecord, MaxGameLen> game_hist;
  uint16_t game_ply;
  uint16_t search_ply;

  std::vector<RootAction> root_moves;

  std::chrono::steady_clock::time_point start_time;

  int seldepth;

  uint64_t max_time;
  uint64_t opt_time;
  uint64_t original_opt;

  uint16_t time_checks;

  MultiArray<int16_t, 14, 64> HistoryScores;
  MultiArray<int16_t, 14, 64, 14, 64> ContHistScores;
  MultiArray<int16_t, 14, 64> CapHistScores;
  MultiArray<int16_t, 2, 16384> PawnCorrHist;
  MultiArray<int16_t, 2, 2, 16384> NonPawnCorrHist;
  MultiArray<Action, MaxSearchPly + 1, 2> KillerMoves;
  MultiArray<Action, 14, 64> CounterMoves;

  uint8_t current_iter;

  uint16_t multipv = 1;
  uint16_t multipv_index;
  uint16_t variety = 150;

  Action excluded_move;
  std::array<Action, MaxActions> best_moves;
  std::array<int, MaxActions> best_scores;

  int max_iter_depth = MaxSearchPly;
  uint64_t max_nodes_searched = UINT64_MAX / 2;
  uint64_t opt_nodes_searched = UINT64_MAX / 2;

  bool doing_datagen = false;
  bool datagen_stop = false;

  bool is_human = false;

  int human_value_margin = 0;
  int human_noise_sigma = 0;
  int human_depth_limit = 0;
  int human_elo = 3401;

  std::array<Action, MaxSearchPly * MaxSearchPly> pv;
  std::array<int, 5> pv_material;

  BoardState position;

  uint8_t searches = 0;
  uint8_t phase;
  bool infinite_search = false;

  float opening_aggressiveness = 1.50f;
  float middlegame_aggressiveness = 1.50f;
  float late_middlegame_aggressiveness = 1.50f;
  float endgame_aggressiveness = 1.50f;

  int sacrifice_lookahead = 1;
  int sacrifice_lookahead_time_multiplier = 200;
  int sacrifice_lookahead_aggressiveness = 150;

  bool attack_mode = false;
  int last_root_eval = 0;
  int prev_root_eval = 0;
  uint8_t target_phase = 0;
  std::array<uint8_t, 5> phase_hit_counts{};
  int root_completed_depth = 0;

  int phase_confirm_hits = 2;
  int sacrifice_enter_cp = 250;
  int sacrifice_exit_cp = 170;
  int sacrifice_drop_threshold = 120;
  int late_phase_material = 4200;
  int endgame_material = 3000;
  int mid_recover_material = 4500;
  int end_recover_material = 3300;
  int opening_min_ply = 20;

  uint64_t max_move_time = 0;
  uint64_t move_overhead = 100;

  uint16_t max_depth = 0;

  uint64_t max_nodes = 0;

  bool use_ponder = true;
  bool pondering = false;
  Action ponder_move = MoveNone;
  bool ponder_hit = false;
  std::chrono::steady_clock::time_point ponder_start_time;

  bool use_syzygy = false;
  std::string syzygy_path;

  TimeManager time_manager;
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
  std::atomic<bool> searching{false};

  ThreadInfo() = default;
  ThreadInfo(const ThreadInfo &other) : ThreadInfoBase(other) {
    nodes.store(other.nodes.load());
    searching.store(other.searching.load());
  }
  ThreadInfo &operator=(const ThreadInfo &other) {
    if (this != &other) {
      ThreadInfoBase::operator=(other);
      nodes.store(other.nodes.load());
      searching.store(other.searching.load());
    }
    return *this;
  }
};

RootAction *find_root_move(ThreadInfo &thread_info, Action move) {
  for (size_t i = 0; i < thread_info.root_moves.size(); i++) {
    if (thread_info.root_moves[i].move == move)
      return &thread_info.root_moves[i];
  }
  return nullptr;
}

struct ThreadData {
  std::vector<ThreadInfo> thread_infos;
  std::vector<std::thread> threads;
  int num_threads = 1;
  std::atomic<bool> stop{true};
  std::atomic<bool> terminate{false};
  std::atomic<bool> is_frc{false};
  std::mutex data_mutex;
  std::atomic<uint64_t> tb_hits{0};
  std::atomic<uint64_t> tb_fails{0};
  std::mutex search_mutex;
  std::condition_variable search_cv;
};

ThreadData thread_data;

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

inline void safe_fflush() {
  std::lock_guard<std::mutex> lg(get_print_mutex());
  fflush(stdout);
}

uint64_t TT_size = (1 << 20);
std::vector<TTBucket> TT(TT_size);

std::atomic<bool> TT_resizing{false};

void new_game(ThreadInfo &thread_info, std::vector<TTBucket> &TT) {

  {
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
    TT.assign(TT_size, TTBucket{});
    TT_resizing.store(false);
    thread_info.searches = 0;
  }
}

uint32_t get_hash_low_bits(uint64_t hash) {
  return static_cast<uint32_t>(hash);
}

int32_t score_to_tt(int32_t score, int32_t ply) {
  if (score == ScoreNone)
    return ScoreNone;
  if (score > MateScore)
    return score + ply;
  if (score < -MateScore)
    return score - ply;
  return score;
}

int32_t score_from_tt(int32_t score, int32_t ply) {
  if (score == ScoreNone)
    return ScoreNone;
  if (score > MateScore)
    return score - ply;
  if (score < -MateScore)
    return score + ply;
  return score;
}

void resize_TT(int size) {
  std::lock_guard<std::mutex> lock(thread_data.data_mutex);

  TT_resizing.store(true, std::memory_order_release);
  TT_size = static_cast<uint64_t>(size) * 1024 * 1024 / sizeof(TTBucket);
  TT.assign(TT_size, TTBucket{});
  TT_resizing.store(false, std::memory_order_release);
}

uint64_t hash_to_idx(uint64_t hash) {
  return (uint128_t(hash) * uint128_t(TT_size)) >> 64;
}

inline uint64_t safe_TT_size() {
  std::lock_guard<std::mutex> lg(thread_data.data_mutex);
  if (TT_resizing.load(std::memory_order_acquire))
    return 0;
  return TT.size();
}

inline void safe_TT_prefetch(uint64_t hash) {
  if (TT_resizing.load(std::memory_order_acquire))
    return;
  std::lock_guard<std::mutex> lg(thread_data.data_mutex);
  if (TT.size() == 0)
    return;
  uint64_t size = TT.size();
  uint64_t idx = (uint128_t(hash) * uint128_t(size)) >> 64;
  if (idx >= size)
    return;
  __builtin_prefetch(&TT[static_cast<size_t>(idx)], 0, 1);
}

int entry_quality(TTEntry &entry, int searches) {
  int age_diff = (MaxAge + searches - entry.get_age()) % MaxAge;
  return entry.depth - age_diff * 8;
}

TTEntry probe_entry(uint64_t hash, bool &hit, uint8_t searches,
                    std::vector<TTBucket> &TT) {

  if (TT_resizing.load(std::memory_order_acquire)) {
    static thread_local TTBucket fallback_bucket_inline;
    hit = false;
    fallback_bucket_inline.entries[0].age_bound =
        (searches << 2) | fallback_bucket_inline.entries[0].get_type();
    return fallback_bucket_inline.entries[0];
  }

  uint64_t size = TT.size();
  if (size == 0) {
    static thread_local TTBucket fallback_bucket;
    hit = false;
    fallback_bucket.entries[0].age_bound =
        (searches << 2) | fallback_bucket.entries[0].get_type();
    return fallback_bucket.entries[0];
  }

  uint32_t zobrist_key = get_hash_low_bits(hash);
  uint64_t idx = (uint128_t(hash) * uint128_t(size)) >> 64;

  if (idx >= size) {
    static thread_local TTBucket fallback_bucket2;
    hit = false;
    fallback_bucket2.entries[0].age_bound =
        (searches << 2) | fallback_bucket2.entries[0].get_type();
    return fallback_bucket2.entries[0];
  }

  auto &bucket = TT[idx];
  __builtin_prefetch(&bucket, 0, 1);
  auto &entries = bucket.entries;

  for (int i = 0; i < BucketEntries; i++) {
    bool empty =
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
    int this_quality = entry_quality(entries[i], searches);
    if (this_quality < worst_quality) {
      worst = &(entries[i]);
      worst_quality = this_quality;
    }
  }

  hit = false;
  return *worst;
}

void insert_entry(TTEntry &entry, uint64_t hash, int depth, Action best_move,
                  int32_t static_eval, int32_t score, uint8_t bound_type,
                  uint8_t searches) {

  uint32_t zobrist_key = get_hash_low_bits(hash);

  if (TT_resizing.load(std::memory_order_acquire)) {
    static thread_local TTBucket fallback_bucket;
    TTEntry &fe = fallback_bucket.entries[0];
    if (best_move != MoveNone || zobrist_key != fe.position_key)
      fe.best_move = best_move;
    if (fe.position_key == zobrist_key && (bound_type != EntryTypes::Exact) &&
        fe.depth > depth + 4)
      return;
    fe.position_key = zobrist_key;
    fe.depth = static_cast<uint8_t>(depth);
    fe.static_eval = static_eval;
    fe.score = score;
    fe.age_bound = (searches << 2) | bound_type;
    return;
  }

  uint64_t size = TT.size();
  if (size == 0)
    return;

  uint64_t idx = (uint128_t(hash) * uint128_t(size)) >> 64;
  if (idx >= size)
    idx = idx % size;

  auto &bucket = TT[idx];
  auto &entries = bucket.entries;

  for (int i = 0; i < BucketEntries; i++) {
    bool empty =
        entries[i].score == 0 && entries[i].get_type() == EntryTypes::None;
    if (empty || entries[i].position_key == zobrist_key) {
      TTEntry &e = entries[i];
      if (best_move != MoveNone || zobrist_key != e.position_key)
        e.best_move = best_move;
      if (e.position_key == zobrist_key && (bound_type != EntryTypes::Exact) &&
          e.depth > depth + 4)
        return;
      e.position_key = zobrist_key;
      e.depth = static_cast<uint8_t>(depth);
      e.static_eval = static_eval;
      e.score = score;
      e.age_bound = (searches << 2) | bound_type;
      return;
    }
  }

  TTEntry *worst = &entries[0];
  int worst_q = entry_quality(*worst, searches);
  for (int i = 1; i < BucketEntries; i++) {
    int q = entry_quality(entries[i], searches);
    if (q < worst_q) {
      worst = &entries[i];
      worst_q = q;
    }
  }

  TTEntry &we = *worst;
  we.position_key = zobrist_key;
  we.depth = static_cast<uint8_t>(depth);
  we.static_eval = static_eval;
  we.score = score;
  we.age_bound = (searches << 2) | bound_type;
  if (best_move != MoveNone)
    we.best_move = best_move;
}

void calculate(BoardState &position) {

  uint64_t hash = 0;
  uint64_t pawn_hash = 0;
  position.non_pawn_key[Colors::White] = 0,
  position.non_pawn_key[Colors::Black] = 0;

  for (int indx = 0; indx < 64; indx++) {
    int piece = position.board[indx];
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
  if (position.ep_square != 255) {
    hash ^= zobrist_keys[ep_index];
  }
  for (int indx = castling_index; indx < 778; indx++) {
    if (position.castling_squares[indx > 775][(indx & 1)] != SquareNone) {
      hash ^= zobrist_keys[indx];
    }
  }
  position.zobrist_key = hash;
  position.pawn_key = pawn_hash;
}

int get_corrhist_index(uint64_t key) { return key % 16384; }

int64_t time_elapsed(std::chrono::steady_clock::time_point start_time) {

  auto now = std::chrono::steady_clock::now();
  return std::chrono::duration_cast<std::chrono::milliseconds>(now - start_time)
      .count();
}

class Barrier {
public:
  Barrier(int64_t expected) { reset(expected); }

  auto reset(int64_t expected) -> void {

    m_total.store(expected, std::memory_order_seq_cst);
    m_current.store(expected, std::memory_order_seq_cst);
  }

  auto arrive_and_wait() {
    std::unique_lock lock{wait_mutex};

    const auto current = --m_current;

    if (current > 0) {
      const auto phase = m_phase.load(std::memory_order_relaxed);
      wait_signal.wait(lock, [this, phase] {
        return (phase - m_phase.load(std::memory_order_acquire)) < 0 ||
               m_cancel.load(std::memory_order_acquire);
      });

      if (m_cancel.load(std::memory_order_acquire))
        return;
    } else {
      const auto total = m_total.load(std::memory_order_acquire);
      m_current.store(total, std::memory_order_release);

      m_phase++;

      wait_signal.notify_all();
    }
  }

  auto cancel() {
    m_cancel.store(true, std::memory_order_release);
    wait_signal.notify_all();
  }

  auto clear_cancel() { m_cancel.store(false, std::memory_order_release); }

private:
  std::atomic<int64_t> m_total{};
  std::atomic<int64_t> m_current{};
  std::atomic<int64_t> m_phase{};

  std::atomic<bool> m_cancel{};

  std::mutex wait_mutex{};
  std::condition_variable wait_signal{};
};

Barrier reset_barrier{1};
Barrier idle_barrier{1};
Barrier search_end_barrier{1};

bool OpeningBook::load_book(const std::string &path) {
  book_path = path;
  if (path.empty()) {
    book_loaded = false;
    return false;
  }

  if (path.size() >= 4) {
    auto ext = path.substr(path.size() - 4);
    std::transform(ext.begin(), ext.end(), ext.begin(), ::tolower);

    if (ext != ".bin" && ext != ".bok" && ext != "book") {

      safe_print_cerr(std::string("Unsupported opening book format: ") + path +
                      std::string(" (supported: .bin/.book)"));
      book_loaded = false;
      return false;
    }
  }

  if (path.find(".bin") != std::string::npos ||
      path.find(".book") != std::string::npos) {
    return load_polyglot_book(path);
  }

  return load_polyglot_book(path);
}

Action OpeningBook::probe_book(uint64_t position_key, int min_weight) {
  if (!book_loaded || book_positions.empty())
    return MoveNone;
  auto it = book_positions.find(position_key);
  if (it == book_positions.end()) {
    return MoveNone;
  }

  const auto &entries = it->second;
  if (entries.empty()) {
    return MoveNone;
  }

  std::vector<const BookEntry *> filtered;
  filtered.reserve(entries.size());
  for (auto &e : entries)
    if (e.weight >= (uint16_t)min_weight)
      filtered.push_back(&e);
  if (filtered.empty())
    return MoveNone;

  uint32_t total_weight = 0;
  for (const auto *entry : filtered)
    total_weight += entry->weight;

  if (total_weight == 0) {
    return entries[0].move;
  }

  std::mt19937 rng(Random::rd());
  uint32_t random_value =
      std::uniform_int_distribution<uint32_t>(0, total_weight - 1)(rng);
  uint32_t current_weight = 0;

  for (const auto *entry : filtered) {
    current_weight += entry->weight;
    if (random_value < current_weight)
      return entry->move;
  }
  return filtered[0]->move;
}

bool OpeningBook::load_polyglot_book(const std::string &filename) {
  std::ifstream file(filename, std::ios::binary);
  if (!file.is_open()) {
    return false;
  }

  book_positions.clear();

  struct PolyglotEntry {
    uint64_t key;
    uint16_t move;
    uint16_t weight;
    uint32_t learn;
  };

  PolyglotEntry entry;
  while (file.read(reinterpret_cast<char *>(&entry), sizeof(entry))) {

    uint64_t key = __builtin_bswap64(entry.key);
    uint16_t move = __builtin_bswap16(entry.move);
    uint16_t weight = __builtin_bswap16(entry.weight);
    uint32_t learn = __builtin_bswap32(entry.learn);

    int from = ((move >> 6) & 0x3F);
    int to = (move & 0x3F);
    int promotion = ((move >> 12) & 0x7);

    Action internal_move;
    if (promotion == 0) {
      internal_move = pack_move(from, to, MoveTypes::Normal);
    } else {

      uint8_t promo_piece = promotion - 1;
      internal_move = pack_move_promo(from, to, promo_piece);
    }

    BookEntry book_entry;
    book_entry.key = key;
    book_entry.move = internal_move;
    book_entry.weight = weight;
    book_entry.learn = learn;

    book_positions[key].push_back(book_entry);
  }

  file.close();
  book_loaded = !book_positions.empty();
  return book_loaded;
}

uint64_t OpeningBook::polyglot_key(const BoardState &pos) {
  uint64_t key = 0ULL;

  for (int sq = 0; sq < 64; ++sq) {
    int piece = pos.board[sq];
    if (!piece)
      continue;
    int color = get_color(piece);
    int ptype = get_piece_type(piece);
    int poly_index = -1;

    switch (ptype) {
    case PieceTypes::Pawn:
      poly_index = color == Colors::White ? 0 : 6;
      break;
    case PieceTypes::Knight:
      poly_index = color == Colors::White ? 1 : 7;
      break;
    case PieceTypes::Bishop:
      poly_index = color == Colors::White ? 2 : 8;
      break;
    case PieceTypes::Rook:
      poly_index = color == Colors::White ? 3 : 9;
      break;
    case PieceTypes::Queen:
      poly_index = color == Colors::White ? 4 : 10;
      break;
    case PieceTypes::King:
      poly_index = color == Colors::White ? 5 : 11;
      break;
    default:
      break;
    }
    if (poly_index >= 0)
      key ^= poly_random[64 * poly_index + sq];
  }

  if (pos.castling_squares[Colors::White][Sides::Kingside] != SquareNone)
    key ^= poly_random[768 + 0];
  if (pos.castling_squares[Colors::White][Sides::Queenside] != SquareNone)
    key ^= poly_random[768 + 1];
  if (pos.castling_squares[Colors::Black][Sides::Kingside] != SquareNone)
    key ^= poly_random[768 + 2];
  if (pos.castling_squares[Colors::Black][Sides::Queenside] != SquareNone)
    key ^= poly_random[768 + 3];

  if (pos.ep_square != SquareNone && pos.ep_square < 64) {
    int ep_file = pos.ep_square % 8;
    int ep_rank = pos.ep_square / 8;
    bool ep_valid = false;
    if (pos.color == Colors::White && ep_rank == 5) {

      uint64_t rank5 = Ranks[4];
      if (ep_file > 0) {
        int sq = (ep_rank - 1) * 8 + (ep_file - 1);
        if (pos.board[sq] == Pieces::WPawn)
          ep_valid = true;
      }
      if (!ep_valid && ep_file < 7) {
        int sq = (ep_rank - 1) * 8 + (ep_file + 1);
        if (pos.board[sq] == Pieces::WPawn)
          ep_valid = true;
      }
    } else if (pos.color == Colors::Black && ep_rank == 2) {

      if (ep_file > 0) {
        int sq = (ep_rank + 1) * 8 + (ep_file - 1);
        if (pos.board[sq] == Pieces::BPawn)
          ep_valid = true;
      }
      if (!ep_valid && ep_file < 7) {
        int sq = (ep_rank + 1) * 8 + (ep_file + 1);
        if (pos.board[sq] == Pieces::BPawn)
          ep_valid = true;
      }
    }
    if (ep_valid)
      key ^= poly_random[768 + 4 + ep_file];
  }
  if (pos.color == Colors::Black)
    key ^= poly_random[780];
  return key;
}

void TimeManager::initialize(uint64_t time_left, uint64_t increment,
                             int moves_to_go, int game_move) {
  move_number = game_move;

  if (moves_to_go > 0) {

    allocated_time =
        (time_left +
         increment * static_cast<uint64_t>(std::max(moves_to_go - 1, 0))) /
        static_cast<uint64_t>(std::max(moves_to_go, 1));
    allocated_time = std::min(allocated_time, time_left / 3);
  } else {

    double time_factor = 1.0;

    if (game_move < 20)
      time_factor = 0.8;
    else if (game_move < 40)
      time_factor = 1.2;
    else
      time_factor = 1.0;

    allocated_time = std::max<uint64_t>(
        1, static_cast<uint64_t>((time_left / 8.0 + increment * 0.5) *
                                 time_factor));
  }

  uint64_t reserved = std::max<uint64_t>(100, time_left / 20);
  if (time_left > reserved)
    time_left -= reserved;
  else
    time_left = 1;

  max_time = std::min<uint64_t>(allocated_time * 5, time_left / 2);
  panic_time = std::min<uint64_t>(allocated_time * 2, time_left / 4);
  soft_limit = allocated_time;
  hard_limit = max_time;

  use_panic_mode = false;
  time_stability = 0;
}

bool TimeManager::should_stop(uint64_t elapsed, bool best_move_stable,
                              bool in_trouble) {

  if (elapsed >= hard_limit)
    return true;

  if (in_trouble && !use_panic_mode && elapsed < panic_time) {
    use_panic_mode = true;
    soft_limit = panic_time;
  }

  if (elapsed >= soft_limit) {
    if (best_move_stable || use_panic_mode) {
      return true;
    }

    soft_limit = std::min(soft_limit + allocated_time / 8, hard_limit);
  }

  return elapsed >= hard_limit;
}

void TimeManager::update_node_count(uint64_t nodes) { nodes_searched = nodes; }

void adjust_soft_limit(ThreadInfo &thread_info, uint64_t best_move_nodes,
                       int bm_stability, int best_score) noexcept {
  uint64_t node_count = thread_info.nodes.load();
  if (node_count == 0)
    return;
  double fract = static_cast<double>(best_move_nodes) / node_count;
  double factor = (static_cast<double>(NodeTmFactor1) / 100.0 - fract) *
                  NodeTmFactor2 / 100.0;
  double bm_factor = BmFactor1 / 100.0f - (bm_stability * 0.06);

  if (thread_info.time_manager.use_panic_mode) {
    factor *= 1.5;
  }

  double win_rate = 1.0 / (1.0 + std::exp(WDL_A * best_score));

  double closeness = 1.0 - std::abs(win_rate - 0.5) * 2.0;

  double wdl_factor = 1.0 + closeness * 0.5;

  double node_factor = 1.0;
  if (node_count > 100000) {
    node_factor = std::min(2.0, node_count / 50000.0);
  }

  uint64_t new_time =
      thread_info.original_opt * factor * bm_factor * node_factor * wdl_factor;
  thread_info.opt_time = std::min<uint64_t>(new_time, thread_info.max_time);
}

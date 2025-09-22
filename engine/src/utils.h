#pragma once
#include "defs.h"
#include "nnue.h"
#include "params.h"
#include <condition_variable>
#include <mutex>
#include <stdio.h>
#include <thread>
#include <vector>
#include <atomic>
#include <string>
#include <fstream>
#include <unordered_map>
#include <algorithm>
#include <iostream>
#include <cctype>
#include "poly_random.h"
#include <cstdarg>

using std::array;

typedef unsigned __int128 uint128_t;

// Opening Book structure
struct BookEntry {
  uint64_t key;
  Move move;
  uint16_t weight;
  uint32_t learn;
};

class OpeningBook {
private:
  std::unordered_map<uint64_t, std::vector<BookEntry>> book_positions;
  bool book_loaded;
  std::string book_path;
  // Official Polyglot random array (781 entries) defined in poly_random.h
  
public:
  OpeningBook() : book_loaded(false) {}
  
  bool load_book(const std::string& path);
  Move probe_book(uint64_t position_key, int min_weight = 1);
  bool is_loaded() const { return book_loaded; }
  void clear_book() { book_positions.clear(); book_loaded = false; }
  // Expose key computation for caller to query
  uint64_t polyglot_key(const Position& pos);
  
private:
  bool load_polyglot_book(const std::string& filename);
  // Only Polyglot (.bin/.book) supported natively.
};

// Enhanced time management with multiple factors
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
  
  TimeManager() : allocated_time(0), max_time(0), panic_time(0),
                  soft_limit(0), hard_limit(0), use_panic_mode(false),
                  move_number(0), time_stability(0), nodes_searched(0) {}

  void initialize(uint64_t time_left, uint64_t increment, int moves_to_go, int game_move);
  bool should_stop(uint64_t elapsed, bool best_move_stable, bool in_trouble);
  void update_node_count(uint64_t nodes);
};

struct ThreadInfoBase {
  uint16_t thread_id = 0; // ID of the thread
  std::array<GameHistory, GameSize>
      game_hist;       // all positions from earlier in the game
  uint16_t game_ply;   // how far we're into the game
  uint16_t search_ply; // depth that we are in the search tree

  std::vector<RootMoveInfo> root_moves;

  std::chrono::steady_clock::time_point start_time; // Start time of the search

  int seldepth;

  uint64_t max_time;
  uint64_t opt_time;
  uint64_t original_opt;

  uint16_t time_checks;

  NNUE_State nnue_state;

  MultiArray<int16_t, 14, 64> HistoryScores;
  MultiArray<int16_t, 14, 64, 14, 64> ContHistScores;
  MultiArray<int16_t, 14, 64> CapHistScores;
  MultiArray<int16_t, 2, 16384> PawnCorrHist;
  MultiArray<int16_t, 2, 2, 16384> NonPawnCorrHist;
  std::array<Move, MaxSearchDepth + 1> KillerMoves;

  uint8_t current_iter;
  // MultiPV: internal invariant >=1. Previously 0 meant "treat as 1"; now we
  // store a proper value (default 1) and enforce min=1 at UCI boundary.
  uint16_t multipv = 1;
  uint16_t multipv_index;
  uint16_t variety = 150;  // Tal-style higher default move diversity.

  Move excluded_move;
  std::array<Move, ListSize> best_moves;
  std::array<int, ListSize> best_scores;

  int max_iter_depth = MaxSearchDepth;
  uint64_t max_nodes_searched = UINT64_MAX / 2;
  uint64_t opt_nodes_searched = UINT64_MAX / 2;

  bool doing_datagen = false;
  bool datagen_stop = false;

  bool is_human = false;
  // Legacy cp_loss removed; all weakening handled via human_* parameters.
  // Human / limit strength (Stockfish-like) derived parameters
  // These are computed from UCI_Elo when UCI_LimitStrength (is_human) is enabled.
  int human_value_margin = 0;    // Centipawn window within which alternative moves are considered
  int human_noise_sigma = 0;     // Approximate amplitude of evaluation noise injected (cp)
  int human_depth_limit = 0;     // Soft depth cap derived from Elo (applied via max_depth adjustment)
  int human_elo = 3401;          // Cached last set UCI_Elo value

  std::array<Move, MaxSearchDepth * MaxSearchDepth> pv;
  std::array<int, 5> pv_material;

  Position position;

  uint8_t searches = 0;
  uint8_t phase;
  bool infinite_search = false;  // true if current search is go infinite mode

  // Oyun aşamasına göre agresiflik faktörleri
  // UCI defaults: 150 -> represented internally as value/100.f
  float opening_aggressiveness = 1.50f;
  float middlegame_aggressiveness = 1.50f;
  float late_middlegame_aggressiveness = 1.50f;
  float endgame_aggressiveness = 1.50f;

  // (Legacy) Sacrifice lookahead parametreleri - ileride kaldırılabilir
  // Sacrifice lookahead: 0 = disabled, 1 = one-ply lookahead. Keep small to
  // limit extra search cost. Default 1.
  int sacrifice_lookahead = 1;
  int sacrifice_lookahead_time_multiplier = 200;
  int sacrifice_lookahead_aggressiveness = 150;

  // Yeni faz / saldırı modu durumları
  bool attack_mode = false;              // Sacrifice fazını NN değişimi yerine arama profili olarak uygula
  int last_root_eval = 0;                // Son tamamlanan kök iterasyon skoru (cp)
  int prev_root_eval = 0;                // Bir önceki kök skoru (cp)
  uint8_t target_phase = 0;              // Hedeflenen faz (histerezis doğrulana kadar phase değiştirmez)
  std::array<uint8_t, 5> phase_hit_counts{}; // Art arda faz aday sayacı
  int root_completed_depth = 0;          // Son tamamlanan arama derinliği

  // Histerezis eşiği (hard-coded şimdilik; ileride UCI opsiyona açılabilir)
  int phase_confirm_hits = 2;            // Bir faza geçmek için gereken ardışık doğrulama sayısı
  int sacrifice_enter_cp = 250;          // attack_mode giriş eşiği
  int sacrifice_exit_cp  = 170;          // attack_mode çıkış eşiği
  int sacrifice_drop_threshold = 120;    // attack_mode'dan çıkmak için skor düşüşü
  int late_phase_material = 4200;        // Middle->Late materyal eşiği (≤)
  int endgame_material = 3000;           // Late->End materyal eşiği (≤)
  int mid_recover_material = 4500;       // Late'den geri Middle'a çıkış tamponu (> )
  int end_recover_material = 3300;       // Endgame'den Late'e geri dönüş tamponu (> )
  int opening_min_ply = 20;              // Opening'ten çıkış için minimum hamle sayısı

  // Zaman yönetimi için yeni seçenekler
  uint64_t max_move_time = 0;       // Maksimum hamle süresi (ms), 0 ile kapatılır
  uint64_t move_overhead = 30;      // Her hamle için düşülecek ek yük (ms)

  // UCI: MaxDepth limit (0 = disabled)
  uint16_t max_depth = 0;           // Maximum search depth limit from UCI option
  // UCI: MaxNodes limit (0 = disabled)
  uint64_t max_nodes = 0;           // Maximum nodes limit from UCI option

  // Ponder mod desteği - Enhanced
  bool pondering = false;           // Ponderlama modunda mı?
  Move ponder_move = MoveNone;      // Ponder için tahmin edilen rakip hamlesi
  bool ponder_hit = false;          // Ponder hit durumu
  std::chrono::steady_clock::time_point ponder_start_time; // Ponder başlangıç zamanı

  // Syzygy (Syzygy tablebases) integration
  bool use_syzygy = false;           // Syzygy kullanımını etkinleştir
  std::string syzygy_path;           // Syzygy TB dosya yolu

  // Enhanced Time Management
  TimeManager time_manager;
  bool best_move_stable = false;
  int stability_counter = 0;
  Move previous_best_move = MoveNone;

  // Opening Book Integration
  OpeningBook opening_book;
  bool use_opening_book = false;  // Reverted per request
  std::string book_path;
  int book_depth_limit = 0;        // Tal-style: search book deeper
  // Advanced / Stockfish-inspired additions
  int syzygy_probe_depth = 6;       // Minimum remaining depth to allow mid-node probing
  int syzygy_probe_limit = 6;       // Max pieces for mid-node probes (<=7 typical)
  bool syzygy_50_move_rule = true;  // Honor 50-move rule in TB scores
  int book_min_weight = 0;          // Tal-style: allow even rare/dubious book lines
  int ponder_time_factor = 200;     // Tal-style: extend more time after successful ponder
  // Book variety memory
  std::array<uint64_t, 32> recent_book_keys{}; // circular buffer
  uint8_t recent_book_head = 0;
  // auto_ponder_restart removed (was experimental) – GUI kontrolüne bırakıldı
};

struct ThreadInfo : ThreadInfoBase {
  std::atomic<uint64_t> nodes{0}; // Total nodes searched so far this search
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


RootMoveInfo *find_root_move(ThreadInfo &thread_info, Move move) {
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
  std::mutex data_mutex; // Thread güvenliği için mutex ekle
  std::atomic<uint64_t> tb_hits{0};
  std::atomic<uint64_t> tb_fails{0};
  std::mutex search_mutex;
  std::condition_variable search_cv;
};

ThreadData thread_data;

// Global safe printing helpers: use a single static mutex to serialize
// all stdout/stderr output across threads. This avoids races between
// std::iostreams and C stdio that can cause crashes under ASAN.
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

// Thread-safe fflush wrapper uses the same print mutex to avoid races
inline void safe_fflush() {
  std::lock_guard<std::mutex> lg(get_print_mutex());
  fflush(stdout);
}

uint64_t TT_size = (1 << 20);
std::vector<TTBucket> TT(TT_size);
// Guard flag: indicates TT is being resized/reassigned. When true, probes
// should fall back to a thread-local bucket to avoid dereferencing a
// potentially-reallocated vector pointer.
std::atomic<bool> TT_resizing{false};

void new_game(ThreadInfo &thread_info, std::vector<TTBucket> &TT) {
  // Reset TT and other thread_info values for a new game

  // Hold data mutex while reinitializing shared structures to avoid races
  // with running search threads that might probe/insert into the TT.
  {
    std::lock_guard<std::mutex> lg(thread_data.data_mutex);
    thread_info.game_ply = 0; // Fixed: start from 0 for proper book consultation
    thread_info.thread_id = 0;
    thread_info.HistoryScores.fill({});
    thread_info.ContHistScores.fill({});
    thread_info.CapHistScores.fill({});
    thread_info.PawnCorrHist.fill({});
    thread_info.NonPawnCorrHist.fill({});
    thread_info.game_hist.fill({});
    thread_info.nodes.store(0);
    // Mark resizing so concurrent probes fall back to a safe thread-local
    // bucket instead of attempting to dereference a moved vector pointer.
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
  // Mark TT as resizing so concurrent probe_entry calls will fall back to
  // a thread-local bucket instead of attempting to dereference the
  // vector while it is being reallocated.
  TT_resizing.store(true, std::memory_order_release);
  TT_size = static_cast<uint64_t>(size) * 1024 * 1024 / sizeof(TTBucket);
  TT.assign(TT_size, TTBucket{});
  TT_resizing.store(false, std::memory_order_release);
}

uint64_t hash_to_idx(uint64_t hash) {
  return (uint128_t(hash) * uint128_t(TT_size)) >> 64;
}

// Safe snapshot helper: returns current TT size while holding data_mutex and
// ensuring we won't observe a resizing in progress. Use this for diagnostics
// or read-only sampling where a tiny mutex hold is acceptable.
inline uint64_t safe_TT_size() {
  std::lock_guard<std::mutex> lg(thread_data.data_mutex);
  if (TT_resizing.load(std::memory_order_acquire)) return 0;
  return TT.size();
}

// Safe prefetch helper: some call sites used __builtin_prefetch(&TT[idx])
// without synchronization which can race with TT.assign() and cause
// SEGVs under ASAN. Use this helper to prefetch the bucket under the
// TT data mutex (or skip prefetch if resizing is in progress).
inline void safe_TT_prefetch(uint64_t hash) {
  if (TT_resizing.load(std::memory_order_acquire)) return;
  std::lock_guard<std::mutex> lg(thread_data.data_mutex);
  if (TT.size() == 0) return;
  uint64_t size = TT.size();
  uint64_t idx = (uint128_t(hash) * uint128_t(size)) >> 64;
  if (idx >= size) return;
  __builtin_prefetch(&TT[static_cast<size_t>(idx)], 0, 1);
}

int entry_quality(TTEntry &entry, int searches) {
  int age_diff = (MaxAge + searches - entry.get_age()) % MaxAge;
  return entry.depth - age_diff * 8;
}

TTEntry probe_entry(uint64_t hash, bool &hit, uint8_t searches,
                    std::vector<TTBucket> &TT) {
  // Double-snapshot approach: capture TT.data() and TT.size() twice and only
  // access TT if both snapshots match. This avoids dereferencing a vector
  // that was reallocated concurrently (which would cause a SEGV). It's a
  // lightweight mitigation; true thread-safety requires lock striping or
  // other synchronization (TODO).
  // If a resize/new_game is in progress, avoid probing the shared TT and
  // instead return a thread-local fallback to prevent use-after-free.
  // Lightweight path: if a resize is in progress, use a thread-local fallback.
  if (TT_resizing.load(std::memory_order_acquire)) {
    static thread_local TTBucket fallback_bucket_inline;
    hit = false;
    fallback_bucket_inline.entries[0].age_bound = (searches << 2) | fallback_bucket_inline.entries[0].get_type();
    return fallback_bucket_inline.entries[0];
  }

  // To prevent TOCTOU/in-flight reallocation races observed under heavy
  // concurrent stress (ASAN SEGVs), take the global TT data mutex while
  // probing. resize_TT / new_game already hold this mutex when they modify
  // the TT, so holding it here guarantees the vector won't be reallocated
  // while we're accessing its contents.
  std::lock_guard<std::mutex> lg(thread_data.data_mutex);

  void *data_ptr = TT.data();
  uint64_t size = TT.size();
  if (size == 0 || data_ptr == nullptr) {
    static thread_local TTBucket fallback_bucket;
    hit = false;
    fallback_bucket.entries[0].age_bound = (searches << 2) | fallback_bucket.entries[0].get_type();
    return fallback_bucket.entries[0];
  }

  uint32_t hash_key = get_hash_low_bits(hash);
  uint64_t idx = (uint128_t(hash) * uint128_t(size)) >> 64;

  if (idx >= size) {
    static thread_local TTBucket fallback_bucket2;
    hit = false;
    fallback_bucket2.entries[0].age_bound = (searches << 2) | fallback_bucket2.entries[0].get_type();
    return fallback_bucket2.entries[0];
  }

  // Safe to index because we hold the mutex.
  TTBucket *buckets = reinterpret_cast<TTBucket *>(data_ptr);
  auto &bucket = buckets[idx];
  __builtin_prefetch(&bucket, 0, 1);
  auto &entries = bucket.entries;

  for (int i = 0; i < BucketEntries; i++) {
    bool empty =
        entries[i].score == 0 && entries[i].get_type() == EntryTypes::None;

    if (empty || entries[i].position_key == hash_key) {
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

void insert_entry(
    TTEntry &entry, uint64_t hash, int depth, Move best_move,
    int32_t static_eval, int32_t score, uint8_t bound_type,
    uint8_t searches) { // Inserts an entry into the transposition table.
  // Make TT writes safe by acquiring the global TT data mutex and writing
  // directly into the shared TT vector at the computed bucket index. This
  // avoids writing into an address that a caller may hold by reference if a
  // concurrent resize happens between probe and insert.
  std::lock_guard<std::mutex> lg(thread_data.data_mutex);

  uint32_t hash_key = get_hash_low_bits(hash);

  // If TT is not available, write into a thread-local fallback and return.
  if (TT.size() == 0 || TT.data() == nullptr) {
    static thread_local TTBucket fallback_bucket;
    TTEntry &fe = fallback_bucket.entries[0];
    if (best_move != MoveNone || hash_key != fe.position_key) fe.best_move = best_move;
    if (fe.position_key == hash_key && (bound_type != EntryTypes::Exact) && fe.depth > depth + 4) return;
    fe.position_key = hash_key;
    fe.depth = static_cast<uint8_t>(depth);
    fe.static_eval = static_eval;
    fe.score = score;
    fe.age_bound = (searches << 2) | bound_type;
    return;
  }

  // Compute bucket index with current TT size (we hold the mutex so this is stable).
  uint64_t size = TT.size();
  uint64_t idx = (uint128_t(hash) * uint128_t(size)) >> 64;
  if (idx >= size) idx = idx % size;

  TTBucket *buckets = TT.data();
  auto &bucket = buckets[idx];
  auto &entries = bucket.entries;

  // First try to find an empty slot or a matching key to overwrite.
  for (int i = 0; i < BucketEntries; i++) {
    bool empty = entries[i].score == 0 && entries[i].get_type() == EntryTypes::None;
    if (empty || entries[i].position_key == hash_key) {
      TTEntry &e = entries[i];
      if (best_move != MoveNone || hash_key != e.position_key) e.best_move = best_move;
      if (e.position_key == hash_key && (bound_type != EntryTypes::Exact) && e.depth > depth + 4) return;
      e.position_key = hash_key;
      e.depth = static_cast<uint8_t>(depth);
      e.static_eval = static_eval;
      e.score = score;
      e.age_bound = (searches << 2) | bound_type;
      return;
    }
  }

  // No empty slot or exact key found: pick worst-quality entry to replace.
  TTEntry *worst = &entries[0];
  int worst_q = entry_quality(*worst, searches);
  for (int i = 1; i < BucketEntries; i++) {
    int q = entry_quality(entries[i], searches);
    if (q < worst_q) {
      worst = &entries[i];
      worst_q = q;
    }
  }

  // Overwrite worst entry.
  TTEntry &we = *worst;
  we.position_key = hash_key;
  we.depth = static_cast<uint8_t>(depth);
  we.static_eval = static_eval;
  we.score = score;
  we.age_bound = (searches << 2) | bound_type;
  if (best_move != MoveNone) we.best_move = best_move;
}

void calculate(Position &position) { // Calculates the zobrist key of
                                               // a given position.
  // Useful when initializing positions, in search though
  // incremental updates are faster.
  uint64_t hash = 0;
  uint64_t pawn_hash = 0;
  position.non_pawn_key[Colors::White] = 0, position.non_pawn_key[Colors::Black] = 0;

  for (int indx = 0; indx < 64; indx++) {
    int piece = position.board[indx];
    if (piece) {
      hash ^= zobrist_keys[get_zobrist_key(piece, indx)];
      if (get_piece_type(piece) == PieceTypes::Pawn){
        pawn_hash ^= zobrist_keys[get_zobrist_key(piece, indx)];
      }
      else{
        position.non_pawn_key[get_color(piece)] ^= zobrist_keys[get_zobrist_key(piece, indx)];
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

int get_corrhist_index(uint64_t key){
  return key % 16384;
}

int64_t time_elapsed(std::chrono::steady_clock::time_point start_time) {
  // get the time that has elapsed since the start of search

  auto now = std::chrono::steady_clock::now();
  return std::chrono::duration_cast<std::chrono::milliseconds>(now - start_time)
      .count();
}

// ty to Ciekce (https://github.com/Ciekce/Stormphrax) for providing this code
// in a pinch to fix a critical bug.

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
        // Wake up either when phase advanced or when canceled
        return (phase - m_phase.load(std::memory_order_acquire)) < 0 || m_cancel.load(std::memory_order_acquire);
      });
      // If canceled, return early to allow shutdown/resizing to proceed
      if (m_cancel.load(std::memory_order_acquire)) return;
    } else {
      const auto total = m_total.load(std::memory_order_acquire);
      m_current.store(total, std::memory_order_release);

      m_phase++;

      wait_signal.notify_all();
    }
  }

  // Cancel any waiting threads so arrive_and_wait returns early.
  auto cancel() {
    m_cancel.store(true, std::memory_order_release);
    wait_signal.notify_all();
  }

  // Clear cancel flag after threads have been joined and barrier state reset.
  auto clear_cancel() {
    m_cancel.store(false, std::memory_order_release);
  }

private:
  std::atomic<int64_t> m_total{};
  std::atomic<int64_t> m_current{};
  std::atomic<int64_t> m_phase{};

  // Cancellation flag: when set, waiting threads should wake early.
  std::atomic<bool> m_cancel{};

  std::mutex wait_mutex{};
  std::condition_variable wait_signal{};
};

Barrier reset_barrier{1};
Barrier idle_barrier{1};
Barrier search_end_barrier{1};

// Opening Book implementations
bool OpeningBook::load_book(const std::string& path) {
  book_path = path;
  if (path.empty()) {
    book_loaded = false;
    return false;
  }
  
  // Try to detect book format and load accordingly
  if (path.size() >= 4) {
    auto ext = path.substr(path.size() - 4);
    std::transform(ext.begin(), ext.end(), ext.begin(), ::tolower);
    // Unknown non-polyglot extension: reject with a concise message.
    if (ext != ".bin" && ext != ".bok" && ext != "book") {
      // Accept common .bin; .book is also treated as polyglot in some ecosystems.
      // For other formats, convert to Polyglot.
  safe_print_cerr(std::string("Unsupported opening book format: ") + path + std::string(" (supported: .bin/.book)"));
      book_loaded = false;
      return false;
    }
  }
  
  if (path.find(".bin") != std::string::npos || path.find(".book") != std::string::npos) {
    return load_polyglot_book(path);
  }
  
  // Default to polyglot format
  return load_polyglot_book(path);
}

Move OpeningBook::probe_book(uint64_t position_key, int min_weight) {
  if (!book_loaded || book_positions.empty()) return MoveNone;
  auto it = book_positions.find(position_key);
  if (it == book_positions.end()) {
    return MoveNone;
  }
  
  const auto& entries = it->second;
  if (entries.empty()) {
    return MoveNone;
  }
  
  // Filter by min weight
  std::vector<const BookEntry*> filtered;
  filtered.reserve(entries.size());
  for (auto &e : entries) if (e.weight >= (uint16_t)min_weight) filtered.push_back(&e);
  if (filtered.empty()) return MoveNone;
  // Weighted random selection
  uint32_t total_weight = 0;
  for (const auto* entry : filtered) total_weight += entry->weight;
  
  if (total_weight == 0) {
    return entries[0].move; // Fallback to first move
  }
  
  uint32_t random_value = Random::dist(Random::rd) % total_weight;
  uint32_t current_weight = 0;
  
  for (const auto* entry : filtered) {
    current_weight += entry->weight;
    if (random_value < current_weight) return entry->move;
  }
  return filtered[0]->move; // Fallback
}

bool OpeningBook::load_polyglot_book(const std::string& filename) {
  std::ifstream file(filename, std::ios::binary);
  if (!file.is_open()) {
    return false;
  }
  
  book_positions.clear();
  
  // Polyglot book entry is 16 bytes: 8 bytes key + 2 bytes move + 2 bytes weight + 4 bytes learn
  struct PolyglotEntry {
    uint64_t key;
    uint16_t move;
    uint16_t weight;
    uint32_t learn;
  };
  
  PolyglotEntry entry;
  while (file.read(reinterpret_cast<char*>(&entry), sizeof(entry))) {
    // Convert from big-endian to little-endian
    uint64_t key = __builtin_bswap64(entry.key);
    uint16_t move = __builtin_bswap16(entry.move);
    uint16_t weight = __builtin_bswap16(entry.weight);
    uint32_t learn = __builtin_bswap32(entry.learn);
    
    // Convert polyglot move format to internal format
    int from = ((move >> 6) & 0x3F);
    int to = (move & 0x3F);
    int promotion = ((move >> 12) & 0x7);
    
    Move internal_move;
    if (promotion == 0) {
      internal_move = pack_move(from, to, MoveTypes::Normal);
    } else {
      // Polyglot promotion encoding: 1=knight, 2=bishop, 3=rook, 4=queen
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


uint64_t OpeningBook::polyglot_key(const Position& pos) {
  uint64_t key = 0ULL;
  // NOTE: piece_map previously declared but unused; removed for cleanliness.
  // We map internal piece types directly via switch below to Polyglot indices.
  // Polyglot piece order (0..11): WP WN WB WR WQ WK BP BN BB BR BQ BK
  // Internal get_piece_type() is expected to return Pawn=1..King=6.
  // Any change in internal piece encoding must keep this mapping in sync.
  // Piece enumeration
  for (int sq = 0; sq < 64; ++sq) {
    int piece = pos.board[sq];
    if (!piece) continue;
    int color = get_color(piece);
    int ptype = get_piece_type(piece); // 1=Pawn.. per engine
    int poly_index = -1;
    // Map to Polyglot piece order: White: P N B R Q K (0..5), Black: p n b r q k (6..11)
    switch (ptype) {
      case PieceTypes::Pawn:   poly_index = color==Colors::White ? 0 : 6; break;
      case PieceTypes::Knight: poly_index = color==Colors::White ? 1 : 7; break;
      case PieceTypes::Bishop: poly_index = color==Colors::White ? 2 : 8; break;
      case PieceTypes::Rook:   poly_index = color==Colors::White ? 3 : 9; break;
      case PieceTypes::Queen:  poly_index = color==Colors::White ? 4 : 10; break;
      case PieceTypes::King:   poly_index = color==Colors::White ? 5 : 11; break;
      default: break;
    }
    if (poly_index >= 0)
      key ^= poly_random[64 * poly_index + sq];
  }
  // Castling rights (assume standard four flags exist if squares not None)
  if (pos.castling_squares[Colors::White][Sides::Kingside] != SquareNone)
    key ^= poly_random[768 + 0];
  if (pos.castling_squares[Colors::White][Sides::Queenside] != SquareNone)
    key ^= poly_random[768 + 1];
  if (pos.castling_squares[Colors::Black][Sides::Kingside] != SquareNone)
    key ^= poly_random[768 + 2];
  if (pos.castling_squares[Colors::Black][Sides::Queenside] != SquareNone)
    key ^= poly_random[768 + 3];
  // En passant (Polyglot spec): include only if there exists a pawn that can capture en passant
  if (pos.ep_square != SquareNone && pos.ep_square < 64) {
    int ep_file = pos.ep_square % 8;
    int ep_rank = pos.ep_square / 8; // target square rank (where capture would land)
    bool ep_valid = false;
    if (pos.color == Colors::White && ep_rank == 5) { // white to move, black just advanced
      // White pawns must be on rank 5 (index 4) adjacent files
      uint64_t rank5 = Ranks[4];
      if (ep_file > 0) {
        int sq = (ep_rank - 1) * 8 + (ep_file - 1); // square of potential white pawn
        if (pos.board[sq] == Pieces::WPawn) ep_valid = true;
      }
      if (!ep_valid && ep_file < 7) {
        int sq = (ep_rank - 1) * 8 + (ep_file + 1);
        if (pos.board[sq] == Pieces::WPawn) ep_valid = true;
      }
    } else if (pos.color == Colors::Black && ep_rank == 2) {
      // black to move, white just advanced; black pawns on rank 4 (index 3)
      if (ep_file > 0) {
        int sq = (ep_rank + 1) * 8 + (ep_file - 1);
        if (pos.board[sq] == Pieces::BPawn) ep_valid = true;
      }
      if (!ep_valid && ep_file < 7) {
        int sq = (ep_rank + 1) * 8 + (ep_file + 1);
        if (pos.board[sq] == Pieces::BPawn) ep_valid = true;
      }
    }
    if (ep_valid) key ^= poly_random[768 + 4 + ep_file];
  }
  if (pos.color == Colors::Black) key ^= poly_random[780];
  return key;
}

// (Duplicate probe_book removed by patch)

// TimeManager method implementations
void TimeManager::initialize(uint64_t time_left, uint64_t increment, int moves_to_go, int game_move) {
  move_number = game_move;
  
  // Base time calculation with improved algorithm
  if (moves_to_go > 0) {
    // Tournament time control
    allocated_time =
        (time_left + increment * static_cast<uint64_t>(std::max(moves_to_go - 1, 0))) /
        static_cast<uint64_t>(std::max(moves_to_go, 1));
    allocated_time = std::min(allocated_time, time_left / 3);
  } else {
    // Sudden death or increment-based
    double time_factor = 1.0;

    // Adjust based on game phase
    if (game_move < 20)
      time_factor = 0.8; // Opening: be conservative
    else if (game_move < 40)
      time_factor = 1.2; // Middlegame: use more time
    else
      time_factor = 1.0; // Endgame: standard

    // Use a larger share of remaining time and scale increment less
    allocated_time = std::max<uint64_t>(
        1, static_cast<uint64_t>((time_left / 8.0 + increment * 0.5) * time_factor));
  }

  // Safety margins
  max_time = std::min<uint64_t>(allocated_time * 8, time_left / 2);
  panic_time = std::min<uint64_t>(allocated_time * 3, time_left / 4);
  soft_limit = allocated_time;
  hard_limit = max_time;
  
  use_panic_mode = false;
  time_stability = 0;
}

bool TimeManager::should_stop(uint64_t elapsed, bool best_move_stable, bool in_trouble) {
  // Hard limit - always stop
  if (elapsed >= hard_limit) return true;
  
  // Panic mode - if we're in trouble, use more time
  if (in_trouble && !use_panic_mode && elapsed < panic_time * 2) {
    use_panic_mode = true;
    soft_limit = panic_time * 2;
  }
  
  // Soft limit with stability consideration
  if (elapsed >= soft_limit) {
    if (best_move_stable || use_panic_mode) {
      return true;
    }
    // Extend slightly if unstable
    soft_limit = std::min(soft_limit + allocated_time / 4, hard_limit);
  }
  
  return false;
}

void TimeManager::update_node_count(uint64_t nodes) {
  nodes_searched = nodes;
}

void adjust_soft_limit(ThreadInfo &thread_info, uint64_t best_move_nodes, int bm_stability) noexcept {
  uint64_t node_count = thread_info.nodes.load();
  if (node_count == 0) return;
  double fract = static_cast<double>(best_move_nodes) / node_count;
  double factor = (static_cast<double>(NodeTmFactor1) / 100.0 - fract) * NodeTmFactor2 / 100.0;
  double bm_factor = BmFactor1 / 100.0f - (bm_stability * 0.06);

  // Enhanced with TimeManager integration
  if (thread_info.time_manager.use_panic_mode) {
    factor *= 1.5; // Use more time in panic mode
  }
  
  // Node-based time scaling
  double node_factor = 1.0;
  if (node_count > 100000) {
    node_factor = std::min(2.0, node_count / 50000.0);
  }

  uint64_t new_time = thread_info.original_opt * factor * bm_factor * node_factor;
  thread_info.opt_time = std::min<uint64_t>(new_time, thread_info.max_time);
}

inline uint64_t compute_move_overhead(uint64_t time_left, uint64_t base_overhead) noexcept {
  uint64_t dynamic = std::min<uint64_t>(50, time_left / 10);
  return base_overhead + dynamic;
}

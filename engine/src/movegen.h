#pragma once
#include "defs.h"
#include "position.h"
#include <cassert>
#include <cstdint>
#include <cstdio>
// Removed <span> for wider C++17 compatibility

namespace Generate {
uint8_t GenQuiets = 0;
uint8_t GenCaptures = 1;
uint8_t GenAll = 2;
} // namespace Generate

constexpr int TTMoveScore = 10000000;
constexpr int QueenPromoScore = 5000000;
constexpr int GoodCaptureBaseScore = 2000000;
constexpr int BadCaptureBaseScore = -2000000;
constexpr int KillerMoveScore = 100000;

void pawn_moves(const Position &position, uint64_t check_filter,
                Move *move_list, int &key, int gen_type) {

  uint8_t color = position.color;
  uint64_t third_rank = color ? Ranks[5] : Ranks[2];
  uint64_t seventh_rank = color ? Ranks[1] : Ranks[6];
  int8_t dir = color ? Directions::South : Directions::North;
  int8_t left = color ? Directions::Southwest : Directions::Northwest;
  int8_t right = color ? Directions::Southeast : Directions::Northeast;

  uint64_t empty_squares = ~(position.colors_bb[0] | position.colors_bb[1]);
  uint64_t our_promos = position.pieces_bb[PieceTypes::Pawn] &
                        position.colors_bb[color] & seventh_rank;
  uint64_t our_non_promos = position.pieces_bb[PieceTypes::Pawn] &
                            position.colors_bb[color] & (~seventh_rank);

  if (gen_type != Generate::GenCaptures) {
    uint64_t move_1 = shift_pawns(our_non_promos, dir) & empty_squares;
    uint64_t move_2 =
        shift_pawns(move_1 & third_rank, dir) & empty_squares & check_filter;
    move_1 &= check_filter;

    while (move_1) {
      int to = pop_lsb(move_1);
      if (key < ListSize) {
        move_list[key++] = pack_move(to - (dir), to, MoveTypes::Normal);
      }
    }
    while (move_2) {
      int to = pop_lsb(move_2);
      if (key < ListSize) {
        move_list[key++] = pack_move(to - (2 * dir), to, MoveTypes::Normal);
      }
    }
  }

  if (gen_type != Generate::GenQuiets) {
    uint64_t cap_left = shift_pawns(our_non_promos & ~Files[0], left) &
                        position.colors_bb[color ^ 1] & check_filter;
    uint64_t cap_right = shift_pawns(our_non_promos & ~Files[7], right) &
                         position.colors_bb[color ^ 1] & check_filter;

    while (cap_left) {
      int to = pop_lsb(cap_left);
      if (key < ListSize) {
        move_list[key++] = pack_move(to - (left), to, MoveTypes::Normal);
      }
    }
    while (cap_right) {
      int to = pop_lsb(cap_right);
      if (key < ListSize) {
        move_list[key++] = pack_move(to - (right), to, MoveTypes::Normal);
      }
    }

    if (position.ep_square != SquareNone) {
      uint64_t ep_targets =
          PAWN_ATK_SAFE(color ^ 1, position.ep_square);
      uint64_t ep_captures = our_non_promos & ep_targets;
      while (ep_captures) {
        int from = pop_lsb(ep_captures);
        if (key < ListSize) {
          move_list[key++] =
              pack_move(from, position.ep_square, MoveTypes::EnPassant);
        }
      }
    }
  }

  uint64_t move_promo =
      shift_pawns(our_promos, dir) & empty_squares & check_filter;
  uint64_t cap_left_promo = shift_pawns(our_promos & ~Files[0], left) &
                            position.colors_bb[color ^ 1] & check_filter;
  uint64_t cap_right_promo = shift_pawns(our_promos & ~Files[7], right) &
                             position.colors_bb[color ^ 1] & check_filter;

  // Safe push wrapper to avoid buffer overflows or null pointer writes
  auto safe_push = [&](Move m) {
    if (move_list && key >= 0 && key < ListSize) move_list[key++] = m;
  };

  while (move_promo) {
    int to = pop_lsb(move_promo);
    // Always generate all promotion types (knight, bishop, rook, queen)
    for (int i = 0; i < 4; i++) {
      safe_push(pack_move_promo(to - (dir), to, i));
    }
  }

  if (gen_type != Generate::GenQuiets) {
    while (cap_left_promo) {
      int to = pop_lsb(cap_left_promo);
      for (int i = 0; i < 4; i++) {
        safe_push(pack_move_promo(to - (left), to, i));
      }
    }
    while (cap_right_promo) {
      int to = pop_lsb(cap_right_promo);
      for (int i = 0; i < 4; i++) {
        safe_push(pack_move_promo(to - (right), to, i));
      }
    }
  }
}

int movegen(const Position &position, Move *move_list,
            uint64_t checkers, int gen_type) {

  uint8_t color = position.color, king_pos = get_king_pos(position, color);
  int opp_color = color ^ 1;
  int idx = 0;
  uint64_t stm_pieces = position.colors_bb[color],
           opp_pieces = position.colors_bb[color ^ 1];

  auto load_between_bb = [&](int from_sq, int to_sq, const char *context,
                             uint64_t &mask) -> bool {
    if (!is_valid_square(from_sq) || !is_valid_square(to_sq) ||
    from_sq >= 64 || to_sq >= 64) {
#ifndef NDEBUG
  {
    char _m_buf[128];
    std::snprintf(_m_buf, sizeof(_m_buf), "[movegen] Invalid BetweenBBs indices %d -> %d in %s\n", from_sq, to_sq, context);
    safe_print_cerr(std::string(_m_buf));
  }
#endif
  return false;
    }
    int idx_local = from_sq * 64 + to_sq;
    assert(idx_local >= 0 && idx_local < 4096);
    if (idx_local < 0 || idx_local >= 4096) {
#ifndef NDEBUG
  {
    char _m_buf2[128];
    std::snprintf(_m_buf2, sizeof(_m_buf2), "[movegen] BetweenBBs index overflow %d -> %d in %s\n", from_sq, to_sq, context);
    safe_print_cerr(std::string(_m_buf2));
  }
#endif
  return false;
    }
    mask = BetweenBBs[static_cast<size_t>(from_sq)]
                         [static_cast<size_t>(to_sq)];
    return true;
  };

  uint64_t targets = 0;
  if (gen_type != Generate::GenCaptures) {
    targets |= ~opp_pieces;
  }
  if (gen_type != Generate::GenQuiets) {
    targets |= opp_pieces;
  }
  targets &= ~stm_pieces;

  uint64_t occ = position.colors_bb[0] | position.colors_bb[1];
  uint64_t check_filter = ~0;

  int king_sq = static_cast<int>(king_pos);
  if (!is_valid_square(king_sq)) {
    return idx;
  }
  uint64_t king_attacks = KING_ATK_SAFE(king_sq) & targets;
  while (king_attacks) {
  if (move_list && idx < ListSize) move_list[idx++] =
    pack_move(king_pos, pop_lsb(king_attacks), MoveTypes::Normal);
  }

  if (checkers) {
    if (checkers & (checkers - 1)) {
      return idx;
    }
    // Single check: restrict quiet/capture generation (except king moves already added)
    // Allow: moving into squares between king and checker (for sliders) or capturing the checker.
    int checker_sq = get_lsb(checkers);
    if (!is_valid_square(checker_sq)) {
#ifndef NDEBUG
  {
    char _m_buf3[80];
    std::snprintf(_m_buf3, sizeof(_m_buf3), "[movegen] Invalid checker square %d\n", checker_sq);
    safe_print_cerr(std::string(_m_buf3));
  }
#endif
  return idx;
    }
    uint64_t between_mask = 0ULL;
    if (load_between_bb(king_sq, checker_sq, "single check", between_mask)) {
      check_filter = between_mask | (1ULL << checker_sq);
    } else {
      check_filter = (1ULL << checker_sq);
    }
  }

  pawn_moves(position, check_filter, move_list, idx, gen_type);

  uint64_t knights = position.pieces_bb[PieceTypes::Knight] & stm_pieces;
  while (knights) {
    int from = pop_lsb(knights);
    uint64_t to = KNIGHT_ATK_SAFE(from) & targets & check_filter;
    while (to) {
      if (move_list && idx < ListSize) move_list[idx++] = pack_move(from, pop_lsb(to), MoveTypes::Normal);
      else pop_lsb(to); // consume bit to avoid infinite loop
    }
  }

  uint64_t diagonals = (position.pieces_bb[PieceTypes::Bishop] |
                        position.pieces_bb[PieceTypes::Queen]) &
                       stm_pieces;
  while (diagonals) {
    int from = pop_lsb(diagonals);
    uint64_t to = get_bishop_attacks(from, occ) & targets & check_filter;
    while (to) {
      if (move_list && idx < ListSize) move_list[idx++] = pack_move(from, pop_lsb(to), MoveTypes::Normal);
      else pop_lsb(to);
    }
  }

  uint64_t orthogonals = (position.pieces_bb[PieceTypes::Rook] |
                          position.pieces_bb[PieceTypes::Queen]) &
                         stm_pieces;
  while (orthogonals) {
    int from = pop_lsb(orthogonals);
    uint64_t to = get_rook_attacks(from, occ) & targets & check_filter;
    while (to) {
      if (move_list && idx < ListSize) move_list[idx++] = pack_move(from, pop_lsb(to), MoveTypes::Normal);
      else pop_lsb(to);
    }
  }

  if (checkers ||
      gen_type ==
          Generate::GenCaptures) { // If we're in check there's no point in
                                   // seeing if we can castle (can be optimized)
    return idx;
  }
  // Requirements: the king and rook cannot have moved, all squares between them
  // must be empty, and the king can not go through check.
  // (It can't end up in check either, but that gets filtered just like any
  // illegal move.)

  for (int side : {Sides::Queenside, Sides::Kingside}) {

    if (position.castling_squares[color][side] == SquareNone ||
        !is_valid_square(position.castling_squares[color][side])) {
      continue;
    }

    int rook_target = 56 * color + 3 + 2 * side;
    int king_target = 56 * color + 2 + 4 * side;
    if (!is_valid_square(rook_target) || !is_valid_square(king_target)) {
      continue;
    }

    uint64_t castle_bb = 0ULL;
    if (!load_between_bb(static_cast<int>(position.castling_squares[color][side]),
                         rook_target, "castling rook path", castle_bb)) {
      continue;
    }
    uint64_t king_path = 0ULL;
    if (!load_between_bb(king_sq, king_target, "castling king path",
                         king_path)) {
      continue;
    }
    castle_bb |= king_path;
    castle_bb &=
        ~(1ull << king_pos) & ~(1ull << position.castling_squares[color][side]);

    if (occ & castle_bb) {
      continue;
    }
    bool invalid = false;

    if (king_target != king_pos) {
      int dir = (king_target > king_pos) ? 1 : -1;

      for (int i = king_pos + dir; i != king_target; i += dir) {
        if (attacks_square(position, i, opp_color)) {
          invalid = true;
          break;
        }
      }
    }

    if (!invalid) {
      if (move_list && idx < ListSize)
        move_list[idx++] =
            pack_move(king_pos, position.castling_squares[color][side],
                      MoveTypes::Castling);
    }
  }

  return idx;
}

// Not to be used in performance critical areas
int legal_movegen(const Position &position, Move *move_list) {
  uint64_t checkers = attacks_square(
      position, get_king_pos(position, position.color), position.color ^ 1);
  std::array<Move, ListSize> pseudo_list;
  int pseudo_nmoves =
      movegen(position, pseudo_list.data(), checkers, Generate::GenAll);

  int legal_nmoves = 0;
  for (int i = 0; i < pseudo_nmoves; i++) {
    if (is_legal(position, pseudo_list[i]))
      move_list[legal_nmoves++] = pseudo_list[i];
  }

  return legal_nmoves;
}

bool SEE(Position &position, Move move, int threshold) {

  int stm = position.color, from = extract_from(move), to = extract_to(move);

  // Enhanced validation to prevent SEGV crashes
  if (!is_valid_square(from) || !is_valid_square(to)) return false;

  // Validate position integrity - check for corrupted board data
  if (from >= 64 || to >= 64 || from < 0 || to < 0) return false;

  // Check if pieces exist and are valid
  if (position.board[from] == Pieces::Blank || position.board[to] == Pieces::Blank) {
    if (position.board[from] == Pieces::Blank) return false;
  }

  // Validate piece colors match position
  int from_piece = position.board[from];
  int from_color = get_color(from_piece);
  if (from_color != position.color) return false;

  int gain = SeeValues[get_piece_type(position.board[to])] - threshold;
  if (gain < 0) {
    // If taking the piece isn't good enough return
    return false;
  }

  gain -= SeeValues[get_piece_type(position.board[from])];
  if (gain >= 0) {
    return true;
  }

  // Store bishops and rooks here to more quickly determine later new revealed
  // attackers
  uint64_t bishops = position.pieces_bb[PieceTypes::Bishop] |
                     position.pieces_bb[PieceTypes::Queen];
  uint64_t rooks = position.pieces_bb[PieceTypes::Rook] |
                   position.pieces_bb[PieceTypes::Queen];

  uint64_t occ =
      (position.colors_bb[Colors::White] | position.colors_bb[Colors::Black]) -
      (1ull << from);

  // Additional safety check for attacks_square
  if (!is_valid_square(to)) return false;

  uint64_t all_attackers = attacks_square(position, to, occ);

  while (true) {
    stm ^= 1;

    all_attackers &= occ;

    uint64_t stm_attackers = all_attackers & position.colors_bb[stm];

    if (!stm_attackers) {
      return stm != position.color;
    }

    // find cheapest attacker
    int attackerType = PieceTypes::PieceNone;

    for (int pt = PieceTypes::Pawn; pt <= PieceTypes::King; pt++) {
      uint64_t match = stm_attackers & position.pieces_bb[pt];
      if (match) {
        // Validate the attacker square
        int attacker_sq = get_lsb(match);
        if (!is_valid_square(attacker_sq)) return false;

        occ -= get_lsb_bb(match);
        attackerType = pt;
        break;
      }
    }

    if (attackerType == PieceTypes::PieceNone) {
      return false; // Corrupted position, no valid attacker found
    }

    // A new slider behind might be revealed
    if (attackerType == PieceTypes::Pawn ||
        attackerType == PieceTypes::Bishop ||
        attackerType == PieceTypes::Queen) {
      all_attackers |= get_bishop_attacks(to, occ) & bishops;
    }
    if (attackerType == PieceTypes::Rook || attackerType == PieceTypes::Queen) {
      all_attackers |= get_rook_attacks(to, occ) & rooks;
    }

    gain = -gain - SeeValues[attackerType] - 1;
    if (gain >= 0) {
      return stm == position.color;
    }
  }

  return true;
}

// Forward declaration for make_move used in evaluate_promotion_tactics
void make_move(Position &position, Move move);

// Evaluate tactical benefits of under-promotion
int evaluate_promotion_tactics(Position &position, Move move) {
  int from = extract_from(move);
  int to = extract_to(move);
  int promo_type = extract_promo(move);
  int color = position.color;
  int bonus = 0;

  if (!is_valid_square(from) || !is_valid_square(to)) {
    return 0;
  }
  
  // Make a temporary copy of the position to simulate the promotion
  auto temp_pos_uptr = std::make_unique<Position>(position);
  Position &temp_pos = *temp_pos_uptr;
  make_move(temp_pos, move);
  
  // Knight promotion tactics
  if (promo_type == Promos::Knight) {
    uint64_t knight_attacks = KNIGHT_ATK_SAFE(to);
    
    // Check for forks
    int fork_targets = 0;
    uint64_t valuable_pieces = (temp_pos.pieces_bb[PieceTypes::Queen] | 
                               temp_pos.pieces_bb[PieceTypes::Rook] |
                               temp_pos.pieces_bb[PieceTypes::King]) &
                               temp_pos.colors_bb[color ^ 1];
                               
    while (valuable_pieces) {
      int piece_sq = pop_lsb(valuable_pieces);
      if (knight_attacks & (1ULL << piece_sq)) {
        fork_targets++;
      }
    }
    
    if (fork_targets >= 2) {
      bonus += 150; // Strong fork
    } else if (fork_targets == 1) {
      bonus += 50;  // Attacking valuable piece
    }
    
    // Knight vs passed pawn endgame considerations
    if (temp_pos.material_count[PieceTypes::Pawn] < 4 && 
        !temp_pos.pieces_bb[PieceTypes::Queen] && 
        !temp_pos.pieces_bb[PieceTypes::Rook]) {
      bonus += 25;
    }
  }
  
  // Bishop promotion tactics
  if (promo_type == Promos::Bishop) {
    uint64_t bishop_attacks = get_bishop_attacks(to, temp_pos.colors_bb[0] | temp_pos.colors_bb[1]);
    
    // Check for diagonals control
    uint64_t central_diagonals = 0x8040201008040201ULL | 0x0102040810204080ULL;
    if (bishop_attacks & central_diagonals) {
      bonus += 40;
    }
    
    // Check for bishop pair
    if (pop_count(temp_pos.pieces_bb[PieceTypes::Bishop] & temp_pos.colors_bb[color]) > 1) {
      bonus += 30;
    }
    
    // Check for tactical threats
    uint64_t targets = bishop_attacks & temp_pos.colors_bb[color ^ 1];
    if (pop_count(targets) > 1) {
      bonus += 35;
    }
  }
  
  // Evaluate escape from perpetual check
  if (promo_type != Promos::Queen) {
    int king_pos = get_king_pos(temp_pos, color ^ 1);
    if (attacks_square(temp_pos, king_pos, color)) {
      bonus += 40;
    }
  }
  
  return bonus;
}

void score_moves(Position &position, ThreadInfo &thread_info,
                 MoveInfo &scored_moves, Move tt_move, int len) {

  // score the moves

  int ply = thread_info.search_ply;

  // Safely compute history-derived values. The original code assumed
  // thread_info.game_ply was always large enough for indexing based on ply.
  // Under corruption or unexpected states this could index out-of-bounds.
  bool have_their = (ply >= 1) && (thread_info.game_ply > 0);
  bool have_our = (ply >= 2) && (thread_info.game_ply > 1);

  int their_last = MoveNone;
  int their_piece = Pieces::Blank;
  int our_last = MoveNone;
  int our_piece = Pieces::Blank;

  if (have_their) {
    auto &h = thread_info.game_hist[thread_info.game_ply - 1];
    their_last = extract_to(h.played_move);
    their_piece = h.piece_moved;
  }
  if (have_our) {
    auto &h2 = thread_info.game_hist[thread_info.game_ply - 2];
    our_last = extract_to(h2.played_move);
    our_piece = h2.piece_moved;
  }

  for (int idx = 0; idx < len; idx++) {
    Move move = scored_moves.moves[idx];

    // Validate move encoding before using it to index arrays. A corrupted
    // move could produce indices outside [0,63] and cause ASAN SEGVs.
    int from_sq = extract_from(move);
    int to_sq = extract_to(move);
    if (!is_valid_square(from_sq) || !is_valid_square(to_sq)) {
      // Give invalid moves a very low score and skip further processing.
      scored_moves.scores[idx] = -100000000;
      continue;
    }

    if (move == tt_move) {
      scored_moves.scores[idx] = TTMoveScore;
      // TT move score;
    }

    else if (extract_type(move) == MoveTypes::Promotion) {
      int promo_type = extract_promo(move);
      
      // Base scoring for promotions
      if (promo_type == Promos::Queen) {
        // Queen promo score
        scored_moves.scores[idx] = QueenPromoScore;
      } else {
        // Underpromotions - start with a lower base score
        int base_score = QueenPromoScore - 1000000; // Lower than queen but still very high
        
        // Add tactical evaluation bonus - scales with Variety
        if (thread_info.variety > 0) {
          int tactical_bonus = evaluate_promotion_tactics(position, move);
          
          // Scale the tactical bonus based on Variety
          int variety_factor = std::min<int>(150, static_cast<int>(thread_info.variety));
          tactical_bonus = (tactical_bonus * variety_factor) / 100;
          
          base_score += tactical_bonus;
          
          // For high variety values, make underpromotions more competitive
          if (variety_factor > 100) {
            base_score += (variety_factor - 100) * 500;
          }
        }
        
        scored_moves.scores[idx] = base_score;
      }
    }

    else if (is_cap(position, move)) {
      // Capture score
      int from_piece = position.board[from_sq];
      int to_piece = position.board[to_sq];

      scored_moves.scores[idx] = GoodCaptureBaseScore +
                                 SeeValues[get_piece_type(to_piece)] * 100 -
                                 SeeValues[get_piece_type(from_piece)] / 100 -
                                 TTMoveScore * !SEE(position, move, -107);

      int piece = from_piece, to = to_sq;

      // Guard array accesses into history tables by ensuring indices are in
      // expected ranges. If out-of-range, skip adding the history bonus.
      if (piece >= 0 && piece < (int)std::size(thread_info.CapHistScores) &&
          to >= 0 && to < 64) {
        scored_moves.scores[idx] += thread_info.CapHistScores[piece][to];
      }

    }

    else if (move == thread_info.KillerMoves[thread_info.search_ply]) {
      // Killer move score
      scored_moves.scores[idx] = KillerMoveScore;
    }

    else {
      // Normal moves are scored using history
      int piece = position.board[from_sq], to = to_sq;
      if (piece >= 0 && piece < (int)std::size(thread_info.HistoryScores) &&
          to >= 0 && to < 64) {
        scored_moves.scores[idx] = thread_info.HistoryScores[piece][to];
      } else {
        scored_moves.scores[idx] = 0;
      }

      if (ply > 0 && their_last != MoveNone) {
        // Guard multi-dimensional historical table access; if indices are
        // outside expected ranges, skip the contribution.
        if (their_piece >= 0 && their_piece < (int)std::size(thread_info.ContHistScores) &&
            their_last >= 0 && their_last < 64 && piece >= 0 && piece < (int)std::size(thread_info.ContHistScores[0][0]) &&
            to >= 0 && to < 64) {
          scored_moves.scores[idx] +=
              thread_info.ContHistScores[their_piece][their_last][piece][to];
        }
      }
      if (ply > 1 && our_last != MoveNone) {
        if (our_piece >= 0 && our_piece < (int)std::size(thread_info.ContHistScores) &&
            our_last >= 0 && our_last < 64 && piece >= 0 && piece < (int)std::size(thread_info.ContHistScores[0][0]) &&
            to >= 0 && to < 64) {
          scored_moves.scores[idx] +=
              thread_info.ContHistScores[our_piece][our_last][piece][to];
        }
      }
    }
  }
}

Move get_next_move(Move *moves, int *scores, int start_idx,
                   int len) {
  // Performs a selection sort
  int best_idx = start_idx, best_score = scores[start_idx];
  for (int i = start_idx + 1; i < len; i++) {
    if (scores[i] > best_score) {
      best_score = scores[i];
      best_idx = i;
    }
  }
  std::swap(moves[start_idx], moves[best_idx]);
  std::swap(scores[start_idx], scores[best_idx]);

  return moves[start_idx];
}
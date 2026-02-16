#pragma once
#include "movepick.h"
#include "params.h"
#include "position.h"
#include "pst.h"

#include "../fathom/src/tbprobe.h"
#include "utils.h"
#include <memory>

inline Action uci_to_internal(const BoardState &position, std::string uci) {
  std::array<Action, MaxActions> list;
  int nmoves = legal_movegen(position, list.data());

  for (int i = 0; i < nmoves; i++) {
    if (internal_to_uci(position, list[i]) == uci)
      return list[i];
  }

  return 0;
}

extern bool tb_initialized;

inline int64_t
safe_elapsed(const std::chrono::steady_clock::time_point &start) {
  auto ms = time_elapsed(start);
  return ms ? ms : 1;
}

int analyze_sacrifice(BoardState &position, ThreadInfo &thread_info, int depth,
                      int ply, int sacrificer_color);

int probe_wdl_tb(BoardState &position, const ThreadInfo &thread_info) {

  if (!tb_initialized || !thread_info.use_syzygy)
    return ScoreNone;

  if (thread_info.syzygy_probe_depth > 0 && thread_info.max_iter_depth > 0) {
    int remaining = thread_info.max_iter_depth - thread_info.search_ply;
    if (remaining < thread_info.syzygy_probe_depth)
      return ScoreNone;
  }

  int material_count = pop_count(position.colors_bb[0] | position.colors_bb[1]);
  int compiled_limit = TB_LARGEST ? (int)TB_LARGEST : 7;
  if (material_count > compiled_limit)
    return ScoreNone;
  if (material_count > thread_info.syzygy_probe_limit)
    return ScoreNone;

  unsigned castling = 0;
  if (position.castling_squares[Colors::White][Sides::Kingside] != SquareNone)
    castling |= TB_CASTLING_K;
  if (position.castling_squares[Colors::White][Sides::Queenside] != SquareNone)
    castling |= TB_CASTLING_Q;
  if (position.castling_squares[Colors::Black][Sides::Kingside] != SquareNone)
    castling |= TB_CASTLING_k;
  if (position.castling_squares[Colors::Black][Sides::Queenside] != SquareNone)
    castling |= TB_CASTLING_q;
  if (castling)
    return ScoreNone;

  unsigned ep = position.ep_square != SquareNone ? position.ep_square : 0;

  unsigned rule50 = thread_info.syzygy_50_move_rule ? position.halfmoves : 0;

  unsigned result = tb_probe_wdl(position.colors_bb[0], position.colors_bb[1],
                                 position.pieces_bb[PieceTypes::King],
                                 position.pieces_bb[PieceTypes::Queen],
                                 position.pieces_bb[PieceTypes::Rook],
                                 position.pieces_bb[PieceTypes::Bishop],
                                 position.pieces_bb[PieceTypes::Knight],
                                 position.pieces_bb[PieceTypes::Pawn], rule50,
                                 castling, ep, position.color);
  if (result == TB_RESULT_FAILED) {
    thread_data.tb_fails++;
    return ScoreNone;
  }
  thread_data.tb_hits++;
  int wdl = TB_GET_WDL(result);
  switch (wdl) {
  case TB_WIN:
    return TB_WIN_SCORE;
  case TB_CURSED_WIN:
    return TB_WIN_SCORE;
  case TB_DRAW:
    return 0;
  case TB_BLESSED_LOSS:
    return -TB_WIN_SCORE;
  case TB_LOSS:
    return -TB_WIN_SCORE;
  default:
    return ScoreNone;
  }
}

void update_history(int16_t &entry, int score) {
  entry += score - entry * abs(score) / 16384;
}
void update_corrhist(int16_t &entry, int score) {
  entry += score - entry * abs(score) / 1024;
}

inline void update_continuation_histories(ThreadInfo &thread_info, int piece,
                                          int sq, int bonus, Action their_last,
                                          int their_piece, Action our_last,
                                          int our_piece, Action ply4_last,
                                          int ply4_piece) {

  update_history(thread_info.HistoryScores[piece][sq], bonus);

  if (their_last != MoveNone) {
    update_history(
        thread_info.ContHistScores[their_piece][their_last][piece][sq], bonus);
  }
  if (our_last != MoveNone) {
    update_history(thread_info.ContHistScores[our_piece][our_last][piece][sq],
                   bonus);
  }
  if (ply4_last != MoveNone) {
    update_history(thread_info.ContHistScores[ply4_piece][ply4_last][piece][sq],
                   bonus / 2);
  }
}

bool out_of_time(ThreadInfo &thread_info) {
  if (thread_data.stop || thread_info.datagen_stop)
    return true;

  if (thread_info.thread_id != 0)
    return false;

  uint64_t total_nodes = thread_info.nodes.load();
  for (auto &ti : thread_data.thread_infos)
    total_nodes += ti.nodes.load();
  if (total_nodes >= thread_info.max_nodes_searched) {
    if (thread_info.doing_datagen)
      thread_info.datagen_stop = true;
    else
      thread_data.stop = true;
    return true;
  }

  thread_info.time_checks++;
  const uint16_t check_interval = 512;
  if (thread_info.time_checks >= check_interval) {
    thread_info.time_checks = 0;
    if (!thread_info.infinite_search && !thread_info.pondering) {
      uint64_t elapsed = time_elapsed(thread_info.start_time);
      thread_info.time_manager.update_node_count(total_nodes);
      bool in_trouble = false;
      if (thread_info.time_manager.should_stop(
              elapsed, thread_info.best_move_stable, in_trouble) ||
          elapsed > thread_info.max_time) {
        thread_data.stop = true;
        return true;
      }
    }
  }
  return false;
}

int16_t material_eval(const BoardState &position) {
  int m = 0;

  for (int pt = 1; pt <= 5; pt++) {
    int count = position.material_count[(pt - 1) * 2];
    if (count > 0) {
      m += count * MaterialBasis[pt];

      for (int pt2 = 1; pt2 <= 5; pt2++) {
        int total_count_pt2 = position.material_count[(pt2 - 1) * 2] +
                              position.material_count[(pt2 - 1) * 2 + 1];
        m += count * total_count_pt2 * QuadraticImbalance[pt][pt2];
      }
    }
  }

  for (int pt = 1; pt <= 5; pt++) {
    int count = position.material_count[(pt - 1) * 2 + 1];
    if (count > 0) {
      m -= count * MaterialBasis[pt];

      for (int pt2 = 1; pt2 <= 5; pt2++) {
        int total_count_pt2 = position.material_count[(pt2 - 1) * 2] +
                              position.material_count[(pt2 - 1) * 2 + 1];
        m -= count * total_count_pt2 * QuadraticImbalance[pt][pt2];
      }
    }
  }

  return position.color ? -m : m;
}

bool has_non_pawn_material(const BoardState &position, int color) {
  int s_indx = 2 + color;
  return (position.material_count[s_indx] ||
          position.material_count[s_indx + 2] ||
          position.material_count[s_indx + 4] ||
          position.material_count[s_indx + 6]);
}

int16_t total_mat_color(const BoardState &position, int color) {

  int m = 0;
  for (int i = 0; i < 5; i++) {
    m += position.material_count[i * 2 + color] * SeeValues[i + 1];
  }
  return m;
}

int eval_pst(const BoardState &position, int color) {
  int score = 0;
  int opp_color = color ^ 1;
  int total_mat_val = total_mat(position);
  bool is_endgame = total_mat_val < 1500;

  uint64_t pawns =
      position.pieces_bb[PieceTypes::Pawn] & position.colors_bb[color];
  while (pawns) {
    int sq = pop_lsb(pawns);
    int idx = (color == Colors::White) ? sq : PST::mirror_square(sq);
    score += PST::Pawn[idx];
  }

  uint64_t knights =
      position.pieces_bb[PieceTypes::Knight] & position.colors_bb[color];
  while (knights) {
    int sq = pop_lsb(knights);
    int idx = (color == Colors::White) ? sq : PST::mirror_square(sq);
    score += PST::Knight[idx];
  }

  uint64_t bishops =
      position.pieces_bb[PieceTypes::Bishop] & position.colors_bb[color];
  while (bishops) {
    int sq = pop_lsb(bishops);
    int idx = (color == Colors::White) ? sq : PST::mirror_square(sq);
    score += PST::Bishop[idx];
  }

  uint64_t rooks =
      position.pieces_bb[PieceTypes::Rook] & position.colors_bb[color];
  while (rooks) {
    int sq = pop_lsb(rooks);
    int idx = (color == Colors::White) ? sq : PST::mirror_square(sq);
    score += PST::Rook[idx];
  }

  uint64_t queens =
      position.pieces_bb[PieceTypes::Queen] & position.colors_bb[color];
  while (queens) {
    int sq = pop_lsb(queens);
    int idx = (color == Colors::White) ? sq : PST::mirror_square(sq);
    score += PST::Queen[idx];
  }

  int king_sq = get_king_pos(position, color);
  if (is_valid_square(king_sq)) {
    int idx = (color == Colors::White) ? king_sq : PST::mirror_square(king_sq);
    score += is_endgame ? PST::KingEG[idx] : PST::KingMG[idx];
  }

  return score;
}

int eval_king_tropism(const BoardState &position, int color) {
  int score = 0;
  int opp_color = color ^ 1;
  int opp_king = get_king_pos(position, opp_color);
  if (!is_valid_square(opp_king))
    return 0;

  int opp_k_rank = get_rank(opp_king);
  int opp_k_file = get_file(opp_king);

  uint64_t queens =
      position.pieces_bb[PieceTypes::Queen] & position.colors_bb[color];
  while (queens) {
    int sq = pop_lsb(queens);
    int dist = std::max(abs(get_rank(sq) - opp_k_rank),
                        abs(get_file(sq) - opp_k_file));
    score += (8 - dist) * TropismQueenWeight;
  }

  uint64_t rooks =
      position.pieces_bb[PieceTypes::Rook] & position.colors_bb[color];
  while (rooks) {
    int sq = pop_lsb(rooks);
    int dist = std::max(abs(get_rank(sq) - opp_k_rank),
                        abs(get_file(sq) - opp_k_file));
    score += (8 - dist) * TropismRookWeight;
  }

  uint64_t knights =
      position.pieces_bb[PieceTypes::Knight] & position.colors_bb[color];
  while (knights) {
    int sq = pop_lsb(knights);
    int dist = std::max(abs(get_rank(sq) - opp_k_rank),
                        abs(get_file(sq) - opp_k_file));
    score += (8 - dist) * TropismKnightWeight;
  }

  uint64_t bishops =
      position.pieces_bb[PieceTypes::Bishop] & position.colors_bb[color];
  while (bishops) {
    int sq = pop_lsb(bishops);
    int dist = std::max(abs(get_rank(sq) - opp_k_rank),
                        abs(get_file(sq) - opp_k_file));
    score += (8 - dist) * TropismBishopWeight;
  }

  return score;
}

int eval_threats(const BoardState &position, int color) {
  int score = 0;
  int opp_color = color ^ 1;
  uint64_t occ = position.colors_bb[0] | position.colors_bb[1];

  uint64_t opp_pieces =
      position.colors_bb[opp_color] & ~position.pieces_bb[PieceTypes::Pawn];
  uint64_t my_pawn_attacks = 0;
  uint64_t my_pawns =
      position.pieces_bb[PieceTypes::Pawn] & position.colors_bb[color];
  while (my_pawns) {
    int sq = pop_lsb(my_pawns);
    my_pawn_attacks |= PAWN_ATK_SAFE(color, sq);
  }

  uint64_t attacked_by_pawns = opp_pieces & my_pawn_attacks;
  score += pop_count(attacked_by_pawns) * ThreatPawnAttack;

  uint64_t my_minor_attacks = 0;
  uint64_t minors = (position.pieces_bb[PieceTypes::Knight] |
                     position.pieces_bb[PieceTypes::Bishop]) &
                    position.colors_bb[color];
  while (minors) {
    int sq = pop_lsb(minors);
    if (position.board[sq] ==
        (color == Colors::White ? Pieces::WKnight : Pieces::BKnight)) {
      my_minor_attacks |= KNIGHT_ATK_SAFE(sq);
    } else {
      my_minor_attacks |= get_bishop_attacks(sq, occ);
    }
  }

  uint64_t opp_heavy = (position.pieces_bb[PieceTypes::Queen] |
                        position.pieces_bb[PieceTypes::Rook]) &
                       position.colors_bb[opp_color];
  uint64_t attacked_heavy = opp_heavy & my_minor_attacks;
  score += pop_count(attacked_heavy) * ThreatMinorOnHeavy;

  uint64_t my_rook_attacks = 0;
  uint64_t my_rooks =
      position.pieces_bb[PieceTypes::Rook] & position.colors_bb[color];
  while (my_rooks) {
    int sq = pop_lsb(my_rooks);
    my_rook_attacks |= get_rook_attacks(sq, occ);
  }
  uint64_t opp_queens =
      position.pieces_bb[PieceTypes::Queen] & position.colors_bb[opp_color];
  score += pop_count(opp_queens & my_rook_attacks) * ThreatRookOnQueen;

  uint64_t opp_minors = (position.pieces_bb[PieceTypes::Knight] |
                         position.pieces_bb[PieceTypes::Bishop]) &
                        position.colors_bb[opp_color];
  score += pop_count(opp_minors & my_rook_attacks) * ThreatRookOnMinor;

  uint64_t hanging = opp_pieces & ~my_pawn_attacks;
  uint64_t all_my_attacks = my_pawn_attacks | my_minor_attacks | my_rook_attacks;
  uint64_t undefended_hanging = hanging & all_my_attacks;
  score += pop_count(undefended_hanging) * ThreatHanging;

  return score;
}

int eval_sacrifice_patterns(const BoardState &position, int color) {
  int bonus = 0;
  int opp_color = color ^ 1;
  int my_q = pop_count(position.pieces_bb[PieceTypes::Queen] &
                       position.colors_bb[color]);
  int opp_q = pop_count(position.pieces_bb[PieceTypes::Queen] &
                        position.colors_bb[opp_color]);
  int my_r = pop_count(position.pieces_bb[PieceTypes::Rook] &
                       position.colors_bb[color]);
  int opp_r = pop_count(position.pieces_bb[PieceTypes::Rook] &
                        position.colors_bb[opp_color]);
  int my_l = pop_count((position.pieces_bb[PieceTypes::Knight] |
                        position.pieces_bb[PieceTypes::Bishop]) &
                       position.colors_bb[color]);
  int opp_l = pop_count((position.pieces_bb[PieceTypes::Knight] |
                         position.pieces_bb[PieceTypes::Bishop]) &
                        position.colors_bb[opp_color]);
  int my_p = pop_count(position.pieces_bb[PieceTypes::Pawn] &
                       position.colors_bb[color]);
  int opp_p = pop_count(position.pieces_bb[PieceTypes::Pawn] &
                        position.colors_bb[opp_color]);
  int p_diff = opp_p - my_p;

  if (my_q == 0 && opp_q >= 1 && my_p >= 3) {
    if (my_r == opp_r && my_l == opp_l)
      bonus += 80;
    else if (my_r >= opp_r && my_l >= opp_l)
      bonus += 60;
    else if (my_l > opp_l)
      bonus += 40;
  }

  if (p_diff >= 1 && p_diff <= 5 && my_q >= 1 && my_p >= 3) {
    if (my_r == opp_r && my_l == opp_l)
      bonus += 60 + (6 - p_diff) * 8;
    else if (my_r >= opp_r - 1 && my_l >= opp_l - 1)
      bonus += 40 + (6 - p_diff) * 5;
    else if (my_l >= opp_l)
      bonus += 25;
  }

  if (my_q == 0 && my_l >= 3 && my_r >= 1 && my_p >= 4) {
    if (opp_q >= 1)
      bonus += 70;
  }

  if (my_r < opp_r && my_l > opp_l + 1 && my_p >= opp_p)
    bonus += 50;
  if (my_r == opp_r - 1 && my_l >= opp_l + 1 && my_p >= opp_p)
    bonus += 40;

  return bonus;
}

int eval_king_safety(const BoardState &position, int color) {
  int score = 0;
  int king_sq = get_king_pos(position, color);

  if (!is_valid_square(king_sq))
    return 0;

  int king_file = get_file(king_sq);
  int king_rank = get_rank(king_sq);
  int opp_color = color ^ 1;

  int forward =
      (color == Colors::White) ? Directions::North : Directions::South;
  for (int f = std::max(0, king_file - 1); f <= std::min(7, king_file + 1);
       ++f) {
    uint64_t file_bb = Files[f];
    uint64_t my_pawns = position.pieces_bb[PieceTypes::Pawn] &
                        position.colors_bb[color] & file_bb;

    if (my_pawns) {
      score += KSPawnShield;
      int pawn_sq =
          (color == Colors::White) ? get_msb(my_pawns) : get_lsb(my_pawns);
      int pawn_rank = get_rank(pawn_sq);
      int dist = abs(pawn_rank - king_rank);
      if (dist == 1)
        score += KSPawnClose;
      else if (dist == 2)
        score += KSPawnMed;
    } else {
      score += KSNoPawn;
      uint64_t opp_pawns = position.pieces_bb[PieceTypes::Pawn] &
                           position.colors_bb[opp_color] & file_bb;
      if (!opp_pawns) {
        score += KSOpenFile;
      }
    }

    uint64_t opp_pawns_on_file = position.pieces_bb[PieceTypes::Pawn] &
                                 position.colors_bb[opp_color] & file_bb;
    if (opp_pawns_on_file) {
      int opp_pawn_sq = (color == Colors::White) ? get_lsb(opp_pawns_on_file)
                                                 : get_msb(opp_pawns_on_file);
      int opp_pawn_rank = get_rank(opp_pawn_sq);
      int dist_to_king = abs(opp_pawn_rank - king_rank);

      int storm_idx = std::clamp(dist_to_king - 1, 0, 3);
      score -= PawnStormConfig[storm_idx];
    }
  }

  if (total_mat(position) > PhaseMaterial::Endgame) {
    if (king_file >= 3 && king_file <= 4)
      score += KSCentralKingMajor;
    else if (king_file == 2 || king_file == 5)
      score += KSCentralKingMinor;

    if (color == Colors::White && king_rank > 2)
      score += KSAdvancedKing;
    if (color == Colors::Black && king_rank < 5)
      score += KSAdvancedKing;
  }

  int safe_squares = 0;
  uint64_t king_attacks = KING_ATK_SAFE(king_sq);
  while (king_attacks) {
    int sq = pop_lsb(king_attacks);
    if (!attacks_square(position, sq, opp_color)) {
      safe_squares++;
    }
  }
  if (safe_squares < 2)
    score += KSSafeSqLow;
  else if (safe_squares < 4)
    score += KSSafeSqMed;

  bool can_castle_ks =
      position.castling_squares[color][Sides::Kingside] != SquareNone;
  bool can_castle_qs =
      position.castling_squares[color][Sides::Queenside] != SquareNone;

  if (can_castle_ks || can_castle_qs)
    score += KSCastleBonus;
  if (!can_castle_ks && !can_castle_qs) {
    if (king_file == 1 || king_file == 2 || king_file == 6 || king_file == 7) {
      score += KSCastledFlank;
    } else {
      if (total_mat(position) > PhaseMaterial::LateMiddle)
        score -= KSCastledFlank;
    }
  }

  int king_start_sq = (color == Colors::White) ? 4 : 60;
  if (king_sq != king_start_sq && (can_castle_ks || can_castle_qs)) {
    score += KSMovedKingCastle;
  }

  if (king_sq != king_start_sq) {
    bool is_castled = (king_file == 1 || king_file == 2 || king_file == 6);
    if (!is_castled && total_mat(position) > PhaseMaterial::LateMiddle) {
      score += KSUncastledKing;
    }
  }

  const int *AttackWeight = KZAttackWeight;

  uint64_t king_zone = KING_ATK_SAFE(king_sq) | (1ULL << king_sq);
  if (color == Colors::White && king_rank < 6) {
    king_zone |= (KING_ATK_SAFE(king_sq) << 8) & ~Ranks[0];
    if (king_rank < 5)
      king_zone |= (KING_ATK_SAFE(king_sq) << 16) & ~Ranks[0] & ~Ranks[1];
  } else if (color == Colors::Black && king_rank > 1) {
    king_zone |= (KING_ATK_SAFE(king_sq) >> 8) & ~Ranks[7];
    if (king_rank > 2)
      king_zone |= (KING_ATK_SAFE(king_sq) >> 16) & ~Ranks[7] & ~Ranks[6];
  }

  int attack_units = 0;
  int attacker_count = 0;
  uint64_t occ = position.colors_bb[0] | position.colors_bb[1];

  uint64_t opp_pawn_attacks_kz = 0;
  uint64_t opp_p =
      position.pieces_bb[PieceTypes::Pawn] & position.colors_bb[opp_color];
  while (opp_p) {
    int sq = pop_lsb(opp_p);
    uint64_t atk = PAWN_ATK_SAFE(opp_color, sq);
    if (atk & king_zone) {
      opp_pawn_attacks_kz |= atk;
      attack_units += AttackWeight[PieceTypes::Pawn];
      attacker_count++;
    }
  }

  uint64_t opp_knights =
      position.pieces_bb[PieceTypes::Knight] & position.colors_bb[opp_color];
  while (opp_knights) {
    int sq = pop_lsb(opp_knights);
    if (KNIGHT_ATK_SAFE(sq) & king_zone) {
      attack_units += AttackWeight[PieceTypes::Knight];
      attacker_count++;
    }
  }

  uint64_t opp_bishops =
      position.pieces_bb[PieceTypes::Bishop] & position.colors_bb[opp_color];
  while (opp_bishops) {
    int sq = pop_lsb(opp_bishops);
    uint64_t batk = get_bishop_attacks(sq, occ);
    if (batk & king_zone) {
      attack_units += AttackWeight[PieceTypes::Bishop];
      attacker_count++;
      uint64_t xray = get_bishop_attacks(sq, occ ^ (batk & position.colors_bb[color]));
      if (xray & king_zone & ~batk)
        attack_units += KZBishopXray;
    }
  }

  uint64_t opp_rooks =
      position.pieces_bb[PieceTypes::Rook] & position.colors_bb[opp_color];
  while (opp_rooks) {
    int sq = pop_lsb(opp_rooks);
    uint64_t ratk = get_rook_attacks(sq, occ);
    if (ratk & king_zone) {
      attack_units += AttackWeight[PieceTypes::Rook];
      attacker_count++;
      uint64_t xray = get_rook_attacks(sq, occ ^ (ratk & position.colors_bb[color]));
      if (xray & king_zone & ~ratk)
        attack_units += KZRookXray;
    }
  }

  uint64_t opp_queens =
      position.pieces_bb[PieceTypes::Queen] & position.colors_bb[opp_color];
  while (opp_queens) {
    int sq = pop_lsb(opp_queens);
    if ((get_rook_attacks(sq, occ) | get_bishop_attacks(sq, occ)) & king_zone) {
      attack_units += AttackWeight[PieceTypes::Queen];
      attacker_count++;
    }
  }

  if (attacker_count >= 2) {
    int danger = attack_units * attacker_count;
    score -= danger * KZDangerMultiplier;

    if (attacker_count >= 3)
      score -= danger * KZMultiAttackerBonus;
  } else if (attacker_count == 1 && attack_units >= KZSingleAttackerThreshold) {
    score -= attack_units * KZSingleAttackerPenalty;
  }

  int opp_queen_count = pop_count(position.pieces_bb[PieceTypes::Queen] &
                                  position.colors_bb[opp_color]);
  if (opp_queen_count == 0 && attacker_count >= 2)
    score += KZNoQueenBonus;

  return score;
}

int eval_endgame(const BoardState &position, int color) {
  int score = 0;
  int opp_color = color ^ 1;
  int total_mat_val = total_mat(position);

  if (total_mat_val < EGMaterialThreshold && total_mat_color(position, color) >
                                  total_mat_color(position, opp_color) + EGMaterialAdvantage) {
    int opp_king = get_king_pos(position, opp_color);
    int my_king = get_king_pos(position, color);

    if (is_valid_square(opp_king) && is_valid_square(my_king)) {
      int opp_k_rank = get_rank(opp_king);
      int opp_k_file = get_file(opp_king);
      int center_dist = std::max(3 - opp_k_rank, opp_k_rank - 4) +
                        std::max(3 - opp_k_file, opp_k_file - 4);
      score += center_dist * EGCenterDist;
      int k_dist = std::max(abs(get_rank(my_king) - opp_k_rank),
                            abs(get_file(my_king) - opp_k_file));
      score += (14 - k_dist) * EGKingDist;
    }
  }

  uint64_t my_pawns =
      position.pieces_bb[PieceTypes::Pawn] & position.colors_bb[color];
  while (my_pawns) {
    int sq = pop_lsb(my_pawns);
    int rank = get_rank(sq);
    int relative_rank = (color == Colors::White) ? rank : (7 - rank);
    score += relative_rank * relative_rank * EGPassedPawnRank;
  }

  return score;
}

int eval_positional(const BoardState &position, int color) {
  int score = 0;
  int opp_color = color ^ 1;
  int my_bishops = pop_count(position.pieces_bb[PieceTypes::Bishop] &
                             position.colors_bb[color]);
  if (my_bishops >= 2)
    score += BishopPairBonus;

  uint64_t my_rooks =
      position.pieces_bb[PieceTypes::Rook] & position.colors_bb[color];
  uint64_t my_pawns =
      position.pieces_bb[PieceTypes::Pawn] & position.colors_bb[color];
  uint64_t opp_pawns =
      position.pieces_bb[PieceTypes::Pawn] & position.colors_bb[opp_color];
  while (my_rooks) {
    int sq = pop_lsb(my_rooks);
    int file = get_file(sq);
    uint64_t file_bb = Files[file];
    bool my_pawn_on_file = (my_pawns & file_bb) != 0;
    bool opp_pawn_on_file = (opp_pawns & file_bb) != 0;
    if (!my_pawn_on_file && !opp_pawn_on_file)
      score += RookOpenFile;
    else if (!my_pawn_on_file)
      score += RookSemiOpenFile;
  }

  uint64_t pawns =
      position.pieces_bb[PieceTypes::Pawn] & position.colors_bb[color];
  while (pawns) {
    int sq = pop_lsb(pawns);
    int file = get_file(sq);
    int rank = get_rank(sq);
    int relative_rank = (color == Colors::White) ? rank : (7 - rank);

    bool is_passed = true;
    for (int f = std::max(0, file - 1); f <= std::min(7, file + 1); ++f) {
      uint64_t ahead_mask = 0;
      if (color == Colors::White) {
        for (int r = rank + 1; r <= 7; ++r)
          ahead_mask |= (1ULL << (f + r * 8));
      } else {
        for (int r = rank - 1; r >= 0; --r)
          ahead_mask |= (1ULL << (f + r * 8));
      }
      if (opp_pawns & ahead_mask) {
        is_passed = false;
        break;
      }
    }

    if (is_passed) {
      int pass_bonus = PassedPawnBase + relative_rank * relative_rank * PassedPawnRankMul;
      int ahead_sq = sq + (color == Colors::White ? 8 : -8);
      if (is_valid_square(ahead_sq) &&
          position.board[ahead_sq] != Pieces::Blank) {
        pass_bonus += PassedPawnBlocked;
      }
      if (relative_rank >= PassedPawnKingProximityRank) {
        int my_king_sq = get_king_pos(position, color);
        int opp_king_sq = get_king_pos(position, opp_color);
        if (is_valid_square(my_king_sq) && is_valid_square(opp_king_sq)) {
          int promo_sq = (color == Colors::White) ? (file + 56) : file;
          int my_dist = std::max(abs(get_rank(my_king_sq) - get_rank(promo_sq)),
                                 abs(get_file(my_king_sq) - get_file(promo_sq)));
          int opp_dist = std::max(abs(get_rank(opp_king_sq) - get_rank(promo_sq)),
                                  abs(get_file(opp_king_sq) - get_file(promo_sq)));
          if (opp_dist > my_dist + 1)
            pass_bonus += PassedPawnKingProximity;
        }
      }
      score += pass_bonus;
    }
  }

  pawns = position.pieces_bb[PieceTypes::Pawn] & position.colors_bb[color];
  while (pawns) {
    int sq = pop_lsb(pawns);
    int file = get_file(sq);
    bool has_neighbor = false;
    if (file > 0 && (my_pawns & Files[file - 1]))
      has_neighbor = true;
    if (file < 7 && (my_pawns & Files[file + 1]))
      has_neighbor = true;
    if (!has_neighbor)
      score += IsolatedPawnPenalty;
  }

  uint64_t space_mask = 0;
  if (color == Colors::White) {
    space_mask = (Files[2] | Files[3] | Files[4] | Files[5]) &
                 (Ranks[2] | Ranks[3] | Ranks[4]);
  } else {
    space_mask = (Files[2] | Files[3] | Files[4] | Files[5]) &
                 (Ranks[5] | Ranks[4] | Ranks[3]);
  }

  uint64_t opp_pawn_attacks = 0;
  uint64_t opp_p =
      position.pieces_bb[PieceTypes::Pawn] & position.colors_bb[opp_color];
  while (opp_p) {
    int s = pop_lsb(opp_p);
    opp_pawn_attacks |= PAWN_ATK_SAFE(opp_color, s);
  }

  uint64_t safe_space = space_mask & ~opp_pawn_attacks;

  uint64_t our_occupancy = position.colors_bb[color] & safe_space;
  score += pop_count(our_occupancy) * SpaceWeight;

  for (int file = 0; file < 8; ++file) {
    int pawns_on_file = pop_count(my_pawns & Files[file]);
    if (pawns_on_file > 1)
      score += (pawns_on_file - 1) * DoubledPawnPenalty;
  }

  uint64_t my_knights =
      position.pieces_bb[PieceTypes::Knight] & position.colors_bb[color];
  while (my_knights) {
    int sq = pop_lsb(my_knights);
    int rank = get_rank(sq);
    int file = get_file(sq);
    int rel_rank = (color == Colors::White) ? rank : (7 - rank);

    if (rel_rank >= 3 && rel_rank <= 5) {
      int pawn_sq1 = sq + (color == Colors::White ? -9 : 7);
      int pawn_sq2 = sq + (color == Colors::White ? -7 : 9);
      bool pawn_support = false;
      if (is_valid_square(pawn_sq1) &&
          position.board[pawn_sq1] ==
              (color == Colors::White ? Pieces::WPawn : Pieces::BPawn))
        pawn_support = true;
      if (is_valid_square(pawn_sq2) &&
          position.board[pawn_sq2] ==
              (color == Colors::White ? Pieces::WPawn : Pieces::BPawn))
        pawn_support = true;

      if (pawn_support) {
        bool can_be_attacked = false;
        if (file > 0) {
          uint64_t left_file = Files[file - 1];
          uint64_t ahead =
              (color == Colors::White)
                  ? (left_file & (Ranks[rank] | Ranks[rank + 1] |
                                  (rank < 6 ? Ranks[rank + 2] : 0)))
                  : (left_file & (Ranks[rank] | Ranks[rank - 1] |
                                  (rank > 1 ? Ranks[rank - 2] : 0)));
          if (opp_pawns & ahead)
            can_be_attacked = true;
        }
        if (file < 7 && !can_be_attacked) {
          uint64_t right_file = Files[file + 1];
          uint64_t ahead =
              (color == Colors::White)
                  ? (right_file & (Ranks[rank] | Ranks[rank + 1] |
                                   (rank < 6 ? Ranks[rank + 2] : 0)))
                  : (right_file & (Ranks[rank] | Ranks[rank - 1] |
                                   (rank > 1 ? Ranks[rank - 2] : 0)));
          if (opp_pawns & ahead)
            can_be_attacked = true;
        }

        if (!can_be_attacked)
          score += OutpostBonus;
      }
    }
  }

  return score;
}

int eval(BoardState &position, ThreadInfo &thread_info) {
  int color = position.color;

  if (thread_info.use_syzygy && tb_initialized) {
    int tb_score = probe_wdl_tb(position, thread_info);
    if (tb_score != ScoreNone)
      return tb_score;
  }

  int hce_eval = material_eval(position);
  hce_eval += eval_sacrifice_patterns(position, color);
  hce_eval += eval_king_safety(position, color);
  hce_eval -= eval_king_safety(position, color ^ 1);
  hce_eval += eval_endgame(position, color);
  hce_eval -= eval_endgame(position, color ^ 1);
  hce_eval += eval_positional(position, color);
  hce_eval -= eval_positional(position, color ^ 1);
  hce_eval += eval_pst(position, color);
  hce_eval -= eval_pst(position, color ^ 1);
  hce_eval += eval_king_tropism(position, color);
  hce_eval -= eval_king_tropism(position, color ^ 1);
  hce_eval += eval_threats(position, color);
  hce_eval -= eval_threats(position, color ^ 1);

  hce_eval += TempoBonus;

  int bonus2 = 0, bonus3 = 0, bonus4 = 0, bonus5 = 0;
  bool our_side = (thread_info.search_ply % 2 == 0);

  int start_index = std::max(thread_info.game_ply - thread_info.search_ply, 0);
  int s_m = thread_info.game_hist[start_index].m_diff;
  int sacrifice_pattern = 0;

  for (int idx = start_index + 2; idx < thread_info.game_ply - 4; idx += 2) {
    bool pattern = (thread_info.game_hist[idx].m_diff < s_m &&
                    thread_info.game_hist[idx + 1].m_diff > s_m &&
                    thread_info.game_hist[idx + 2].m_diff < s_m &&
                    thread_info.game_hist[idx + 3].m_diff > s_m &&
                    thread_info.game_hist[idx + 4].m_diff < s_m);
    if (pattern) {
      sacrifice_pattern = s_m + thread_info.game_hist[idx + 4].m_diff;
      break;
    }

    if ((thread_info.game_hist[idx].piece_moved == Pieces::WQueen ||
         thread_info.game_hist[idx].piece_moved == Pieces::BQueen ||
         thread_info.game_hist[idx].piece_moved == Pieces::WRook ||
         thread_info.game_hist[idx].piece_moved == Pieces::BRook) &&
        thread_info.game_hist[idx].is_cap) {
      sacrifice_pattern = 3;
      break;
    }

    if (idx < thread_info.game_ply - 6) {
      int sacrifice_count = 0;
      for (int i = idx; i < idx + 6 && i < thread_info.game_ply; i++) {
        if (i > 0 && thread_info.game_hist[i].m_diff <
                         thread_info.game_hist[i - 1].m_diff - 50) {
          sacrifice_count++;
        }
      }
      if (sacrifice_count >= 2) {
        sacrifice_pattern = 4;
        break;
      }
    }
  }

  int total_material = total_mat(position);

  if (sacrifice_pattern && total_material > SacMaterialThreshold) {
    if (thread_info.search_ply % 2) {
      bonus2 = -SacPatternBonus * (hce_eval < -250 ? 3 : hce_eval < 0 ? 2 : 1);
    } else {
      bonus2 = SacPatternBonus * (hce_eval > 250 ? 3 : hce_eval > 0 ? 2 : 1);
    }
    int king_pos = get_king_pos(position, color ^ 1);
    int king_file = get_file(king_pos);
    if ((king_file >= 4 && sacrifice_pattern == 3) ||
        (king_file < 4 && sacrifice_pattern == 3)) {
      bonus2 += (thread_info.search_ply % 2) ? -SacKingFileBonus : SacKingFileBonus;
    }
    if (sacrifice_pattern == 4) {
      bonus2 += (thread_info.search_ply % 2) ? -SacMultiBonus : SacMultiBonus;
    }
  }

  uint64_t center_squares =
      (1ULL << 27) | (1ULL << 28) | (1ULL << 35) | (1ULL << 36);
  uint64_t extended_center = center_squares | (1ULL << 26) | (1ULL << 29) |
                             (1ULL << 34) | (1ULL << 37) | (1ULL << 42) |
                             (1ULL << 43) | (1ULL << 44) | (1ULL << 45);
  int center_control = 0;
  if (position.pieces_bb[PieceTypes::Knight] & position.colors_bb[color] &
      extended_center)
    center_control += CenterKnight;
  if (position.pieces_bb[PieceTypes::Bishop] & position.colors_bb[color] &
      extended_center)
    center_control += CenterBishop;
  if (position.pieces_bb[PieceTypes::Pawn] & position.colors_bb[color] &
      center_squares)
    center_control += CenterPawn;

  int mobility_bonus = 0;
  if (thread_info.game_ply < 20) {
    uint64_t queen_bb =
        position.pieces_bb[PieceTypes::Queen] & position.colors_bb[color];
    if (queen_bb) {
      int queen_sq = get_lsb(queen_bb);
      int rank = get_rank(queen_sq);
      int bonus_rank = color == Colors::White ? 3 : 4;
      if ((color == Colors::White && rank >= bonus_rank) ||
          (color == Colors::Black && rank <= bonus_rank)) {
        mobility_bonus += MobilityEarlyQueenBonus;
      }
    }
  }

  uint64_t own_knights =
      position.pieces_bb[PieceTypes::Knight] & position.colors_bb[color];
  uint64_t own_bishops =
      position.pieces_bb[PieceTypes::Bishop] & position.colors_bb[color];
  uint64_t own_rooks_mob =
      position.pieces_bb[PieceTypes::Rook] & position.colors_bb[color];
  uint64_t own_queens_mob =
      position.pieces_bb[PieceTypes::Queen] & position.colors_bb[color];
  uint64_t occ_mob = position.colors_bb[0] | position.colors_bb[1];
  while (own_knights) {
    int sq = pop_lsb(own_knights);
    int moves = pop_count(KNIGHT_ATK_SAFE(sq) & ~position.colors_bb[color]);
    mobility_bonus += (moves - MobilityKnightBase);
  }
  while (own_bishops) {
    int sq = pop_lsb(own_bishops);
    int moves = pop_count(get_bishop_attacks(sq, occ_mob) &
                  ~position.colors_bb[color]);
    mobility_bonus += (moves - MobilityBishopBase) * MobilityBishopMul / MobilityBishopDiv;
  }
  while (own_rooks_mob) {
    int sq = pop_lsb(own_rooks_mob);
    int moves = pop_count(get_rook_attacks(sq, occ_mob) &
                  ~position.colors_bb[color]);
    mobility_bonus += (moves - MobilityRookBase) * MobilityRookMul / MobilityRookDiv;
  }
  while (own_queens_mob) {
    int sq = pop_lsb(own_queens_mob);
    int moves = pop_count((get_rook_attacks(sq, occ_mob) |
                           get_bishop_attacks(sq, occ_mob)) &
                  ~position.colors_bb[color]);
    mobility_bonus += (moves - MobilityQueenBase) / MobilityQueenDiv;
  }

  int positional_bonus = 0;
  uint64_t home_ranks =
      color == Colors::White ? (Ranks[0] | Ranks[1]) : (Ranks[6] | Ranks[7]);
  uint64_t undeveloped_pieces = position.colors_bb[color] &
                                (position.pieces_bb[PieceTypes::Knight] |
                                 position.pieces_bb[PieceTypes::Bishop]) &
                                home_ranks;
  int undeveloped_count = pop_count(undeveloped_pieces);
  if (thread_info.game_ply > 10 && undeveloped_count > 0) {
    positional_bonus -= undeveloped_count * UndevelopedPenalty;
  }

  if (our_side) {
    bonus3 = center_control;
    bonus4 = mobility_bonus;
    bonus5 = positional_bonus;
  } else {
    bonus3 = -center_control;
    bonus4 = -mobility_bonus;
    bonus5 = -positional_bonus;
  }

  float multiplier = (static_cast<float>(EvalMultBase) + total_material / static_cast<float>(EvalMultMatDiv)) / static_cast<float>(EvalMultNorm);
  float phase_factor = 1.0f;
  switch (thread_info.phase) {
  case PhaseTypes::Opening:
    phase_factor = thread_info.opening_aggressiveness;
    break;
  case PhaseTypes::MiddleGame:
    phase_factor = thread_info.middlegame_aggressiveness;
    break;
  case PhaseTypes::LateMiddleGame:
    phase_factor = thread_info.late_middlegame_aggressiveness;
    break;
  case PhaseTypes::Endgame:
    phase_factor = thread_info.endgame_aggressiveness;
    break;
  case PhaseTypes::Sacrifice:
    phase_factor = thread_info.middlegame_aggressiveness * 1.1f;
    break;
  default:
    break;
  }

  if (hce_eval > 0 && total_material > EvalWinningMatThreshold) {
    multiplier *= (EvalWinningMul / 100.0f) * phase_factor;
  } else if (hce_eval > 0 && total_material > EvalSlightWinMatThreshold) {
    multiplier *= (EvalSlightWinMul / 100.0f) * phase_factor;
  } else if (hce_eval < EvalLosingThreshold && total_material > EvalWinningMatThreshold) {
    multiplier *= (EvalLosingMul / 100.0f) * phase_factor;
  } else if (hce_eval < EvalSlightLoseThreshold && total_material > EvalWinningMatThreshold) {
    multiplier *= (EvalSlightLoseMul / 100.0f) * phase_factor;
  } else {
    multiplier *= phase_factor;
  }

  if (thread_info.is_human && thread_info.search_ply < 3 &&
      thread_info.human_noise_sigma > 0) {

    int span = std::max(4, thread_info.human_noise_sigma / 4);
    int noise = (Random::dist(Random::rd) % (2 * span + 1)) - span;
    hce_eval += noise;
  }

  hce_eval = static_cast<int>(hce_eval * multiplier);
  return std::clamp(hce_eval + bonus2 + bonus3 + bonus4 + bonus5, -MateScore,
                    MateScore);
}

int correct_eval(const BoardState &position, ThreadInfo &thread_info,
                 int eval) {

  eval = eval * (HALFMOVE_SCALE_MAX - position.halfmoves) / HALFMOVE_SCALE_MAX;

  int corr =
      thread_info
          .PawnCorrHist[position.color][get_corrhist_index(position.pawn_key)];

  corr +=
      thread_info
          .NonPawnCorrHist[position.color][Colors::White][get_corrhist_index(
              position.non_pawn_key[Colors::White])];
  corr +=
      thread_info
          .NonPawnCorrHist[position.color][Colors::Black][get_corrhist_index(
              position.non_pawn_key[Colors::Black])];

  return std::clamp(eval + (CorrWeight * corr / 512), -MateScore, MateScore);
}

void ss_push(BoardState &position, ThreadInfo &thread_info, Action move) {

  if (!thread_info.game_hist.data()) {
    thread_data.stop = true;
    return;
  }

  if (thread_info.search_ply + 1 >= MaxSearchPly ||
      thread_info.game_ply >= MaxGameLen) {

    thread_data.tb_hits.fetch_add(1);
    thread_data.stop = true;
    return;
  }

  if (thread_info.game_ply < 0 || thread_info.game_ply >= MaxGameLen) {
    thread_data.tb_hits.fetch_add(1);
    thread_data.stop = true;
    return;
  }

  ++thread_info.search_ply;

  if (thread_info.search_ply > MaxSearchPly - 2) {
    thread_data.tb_fails.fetch_add(1);
    thread_data.stop = true;
    return;
  }

  const int gp = static_cast<int>(thread_info.game_ply);
  if (gp < 0 || gp >= MaxGameLen) {
    thread_data.stop = true;
    return;
  }

  const int from_sq = static_cast<int>(extract_from(move));
  const int to_sq = static_cast<int>(extract_to(move));

  if (!is_valid_square(from_sq) || !is_valid_square(to_sq)) {

    thread_data.stop = true;
    return;
  }

  thread_info.game_hist[gp].position_key = position.zobrist_key;
  thread_info.game_hist[gp].played_move = move;
  thread_info.game_hist[gp].piece_moved = position.board[from_sq];
  thread_info.game_hist[gp].is_cap = is_cap(position, move);
  thread_info.game_hist[gp].m_diff = material_eval(position);

  if (thread_info.game_ply + 1 < MaxGameLen)
    thread_info.game_ply++;
}

void ss_pop(ThreadInfo &thread_info) {

  if (thread_info.search_ply <= 0 || thread_info.game_ply <= 0) {
    return;
  }

  if (thread_info.search_ply > MaxSearchPly ||
      thread_info.game_ply > MaxGameLen) {
    return;
  }

  thread_info.search_ply--;
  thread_info.game_ply--;
}

bool material_draw(const BoardState &position) {

  for (int i : {0, 1, 6, 7, 8, 9}) {
    if (position.material_count[i]) {
      return false;
    }
  }
  if (position.material_count[4] > 1 || position.material_count[2] > 2 ||
      (position.material_count[2] && position.material_count[4])) {

    return false;
  }
  if (position.material_count[5] > 1 || position.material_count[3] > 2 ||
      (position.material_count[3] && position.material_count[5])) {

    return false;
  }
  return true;
}

bool is_draw(const BoardState &position, ThreadInfo &thread_info) {

  const uint64_t hash = position.zobrist_key;
  const int halfmoves = position.halfmoves;
  const int game_ply = thread_info.game_ply;

  if (game_ply < 0 || game_ply > MaxGameLen) {
    return false;
  }

  if (!thread_info.game_hist.data()) {
    return false;
  }

  int white_king = get_king_pos(position, Colors::White);
  int black_king = get_king_pos(position, Colors::Black);
  if (!is_valid_square(white_king) || !is_valid_square(black_king)) {
    return false;
  }

  if (halfmoves >= 100) {
    return true;
  }

  if (material_draw(position)) {
    return true;
  }

  if (game_ply >= 2 && game_ply <= MaxGameLen) {
    const int min_index = std::max(game_ply - halfmoves, 0);
    for (int i = game_ply - 2; i >= min_index && i < MaxGameLen; i -= 2) {

      if (i >= 0 && i < MaxGameLen) {
        if (thread_info.game_hist[i].position_key == hash) {
          return true;
        }
      }
    }
  }

  const int king_sq = get_king_pos(position, position.color);
  if (!is_valid_square(king_sq)) {
    return false;
  }
  if (!attacks_square(position, king_sq, position.color ^ 1)) {

    uint64_t king_moves =
        KING_ATK_SAFE(king_sq) & ~position.colors_bb[position.color];
    bool has_safe_move = false;
    while (king_moves && !has_safe_move) {
      int to_sq = pop_lsb(king_moves);
      if (!attacks_square(position, to_sq, position.color ^ 1)) {
        has_safe_move = true;
      }
    }
    if (!has_safe_move) {

      std::array<Action, 32> limited_moves{};
      int move_count = 0;

      uint64_t king_attacks =
          KING_ATK_SAFE(king_sq) & ~position.colors_bb[position.color];
      while (king_attacks && move_count < 32) {
        int to = pop_lsb(king_attacks);
        if (!attacks_square(position, to, position.color ^ 1)) {
          limited_moves[move_count++] =
              pack_move(king_sq, to, MoveTypes::Normal);
        }
      }

      if (move_count == 0) {
        for (int pt = PieceTypes::Pawn;
             pt <= PieceTypes::Queen && move_count < 32; ++pt) {
          uint64_t pieces =
              position.pieces_bb[pt] & position.colors_bb[position.color];
          if (pieces) {
            int from = get_lsb(pieces);

            uint64_t attacks = 0;
            if (pt == PieceTypes::Pawn) {
              attacks = PAWN_ATK_SAFE(position.color, from) &
                        position.colors_bb[position.color ^ 1];
            } else if (pt == PieceTypes::Knight) {
              attacks = KNIGHT_ATK_SAFE(from);
            } else if (pt == PieceTypes::Bishop || pt == PieceTypes::Rook ||
                       pt == PieceTypes::Queen) {
              attacks = get_bishop_attacks(from, position.colors_bb[0] |
                                                     position.colors_bb[1]);
              if (pt == PieceTypes::Rook || pt == PieceTypes::Queen) {
                attacks |= get_rook_attacks(from, position.colors_bb[0] |
                                                      position.colors_bb[1]);
              }
            } else if (pt == PieceTypes::King) {
              attacks = KING_ATK_SAFE(from);
            }
            attacks &= ~position.colors_bb[position.color];

            while (attacks && move_count < 32) {
              int to = pop_lsb(attacks);
              limited_moves[move_count++] =
                  pack_move(from, to, MoveTypes::Normal);
            }
          }
        }
      }

      bool has_legal_move = false;
      for (int i = 0; i < move_count && !has_legal_move; ++i) {
        if (is_legal(position, limited_moves[i])) {
          has_legal_move = true;
        }
      }

      if (!has_legal_move) {
        return true;
      }
    }
  }

  return false;
}

int qsearch(int alpha, int beta, BoardState &position, ThreadInfo &thread_info,
            std::vector<TTBucket> &TT, int qdepth = 0) {

  constexpr int MAX_QPLY = 32;
  constexpr int MAX_QDEPTH = 16;

  auto eval_now = [&](BoardState &pos) {
    return correct_eval(pos, thread_info, eval(pos, thread_info));
  };

  if (qdepth >= MAX_QDEPTH) {
    return eval_now(position);
  }

  int ply = thread_info.search_ply;

  if (ply >= MaxSearchPly - 4 || ply >= MAX_QPLY) {
    return eval_now(position);
  }

  if (thread_info.game_ply < 0 || thread_info.game_ply > MaxGameLen) {
    return eval_now(position);
  }

  if (!thread_info.game_hist.data()) {
    return eval_now(position);
  }

  if (ply && is_draw(position, thread_info)) {
    return eval_now(position);
  }

  MoveInfo legal_probe;
  if (legal_movegen(position, legal_probe.moves.data()) == 0) {
    return eval_now(position);
  }

  if (thread_info.max_depth > 0 && ply >= thread_info.max_depth) {
    return eval_now(position);
  }

  if (out_of_time(thread_info)) {
    int hce_eval = material_eval(position);
    return correct_eval(position, thread_info, hce_eval);
  }

  if (thread_info.use_syzygy && tb_initialized) {
    int tb_score = probe_wdl_tb(position, thread_info);
    if (tb_score != ScoreNone)
      return tb_score;
  }

  int _hist_idx = thread_info.game_ply;
  if (_hist_idx < 0)
    _hist_idx = 0;
  if (_hist_idx >= MaxGameLen)
    _hist_idx = MaxGameLen - 1;
  StateRecord *ss = &(thread_info.game_hist[_hist_idx]);

  ++thread_info.nodes;
  if (ply > thread_info.seldepth)
    thread_info.seldepth = ply;

  if (ply >= MaxSearchPly - 3 || ply >= MAX_QPLY - 2) {
    return eval_now(position);
  }

  uint64_t hash = position.zobrist_key;
  uint8_t saved_phase = thread_info.phase;

  bool tt_hit;
  TTEntry entry = probe_entry(hash, tt_hit, thread_info.searches, TT);

  int entry_type = EntryTypes::None;
  int tt_static_eval = ScoreNone;
  int tt_score = ScoreNone;
  Action tt_move = MoveNone;

  if (tt_hit) {
    entry_type = entry.get_type();
    tt_static_eval = entry.static_eval;
    tt_score = score_from_tt(entry.score, ply);
    tt_move = entry.best_move;
  }

  if (tt_score != ScoreNone &&
      ((entry_type == EntryTypes::Exact) ||
       (entry_type == EntryTypes::LBound && tt_score >= beta) ||
       (entry_type == EntryTypes::UBound && tt_score <= alpha))) {
    return tt_score;
  }

  bool in_check = attacks_square(
      position, get_king_pos(position, position.color), position.color ^ 1);

  int raw_eval = ScoreNone;
  int static_eval = ScoreNone;
  int stand_pat = ScoreNone;
  int best_score = ScoreNone;
  Action best_move = MoveNone;
  bool raised_alpha = false;

  if (!in_check) {
    raw_eval = (tt_static_eval == ScoreNone) ? eval(position, thread_info)
                                             : tt_static_eval;
    static_eval = correct_eval(position, thread_info, raw_eval);
    ss->static_eval = static_eval;

    stand_pat = static_eval;
    best_score = stand_pat;

    if (tt_score != ScoreNone) {
      if (entry_type == EntryTypes::Exact ||
          (entry_type == EntryTypes::UBound && tt_score < stand_pat) ||
          (entry_type == EntryTypes::LBound && tt_score > stand_pat)) {
        stand_pat = best_score = tt_score;
      }
    }

    if (stand_pat >= beta) {
      insert_entry(entry, hash, 0, MoveNone, raw_eval,
                   score_to_tt(stand_pat, ply), EntryTypes::LBound,
                   thread_info.searches);
      return stand_pat;
    }

    if (stand_pat > alpha) {
      alpha = stand_pat;
      raised_alpha = true;
    }
  } else {
    ss->static_eval = ScoreNone;
  }

  MovePicker picker;
  init_picker(picker, position, -107, in_check, ss);

  if (!in_check && tt_move != MoveNone) {
    bool tt_is_cap = is_cap(position, tt_move);
    bool tt_is_promo = extract_type(tt_move) == MoveTypes::Promotion;
    if (!tt_is_cap && !tt_is_promo)
      tt_move = MoveNone;
  }

  static constexpr int PromoPieceTypes[] = {
      PieceTypes::Knight, PieceTypes::Bishop, PieceTypes::Rook,
      PieceTypes::Queen};

  auto fallback_score = [&](int current_best) {
    if (current_best != ScoreNone)
      return current_best;
    if (!in_check && stand_pat != ScoreNone)
      return stand_pat;
    return eval_now(position);
  };

  while (Action move =
             next_move(picker, position, thread_info, tt_move, !in_check)) {
    if (!in_check && picker.stage > Stages::Captures)
      break;
    if (!is_legal(position, move))
      continue;

    int from_sq = extract_from(move);
    int to_sq = extract_to(move);
    if (!is_valid_square(from_sq) || !is_valid_square(to_sq))
      continue;

    if (!in_check && stand_pat != ScoreNone) {
      int captured_piece = position.board[to_sq];
      if (!captured_piece && extract_type(move) == MoveTypes::EnPassant) {
        captured_piece = Pieces::WPawn + (position.color ^ 1);
      }
      int capture_value = MaterialValues[get_piece_type(captured_piece)];
      int promotion_gain = 0;
      if (extract_type(move) == MoveTypes::Promotion) {
        promotion_gain = MaterialValues[PromoPieceTypes[extract_promo(move)]] -
                         MaterialValues[PieceTypes::Pawn];
      }
      int delta_margin = capture_value + promotion_gain + DELTA_MARGIN_BASE;
      if (stand_pat + delta_margin < alpha)
        continue;
    }

    BoardState moved_position = position;
    make_move(moved_position, move);

    int score = ScoreNone;
    bool can_recurse = (thread_info.search_ply + 1 < MaxSearchPly - 4) &&
                       (thread_info.search_ply + 1 < MAX_QPLY - 2) &&
                       (thread_info.game_ply < MaxGameLen - 2) &&
                       (qdepth + 1 < MAX_QDEPTH);

    if (can_recurse) {

      if (thread_info.game_ply >= 0 && thread_info.game_ply < MaxGameLen &&
          thread_info.game_hist.data()) {
        ss_push(position, thread_info, move);
        score = -qsearch(-beta, -alpha, moved_position, thread_info, TT,
                         qdepth + 1);
        ss_pop(thread_info);
      } else {
        int leaf_eval = eval_now(moved_position);
        score = -leaf_eval;
      }
    } else {
      int leaf_eval = eval_now(moved_position);
      score = -leaf_eval;
    }

    thread_info.phase = saved_phase;

    if (thread_data.stop || thread_info.datagen_stop) {
      return fallback_score(best_score);
    }

    if (best_score == ScoreNone || score > best_score) {
      best_score = score;
    }

    if (score > alpha) {
      alpha = score;
      best_move = move;
      raised_alpha = true;
      if (score >= beta)
        break;
    }
  }

  thread_info.phase = saved_phase;

  if (best_score == ScoreNone) {
    if (in_check) {
      return -MateScore + ply;
    }
    best_score = (stand_pat != ScoreNone) ? stand_pat : eval_now(position);
  }

  uint8_t store_type = EntryTypes::UBound;
  if (best_score >= beta)
    store_type = EntryTypes::LBound;
  else if (raised_alpha)
    store_type = EntryTypes::Exact;

  insert_entry(entry, hash, 0, best_move, raw_eval,
               score_to_tt(best_score, ply), store_type, thread_info.searches);
  return best_score;
}

template <bool is_pv>
int search(int alpha, int beta, int depth, bool cutnode, BoardState &position,
           ThreadInfo &thread_info, std::vector<TTBucket> &TT) {

  StateRecord *ss = &(thread_info.game_hist[thread_info.game_ply]);

  if (!thread_info.search_ply) {
    thread_info.current_iter = depth;
    thread_info.seldepth = 0;
    thread_info.pv.fill(MoveNone);
  }

  int ply = thread_info.search_ply, pv_index = ply * MaxSearchPly;

  if (ply > thread_info.seldepth) {
    thread_info.seldepth = ply;
  }

  if (out_of_time(thread_info) || ply >= MaxSearchPly - 1) {

    return correct_eval(position, thread_info, eval(position, thread_info));
  }

  if (ply && is_draw(position, thread_info)) {
    int draw_score = 1 - (thread_info.nodes.load() & 3);

    int material = material_eval(position);

    if (material < 0) {
      draw_score += DrawContemptMaterial;
    } else if (material > 0) {
      draw_score -= DrawContemptMaterial;
    }

    draw_score += Contempt;

    return draw_score;
  }

  if (thread_info.max_depth > 0 && ply >= thread_info.max_depth) {
    return correct_eval(position, thread_info, eval(position, thread_info));
  }

  if (depth <= 0) {
    return qsearch(alpha, beta, position, thread_info, TT);
  }
  ++thread_info.nodes;

  bool root = !ply, color = position.color, raised_alpha = false;

  Action best_move = MoveNone;
  Action excluded_move = thread_info.excluded_move;

  bool singular_search = (excluded_move != MoveNone);

  if (!singular_search) {
    thread_info.pv[pv_index] = MoveNone;
  }

  thread_info.excluded_move = MoveNone;

  int score = ScoreNone;

  uint64_t hash = position.zobrist_key;
  uint8_t phase = thread_info.phase;

  int mate_distance = MateScore - 1 - ply;
  if (mate_distance < beta)

  {
    beta = mate_distance;
    if (alpha >= beta) {
      return beta;
    }
  }

  bool tt_hit;
  TTEntry entry = probe_entry(hash, tt_hit, thread_info.searches, TT);

  int entry_type = EntryTypes::None, tt_static_eval = ScoreNone,
      tt_score = ScoreNone, tt_move = MoveNone;

  if (tt_hit && !singular_search) {
    entry_type = entry.get_type();
    tt_static_eval = entry.static_eval;
    tt_score = score_from_tt(entry.score, ply);
    tt_move = entry.best_move;
  }

  if (tt_score != ScoreNone && !is_pv && entry.depth >= depth) {

    if ((entry_type == EntryTypes::Exact) ||
        (entry_type == EntryTypes::LBound && tt_score >= beta) ||
        (entry_type == EntryTypes::UBound && tt_score <= alpha)) {
      return tt_score;
    }
  }

  uint64_t in_check =
      attacks_square(position, get_king_pos(position, color), color ^ 1);

  int32_t static_eval;
  int32_t raw_eval;

  if (in_check) {
    static_eval = raw_eval = ScoreNone;
  } else if (singular_search) {
    static_eval = raw_eval = ss->static_eval;
  } else {
    if (tt_static_eval == ScoreNone) {
      raw_eval = eval(position, thread_info);
      static_eval = correct_eval(position, thread_info, raw_eval);

    } else {
      raw_eval = tt_static_eval;
      static_eval = correct_eval(position, thread_info, raw_eval);
    }

    if (!tt_hit) {
      insert_entry(entry, hash, 0, MoveNone, raw_eval, ScoreNone,
                   EntryTypes::None, thread_info.searches);
    }
  }

  ss->static_eval = static_eval;

  bool improving = false;

  if (ply > 1 && thread_info.game_ply >= 2 && !in_check &&
      static_eval > (ss - 2)->static_eval) {
    improving = true;
  }

  if (tt_score != ScoreNone) {
    if (entry_type == EntryTypes::Exact ||
        (entry_type == EntryTypes::UBound && tt_score < static_eval) ||
        (entry_type == EntryTypes::LBound && tt_score > static_eval)) {

      static_eval = tt_score;
    }
  }

  if (!is_pv && !in_check && !singular_search) {

    if (thread_info.use_syzygy && tb_initialized &&
        depth >= thread_info.syzygy_probe_depth) {
      int material_count =
          pop_count(position.colors_bb[0] | position.colors_bb[1]);
      int largest = TB_LARGEST ? (int)TB_LARGEST : 7;
      if (material_count <= std::min(thread_info.syzygy_probe_limit, largest) &&
          !in_check && !is_pv) {
        int tb_score = probe_wdl_tb(position, thread_info);
        if (tb_score != ScoreNone) {

          if (tb_score >= beta)
            return tb_score;
          if (tb_score <= alpha)
            return tb_score;
        }
      }
    }

    if (depth <= RFPMaxDepth &&
        static_eval - RFPMargin * (depth - improving) >= beta) {
      return (static_eval + beta) / 2;
    }

    if (!is_pv && depth <= 3 && static_eval + RazorMargin * depth < alpha) {
      int razor_score = qsearch(alpha, beta, position, thread_info, TT);
      if (razor_score <= alpha)
        return razor_score;
    }

    if (static_eval >= beta && depth >= NMPMinDepth &&
        has_non_pawn_material(position, color) && thread_info.game_ply > 0 &&
        (ss - 1)->played_move != MoveNone) {

      BoardState temp_pos = position;

      make_move(temp_pos, MoveNone);

      if (thread_info.search_ply >= MaxSearchPly ||
          thread_info.game_ply >= MaxGameLen) {
        return ScoreNone;
      }
      ss_push(position, thread_info, MoveNone);

      int R = NMPBase + depth / NMPDepthDiv +
              std::min(3, (static_eval - beta) / NMPEvalDiv);
      score = -search<false>(-alpha - 1, -alpha, depth - R, !cutnode, temp_pos,
                             thread_info, TT);

      thread_info.search_ply--, thread_info.game_ply--;

      if (score >= beta) {
        if (score > MateScore) {
          score = beta;
        }
        return score;
      }
    }
  }

  if (!is_pv && !in_check && !singular_search && cutnode &&
      depth >= MultiCutDepth && tt_move != MoveNone) {
    int mc_cuts = 0;
    int mc_moves = 0;

    MovePicker mc_picker;
    init_picker(mc_picker, position, 0, in_check, ss);

    while (Action move =
               next_move(mc_picker, position, thread_info, tt_move, false)) {
      if (mc_moves >= MultiCutMoves)
        break;
      if (!is_legal(position, move))
        continue;

      mc_moves++;

      BoardState mc_pos = position;
      make_move(mc_pos, move);

      ss_push(position, thread_info, move);
      int mc_score = -search<false>(-beta, -beta + 1, depth - 4, false, mc_pos,
                                    thread_info, TT);
      ss_pop(thread_info);
      thread_info.phase = phase;

      if (mc_score >= beta) {
        mc_cuts++;
        if (mc_cuts >= MultiCutCuts) {
          return beta;
        }
      }
    }
  }

  if ((is_pv || cutnode) && tt_move == MoveNone && depth > IIRMinDepth) {

    depth--;
  }

  int p_beta = beta + ProbCutMargin;
  if (depth >= 5 && abs(beta) < MateScore &&
      (!tt_hit || entry.depth + 4 <= depth || tt_score >= p_beta)) {

    int threshold = p_beta - static_eval;
    MovePicker probcut_p;
    init_picker(probcut_p, position, threshold, in_check, ss);
    Action p_tt_move =
        (tt_move != MoveNone && SEE(position, tt_move, threshold) ? tt_move
                                                                  : MoveNone);

    while (Action move =
               next_move(probcut_p, position, thread_info, p_tt_move, true)) {

      if (probcut_p.stage > Stages::Captures) {
        break;
      }
      if (move == excluded_move || !is_legal(position, move)) {
        continue;
      }

      BoardState moved_position = position;

      make_move(moved_position, move);

      if (thread_info.search_ply >= MaxSearchPly ||
          thread_info.game_ply >= MaxGameLen) {
        return ScoreNone;
      }
      ss_push(position, thread_info, move);

      int score =
          -qsearch(-p_beta, -p_beta + 1, moved_position, thread_info, TT);
      if (score >= p_beta) {
        score = -search<is_pv>(-p_beta, -p_beta + 1, depth - 4, false,
                               moved_position, thread_info, TT);
      }

      ss_pop(thread_info);
      thread_info.phase = phase;

      if (score >= p_beta) {
        return score;
      }
    }
  }

  Action quiets[64];
  int num_quiets = 0;
  Action captures[64];
  int num_captures = 0;
  thread_info.KillerMoves[ply + 1][0] = MoveNone;
  thread_info.KillerMoves[ply + 1][1] = MoveNone;

  MovePicker picker;
  init_picker(picker, position, -107, in_check, ss);

  int best_score = ScoreNone, moves_played = 0;
  bool is_capture = false, skip = false;

  if (thread_info.sacrifice_lookahead > 0 &&
      thread_info.search_ply < thread_info.sacrifice_lookahead && !in_check) {
    std::array<Action, MaxActions> moves;
    uint64_t checkers_local =
        attacks_square(position, get_king_pos(position, color), color ^ 1);
    int nmoves_local =
        movegen(position, moves.data(), checkers_local, Generate::GenAll);

    int lookahead_cap = std::clamp(thread_info.sacrifice_lookahead, 0, 1);

    for (int i = 0; i < nmoves_local; i++) {
      Action m = moves[i];
      if (!is_legal(position, m))
        continue;

      bool isCapture = is_cap(position, m);
      bool losing_exchange = !SEE(position, m, 0);
      if (!isCapture && !losing_exchange)
        continue;

      BoardState test_position = position;

      make_move(test_position, m);

      int sacrifice_score = analyze_sacrifice(test_position, thread_info,
                                              lookahead_cap, 0, position.color);
      if (sacrifice_score <= 0)
        continue;

      int sac_extension =
          1 +
          (thread_info.sacrifice_lookahead_aggressiveness >= 130
               ? 2
               : (thread_info.sacrifice_lookahead_aggressiveness >= 90 ? 1
                                                                       : 0));
      sac_extension = std::clamp(sac_extension, 1, 3);

      BoardState moved_position = position;

      make_move(moved_position, m);

      if (thread_info.search_ply >= MaxSearchPly ||
          thread_info.game_ply >= MaxGameLen) {
        return ScoreNone;
      }
      ss_push(position, thread_info, m);

      int probe_score =
          -search<false>(-alpha - 1, -alpha, std::max(1, depth - 1), false,
                         moved_position, thread_info, TT);
      int score;
      if (probe_score > alpha && abs(probe_score) < MateScore) {

        int extended_depth = std::min(depth + sac_extension, 126);
        score = -search<true>(-beta, -alpha, extended_depth, false,
                              moved_position, thread_info, TT);
      } else {
        score = probe_score;
      }

      ss_pop(thread_info);
      thread_info.phase = phase;

      if (abs(score) < MateScore && score > alpha) {

        float phase_bonus = 0.0f;
        int move_number_local = (thread_info.game_ply / 2) + 1;
        if (move_number_local < 10)
          phase_bonus = thread_info.opening_aggressiveness * 15.0f;
        else if (move_number_local < 25)
          phase_bonus = thread_info.middlegame_aggressiveness * 20.0f;
        else if (move_number_local < 40)
          phase_bonus = thread_info.late_middlegame_aggressiveness * 25.0f;
        else
          phase_bonus = thread_info.endgame_aggressiveness * 15.0f;

        float aggr_factor_local =
            thread_info.sacrifice_lookahead_aggressiveness / 100.0f;
        int bonus = static_cast<int>(phase_bonus * aggr_factor_local);
        score += bonus;

        int piece_from = position.board[extract_from(m)];
        int to_sq = extract_to(m);
        int hist_bonus = std::clamp(16 + sacrifice_score / 8, 8, 128);
        if (thread_info.attack_mode) {
          hist_bonus = hist_bonus * AttackModeHistMul / AttackModeHistDiv + AttackModeHistAdd;
          hist_bonus = std::min(hist_bonus, AttackModeHistCap);
        }
        update_history(thread_info.HistoryScores[piece_from][to_sq],
                       hist_bonus);

        if (score > best_score) {
          best_score = score;
          if (score > alpha) {
            alpha = score;
            best_move = m;
            raised_alpha = true;
            if (is_pv)
              thread_info.pv[pv_index] = m;
            if (score >= beta)
              break;
          }
        }
      }
      if (thread_data.stop || thread_info.datagen_stop)
        break;
    }
  }

  while (Action move =
             next_move(picker, position, thread_info, tt_move, skip)) {

    if (root) {
      bool pv_skip = false;
      for (int i = 0; i < thread_info.multipv_index; i++) {
        if (thread_info.best_moves[i] == move) {
          pv_skip = true;
          break;
        }
      }
      if (pv_skip) {
        continue;
      }
    }

    if (move == excluded_move) {
      continue;
    }
    if (!is_legal(position, move)) {
      continue;
    }

    uint64_t curr_nodes = thread_info.nodes.load();

    int hist_score =
        thread_info.HistoryScores[position.board[extract_from(move)]]
                                 [extract_to(move)];

    is_capture = is_cap(position, move);
    if (!is_capture && !is_pv && best_score > -MateScore) {

      if (depth < LMPDepth &&
          moves_played >= LMPBase + depth * depth / (2 - improving)) {
        skip = true;
      }

      if (!in_check && depth < FPDepth && picker.stage > Stages::Captures) {
        int fp_margin = FPMargin1 + FPMargin2 * depth;
        if (thread_info.attack_mode)
          fp_margin += FPAttackModeBonus;
        if (static_eval + fp_margin < alpha) {
          skip = true;
        }
      }

      if (!is_pv && !is_capture && depth < HistPruneDepth && hist_score < -HistPruneThreshold * depth) {
        skip = true;
      }
    }

    if (!root && best_score > -MateScore && depth < SeePruningDepth) {

      int margin =
          is_capture ? SeePruningQuietMargin : (depth * SeePruningNoisyMargin);

      if (!SEE(position, move, depth * margin)) {

        continue;
      }
    }

    int extension = 0;

    if (!root && ply < thread_info.current_iter * 2) {
      if (!singular_search && depth >= SEDepth && move == tt_move &&
          abs(entry.score) < MateScore && entry.depth >= depth - 3 &&
          entry_type != EntryTypes::UBound) {

        int sBeta = entry.score - depth;
        thread_info.excluded_move = move;
        int sScore = search<false>(sBeta - 1, sBeta, (depth - 1) / 2, cutnode,
                                   position, thread_info, TT);

        if (sScore < sBeta) {
          if (!is_pv && sScore + SEDoubleExtMargin < sBeta &&
              ply < thread_info.current_iter) {

            extension = 2 + (!is_capture && sScore < sBeta - 125);
          } else {
            extension = 1;
          }
        } else if (sBeta >= beta) {

          return sBeta;
        } else if (cutnode) {
          extension = -1;
        }
      }
    }

    if (extension == 0 && !is_capture && depth >= 4) {
      if (hist_score > HistExtThreshold) {
        extension = 1;
      }
    }

    BoardState moved_position = position;
    make_move(moved_position, move);

    if (thread_info.search_ply >= MaxSearchPly ||
        thread_info.game_ply >= MaxGameLen) {
      return best_score;
    }
    ss_push(position, thread_info, move);

    bool full_search = false;
    int newdepth = std::min(depth - 1 + extension, 126);

    if (depth >= LMRMinDepth && moves_played > is_pv) {
      int R = LMRTable[depth][moves_played];
      if (is_capture) {
        R /= 2;
      } else {
        R -= hist_score / 8192;
      }

      R -= is_pv;

      R -= (tt_hit && entry.depth >= depth);

      R += !improving;

      R += cutnode;

      bool gives_check = (attacks_square(moved_position,
                           get_king_pos(position, color ^ 1), color) != 0);
      R -= gives_check;

      if (gives_check && depth >= 6)
        R -= 1;

      if (thread_info.attack_mode && R > 0) {
        R = std::max(0, R - 2);
      }

      R = std::clamp(R, 0, newdepth - 1);

      score = -search<false>(-alpha - 1, -alpha, newdepth - R, true,
                             moved_position, thread_info, TT);
      if (score > alpha) {
        full_search = R > 0;
        newdepth += (score > (best_score + 60 + newdepth * 2));
        newdepth -= (score < best_score + newdepth && !root);
      }
    } else {
      full_search = moves_played || !is_pv;
    }
    if (full_search) {

      score = -search<false>(-alpha - 1, -alpha, newdepth, !cutnode,
                             moved_position, thread_info, TT);
    }
    if ((score > alpha || !moves_played) && is_pv) {

      score = -search<true>(-beta, -alpha, newdepth, false, moved_position,
                            thread_info, TT);
    }

    ss_pop(thread_info);
    thread_info.phase = phase;

    if (thread_data.stop || thread_info.datagen_stop) {

      return best_score;
    }

    if (root) {
      find_root_move(thread_info, move)->nodes +=
          (thread_info.nodes.load() - curr_nodes);
    }

    if (score > best_score) {
      best_score = score;

      if (score > alpha) {
        best_move = move;
        raised_alpha = true;
        alpha = score;

        if (score >= beta) {
          thread_info.pv[pv_index] = best_move;
          break;
        }

        else {

          thread_info.pv[pv_index] = best_move;
          for (int n = 0; n < MaxSearchPly + ply + 1; n++) {
            thread_info.pv[pv_index + 1 + n] =
                thread_info.pv[pv_index + MaxSearchPly + n];
          }
        }
      }
    }

    if (is_capture) {
      if (num_captures < 64)
        captures[num_captures++] = move;
    } else {
      if (num_quiets < 64)
        quiets[num_quiets++] = move;
    }

    moves_played++;
  }

  if (root) {
    if (best_move != MoveNone) {
      thread_info.best_moves[thread_info.multipv_index] = best_move;
    }
    thread_info.best_scores[thread_info.multipv_index] = best_score;
  }

  if (best_score >= beta) {

    int piece = position.board[extract_from(best_move)],
        sq = extract_to(best_move);

    int bonus = std::min(
        (int)HistBonus * (depth - 1 + (best_score > beta + 125)), (int)HistMax);

    if (is_capture) {
      int capture_bonus = bonus / 2;
      update_history(thread_info.CapHistScores[piece][sq], capture_bonus);

    } else {

      Action their_last = MoveNone;
      int their_piece = Pieces::Blank;
      Action our_last = MoveNone;
      int our_piece = Pieces::Blank;
      Action ply4_last = MoveNone;
      int ply4_piece = Pieces::Blank;

      if (thread_info.game_ply >= 1) {
        their_last = extract_to((ss - 1)->played_move);
        their_piece = (ss - 1)->piece_moved;
      }
      if (thread_info.game_ply >= 2) {
        our_last = extract_to((ss - 2)->played_move);
        our_piece = (ss - 2)->piece_moved;
      }
      if (thread_info.game_ply >= 4) {
        ply4_last = extract_to((ss - 4)->played_move);
        ply4_piece = (ss - 4)->piece_moved;
      }

      for (int i = 0; i < num_quiets; i++) {

        Action move = quiets[i];

        int piece_m = position.board[extract_from(move)],
            sq_m = extract_to(move);

        update_continuation_histories(thread_info, piece_m, sq_m, -bonus,
                                      their_last, their_piece, our_last,
                                      our_piece, ply4_last, ply4_piece);
      }

      update_continuation_histories(thread_info, piece, sq, bonus, their_last,
                                    their_piece, our_last, our_piece, ply4_last,
                                    ply4_piece);

      if (best_move != thread_info.KillerMoves[ply][0]) {
        thread_info.KillerMoves[ply][1] = thread_info.KillerMoves[ply][0];
        thread_info.KillerMoves[ply][0] = best_move;
      }

      if (their_piece != Pieces::Blank && their_last != MoveNone) {
        thread_info.CounterMoves[their_piece][their_last] = best_move;
      }
    }

    for (int i = 0; i < num_captures; i++) {
      Action move = captures[i];

      int piece_m = position.board[extract_from(move)], sq_m = extract_to(move);

      int capture_penalty = bonus * 2;
      update_history(thread_info.CapHistScores[piece_m][sq_m],
                     -capture_penalty);
    }
  }

  if (best_score == ScoreNone) {
    return singular_search ? alpha : in_check ? (-MateScore + ply) : 0;
  }

  entry_type = best_score >= beta ? EntryTypes::LBound
               : raised_alpha     ? EntryTypes::Exact
                                  : EntryTypes::UBound;

  bool best_capture = is_cap(position, best_move);

  if (!in_check && (!best_move || !best_capture) &&
      !(best_score >= beta && best_score <= ss->static_eval) &&
      !(!best_move && best_score >= ss->static_eval)) {

    int bonus =
        std::clamp((best_score - ss->static_eval) * depth / 8, -256, 256);

    update_corrhist(
        thread_info.PawnCorrHist[color][get_corrhist_index(position.pawn_key)],
        bonus);
    update_corrhist(
        thread_info.NonPawnCorrHist[color][Colors::White][get_corrhist_index(
            position.non_pawn_key[Colors::White])],
        bonus);
    update_corrhist(
        thread_info.NonPawnCorrHist[color][Colors::Black][get_corrhist_index(
            position.non_pawn_key[Colors::Black])],
        bonus);
  }

  if (!singular_search) {
    insert_entry(entry, hash, depth, best_move, raw_eval,
                 score_to_tt(best_score, ply), entry_type,
                 thread_info.searches);
  }

  return best_score;
}

void print_pv(BoardState &position, ThreadInfo &thread_info) {
  auto temp_pos_uptr = std::make_unique<BoardState>(position);
  BoardState &temp_pos = *temp_pos_uptr;

  int indx = 0;

  while (thread_info.pv[indx] != MoveNone) {

    if (indx == 3 && thread_info.is_human) {
      thread_info.pv_material[thread_info.multipv_index] =
          -material_eval(temp_pos);
    }

    Action best_move = thread_info.pv[indx];

    MoveInfo moves;
    int movelen = legal_movegen(temp_pos, moves.moves.data());

    bool found_move = false;

    for (int i = 0; i < movelen; i++) {
      if (moves.moves[i] == best_move) {
        found_move = true;
        break;
      }
    }

    if (!found_move) {
      break;
    }

    {
      std::string mv = internal_to_uci(temp_pos, best_move);
      safe_printf("%s ", mv.c_str());
    }

    make_move(temp_pos, best_move);

    indx++;
  }

  safe_printf("\n");
}

void iterative_deepen(BoardState &position, ThreadInfo &thread_info,
                      std::vector<TTBucket> &TT) {

  thread_info.original_opt = thread_info.opt_time;
  thread_info.datagen_stop = false;

  int material = total_mat(position);
  int move_number = (thread_info.game_ply / 2) + 1;

  if (move_number < 15 && material > 4500) {
    thread_info.phase = PhaseTypes::Opening;
  } else if (move_number >= 15 && move_number < 30 && material > 4000) {
    thread_info.phase = PhaseTypes::MiddleGame;
  } else if (material > 3000 && material <= 4000) {
    thread_info.phase = PhaseTypes::LateMiddleGame;
  } else if (material <= 3000) {
    thread_info.phase = PhaseTypes::Endgame;
  }

  calculate(position);
  thread_info.nodes.store(0);
  thread_info.time_checks = 0;
  thread_info.search_ply = 0;
  thread_info.excluded_move = MoveNone;
  thread_info.best_moves = {0};
  thread_info.best_scores = {ScoreNone, ScoreNone, ScoreNone, ScoreNone,
                             ScoreNone};
  for (auto &k : thread_info.KillerMoves) {
    k[0] = MoveNone;
    k[1] = MoveNone;
  }

  bool tb_decisive_shortcut = false;
  if (thread_info.use_syzygy && tb_initialized) {

    bool any_castling = false;
    for (int c = 0; c < 2; c++)
      for (int s = 0; s < 2; s++)
        if (position.castling_squares[c][s] != SquareNone)
          any_castling = true;
    if (any_castling)
      goto skip_tb_root;

    int material_count = 0;
    for (int i = 0; i < 64; i++)
      if (position.board[i])
        material_count++;
    if (material_count && TB_LARGEST && material_count <= (int)TB_LARGEST) {
      uint64_t white = 0, black = 0, kings = 0, queens = 0, rooks = 0,
               bishops = 0, knights = 0, pawns = 0;
      for (int sq = 0; sq < 64; ++sq) {
        int pc = position.board[sq];
        if (!pc)
          continue;
        int pt = get_piece_type(pc);
        int c = get_color(pc);
        uint64_t bb = 1ULL << sq;
        if (c == Colors::White)
          white |= bb;
        else
          black |= bb;
        switch (pt) {
        case PieceTypes::King:
          kings |= bb;
          break;
        case PieceTypes::Queen:
          queens |= bb;
          break;
        case PieceTypes::Rook:
          rooks |= bb;
          break;
        case PieceTypes::Bishop:
          bishops |= bb;
          break;
        case PieceTypes::Knight:
          knights |= bb;
          break;
        case PieceTypes::Pawn:
          pawns |= bb;
          break;
        default:
          break;
        }
      }
      unsigned ep = position.ep_square < 64 ? (position.ep_square % 8) + 1 : 0;
      unsigned rule50 =
          thread_info.syzygy_50_move_rule ? position.halfmoves : 0;
      unsigned castling = 0;
      TbRootMoves tbMoves{};
      int ok = 0;

      ok = tb_probe_root_dtz(
          kings | (queens | rooks | bishops | knights | pawns) & white,
          kings | (queens | rooks | bishops | knights | pawns) & black, kings,
          queens, rooks, bishops, knights, pawns, rule50, castling, ep,
          position.color == Colors::White, false, true, &tbMoves);
      if (!ok) {
        ok = tb_probe_root_wdl(
            kings | (queens | rooks | bishops | knights | pawns) & white,
            kings | (queens | rooks | bishops | knights | pawns) & black, kings,
            queens, rooks, bishops, knights, pawns, rule50, castling, ep,
            position.color == Colors::White, true, &tbMoves);
      }
      if (ok) {

        std::vector<std::pair<Action, int>> tbOrdered;
        for (unsigned i = 0; i < tbMoves.size; ++i) {
          TbMove tm = tbMoves.moves[i].move;
          int from = TB_MOVE_FROM(tm);
          int to = TB_MOVE_TO(tm);
          int promo = TB_MOVE_PROMOTES(tm);
          Action m = promo ? pack_move_promo(from, to, promo - 1)
                           : pack_move(from, to, MoveTypes::Normal);
          int wdl = TB_GET_WDL(tbMoves.moves[i].tbScore);
          int mapped;
          switch (wdl) {
          case TB_WIN:
            mapped = TB_WIN_SCORE;
            break;
          case TB_CURSED_WIN:
            mapped = TB_WIN_SCORE;
            break;
          case TB_DRAW:
            mapped = 0;
            break;
          case TB_BLESSED_LOSS:
            mapped = -TB_WIN_SCORE;
            break;
          case TB_LOSS:
            mapped = -TB_WIN_SCORE;
            break;
          default:
            mapped = 0;
            break;
          }
          tbOrdered.emplace_back(m, mapped);
        }

        bool hasWin = false, hasLoss = false;
        for (auto &p : tbOrdered) {
          if (p.second > MateScore - 1000)
            hasWin = true;
          if (p.second < -MateScore + 1000)
            hasLoss = true;
        }
        if (hasWin && !hasLoss && !thread_info.infinite_search) {

          thread_info.best_moves[0] = tbOrdered[0].first;
          thread_info.ponder_move = MoveNone;
          tb_decisive_shortcut = true;

          {
            std::string bm = internal_to_uci(position, tbOrdered[0].first);
            safe_printf("bestmove %s\n", bm.c_str());
          }
          return;
        }

        thread_info.root_moves.clear();
        for (auto &p : tbOrdered)
          thread_info.root_moves.push_back({p.first, 0});
      }
    }
  }
skip_tb_root:;

  thread_info.root_moves.reserve(MaxActions);
  thread_info.root_moves.clear();
  {
    std::array<Action, MaxActions> raw_root_moves;
    int nmoves = legal_movegen(position, raw_root_moves.data());
    for (int i = 0; i < nmoves; i++) {
      thread_info.root_moves.push_back({raw_root_moves[i], 0});
    }
  }

  Action prev_best = MoveNone;
  int alpha = ScoreNone, beta = -ScoreNone;
  int bm_stability = 0;

  int target_depth = std::clamp(thread_info.max_iter_depth, 1, MaxSearchPly);
  int last_completed_depth = 0;

  auto update_phase = [&](ThreadInfo &ti, BoardState &pos) {
    if (ti.thread_id != 0)
      return;

    int total_material = total_mat(pos);
    int root_eval = ti.best_scores[0];
    ti.prev_root_eval = ti.last_root_eval;
    ti.last_root_eval = root_eval;
    ti.root_completed_depth = last_completed_depth;

    if (!ti.attack_mode) {
      if (last_completed_depth >= AttackModeEnterDepth && root_eval >= ti.sacrifice_enter_cp &&
          total_material >= AttackModeMaterial) {

        if (ti.prev_root_eval >= ti.sacrifice_enter_cp - AttackModeEnterRelax) {
          ti.attack_mode = true;
        }
      }
    } else {
      bool drop =
          (ti.prev_root_eval - root_eval) >= ti.sacrifice_drop_threshold + AttackModeDropExtra;
      if (root_eval <= ti.sacrifice_exit_cp - AttackModeExitRelax ||
          total_material < ti.endgame_material - AttackModeMatExit || drop) {
        ti.attack_mode = false;
      }
    }

    uint8_t desired_phase = ti.phase;

    if (ti.phase == PhaseTypes::Opening && ti.game_ply >= ti.opening_min_ply) {
      desired_phase = PhaseTypes::MiddleGame;
    }

    if (ti.phase == PhaseTypes::Endgame) {
      if (total_material > ti.end_recover_material &&
          total_material > ti.endgame_material) {
        desired_phase = PhaseTypes::LateMiddleGame;
      }
    }

    else if (ti.phase == PhaseTypes::LateMiddleGame) {
      if (total_material > ti.mid_recover_material) {
        desired_phase = PhaseTypes::MiddleGame;
      }
    }

    if (total_material <= ti.endgame_material) {
      desired_phase = PhaseTypes::Endgame;
    } else if (total_material <= ti.late_phase_material) {

      if (desired_phase != PhaseTypes::Endgame)
        desired_phase = PhaseTypes::LateMiddleGame;
    } else {

      if (ti.game_ply < ti.opening_min_ply)
        desired_phase = PhaseTypes::Opening;
      else
        desired_phase = PhaseTypes::MiddleGame;
    }

    if (desired_phase != ti.phase) {
      ti.phase_hit_counts[desired_phase]++;

      for (size_t i = 0; i < ti.phase_hit_counts.size(); ++i) {
        if (i != desired_phase)
          ti.phase_hit_counts[i] = 0;
      }
      if (ti.phase_hit_counts[desired_phase] >= ti.phase_confirm_hits) {
        ti.phase = desired_phase;
      }
    } else {

      for (auto &c : ti.phase_hit_counts)
        c = 0;
    }
  };
  int real_multi_pv =
      std::min<int>(thread_info.multipv, (int)thread_info.root_moves.size());

  for (int depth = 1;; ++depth) {
    if (thread_data.stop) {
      break;
    }
    if (thread_info.infinite_search && depth > MaxSearchPly) {
      depth = MaxSearchPly;
    }
    if (!thread_info.infinite_search && depth > target_depth) {
      break;
    }

    real_multi_pv =
        std::min<int>(thread_info.multipv, (int)thread_info.root_moves.size());

    for (thread_info.multipv_index = 0;
         thread_info.multipv_index < real_multi_pv;
         thread_info.multipv_index++) {

      int temp_depth = depth;

      int score, delta = AspStartWindow;

      score =
          search<true>(alpha, beta, depth, false, position, thread_info, TT);

      while (score <= alpha || score >= beta || thread_data.stop ||
             thread_info.datagen_stop) {

        if (thread_data.stop || thread_info.datagen_stop) {
          goto finish;
        }

        if (thread_info.thread_id == 0 && !thread_info.doing_datagen &&
            !(thread_info.is_human && thread_info.multipv_index)) {
          std::string bound_string;
          if (score >= beta) {
            bound_string = "lowerbound";
          } else {
            bound_string = "upperbound";
          }

          uint64_t nodes = thread_info.nodes.load();
          for (auto &td : thread_data.thread_infos) {
            nodes += td.nodes.load();
          }
          int64_t search_time = time_elapsed(thread_info.start_time);
          int64_t nps = search_time
                            ? static_cast<int64_t>(nodes) * 1000 / search_time
                            : 123456789;

          Action move = score <= alpha
                            ? prev_best
                            : thread_info.best_moves[thread_info.multipv_index];

          if (abs(score) < MateScore - MaxSearchPly) {
            {
              std::string pv_str = internal_to_uci(position, move);
              safe_printf(
                  "info multipv %i depth %i seldepth %i score cp %i %s nodes "
                  "%" PRIu64 " nps %" PRIi64 " time %" PRIi64 " pv %s\n",
                  thread_info.multipv_index + 1, depth, thread_info.seldepth,
                  score * 100 / NormalizationFactor, bound_string.c_str(),
                  nodes, nps, search_time, pv_str.c_str());
            }
          } else if (score > MateScore) {
            int dist = (MateScore - score) / 2;
            {
              std::string pv_str = internal_to_uci(position, move);
              safe_printf("info multipv %i depth %i seldepth %i score mate %i "
                          "%s nodes %" PRIu64 " nps %" PRIi64 " time %" PRIi64
                          " pv %s\n",
                          thread_info.multipv_index + 1, depth,
                          thread_info.seldepth, dist, bound_string.c_str(),
                          nodes, nps, search_time, pv_str.c_str());
            }
          } else {
            int dist = (-MateScore - score) / 2;
            {
              std::string pv_str = internal_to_uci(position, move);
              safe_printf("info multipv %i depth %i seldepth %i score mate %i "
                          "%s nodes %" PRIu64 " nps %" PRIi64 " time %" PRIi64
                          " pv %s\n",
                          thread_info.multipv_index + 1, depth,
                          thread_info.seldepth, dist, bound_string.c_str(),
                          nodes, nps, search_time, pv_str.c_str());
            }
          }
        }

        if (score <= alpha) {
          beta = (alpha + beta) / 2;
          alpha -= delta;
          temp_depth = depth;
        } else if (score >= beta) {
          beta += delta;
          temp_depth = std::max(temp_depth - 1, 1);
        }
        delta += delta / 3;

        score = search<true>(alpha, beta, temp_depth, false, position,
                             thread_info, TT);
      }

      if (score == ScoreNone) {
        break;
      }

      std::string eval_string;

      if (abs(score) < MateScore - MaxSearchPly) {
        eval_string = "cp " + std::to_string(score * 100 / NormalizationFactor);
      } else if (score > 0) {
        int dist = (MateScore - score + 1) / 2;
        eval_string = "mate " + std::to_string(dist);
      } else {
        int dist = (-MateScore - score) / 2;
        eval_string = "mate " + std::to_string(dist);
      }

      thread_info.best_moves[thread_info.multipv_index] = thread_info.pv[0];

      if (thread_info.thread_id == 0) {

        uint64_t nodes = thread_info.nodes.load();

        for (auto &td : thread_data.thread_infos) {
          nodes += td.nodes.load();
        }

        int64_t search_time = time_elapsed(thread_info.start_time);
        int64_t nps;
        if (search_time) {
          nps = static_cast<int64_t>(nodes) * 1000 / search_time;
        } else {
          int wezly = 10000000;
          wezly += (wezly / 7);
          nps = wezly;
        }

        if (!thread_info.doing_datagen) {
          safe_printf(
              "info multipv %i depth %i seldepth %i score %s nodes %" PRIu64
              " nps %" PRIi64 " time %" PRIi64 " pv ",
              thread_info.multipv_index + 1, depth, thread_info.seldepth,
              eval_string.c_str(), nodes, nps, search_time);
          print_pv(position, thread_info);
        }

        else {
          thread_info.best_scores[0] = score * 100 / NormalizationFactor;
        }

        if (static_cast<uint64_t>(search_time) > thread_info.opt_time ||
            nodes > thread_info.opt_nodes_searched) {

          if (thread_info.doing_datagen) {
            thread_info.datagen_stop = true;
          } else {
            thread_data.stop = true;
          }
        }

        else if (thread_info.multipv == 1 && depth > 6) {
          if (thread_info.best_moves[0] == prev_best) {
            bm_stability = std::min(bm_stability + 1, 8);
            thread_info.stability_counter++;
            thread_info.best_move_stable = (thread_info.stability_counter >= 3);
          } else {
            bm_stability = 0;
            thread_info.stability_counter = 0;
            thread_info.best_move_stable = false;
            thread_info.previous_best_move = prev_best;
          }

          adjust_soft_limit(
              thread_info,
              find_root_move(thread_info, thread_info.best_moves[0])->nodes,
              bm_stability, thread_info.best_scores[0]);
        }
      }

      if (thread_data.stop || thread_info.datagen_stop) {
        goto finish;
      }

      prev_best = thread_info.best_moves[0];

      if (depth > 6 && thread_info.multipv_index == 0) {
        alpha = score - 20, beta = score + 20;
      } else {
        alpha = ScoreNone, beta = -ScoreNone;
      }
    }

    last_completed_depth = depth;
  }

  update_phase(thread_info, position);

finish:

  if (thread_info.thread_id == 0 && !thread_info.doing_datagen) {
    thread_data.stop = true;
  }
  search_end_barrier.arrive_and_wait();

  auto validate_ponder_move = [&](const BoardState &root_position,
                                  Action best_move,
                                  Action ponder_candidate) -> Action {
    if (ponder_candidate == MoveNone || best_move == MoveNone) {
      return MoveNone;
    }

    std::array<Action, MaxActions> root_legal{};
    int root_count = legal_movegen(root_position, root_legal.data());
    bool best_is_legal = false;
    for (int i = 0; i < root_count; ++i) {
      if (root_legal[i] == best_move) {
        best_is_legal = true;
        break;
      }
    }
    if (!best_is_legal) {
      return MoveNone;
    }

    auto ponder_pos_uptr = std::make_unique<BoardState>(root_position);
    BoardState &ponder_position = *ponder_pos_uptr;
    make_move(ponder_position, best_move);

    std::array<Action, MaxActions> response_legal{};
    int response_count = legal_movegen(ponder_position, response_legal.data());
    for (int i = 0; i < response_count; ++i) {
      if (response_legal[i] == ponder_candidate) {
        return ponder_candidate;
      }
    }

    return MoveNone;
  };

  if (thread_info.thread_id == 0) {

    if (thread_info.pv[0] != MoveNone && thread_info.pv[1] != MoveNone) {
      thread_info.ponder_move = thread_info.pv[1];
    } else if (thread_info.best_moves[0] != MoveNone) {
      auto temp_pos_uptr2 = std::make_unique<BoardState>(position);
      BoardState &temp_pos = *temp_pos_uptr2;
      make_move(temp_pos, thread_info.best_moves[0]);

      bool tt_hit = false;
      TTEntry tt_entry = probe_entry(temp_pos.zobrist_key, tt_hit,
                                     thread_info.searches, TT);
      if (tt_hit && tt_entry.best_move != MoveNone) {
        thread_info.ponder_move = tt_entry.best_move;
      } else {
        thread_info.ponder_move = MoveNone;
      }
    }
  }

  if (thread_info.thread_id == 0 && thread_info.use_syzygy) {
    auto &pos = position;
    unsigned castling = 0;
    if (pos.castling_squares[Colors::White][Sides::Kingside] != SquareNone)
      castling |= TB_CASTLING_K;
    if (pos.castling_squares[Colors::White][Sides::Queenside] != SquareNone)
      castling |= TB_CASTLING_Q;
    if (pos.castling_squares[Colors::Black][Sides::Kingside] != SquareNone)
      castling |= TB_CASTLING_k;
    if (pos.castling_squares[Colors::Black][Sides::Queenside] != SquareNone)
      castling |= TB_CASTLING_q;
    unsigned ep = pos.ep_square != SquareNone ? pos.ep_square : 0;
    unsigned tb_res = tb_probe_root(
        pos.colors_bb[0], pos.colors_bb[1], pos.pieces_bb[PieceTypes::King],
        pos.pieces_bb[PieceTypes::Queen], pos.pieces_bb[PieceTypes::Rook],
        pos.pieces_bb[PieceTypes::Bishop], pos.pieces_bb[PieceTypes::Knight],
        pos.pieces_bb[PieceTypes::Pawn], pos.halfmoves, castling, ep, pos.color,
        nullptr);
    if (tb_res != TB_RESULT_FAILED) {
      int from = TB_MOVE_FROM(tb_res);
      int to = TB_MOVE_TO(tb_res);
      int tb_prom = TB_GET_PROMOTES(tb_res);
      uint8_t promo = 0;
      switch (tb_prom) {
      case TB_PROMOTES_KNIGHT:
        promo = 0;
        break;
      case TB_PROMOTES_BISHOP:
        promo = 1;
        break;
      case TB_PROMOTES_ROOK:
        promo = 2;
        break;
      case TB_PROMOTES_QUEEN:
        promo = 3;
        break;
      default:
        promo = 0;
      }
      Action best = promo ? pack_move_promo(from, to, promo)
                          : pack_move(from, to, MoveTypes::Normal);

      unsigned wdl = TB_GET_WDL(tb_res);
      const char *wdl_str = "";
      switch (wdl) {
      case TB_WIN:
        wdl_str = "win";
        break;
      case TB_CURSED_WIN:
        wdl_str = "cursed win";
        break;
      case TB_DRAW:
        wdl_str = "draw";
        break;
      case TB_BLESSED_LOSS:
        wdl_str = "blessed loss";
        break;
      case TB_LOSS:
        wdl_str = "loss";
        break;
      default:
        wdl_str = "unknown";
        break;
      }
      {
        std::string tb_bm = internal_to_uci(pos, best);
        safe_printf("info string tablebase hit: %s (%s)\n", tb_bm.c_str(),
                    wdl_str);
      }

      Action validated_ponder =
          validate_ponder_move(pos, best, thread_info.ponder_move);
      thread_info.ponder_move = validated_ponder;

      std::string tb_bm = internal_to_uci(pos, best);
      if (thread_info.use_ponder && validated_ponder != MoveNone) {
        auto ponder_pos = std::make_unique<BoardState>(pos);
        make_move(*ponder_pos, best);
        std::string tb_pd = internal_to_uci(*ponder_pos, validated_ponder);
        safe_printf("bestmove %s ponder %s\n", tb_bm.c_str(), tb_pd.c_str());
      } else {
        safe_printf("bestmove %s\n", tb_bm.c_str());
      }
      return;
    }
  }

  if (thread_info.thread_id == 0 && !thread_info.doing_datagen &&
      thread_info.variety > 0) {

    Action selected_move = thread_info.best_moves[0];
    int best_score = thread_info.best_scores[0];

    int variety_lines =
        real_multi_pv > 1
            ? std::min<int>(real_multi_pv,
                            1 + (static_cast<int>(thread_info.variety) / 50))
            : 1;

    int base_threshold =
        (VARIETY_BASE_THRESHOLD - static_cast<int>(thread_info.variety)) *
        VARIETY_MULTIPLIER;
    if (base_threshold < 0)
      base_threshold = 0;

    if (variety_lines > 1) {

      int threshold = base_threshold;

      int promo_adjust[32];
      std::fill(std::begin(promo_adjust), std::end(promo_adjust), 0);
      for (int i = 0;
           i < variety_lines && thread_info.best_moves[i] != MoveNone; i++) {
        Action move = thread_info.best_moves[i];
        if (extract_type(move) == MoveTypes::Promotion &&
            extract_promo(move) != Promos::Queen) {
          auto temp_pos_uptr3 = std::make_unique<BoardState>(position);
          BoardState &temp_pos = *temp_pos_uptr3;
          make_move(temp_pos, move);
          int to = extract_to(move);
          int promo_type = extract_promo(move);
          int promo_bonus = 0;
          if (!is_valid_square(to))
            continue;
          if (promo_type == Promos::Knight) {
            uint64_t knight_attacks = KNIGHT_ATK_SAFE(to);
            uint64_t valuable_targets = (temp_pos.pieces_bb[PieceTypes::Queen] |
                                         temp_pos.pieces_bb[PieceTypes::Rook] |
                                         temp_pos.pieces_bb[PieceTypes::King]) &
                                        temp_pos.colors_bb[position.color ^ 1];
            int fork_count = 0;
            while (valuable_targets) {
              int target_sq = pop_lsb(valuable_targets);
              if (knight_attacks & (1ULL << target_sq))
                fork_count++;
            }
            if (fork_count >= 2)
              promo_bonus = PROMO_BONUS_DOUBLE_FORK;
            else if (fork_count == 1)
              promo_bonus = PROMO_BONUS_SINGLE_FORK;
          } else if (promo_type == Promos::Bishop) {
            uint64_t bishop_attacks = get_bishop_attacks(
                to, temp_pos.colors_bb[0] | temp_pos.colors_bb[1]);
            uint64_t central_diagonals =
                0x8040201008040201ULL | 0x0102040810204080ULL;
            if (bishop_attacks & central_diagonals) {
              promo_bonus = 50;
              if (pop_count(temp_pos.pieces_bb[PieceTypes::Bishop] &
                            temp_pos.colors_bb[position.color]) > 1)
                promo_bonus += 50;
            }
          }
          promo_bonus =
              (promo_bonus * static_cast<int>(thread_info.variety)) / 100;
          if (promo_bonus > 0) {
            int score_diff = best_score - thread_info.best_scores[i];
            if (score_diff <= threshold + promo_bonus) {
              promo_adjust[i] = promo_bonus;
            }
          }
        }
      }

      for (int i = 0;
           i < variety_lines && thread_info.best_moves[i] != MoveNone; i++) {
        if (promo_adjust[i])
          thread_info.best_scores[i] += promo_adjust[i];
      }

    } else {

      if (thread_info.seldepth > 8 && std::abs(best_score) < 1200) {

        std::array<Action, MaxActions> legal_moves;
        int num_legal = legal_movegen(position, legal_moves.data());
        struct AltCand {
          Action m;
          int score;
          int diff;
        };
        std::vector<AltCand> alts;
        alts.reserve(num_legal);
        for (int i = 0; i < num_legal; ++i) {
          Action m = legal_moves[i];
          if (m == thread_info.best_moves[0])
            continue;

          bool cap = is_cap(position, m);
          if (cap && thread_info.variety < 40)
            continue;

          int piece = position.board[extract_from(m)];
          int to = extract_to(m);
          int hist_score = thread_info.HistoryScores[piece][to];

          int diff = (thread_info.best_scores[0] / 4) - (hist_score / 4);

          int effective_window =
              base_threshold + (static_cast<int>(thread_info.variety) * 3) / 2;
          if (diff <= effective_window) {
            alts.push_back({m, hist_score, diff});
          }
        }
        if (!alts.empty()) {

          uint64_t total_w = 0;
          std::vector<uint64_t> prefix(alts.size());
          for (size_t i = 0; i < alts.size(); ++i) {
            int quality = std::max(1, 1000 - std::max(0, alts[i].diff));

            double flatten = (100.0 - (thread_info.variety / 2.0)) / 100.0;
            double w = std::pow((double)quality, std::max(0.25, flatten));
            uint64_t iw = (uint64_t)std::max<double>(1.0, w);
            total_w += iw;
            prefix[i] = total_w;
          }
          if (total_w > 0) {
            uint64_t r = (uint64_t)Random::dist(Random::rd) % total_w;
            for (size_t i = 0; i < alts.size(); ++i) {
              if (r < prefix[i]) {
                selected_move = alts[i].m;
                break;
              }
            }
          }
        }
      }
    }

    if (real_multi_pv > 1) {

      std::vector<int> candidates;
      int threshold = base_threshold;
      for (int i = 0;
           i < variety_lines && thread_info.best_moves[i] != MoveNone; i++) {
        int score_diff = best_score - thread_info.best_scores[i];
        if (score_diff <= threshold)
          candidates.push_back(i);
      }
      if (!candidates.empty()) {
        int variety_bias = static_cast<int>(thread_info.variety);
        int selection = 0;
        if (candidates.size() > 1 && variety_bias > 0) {
          int r = Random::dist(Random::rd) % 150;
          if (r < variety_bias)
            selection =
                candidates[Random::dist(Random::rd) % candidates.size()];
        }
        selected_move = thread_info.best_moves[selection];
      }
    }

    if (selected_move != thread_info.best_moves[0]) {

      int selected_idx = 0;
      for (int i = 0; i < real_multi_pv; i++) {
        if (thread_info.best_moves[i] == selected_move) {
          selected_idx = i;
          break;
        }
      }

      std::swap(thread_info.best_moves[0],
                thread_info.best_moves[selected_idx]);
      std::swap(thread_info.best_scores[0],
                thread_info.best_scores[selected_idx]);
    }
  }

  if (thread_info.thread_id == 0 && thread_info.is_human &&
      !thread_info.doing_datagen) {

    bool can_weaken = !(thread_info.pondering && !thread_info.ponder_hit);
    if (can_weaken && thread_info.best_moves[0] != MoveNone) {

      int true_top = thread_info.best_scores[0];
      for (int i = 1; i < 16 && thread_info.best_moves[i] != MoveNone; i++)
        if (thread_info.best_scores[i] > true_top)
          true_top = thread_info.best_scores[i];

      int base_margin = std::max(0, thread_info.human_value_margin);

      int v = std::clamp<int>(thread_info.variety, 0, 150);
      double v_norm = v / 150.0;
      double attenuation = 1.0 - 0.55 * v_norm;
      if (attenuation < 0.35)
        attenuation = 0.35;
      int margin = (int)std::lround(base_margin * attenuation);

      if (thread_info.human_elo <= 1600) {
        double elo_scale = (thread_info.human_elo - HUMAN_ELO_MIN) /
                           static_cast<double>(HUMAN_ELO_RANGE);
        if (elo_scale < 0)
          elo_scale = 0;
        if (elo_scale > 1)
          elo_scale = 1;
        margin = (int)std::lround(margin * (0.75 + 0.25 * elo_scale));
      }

      if (margin <= 0)
        margin = 1;

      std::vector<int> cand;
      cand.reserve(16);
      for (int i = 0; i < 16 && thread_info.best_moves[i] != MoveNone; i++) {
        int diff = true_top - thread_info.best_scores[i];
        if (diff >= 0 && diff <= margin)
          cand.push_back(i);
      }
      if (cand.size() > 1) {

        if (thread_info.human_noise_sigma > 0) {
          int extra =
              Random::dist(Random::rd) % (thread_info.human_noise_sigma + 1);
          int widened = margin + extra;
          for (int i = 0; i < 16 && thread_info.best_moves[i] != MoveNone;
               i++) {
            if (std::find(cand.begin(), cand.end(), i) != cand.end())
              continue;
            int diff = true_top - thread_info.best_scores[i];
            if (diff > margin && diff <= widened)
              cand.push_back(i);
          }
        }

        int total_w = 0;
        for (int idx : cand) {
          int diff = true_top - thread_info.best_scores[idx];
          int w = (margin - diff) + 5;
          if (w < 1)
            w = 1;
          total_w += w;
        }
        if (total_w <= 0)
          total_w = (int)cand.size();
        int r = Random::dist(Random::rd) % total_w;
        int chosen_idx = cand[0];
        for (int idx : cand) {
          int diff = true_top - thread_info.best_scores[idx];
          int w = (margin - diff) + 5;
          if (w < 1)
            w = 1;
          if (r < w) {
            chosen_idx = idx;
            break;
          }
          r -= w;
        }
        if (chosen_idx != 0) {
          std::swap(thread_info.best_moves[0],
                    thread_info.best_moves[chosen_idx]);
          std::swap(thread_info.best_scores[0],
                    thread_info.best_scores[chosen_idx]);
        }
      }
    }
  }

  if (thread_info.thread_id == 0 && thread_info.best_moves[0] == MoveNone) {
    std::array<Action, MaxActions> legal_moves;
    int num_legal = legal_movegen(position, legal_moves.data());
    if (num_legal > 0) {
      thread_info.best_moves[0] = legal_moves[0];
      thread_info.best_scores[0] = 0;
    }
  }

  if (thread_info.thread_id == 0 && !thread_info.doing_datagen &&
      thread_info.best_moves[0] != MoveNone &&
      (!thread_info.infinite_search || thread_data.stop)) {
    bool can_output = true;

    if (thread_info.pondering && !thread_info.ponder_hit && !thread_data.stop) {
      can_output = false;
    }
    if (can_output) {
      Action validated_ponder = validate_ponder_move(
          position, thread_info.best_moves[0], thread_info.ponder_move);
      thread_info.ponder_move = validated_ponder;

      std::string bm = internal_to_uci(position, thread_info.best_moves[0]);
      if (thread_info.use_ponder && validated_ponder != MoveNone) {
        auto ponder_pos = std::make_unique<BoardState>(position);
        make_move(*ponder_pos, thread_info.best_moves[0]);
        std::string pd = internal_to_uci(*ponder_pos, validated_ponder);
        safe_printf("bestmove %s ponder %s\n", bm.c_str(), pd.c_str());
      } else {
        safe_printf("bestmove %s\n", bm.c_str());
      }
    }
  }
}

void search_position(BoardState &position, ThreadInfo &thread_info,
                     std::vector<TTBucket> &TT) {
  thread_info.position = position;
  thread_info.thread_id = 0;
  thread_info.nodes.store(0);

  reset_barrier.arrive_and_wait();

  for (size_t i = 0; i < thread_data.thread_infos.size(); i++) {
    thread_data.thread_infos[i] = thread_info;
    thread_data.thread_infos[i].thread_id = i + 1;
  }

  idle_barrier.arrive_and_wait();

  thread_data.stop = false;
  iterative_deepen(position, thread_info, TT);
  if (!thread_info.doing_datagen) {
    thread_data.stop = true;
  }

  thread_info.searches = (thread_info.searches + 1) % MaxAge;
}

void loop(int i) {
  while (true) {
    reset_barrier.arrive_and_wait();
    idle_barrier.arrive_and_wait();
    if (thread_data.terminate) {
      return;
    }
    {
      std::lock_guard<std::mutex> lk(thread_data.search_mutex);
      thread_data.thread_infos[i].searching.store(true);
    }
    thread_data.search_cv.notify_all();
    iterative_deepen(thread_data.thread_infos[i].position,
                     thread_data.thread_infos[i], TT);
    {
      std::lock_guard<std::mutex> lk(thread_data.search_mutex);
      thread_data.thread_infos[i].searching.store(false);
    }
    thread_data.search_cv.notify_all();
  }
}

int analyze_sacrifice(BoardState &position, ThreadInfo &thread_info, int depth,
                      int ply, int sacrificer_color) {

  if (depth < 0 || ply > 10)
    return 0;

  int64_t time_for_sacrifice = static_cast<int64_t>(
      (thread_info.opt_time * thread_info.sacrifice_lookahead_time_multiplier) /
      100);
  if (time_elapsed(thread_info.start_time) > time_for_sacrifice)
    return 0;

  if (depth == 0) {
    int mat = material_eval(position);
    if (position.color != sacrificer_color)
      mat = -mat;

    int stat = eval(position, thread_info);

    if (position.color != sacrificer_color)
      stat = -stat;
    int score = (mat * 3 + stat) / 4;
    return score;
  }

  std::array<Action, MaxActions> moves;
  uint64_t checkers = attacks_square(
      position, get_king_pos(position, position.color), position.color ^ 1);
  int nmoves = movegen(position, moves.data(), checkers, Generate::GenAll);

  int best = -1000000;

  int considered = 0;
  for (int i = 0; i < nmoves && considered < 16; i++) {
    Action m = moves[i];
    if (!is_legal(position, m))
      continue;

    BoardState np = position;
    int before_mat = material_eval(position);
    make_move(np, m);

    if (thread_info.search_ply >= MaxSearchPly ||
        thread_info.game_ply >= MaxGameLen) {
      return best;
    }
    ss_push(position, thread_info, m);

    int after_mat = material_eval(np);

    bool isCapture = is_cap(position, m);
    bool sacrificer_turn = (position.color == sacrificer_color);
    bool sacrificer_loses = false;
    if (sacrificer_turn) {

      if (sacrificer_color == Colors::White && after_mat < before_mat)
        sacrificer_loses = true;
      if (sacrificer_color == Colors::Black && after_mat > before_mat)
        sacrificer_loses = true;
    }
    if (!(isCapture || sacrificer_loses || ply == 0)) {
      ss_pop(thread_info);
      continue;
    }

    considered++;
    int child = -analyze_sacrifice(np, thread_info, depth - 1, ply + 1,
                                   sacrificer_color);
    if (child > best)
      best = child;

    ss_pop(thread_info);
  }

  if (best == -1000000) {

    int mat = material_eval(position);
    if (position.color != sacrificer_color)
      mat = -mat;
    return mat;
  }

  int aggr_bonus = (thread_info.sacrifice_lookahead_aggressiveness - 100) * 2;
  return best + aggr_bonus / 4;
}

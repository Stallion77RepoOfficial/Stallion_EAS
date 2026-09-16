#pragma once
#include "movepick.h"
#include "params.h"
#include "position.h"
#include "pst.h"

#include "../fathom/src/tbprobe.h"
#include "utils.h"
#include <memory>

inline Action uci_to_internal(const BoardState &position, const std::string &uci) {
  std::array<Action, MaxActions> list{};
  const int nmoves = legal_movegen(position, list.data());

  for (int i = 0; i < nmoves; i++) {
    if (internal_to_uci(position, list[i]) == uci)
      return list[i];
  }

  return MoveNone;
}

extern bool tb_initialized;

inline int64_t
safe_elapsed(const std::chrono::steady_clock::time_point &start) noexcept {
  const auto ms = time_elapsed(start);
  return ms ? ms : 1;
}

inline int analyze_sacrifice(BoardState &position, ThreadInfo &thread_info, int depth,
                      int ply, int sacrificer_color);

inline int probe_wdl_tb(const BoardState &position, const ThreadInfo &thread_info) noexcept {

  if (!tb_initialized || !thread_info.use_syzygy)
    return ScoreNone;

  const int material_count = pop_count(position.colors_bb[0] | position.colors_bb[1]);
  const int compiled_limit = TB_LARGEST ? static_cast<int>(TB_LARGEST) : 7;
  if (!TB_LARGEST || material_count > compiled_limit)
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

  if (thread_info.syzygy_50_move_rule && position.halfmoves != 0) return ScoreNone;
  const unsigned ep = position.ep_square != SquareNone ? position.ep_square : 0;

  const unsigned result = tb_probe_wdl(position.colors_bb[0], position.colors_bb[1],
                                       position.pieces_bb[PieceTypes::King],
                                       position.pieces_bb[PieceTypes::Queen],
                                       position.pieces_bb[PieceTypes::Rook],
                                       position.pieces_bb[PieceTypes::Bishop],
                                       position.pieces_bb[PieceTypes::Knight],
                                       position.pieces_bb[PieceTypes::Pawn], 0,
                                       castling, ep,
                                       position.color == Colors::White);
  if (result == TB_RESULT_FAILED) {
    thread_data.tb_fails++;
    return ScoreNone;
  }
  thread_data.tb_hits++;
  const int wdl = TB_GET_WDL(result);
  switch (wdl) {
  case TB_WIN:
    return TB_WIN_SCORE;
  case TB_CURSED_WIN:
    return thread_info.syzygy_50_move_rule ? 0 : TB_WIN_SCORE;
  case TB_DRAW:
    return 0;
  case TB_BLESSED_LOSS:
    return thread_info.syzygy_50_move_rule ? 0 : -TB_WIN_SCORE;
  case TB_LOSS:
    return -TB_WIN_SCORE;
  default:
    return ScoreNone;
  }
}

inline void update_history(int16_t &entry, int score) noexcept {
  entry += score - entry * std::abs(score) / 16384;
}
inline void update_corrhist(int16_t &entry, int score) noexcept {
  entry += score - entry * std::abs(score) / 1024;
}

inline void update_continuation_histories(ThreadInfo &thread_info, int piece,
                                          int sq, int bonus, int their_last,
                                          int their_piece, int our_last,
                                          int our_piece, int ply4_last,
                                          int ply4_piece) noexcept {

  update_history(thread_info.HistoryScores[piece][sq], bonus);

  if (their_last != SquareNone) {
    update_history(
        thread_info.ContHistScores[their_piece][their_last][piece][sq], bonus);
  }
  if (our_last != SquareNone) {
    update_history(thread_info.ContHistScores[our_piece][our_last][piece][sq],
                   bonus);
  }
  if (ply4_last != SquareNone) {
    update_history(thread_info.ContHistScores[ply4_piece][ply4_last][piece][sq],
                   bonus / 2);
  }
}

inline bool out_of_time(ThreadInfo &thread_info) noexcept {
  if (thread_data.stop)
    return true;

  if (thread_info.thread_id != 0)
    return false;

  const int64_t hit_time = thread_data.ponder_hit_time.exchange(-1);
  if (hit_time >= 0) {
    const auto hit = std::chrono::steady_clock::time_point(std::chrono::milliseconds(hit_time));
    const auto ponder_ms = std::max<int64_t>(0, std::chrono::duration_cast<std::chrono::milliseconds>(hit - thread_info.start_time).count());
    thread_info.start_time = hit;
    thread_info.opt_time = std::min(thread_info.max_time, thread_info.opt_time +
        static_cast<uint64_t>(ponder_ms) * thread_info.ponder_time_factor / 100);
    thread_info.time_manager.soft_limit = thread_info.opt_time;
    thread_info.pondering = false;
    thread_info.ponder_hit = true;
  }

  uint64_t total_nodes = thread_info.nodes.load(std::memory_order_relaxed);
  for (auto &ti : thread_data.thread_infos)
    total_nodes += ti.nodes.load(std::memory_order_relaxed);
  if (!thread_data.pondering && total_nodes >= thread_info.max_nodes_searched) {
    thread_data.stop = true;
    return true;
  }

  thread_info.time_checks++;
  constexpr uint16_t check_interval = 256;
  if (thread_info.time_checks >= check_interval) {
    thread_info.time_checks = 0;
    if (!thread_info.infinite_search && !thread_data.pondering) {
      const uint64_t elapsed = time_elapsed(thread_info.start_time);
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

inline int16_t material_eval(const BoardState &position) noexcept {
  int m = 0;

  for (int pt = 1; pt <= 5; pt++) {
    const int count = position.material_count[(pt - 1) * 2];
    if (count > 0) {
      m += count * MaterialBasis[pt];

      for (int pt2 = 1; pt2 <= 5; pt2++) {
        const int total_count_pt2 = position.material_count[(pt2 - 1) * 2] +
                                    position.material_count[(pt2 - 1) * 2 + 1];
        m += count * total_count_pt2 * QuadraticImbalance[pt][pt2];
      }
    }
  }

  for (int pt = 1; pt <= 5; pt++) {
    const int count = position.material_count[(pt - 1) * 2 + 1];
    if (count > 0) {
      m -= count * MaterialBasis[pt];

      for (int pt2 = 1; pt2 <= 5; pt2++) {
        const int total_count_pt2 = position.material_count[(pt2 - 1) * 2] +
                                    position.material_count[(pt2 - 1) * 2 + 1];
        m -= count * total_count_pt2 * QuadraticImbalance[pt][pt2];
      }
    }
  }

  return position.color ? -m : m;
}

inline bool has_non_pawn_material(const BoardState &position, int color) noexcept {
  const int s_indx = 2 + color;
  return (position.material_count[s_indx] ||
          position.material_count[s_indx + 2] ||
          position.material_count[s_indx + 4] ||
          position.material_count[s_indx + 6]);
}

inline int non_pawn_piece_count(const BoardState &position) noexcept {
  return pop_count(position.pieces_bb[PieceTypes::Knight] |
                   position.pieces_bb[PieceTypes::Bishop] |
                   position.pieces_bb[PieceTypes::Rook] |
                   position.pieces_bb[PieceTypes::Queen]);
}

inline bool is_endgame_reduction_zone(const BoardState &position,
                                [[maybe_unused]] const ThreadInfo &thread_info,
                                int total_material = -1) noexcept {
  if (total_material < 0) {
    total_material = total_mat(position);
  }
  return total_material <= EndgameMaterial;
}

inline bool is_zugzwang_prone(const BoardState &position, [[maybe_unused]] const ThreadInfo &thread_info,
                       int total_material = -1) noexcept {
  if (total_material < 0) {
    total_material = total_mat(position);
  }
  const int non_pawn = non_pawn_piece_count(position);
  const int pawns = pop_count(position.pieces_bb[PieceTypes::Pawn]);
  return non_pawn == 0 ||
         (non_pawn <= 2 &&
          total_material <= (EndgameMaterial + 500) && pawns <= 6);
}

inline int16_t total_mat_color(const BoardState &position, int color) noexcept {

  int m = 0;
  for (int i = 0; i < 5; i++) {
    m += position.material_count[i * 2 + color] * SeeValues[i + 1];
  }
  return m;
}

inline int eval_pst(const BoardState &position, int color) noexcept {
  int score = 0;
  constexpr const int16_t *pst[] = {PST::Pawn, PST::Knight, PST::Bishop,
                                    PST::Rook, PST::Queen};
  for (int pt = PieceTypes::Pawn; pt <= PieceTypes::Queen; pt++) {
    uint64_t pieces = position.pieces_bb[pt] & position.colors_bb[color];
    while (pieces) {
      const int sq = pop_lsb(pieces);
      const int idx = (color == Colors::White) ? PST::mirror_square(sq) : sq;
      score += pst[pt - 1][idx];
    }
  }
  const int king_sq = get_king_pos(position, color);
  if (is_valid_square(king_sq)) {
    const int idx = (color == Colors::White) ? PST::mirror_square(king_sq) : king_sq;
    score += (total_mat(position) < 1500) ? PST::KingEG[idx] : PST::KingMG[idx];
  }
  return score;
}

inline int eval_king_tropism(const BoardState &position, int color) noexcept {
  const int opp_king = get_king_pos(position, color ^ 1);
  if (!is_valid_square(opp_king))
    return 0;
  const int opp_k_rank = get_rank(opp_king);
  const int opp_k_file = get_file(opp_king);
  int score = 0;
  constexpr int pts[] = {PieceTypes::Queen, PieceTypes::Rook, PieceTypes::Knight,
                         PieceTypes::Bishop};
  const int weights[] = {TropismQueenWeight, TropismRookWeight,
                         TropismKnightWeight, TropismBishopWeight};
  for (int i = 0; i < 4; i++) {
    uint64_t pieces = position.pieces_bb[pts[i]] & position.colors_bb[color];
    while (pieces) {
      const int sq = pop_lsb(pieces);
      const int dist = std::max(std::abs(get_rank(sq) - opp_k_rank),
                                std::abs(get_file(sq) - opp_k_file));
      score += (8 - dist) * weights[i];
    }
  }
  return score;
}

inline int eval_threats(const BoardState &position, int color) noexcept {
  int score = 0;
  const int opp_color = color ^ 1;
  const uint64_t occ = position.colors_bb[0] | position.colors_bb[1];

  const uint64_t opp_pieces =
      position.colors_bb[opp_color] & ~position.pieces_bb[PieceTypes::Pawn];
  uint64_t my_pawn_attacks = 0;
  uint64_t my_pawns =
      position.pieces_bb[PieceTypes::Pawn] & position.colors_bb[color];
  while (my_pawns) {
    const int sq = pop_lsb(my_pawns);
    my_pawn_attacks |= PAWN_ATK_SAFE(color, sq);
  }

  const uint64_t attacked_by_pawns = opp_pieces & my_pawn_attacks;
  score += pop_count(attacked_by_pawns) * ThreatPawnAttack;

  uint64_t my_minor_attacks = 0;
  uint64_t minors = (position.pieces_bb[PieceTypes::Knight] |
                     position.pieces_bb[PieceTypes::Bishop]) &
                    position.colors_bb[color];
  while (minors) {
    const int sq = pop_lsb(minors);
    if (get_piece_type(position.board[sq]) == PieceTypes::Knight) {
      my_minor_attacks |= KNIGHT_ATK_SAFE(sq);
    } else {
      my_minor_attacks |= get_bishop_attacks(sq, occ);
    }
  }

  const uint64_t opp_heavy = (position.pieces_bb[PieceTypes::Queen] |
                              position.pieces_bb[PieceTypes::Rook]) &
                             position.colors_bb[opp_color];
  const uint64_t attacked_heavy = opp_heavy & my_minor_attacks;
  score += pop_count(attacked_heavy) * ThreatMinorOnHeavy;

  uint64_t my_rook_attacks = 0;
  uint64_t my_rooks =
      position.pieces_bb[PieceTypes::Rook] & position.colors_bb[color];
  while (my_rooks) {
    const int sq = pop_lsb(my_rooks);
    my_rook_attacks |= get_rook_attacks(sq, occ);
  }
  const uint64_t opp_queens =
      position.pieces_bb[PieceTypes::Queen] & position.colors_bb[opp_color];
  score += pop_count(opp_queens & my_rook_attacks) * ThreatRookOnQueen;

  const uint64_t opp_minors = (position.pieces_bb[PieceTypes::Knight] |
                               position.pieces_bb[PieceTypes::Bishop]) &
                              position.colors_bb[opp_color];
  score += pop_count(opp_minors & my_rook_attacks) * ThreatRookOnMinor;

  uint64_t attacked = opp_pieces & (my_pawn_attacks | my_minor_attacks | my_rook_attacks);
  while (attacked) {
    const int square = pop_lsb(attacked);
    if (!attacks_square(position, square, opp_color)) score += ThreatHanging;
  }

  return score;
}

inline int eval_sacrifice_patterns(const BoardState &position, int color) noexcept {
  int bonus = 0;
  const int opp_color = color ^ 1;
  const int my_q = pop_count(position.pieces_bb[PieceTypes::Queen] &
                             position.colors_bb[color]);
  const int opp_q = pop_count(position.pieces_bb[PieceTypes::Queen] &
                              position.colors_bb[opp_color]);
  const int my_r = pop_count(position.pieces_bb[PieceTypes::Rook] &
                             position.colors_bb[color]);
  const int opp_r = pop_count(position.pieces_bb[PieceTypes::Rook] &
                              position.colors_bb[opp_color]);
  const int my_l = pop_count((position.pieces_bb[PieceTypes::Knight] |
                              position.pieces_bb[PieceTypes::Bishop]) &
                             position.colors_bb[color]);
  const int opp_l = pop_count((position.pieces_bb[PieceTypes::Knight] |
                               position.pieces_bb[PieceTypes::Bishop]) &
                              position.colors_bb[opp_color]);
  const int my_p = pop_count(position.pieces_bb[PieceTypes::Pawn] &
                             position.colors_bb[color]);
  const int opp_p = pop_count(position.pieces_bb[PieceTypes::Pawn] &
                              position.colors_bb[opp_color]);
  const int p_diff = opp_p - my_p;

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

inline int eval_king_safety(const BoardState &position, int color) noexcept {
  int score = 0;
  const int king_sq = get_king_pos(position, color);

  if (!is_valid_square(king_sq))
    return 0;

  const int king_file = get_file(king_sq);
  const int king_rank = get_rank(king_sq);
  const int opp_color = color ^ 1;

  for (int f = std::max(0, king_file - 1); f <= std::min(7, king_file + 1);
       ++f) {
    const uint64_t file_bb = Files[f];
    const uint64_t my_pawns = position.pieces_bb[PieceTypes::Pawn] &
                              position.colors_bb[color] & file_bb;

    if (my_pawns) {
      score += KSPawnShield;
      const int pawn_sq =
          (color == Colors::White) ? get_msb(my_pawns) : get_lsb(my_pawns);
      const int pawn_rank = get_rank(pawn_sq);
      const int dist = std::abs(pawn_rank - king_rank);
      if (dist == 1)
        score += KSPawnClose;
      else if (dist == 2)
        score += KSPawnMed;
    } else {
      score += KSNoPawn;
      const uint64_t opp_pawns = position.pieces_bb[PieceTypes::Pawn] &
                                 position.colors_bb[opp_color] & file_bb;
      if (!opp_pawns) {
        score += KSOpenFile;
        const bool opp_has_queen = (position.pieces_bb[PieceTypes::Queen] &
                                    position.colors_bb[opp_color]) != 0;
        if (opp_has_queen) {
          score += (f == king_file) ? KSOpenFile * 3 : KSOpenFile;
        }
      }
    }

    const uint64_t opp_pawns_on_file = position.pieces_bb[PieceTypes::Pawn] &
                                       position.colors_bb[opp_color] & file_bb;
    if (opp_pawns_on_file) {
      const int opp_pawn_sq = (color == Colors::White) ? get_lsb(opp_pawns_on_file)
                                                       : get_msb(opp_pawns_on_file);
      const int opp_pawn_rank = get_rank(opp_pawn_sq);
      const int dist_to_king = std::abs(opp_pawn_rank - king_rank);

      const int storm_idx = std::clamp(dist_to_king - 1, 0, 3);
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
    const int sq = pop_lsb(king_attacks);
    if (!attacks_square(position, sq, opp_color)) {
      safe_squares++;
    }
  }
  if (safe_squares < 2)
    score += KSSafeSqLow;
  else if (safe_squares < 4)
    score += KSSafeSqMed;

  const bool can_castle_ks =
      position.castling_squares[color][Sides::Kingside] != SquareNone;
  const bool can_castle_qs =
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

  const int king_start_sq = (color == Colors::White) ? 4 : 60;
  if (king_sq != king_start_sq && (can_castle_ks || can_castle_qs)) {
    score += KSMovedKingCastle;
  }

  if (king_sq != king_start_sq) {
    const bool is_castled = (king_file == 1 || king_file == 2 || king_file == 6);
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
  const uint64_t occ = position.colors_bb[0] | position.colors_bb[1];

  uint64_t opp_p =
      position.pieces_bb[PieceTypes::Pawn] & position.colors_bb[opp_color];
  while (opp_p) {
    const int sq = pop_lsb(opp_p);
    const uint64_t atk = PAWN_ATK_SAFE(opp_color, sq);
    if (atk & king_zone) {
      attack_units += AttackWeight[PieceTypes::Pawn];
      attacker_count++;
    }
  }

  uint64_t opp_knights =
      position.pieces_bb[PieceTypes::Knight] & position.colors_bb[opp_color];
  while (opp_knights) {
    const int sq = pop_lsb(opp_knights);
    if (KNIGHT_ATK_SAFE(sq) & king_zone) {
      attack_units += AttackWeight[PieceTypes::Knight];
      attacker_count++;
    }
  }

  uint64_t opp_bishops =
      position.pieces_bb[PieceTypes::Bishop] & position.colors_bb[opp_color];
  while (opp_bishops) {
    const int sq = pop_lsb(opp_bishops);
    const uint64_t batk = get_bishop_attacks(sq, occ);
    if (batk & king_zone) {
      attack_units += AttackWeight[PieceTypes::Bishop];
      attacker_count++;
      const uint64_t xray =
          get_bishop_attacks(sq, occ ^ (batk & position.colors_bb[color]));
      if (xray & king_zone & ~batk)
        attack_units += KZBishopXray;
    }
  }

  uint64_t opp_rooks =
      position.pieces_bb[PieceTypes::Rook] & position.colors_bb[opp_color];
  while (opp_rooks) {
    const int sq = pop_lsb(opp_rooks);
    const uint64_t ratk = get_rook_attacks(sq, occ);
    if (ratk & king_zone) {
      attack_units += AttackWeight[PieceTypes::Rook];
      attacker_count++;
      const uint64_t xray =
          get_rook_attacks(sq, occ ^ (ratk & position.colors_bb[color]));
      if (xray & king_zone & ~ratk)
        attack_units += KZRookXray;
    }
  }

  uint64_t opp_queens =
      position.pieces_bb[PieceTypes::Queen] & position.colors_bb[opp_color];
  while (opp_queens) {
    const int sq = pop_lsb(opp_queens);
    if ((get_rook_attacks(sq, occ) | get_bishop_attacks(sq, occ)) & king_zone) {
      attack_units += AttackWeight[PieceTypes::Queen];
      attacker_count++;
    }
  }

  if (attacker_count >= 2) {
    const int danger = attack_units * attacker_count;
    score -= danger * KZDangerMultiplier;

    if (attacker_count >= 3)
      score -= danger * KZMultiAttackerBonus;
  } else if (attacker_count == 1 && attack_units >= KZSingleAttackerThreshold) {
    score -= attack_units * KZSingleAttackerPenalty;
  }

  const int opp_queen_count = pop_count(position.pieces_bb[PieceTypes::Queen] &
                                        position.colors_bb[opp_color]);
  if (opp_queen_count == 0 && attacker_count >= 2)
    score += KZNoQueenBonus;

  return score;
}

inline int eval_endgame(const BoardState &position, int color) noexcept {
  int score = 0;
  const int opp_color = color ^ 1;
  const int total_mat_val = total_mat(position);

  if (total_mat_val < EGMaterialThreshold &&
      total_mat_color(position, color) >
          total_mat_color(position, opp_color) + EGMaterialAdvantage) {
    const int opp_king = get_king_pos(position, opp_color);
    const int my_king = get_king_pos(position, color);

    if (is_valid_square(opp_king) && is_valid_square(my_king)) {
      const int opp_k_rank = get_rank(opp_king);
      const int opp_k_file = get_file(opp_king);
      const int center_dist = std::max(3 - opp_k_rank, opp_k_rank - 4) +
                              std::max(3 - opp_k_file, opp_k_file - 4);
      score += center_dist * EGCenterDist;
      const int k_dist = std::max(std::abs(get_rank(my_king) - opp_k_rank),
                                  std::abs(get_file(my_king) - opp_k_file));
      score += (14 - k_dist) * EGKingDist;
    }
  }

  return score;
}

inline int eval_positional(const BoardState &position, int color) noexcept {
  int score = 0;
  const int opp_color = color ^ 1;
  const int my_bishops = pop_count(position.pieces_bb[PieceTypes::Bishop] &
                                   position.colors_bb[color]);
  if (my_bishops >= 2)
    score += BishopPairBonus;

  uint64_t my_rooks =
      position.pieces_bb[PieceTypes::Rook] & position.colors_bb[color];
  const uint64_t my_pawns =
      position.pieces_bb[PieceTypes::Pawn] & position.colors_bb[color];
  const uint64_t opp_pawns =
      position.pieces_bb[PieceTypes::Pawn] & position.colors_bb[opp_color];
  while (my_rooks) {
    const int sq = pop_lsb(my_rooks);
    const int file = get_file(sq);
    const uint64_t file_bb = Files[file];
    const bool my_pawn_on_file = (my_pawns & file_bb) != 0;
    const bool opp_pawn_on_file = (opp_pawns & file_bb) != 0;
    if (!my_pawn_on_file && !opp_pawn_on_file)
      score += RookOpenFile;
    else if (!my_pawn_on_file)
      score += RookSemiOpenFile;
  }

  uint64_t pawns =
      position.pieces_bb[PieceTypes::Pawn] & position.colors_bb[color];
  while (pawns) {
    const int sq = pop_lsb(pawns);
    const int file = get_file(sq);
    const int rank = get_rank(sq);
    const int relative_rank = (color == Colors::White) ? rank : (7 - rank);

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
      int pass_bonus =
          PassedPawnBase + relative_rank * relative_rank * PassedPawnRankMul;
      const int ahead_sq = sq + (color == Colors::White ? 8 : -8);
      if (is_valid_square(ahead_sq) &&
          position.board[ahead_sq] != Pieces::Blank) {
        pass_bonus += PassedPawnBlocked;
      }
      if (relative_rank >= PassedPawnKingProximityRank) {
        const int my_king_sq = get_king_pos(position, color);
        const int opp_king_sq = get_king_pos(position, opp_color);
        if (is_valid_square(my_king_sq) && is_valid_square(opp_king_sq)) {
          const int promo_sq = (color == Colors::White) ? (file + 56) : file;
          const int my_dist =
              std::max(std::abs(get_rank(my_king_sq) - get_rank(promo_sq)),
                       std::abs(get_file(my_king_sq) - get_file(promo_sq)));
          const int opp_dist =
              std::max(std::abs(get_rank(opp_king_sq) - get_rank(promo_sq)),
                       std::abs(get_file(opp_king_sq) - get_file(promo_sq)));
          if (opp_dist > my_dist + 1)
            pass_bonus += PassedPawnKingProximity;
        }
      }
      score += pass_bonus;
    }
  }

  pawns = position.pieces_bb[PieceTypes::Pawn] & position.colors_bb[color];
  while (pawns) {
    const int sq = pop_lsb(pawns);
    const int file = get_file(sq);
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
    const int s = pop_lsb(opp_p);
    opp_pawn_attacks |= PAWN_ATK_SAFE(opp_color, s);
  }

  const uint64_t safe_space = space_mask & ~opp_pawn_attacks;
  const uint64_t our_occupancy = position.colors_bb[color] & safe_space;
  score += pop_count(our_occupancy) * SpaceWeight;

  for (int file = 0; file < 8; ++file) {
    const int pawns_on_file = pop_count(my_pawns & Files[file]);
    if (pawns_on_file > 1)
      score += (pawns_on_file - 1) * DoubledPawnPenalty;
  }

  uint64_t my_knights =
      position.pieces_bb[PieceTypes::Knight] & position.colors_bb[color];
  while (my_knights) {
    const int sq = pop_lsb(my_knights);
    const int rank = get_rank(sq);
    const int file = get_file(sq);
    const int rel_rank = (color == Colors::White) ? rank : (7 - rank);

    if (rel_rank >= 3 && rel_rank <= 5) {
      const bool pawn_support = (PAWN_ATK_SAFE(color ^ 1, sq) & my_pawns) != 0;

      if (pawn_support) {
        bool can_be_attacked = false;
        if (file > 0) {
          const uint64_t left_file = Files[file - 1];
          const uint64_t ahead =
              (color == Colors::White)
                  ? (left_file & (Ranks[rank] | Ranks[rank + 1] |
                                  (rank < 6 ? Ranks[rank + 2] : 0)))
                  : (left_file & (Ranks[rank] | Ranks[rank - 1] |
                                  (rank > 1 ? Ranks[rank - 2] : 0)));
          if (opp_pawns & ahead)
            can_be_attacked = true;
        }
        if (file < 7 && !can_be_attacked) {
          const uint64_t right_file = Files[file + 1];
          const uint64_t ahead =
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

inline int eval(BoardState &position, ThreadInfo &thread_info) {
  const int color = position.color;
  const int total_material = total_mat(position);

  int base_eval;
  if (use_nnue && nnue_loaded) {
    base_eval = thread_info.nnue_state.evaluate(color);
    const int mat = material_eval(position);
    constexpr int max_compensation = 900;
    if (base_eval > mat + max_compensation) {
      base_eval = mat + max_compensation;
    } else if (base_eval < mat - max_compensation) {
      base_eval = mat - max_compensation;
    }
  } else {
    int hce = material_eval(position);
    hce += eval_sacrifice_patterns(position, color);
    hce += eval_king_safety(position, color);
    hce -= eval_king_safety(position, color ^ 1);
    hce += eval_endgame(position, color);
    hce -= eval_endgame(position, color ^ 1);
    hce += eval_positional(position, color);
    hce -= eval_positional(position, color ^ 1);
    hce += eval_pst(position, color);
    hce -= eval_pst(position, color ^ 1);
    hce += eval_king_tropism(position, color);
    hce -= eval_king_tropism(position, color ^ 1);
    hce += eval_threats(position, color);
    hce -= eval_threats(position, color ^ 1);
    hce += TempoBonus;
    base_eval = hce;
  }

  int hce_eval = base_eval;

  int bonus2 = 0, bonus3 = 0, bonus4 = 0, bonus5 = 0;

  const int start_index = std::max(thread_info.game_ply - thread_info.search_ply, 0);
  const int s_m = thread_info.game_hist[start_index].m_diff;
  int sacrifice_pattern = 0;

  for (int idx = start_index + 2; idx < thread_info.game_ply - 4; idx += 2) {
    const bool pattern = (thread_info.game_hist[idx].m_diff < s_m &&
                          thread_info.game_hist[idx + 1].m_diff > s_m &&
                          thread_info.game_hist[idx + 2].m_diff < s_m &&
                          thread_info.game_hist[idx + 3].m_diff > s_m &&
                          thread_info.game_hist[idx + 4].m_diff < s_m);
    if (pattern) {
      sacrifice_pattern = s_m + thread_info.game_hist[idx + 4].m_diff;
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

  if (sacrifice_pattern && total_material > SacMaterialThreshold) {
    const int bounded_bonus = std::clamp(SacPatternBonus, 0, 25);
    if (thread_info.search_ply % 2) {
      bonus2 = -bounded_bonus * (hce_eval < 0 ? 2 : 1);
    } else {
      bonus2 = bounded_bonus * (hce_eval > 0 ? 2 : 1);
    }
    if (sacrifice_pattern == 4) {
      const int multi_b = std::clamp(SacMultiBonus, 0, 15);
      bonus2 += (thread_info.search_ply % 2) ? -multi_b : multi_b;
    }
  }

  constexpr uint64_t center_squares = (Files[3] | Files[4]) & (Ranks[3] | Ranks[4]);
  constexpr uint64_t extended_center =
      (Files[2] | Files[3] | Files[4] | Files[5]) &
      (Ranks[2] | Ranks[3] | Ranks[4] | Ranks[5]);
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
    const uint64_t queen_bb =
        position.pieces_bb[PieceTypes::Queen] & position.colors_bb[color];
    if (queen_bb) {
      const int queen_sq = get_lsb(queen_bb);
      const int rank = get_rank(queen_sq);
      const int bonus_rank = color == Colors::White ? 3 : 4;
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
  const uint64_t occ_mob = position.colors_bb[0] | position.colors_bb[1];
  while (own_knights) {
    const int sq = pop_lsb(own_knights);
    const int moves = pop_count(KNIGHT_ATK_SAFE(sq) & ~position.colors_bb[color]);
    mobility_bonus += (moves - MobilityKnightBase);
  }
  while (own_bishops) {
    const int sq = pop_lsb(own_bishops);
    const int moves =
        pop_count(get_bishop_attacks(sq, occ_mob) & ~position.colors_bb[color]);
    mobility_bonus +=
        (moves - MobilityBishopBase) * MobilityBishopMul / MobilityBishopDiv;
  }
  while (own_rooks_mob) {
    const int sq = pop_lsb(own_rooks_mob);
    const int moves =
        pop_count(get_rook_attacks(sq, occ_mob) & ~position.colors_bb[color]);
    mobility_bonus +=
        (moves - MobilityRookBase) * MobilityRookMul / MobilityRookDiv;
  }
  while (own_queens_mob) {
    const int sq = pop_lsb(own_queens_mob);
    const int moves = pop_count(
        (get_rook_attacks(sq, occ_mob) | get_bishop_attacks(sq, occ_mob)) &
        ~position.colors_bb[color]);
    mobility_bonus += (moves - MobilityQueenBase) / MobilityQueenDiv;
  }

  int positional_bonus = 0;
  const uint64_t home_ranks =
      color == Colors::White ? (Ranks[0] | Ranks[1]) : (Ranks[6] | Ranks[7]);
  const uint64_t undeveloped_pieces = position.colors_bb[color] &
                                      (position.pieces_bb[PieceTypes::Knight] |
                                       position.pieces_bb[PieceTypes::Bishop]) &
                                      home_ranks;
  const int undeveloped_count = pop_count(undeveloped_pieces);
  if (thread_info.game_ply > 10 && undeveloped_count > 0) {
    positional_bonus -= undeveloped_count * UndevelopedPenalty;
  }

  for (int c = 0; c < 2; ++c) {
    const int side_c = c;
    const int opp_c = c ^ 1;
    uint64_t my_p = position.pieces_bb[PieceTypes::Pawn] & position.colors_bb[side_c];
    const uint64_t opp_p = position.pieces_bb[PieceTypes::Pawn] & position.colors_bb[opp_c];
    while (my_p) {
      const int sq = pop_lsb(my_p);
      const int file = get_file(sq);
      const int rank = get_rank(sq);
      const int r_rank = (side_c == Colors::White) ? rank : (7 - rank);
      if (r_rank >= 4) {
        bool is_passed = true;
        for (int f = std::max(0, file - 1); f <= std::min(7, file + 1); ++f) {
          uint64_t ahead_mask = 0;
          if (side_c == Colors::White) {
            for (int r = rank + 1; r <= 7; ++r) ahead_mask |= (1ULL << (f + r * 8));
          } else {
            for (int r = rank - 1; r >= 0; --r) ahead_mask |= (1ULL << (f + r * 8));
          }
          if (opp_p & ahead_mask) { is_passed = false; break; }
        }
        if (is_passed) {
          int bonus = (r_rank >= 6) ? 450 : (r_rank == 5 ? 200 : 70);
          const int ahead_sq = sq + (side_c == Colors::White ? 8 : -8);
          if (is_valid_square(ahead_sq) && position.board[ahead_sq] != Pieces::Blank) {
            bonus /= 2;
          }
          if (side_c == color) positional_bonus += bonus;
          else positional_bonus -= bonus;
        }
      }
    }

    const uint64_t opp_q = position.pieces_bb[PieceTypes::Queen] & position.colors_bb[opp_c];
    const uint64_t opp_r = position.pieces_bb[PieceTypes::Rook] & position.colors_bb[opp_c];
    const int k_sq = get_king_pos(position, side_c);
    if (is_valid_square(k_sq)) {
      if (opp_q || opp_r) {
        const int k_file = get_file(k_sq);
        int open_file_penalty = 0;
        for (int f = std::max(0, k_file - 1); f <= std::min(7, k_file + 1); ++f) {
          const uint64_t file_bb = Files[f];
          const bool my_pawn = (position.pieces_bb[PieceTypes::Pawn] & position.colors_bb[side_c] & file_bb) != 0;
          if (!my_pawn) {
            const bool enemy_pawn = (position.pieces_bb[PieceTypes::Pawn] & position.colors_bb[opp_c] & file_bb) != 0;
            int pen = 0;
            if (!enemy_pawn) {
              pen = (f == k_file) ? 100 : 50;
            } else {
              pen = (f == k_file) ? 50 : 25;
            }
            if (opp_q) pen = (pen * 3) / 2;
            open_file_penalty += pen;
          }
        }
        if (side_c == color) positional_bonus -= open_file_penalty;
        else positional_bonus += open_file_penalty;
      }

      const uint64_t occ = position.colors_bb[0] | position.colors_bb[1];
      uint64_t snipers = (get_rook_attacks(k_sq, 0) & (position.pieces_bb[PieceTypes::Rook] | position.pieces_bb[PieceTypes::Queen])) |
                         (get_bishop_attacks(k_sq, 0) & (position.pieces_bb[PieceTypes::Bishop] | position.pieces_bb[PieceTypes::Queen]));
      snipers &= position.colors_bb[opp_c];
      while (snipers) {
        const int s_sq = pop_lsb(snipers);
        const uint64_t between = (BetweenBBs[k_sq][s_sq] & occ) & ~(1ULL << s_sq);
        if (between && !(between & (between - 1))) {
          if (between & position.colors_bb[side_c]) {
            const int pinned_sq = get_lsb(between);
            const int pt = get_piece_type(position.board[pinned_sq]);
            int pin_penalty = 0;
            if (pt == PieceTypes::Rook) pin_penalty = 120;
            else if (pt == PieceTypes::Queen) pin_penalty = 180;
            else if (pt == PieceTypes::Knight || pt == PieceTypes::Bishop) pin_penalty = 40;
            if (side_c == color) positional_bonus -= pin_penalty;
            else positional_bonus += pin_penalty;
          }
        }
      }
    }
  }

  bonus3 = center_control;
  bonus4 = mobility_bonus;
  bonus5 = positional_bonus;

  float multiplier = (static_cast<float>(EvalMultBase) +
                      total_material / static_cast<float>(EvalMultMatDiv)) /
                     static_cast<float>(EvalMultNorm);
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
  } else if (hce_eval < EvalLosingThreshold &&
             total_material > EvalWinningMatThreshold) {
    multiplier *= (EvalLosingMul / 100.0f) * phase_factor;
  } else if (hce_eval < EvalSlightLoseThreshold &&
             total_material > EvalWinningMatThreshold) {
    multiplier *= (EvalSlightLoseMul / 100.0f) * phase_factor;
  } else {
    multiplier *= phase_factor;
  }
  multiplier = std::clamp(multiplier, 0.70f, 1.35f);

  if (thread_info.is_human && thread_info.search_ply < 3 &&
      thread_info.human_noise_sigma > 0) {
    const int span = std::max(4, thread_info.human_noise_sigma / 4);
    const int noise = (Random::dist(Random::rd) % (2 * span + 1)) - span;
    hce_eval += noise;
  }

  hce_eval = static_cast<int>(hce_eval * multiplier);
  return std::clamp(hce_eval + bonus2 + bonus3 + bonus4 + bonus5, -MaxEval,
                    MaxEval);
}

inline int correct_eval(const BoardState &position, const ThreadInfo &thread_info,
                        int eval) noexcept {

  eval = eval * std::max(0, HALFMOVE_SCALE_MAX - position.halfmoves) / HALFMOVE_SCALE_MAX;

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

  return std::clamp(eval + (CorrWeight * corr / 512), -MaxEval, MaxEval);
}

inline void ss_push(const BoardState &position, ThreadInfo &thread_info, Action move) noexcept {
  assert(thread_info.search_ply + 1 < MaxSearchPly);
  assert(thread_info.game_ply + 1 < MaxGameLen);
  auto &record = thread_info.game_hist[thread_info.game_ply];
  record.position_key = position.zobrist_key;
  record.played_move = move;
  record.piece_moved = move == MoveNone ? Pieces::Blank : position.board[extract_from(move)];
  record.is_cap = is_cap(position, move);
  record.m_diff = material_eval(position);
  ++thread_info.search_ply;
  ++thread_info.game_ply;
}

inline void ss_pop(ThreadInfo &thread_info) noexcept {
  assert(thread_info.search_ply > 0 && thread_info.game_ply > 0);
  --thread_info.search_ply;
  --thread_info.game_ply;
  if (use_nnue && nnue_loaded) thread_info.nnue_state.pop();
}

inline bool material_draw(const BoardState &position) noexcept {
  if (position.pieces_bb[PieceTypes::Pawn] || position.pieces_bb[PieceTypes::Rook] ||
      position.pieces_bb[PieceTypes::Queen]) return false;
  const uint64_t knights = position.pieces_bb[PieceTypes::Knight];
  const uint64_t bishops = position.pieces_bb[PieceTypes::Bishop];
  if (pop_count(knights | bishops) <= 1) return true;
  constexpr uint64_t dark = 0xAA55AA55AA55AA55ULL;
  return !knights && (!(bishops & dark) || !(bishops & ~dark));
}

inline bool is_draw(const BoardState &position, ThreadInfo &thread_info) noexcept {
  if (position.halfmoves >= 100) {
    if (attacks_square(position, get_king_pos(position, position.color), position.color ^ 1)) {
      std::array<Action, MaxActions> moves{};
      if (!legal_movegen(position, moves.data())) return false;
    }
    return true;
  }
  if (material_draw(position)) return true;
  int repetitions = 0;
  const int earliest = std::max(0, int(thread_info.game_ply) - position.halfmoves);
  const int root_ply = int(thread_info.game_ply) - thread_info.search_ply;
  for (int i = int(thread_info.game_ply) - 1; i >= earliest; --i) {
    const auto &record = thread_info.game_hist[i];
    if (record.played_move == MoveNone) break;
    if (record.position_key == position.zobrist_key &&
        (++repetitions == 2 || i >= root_ply)) return true;
  }
  return false;
}

inline int draw_score(const BoardState &position, ThreadInfo &thread_info) noexcept {
  int score = 1 - (thread_info.nodes.load(std::memory_order_relaxed) & 3);
  const int material = material_eval(position);

  if (material < 0) {
    score += DrawContemptMaterial;
  } else if (material > 0) {
    score -= DrawContemptMaterial;
  }

  score += Contempt;
  return score;
}

inline int qsearch(int alpha, int beta, BoardState &position, ThreadInfo &thread_info,
            std::vector<TTBucket> &table, int qdepth = 0) {

  constexpr int MAX_QDEPTH = 16;

  auto eval_now = [&](BoardState &pos) {
    return correct_eval(pos, thread_info, eval(pos, thread_info));
  };

  std::array<Action, MaxActions> terminal_moves{};
  if (!legal_movegen(position, terminal_moves.data())) {
    return attacks_square(position, get_king_pos(position, position.color), position.color ^ 1)
               ? -MateScore + thread_info.search_ply : 0;
  }

  if (qdepth >= MAX_QDEPTH) {
    return eval_now(position);
  }

  int ply = thread_info.search_ply;

  if (ply >= MaxSearchPly - 4) {
    return eval_now(position);
  }

  if (thread_info.game_ply >= MaxGameLen - 2) {
    return eval_now(position);
  }

  if (ply && is_draw(position, thread_info)) {
    return draw_score(position, thread_info);
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

  StateRecord *ss = &(thread_info.game_hist[thread_info.game_ply]);

  ++thread_info.nodes;
  if (ply > thread_info.seldepth)
    thread_info.seldepth = ply;

  uint64_t hash = position.zobrist_key;
  uint8_t saved_phase = thread_info.phase;

  bool tt_hit;
  TTEntry entry = probe_entry(hash, tt_hit, thread_info.searches, table);

  int entry_type = EntryTypes::None;
  int tt_static_eval = ScoreNone;
  int tt_score = ScoreNone;
  Action tt_move = MoveNone;

  if (tt_hit) {
    entry_type = entry.get_type();
    tt_static_eval = entry.static_eval;
    tt_score = position.halfmoves < 90 ? score_from_tt(entry.score, ply) : ScoreNone;
    tt_move = entry.best_move;
  }

  if (tt_score != ScoreNone &&
      ((entry_type == EntryTypes::Exact) ||
       (entry_type == EntryTypes::LBound && tt_score >= beta) ||
       (entry_type == EntryTypes::UBound && tt_score <= alpha))) {
    return tt_score;
  }

  uint64_t in_check = attacks_square(
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
    if (thread_data.stop)
      break;
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
    auto *nnue_before = thread_info.nnue_state.m_curr;
    update_nnue_state(thread_info, move, position, moved_position);

    int score = ScoreNone;
    bool can_recurse = (thread_info.search_ply + 1 < MaxSearchPly - 4) &&
                       (thread_info.game_ply < MaxGameLen - 2);

    if (can_recurse) {
      ss_push(position, thread_info, move);
      score = -qsearch(-beta, -alpha, moved_position, thread_info, table,
                       qdepth + 1);
      ss_pop(thread_info);
    } else {
      const int leaf_eval = eval_now(moved_position);
      score = -leaf_eval;
      if (thread_info.nnue_state.m_curr != nnue_before) {
        thread_info.nnue_state.pop();
      }
    }

    thread_info.phase = saved_phase;

    if (thread_data.stop) {
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
inline int search(int alpha, int beta, int depth, bool cutnode, BoardState &position,
           ThreadInfo &thread_info, std::vector<TTBucket> &table) {

  if (thread_info.game_ply >= MaxGameLen - 2)
    return correct_eval(position, thread_info, eval(position, thread_info));
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

  if (ply >= MaxRootDepth) {
    return correct_eval(position, thread_info, eval(position, thread_info));
  }
  depth = std::min(depth, MaxRootDepth - ply);

  if (out_of_time(thread_info) || ply >= MaxSearchPly - 1) {

    return correct_eval(position, thread_info, eval(position, thread_info));
  }

  if (ply && is_draw(position, thread_info)) {
    return draw_score(position, thread_info);
  }

  if (thread_info.max_depth > 0 && ply >= thread_info.max_depth) {
    return correct_eval(position, thread_info, eval(position, thread_info));
  }

  if (depth <= 0) {
    return qsearch(alpha, beta, position, thread_info, table);
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
  TTEntry entry = probe_entry(hash, tt_hit, thread_info.searches, table);

  int entry_type = EntryTypes::None, tt_static_eval = ScoreNone,
      tt_score = ScoreNone, tt_move = MoveNone;

  if (tt_hit && !singular_search) {
    entry_type = entry.get_type();
    tt_static_eval = entry.static_eval;
    tt_score = position.halfmoves < 90 ? score_from_tt(entry.score, ply) : ScoreNone;
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
  int total_material_here = total_mat(position);
  bool endgame_node =
      is_endgame_reduction_zone(position, thread_info, total_material_here);
  bool zugzwang_prone =
      is_zugzwang_prone(position, thread_info, total_material_here);

  int32_t static_eval;
  int32_t raw_eval;

  if (in_check) {
    static_eval = raw_eval = ScoreNone;
  } else if (singular_search) {
    static_eval = raw_eval = ss->static_eval;
  } else {
    raw_eval = (tt_static_eval == ScoreNone) ? eval(position, thread_info) : tt_static_eval;
    static_eval = correct_eval(position, thread_info, raw_eval);

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

    if (!endgame_node && depth <= RFPMaxDepth &&
        static_eval - RFPMargin * (depth - improving) >= beta) {
      return (static_eval + beta) / 2;
    }

    if (!endgame_node && !is_pv && depth <= 3 &&
        static_eval + RazorMargin * depth < alpha) {
      int razor_score = qsearch(alpha, beta, position, thread_info, table);
      if (razor_score <= alpha)
        return razor_score;
    }

    if (!zugzwang_prone && static_eval >= beta && depth >= NMPMinDepth &&
        has_non_pawn_material(position, color) && thread_info.game_ply > 0 &&
        (ss - 1)->played_move != MoveNone) {

      BoardState temp_pos = position;

      make_move(temp_pos, MoveNone);

      if (thread_info.search_ply >= MaxSearchPly ||
          thread_info.game_ply >= MaxGameLen) {
        return ScoreNone;
      }
      update_nnue_state(thread_info, MoveNone, position, temp_pos);
      ss_push(position, thread_info, MoveNone);

      int R = NMPBase + depth / NMPDepthDiv +
              std::min(3, (static_eval - beta) / NMPEvalDiv);
      score = -search<false>(-beta, -beta + 1, depth - R, !cutnode, temp_pos,
                             thread_info, table);

      ss_pop(thread_info);

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
      if (thread_data.stop)
        break;
      if (mc_moves >= MultiCutMoves)
        break;
      if (!is_legal(position, move))
        continue;

      mc_moves++;

      BoardState mc_pos = position;
      make_move(mc_pos, move);

      update_nnue_state(thread_info, move, position, mc_pos);
      ss_push(position, thread_info, move);
      int mc_score = -search<false>(-beta, -beta + 1, depth - 4, false, mc_pos,
                                    thread_info, table);
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
  if (!root && !is_pv && !in_check && !singular_search && !endgame_node && depth >= 5 && abs(beta) < MateThreshold &&
      (!tt_hit || entry.depth + 4 <= depth || tt_score >= p_beta)) {

    int threshold = p_beta - static_eval;
    MovePicker probcut_p;
    init_picker(probcut_p, position, threshold, in_check, ss);
    Action p_tt_move =
        (tt_move != MoveNone && SEE(position, tt_move, threshold) ? tt_move
                                                                  : MoveNone);

    while (Action move =
               next_move(probcut_p, position, thread_info, p_tt_move, true)) {
      if (thread_data.stop)
        break;

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
      update_nnue_state(thread_info, move, position, moved_position);
      ss_push(position, thread_info, move);

      int probcut_score =
          -qsearch(-p_beta, -p_beta + 1, moved_position, thread_info, table);
      if (probcut_score >= p_beta) {
        probcut_score = -search<is_pv>(-p_beta, -p_beta + 1, depth - 4, false,
                               moved_position, thread_info, table);
      }

      ss_pop(thread_info);
      thread_info.phase = phase;

      if (probcut_score >= p_beta) {
        return probcut_score;
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

  while (Action move =
             next_move(picker, position, thread_info, tt_move, skip)) {
    if (thread_data.stop) break;

    RootAction *root_move_entry = nullptr;
    if (root) {
      root_move_entry = find_root_move(thread_info, move);
      if (!root_move_entry) {
        continue;
      }
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

    const int from_sq = extract_from(move);
    const int to_sq = extract_to(move);
    const int moving_piece = is_valid_square(from_sq) ? position.board[from_sq] : Pieces::Blank;
    const bool is_pawn = (get_piece_type(moving_piece) == PieceTypes::Pawn);
    const int rel_rank = is_pawn ? ((position.color == Colors::White) ? get_rank(to_sq) : (7 - get_rank(to_sq))) : 0;
    const bool is_advanced_pawn = is_pawn && (rel_rank >= 5);

    int hist_score =
        thread_info.HistoryScores[moving_piece]
                                 [to_sq];

    is_capture = is_cap(position, move);
    if (!is_capture && !is_pv && best_score > -MateScore) {

      if (!is_advanced_pawn && !endgame_node && !in_check && depth < LMPDepth &&
          moves_played >= LMPBase + depth * depth / (2 - improving)) {
        skip = true;
      }

      if (!is_advanced_pawn && !endgame_node && !in_check && depth < FPDepth &&
          picker.stage > Stages::Captures) {
        int fp_margin = FPMargin1 + FPMargin2 * depth;
        if (thread_info.attack_mode)
          fp_margin += FPAttackModeBonus;
        if (static_eval + fp_margin < alpha) {
          skip = true;
        }
      }

      if (!is_advanced_pawn && !endgame_node && !in_check && !is_pv && !is_capture && depth < HistPruneDepth &&
          hist_score < -HistPruneThreshold * depth) {
        skip = true;
      }
    }

    if (!root && !in_check && !is_advanced_pawn && best_score > -MateThreshold && depth < SeePruningDepth &&
        (!endgame_node || is_capture)) {

      int margin =
          is_capture ? (depth * SeePruningNoisyMargin) : SeePruningQuietMargin;

      if (!SEE(position, move, depth * margin)) {

        continue;
      }
    }

    int extension = 0;

    if (!root && ply < thread_info.current_iter * 2) {
      if (!singular_search && depth >= SEDepth && move == tt_move &&
          tt_hit && tt_score != ScoreNone && abs(tt_score) < MateThreshold && entry.depth >= depth - 3 &&
          (entry_type == EntryTypes::LBound || entry_type == EntryTypes::Exact)) {

        int sBeta = tt_score - depth;
        thread_info.excluded_move = move;
        int sScore = search<false>(sBeta - 1, sBeta, (depth - 1) / 2, cutnode,
                                   position, thread_info, table);

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

    if (extension == 0 && !is_capture && is_advanced_pawn && depth >= 2) {
      extension = (rel_rank >= 6) ? 2 : 1;
    }

    BoardState moved_position = position;
    make_move(moved_position, move);

    if (thread_info.search_ply >= MaxSearchPly ||
        thread_info.game_ply >= MaxGameLen) {
      return best_score;
    }
    update_nnue_state(thread_info, move, position, moved_position);
    ss_push(position, thread_info, move);

    bool full_search = false;
    if (extension > 0) {
      constexpr int MaxExtensionBudget = 10;
      int remaining_extension_budget =
          thread_info.current_iter + MaxExtensionBudget - (ply + depth);
      if (remaining_extension_budget <= 0) {
        extension = 0;
      } else {
        extension = std::min(extension, remaining_extension_budget);
      }
    }
    auto clamp_child_depth = [&](int child_depth) {
      int max_child_depth = std::max(0, depth - 1 + extension);
      return std::clamp(child_depth, 0, max_child_depth);
    };
    if (!extension && root && thread_info.sacrifice_lookahead && !in_check &&
        !SEE(position, move, 0) && !out_of_time(thread_info)) {
      const int compensation = analyze_sacrifice(moved_position, thread_info, 1, 0, color);
      const int required = std::max(60, 150 - thread_info.sacrifice_lookahead_aggressiveness);
      if (compensation > required) extension = 1;
    }
    int newdepth = clamp_child_depth(std::min(depth - 1 + extension, 126));

    if (newdepth > 0 && depth >= LMRMinDepth && moves_played > is_pv) {
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

      bool gives_check =
          (attacks_square(moved_position, get_king_pos(position, color ^ 1),
                          color) != 0);
      R -= gives_check;

      if (gives_check && depth >= 6)
        R -= 1;

      if (is_advanced_pawn && R > 0) {
        R = (rel_rank >= 6) ? 0 : std::max(0, R - 2);
      }

      if (thread_info.attack_mode && R > 0) {
        R = std::max(0, R - 2);
      }

      if (endgame_node && R > 0) {
        R = std::max(0, R - 1);
        if (zugzwang_prone && R > 0) {
          R = std::max(0, R - 1);
        }
      }

      R = std::clamp(R, 0, newdepth - 1);

      score = -search<false>(-alpha - 1, -alpha, newdepth - R, true,
                             moved_position, thread_info, table);
      if (score > alpha) {
        full_search = R > 0;
        newdepth += (score > (best_score + 60 + newdepth * 2));
        newdepth -= (score < best_score + newdepth && !root);
        newdepth = clamp_child_depth(newdepth);
      }
    } else {
      full_search = moves_played || !is_pv;
    }
    if (full_search) {

      score = -search<false>(-alpha - 1, -alpha, newdepth, !cutnode,
                             moved_position, thread_info, table);
    }
    if ((score > alpha || !moves_played) && is_pv) {

      score = -search<true>(-beta, -alpha, newdepth, false, moved_position,
                            thread_info, table);
    }

    ss_pop(thread_info);
    thread_info.phase = phase;

    if (thread_data.stop) {

      return best_score;
    }

    if (root && root_move_entry) {
      root_move_entry->nodes += (thread_info.nodes.load() - curr_nodes);
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
          for (int n = 0; n < MaxSearchPly - ply - 1; n++) {
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

      int their_last = SquareNone;
      int their_piece = Pieces::Blank;
      int our_last = SquareNone;
      int our_piece = Pieces::Blank;
      int ply4_last = SquareNone;
      int ply4_piece = Pieces::Blank;

      if (thread_info.game_ply >= 1 && (ss - 1)->played_move != MoveNone) {
        their_last = extract_to((ss - 1)->played_move);
        their_piece = (ss - 1)->piece_moved;
      }
      if (thread_info.game_ply >= 2 && (ss - 2)->played_move != MoveNone) {
        our_last = extract_to((ss - 2)->played_move);
        our_piece = (ss - 2)->piece_moved;
      }
      if (thread_info.game_ply >= 4 && (ss - 4)->played_move != MoveNone) {
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

      if (their_piece != Pieces::Blank && their_last != SquareNone) {
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

  if (!singular_search && !(root && (thread_info.root_moves_limited || thread_info.multipv_index))) {
    insert_entry(entry, hash, depth, best_move, raw_eval,
                 score_to_tt(best_score, ply), entry_type,
                 thread_info.searches);
  }

  return best_score;
}

inline std::string format_pv(const BoardState &position, const ThreadInfo &thread_info) {
  BoardState temp_pos = position;
  std::string result;
  int indx = 0;

  while (indx < MaxSearchPly && thread_info.pv[indx] != MoveNone) {

    const Action best_move = thread_info.pv[indx];

    std::array<Action, MaxActions> legal_moves{};
    const int movelen = legal_movegen(temp_pos, legal_moves.data());

    bool found_move = false;

    for (int i = 0; i < movelen; i++) {
      if (legal_moves[i] == best_move) {
        found_move = true;
        break;
      }
    }

    if (!found_move) {
      break;
    }

    {
      const std::string mv = internal_to_uci(temp_pos, best_move);
      if (!result.empty()) result += ' ';
      result += mv;
    }

    make_move(temp_pos, best_move);

    indx++;
  }

  return result;
}

inline uint8_t root_phase(const BoardState &position) noexcept {
  const int material = total_mat(position);
  if (material <= EndgameMaterial) return PhaseTypes::Endgame;
  if (material <= LatePhaseMaterial) return PhaseTypes::LateMiddleGame;
  const uint64_t played_plies = 2ULL * (position.fullmove - 1) + position.color;
  return played_plies < static_cast<uint64_t>(OpeningMinPly)
             ? PhaseTypes::Opening : PhaseTypes::MiddleGame;
}

inline void prepare_search_evaluator(const BoardState &position, ThreadInfo &info,
                                     std::vector<TTBucket> &table) noexcept {
  uint8_t desired = root_phase(position);
  const int material = total_mat(position);
  if (info.cached_eval_phase != SquareNone && desired != PhaseTypes::Endgame) {
    if (info.attack_mode) desired = PhaseTypes::Sacrifice;
    else if (info.phase == PhaseTypes::Endgame && material <= EndRecoverMaterial)
      desired = PhaseTypes::Endgame;
    else if (info.phase == PhaseTypes::LateMiddleGame && material <= MidRecoverMaterial)
      desired = PhaseTypes::LateMiddleGame;
  }
  if (info.cached_eval_phase == SquareNone || desired == PhaseTypes::Endgame || desired == PhaseTypes::Sacrifice) {
    info.phase = desired;
    info.phase_hit_counts.fill(0);
  } else if (desired != info.phase) {
    const auto hits = static_cast<uint8_t>(info.phase_hit_counts[desired] + 1);
    info.phase_hit_counts.fill(0);
    info.phase_hit_counts[desired] = hits;
    if (hits >= PhaseConfirmHits) info.phase = desired;
  } else {
    info.phase_hit_counts.fill(0);
  }
  select_active_nnue(info.phase);
  const NNUE_Params *network = use_nnue ? g_nnue : nullptr;
  if (info.cached_eval_network != network || info.cached_eval_phase != info.phase) {
    std::fill(table.begin(), table.end(), TTBucket{});
    info.PawnCorrHist.fill({});
    info.NonPawnCorrHist.fill({});
    info.cached_eval_network = network;
  }
  info.cached_eval_phase = info.phase;
}

inline void iterative_deepen(BoardState &position, ThreadInfo &thread_info,
                      std::vector<TTBucket> &table) {

  thread_info.original_opt = thread_info.opt_time;

  calculate(position);
  thread_info.nodes.store(0);
  thread_info.time_checks = 0;
  thread_info.search_ply = 0;
  if (use_nnue && nnue_loaded) {
    select_active_nnue(thread_info.phase);
    thread_info.nnue_state.reset_nnue(position);
  }
  thread_info.excluded_move = MoveNone;
  thread_info.best_moves = {0};
  thread_info.best_scores.fill(ScoreNone);
  for (auto &k : thread_info.KillerMoves) {
    k[0] = MoveNone;
    k[1] = MoveNone;
  }

  thread_info.root_moves.reserve(MaxActions);
  std::array<Action, MaxActions> raw_root_moves{};
  int nmoves = legal_movegen(position, raw_root_moves.data());
  if (thread_info.root_moves_limited) {
    std::vector<RootAction> filtered;
    filtered.reserve(thread_info.root_moves.size());
    for (const auto &rm : thread_info.root_moves) {
      Action candidate = rm.move;
      bool legal = false;
      for (int i = 0; i < nmoves; i++) {
        if (raw_root_moves[i] == candidate) {
          legal = true;
          break;
        }
      }
      if (!legal)
        continue;
      bool duplicate =
          std::any_of(filtered.begin(), filtered.end(),
                      [&](const RootAction &r) { return r.move == candidate; });
      if (!duplicate) {
        filtered.push_back({candidate, 0});
      }
    }
    thread_info.root_moves = std::move(filtered);
  } else {
    thread_info.root_moves.clear();
    for (int i = 0; i < nmoves; i++) {
      thread_info.root_moves.push_back({raw_root_moves[i], 0});
    }
  }

  Action prev_best = MoveNone;
  int alpha = ScoreNone, beta = -ScoreNone;
  int bm_stability = 0;

  int target_depth = std::clamp(thread_info.max_iter_depth, 1, MaxRootDepth);
  int last_completed_depth = 0;
  std::array<Action, MaxActions> completed_moves{};
  std::array<int, MaxActions> completed_scores{};
  completed_scores.fill(ScoreNone);
  std::array<Action, MaxSearchPly> completed_pv{};

  auto update_attack_mode = [&](ThreadInfo &ti, BoardState &pos) {
    if (ti.thread_id != 0 || last_completed_depth == 0)
      return;

    int total_material = total_mat(pos);
    int root_eval = ti.best_scores[0];
    ti.prev_root_eval = ti.last_root_eval;
    ti.last_root_eval = root_eval;
    ti.root_completed_depth = last_completed_depth;

    if (!ti.attack_mode) {
      if (last_completed_depth >= AttackModeEnterDepth &&
          root_eval >= SacrificeEnterCp &&
          total_material >= AttackModeMaterial) {

        if (ti.prev_root_eval >= SacrificeEnterCp - AttackModeEnterRelax) {
          ti.attack_mode = true;
        }
      }
    } else {
      bool drop = (ti.prev_root_eval - root_eval) >=
                  SacrificeDropThreshold + AttackModeDropExtra;
      if (root_eval <= SacrificeExitCp - AttackModeExitRelax ||
          total_material < EndgameMaterial - AttackModeMatExit || drop) {
        ti.attack_mode = false;
      }
    }

  };
  int real_multi_pv =
      std::min<int>(thread_info.multipv, (int)thread_info.root_moves.size());

  for (int depth = 1; !thread_info.root_moves.empty(); ++depth) {
    if (thread_data.stop) {
      break;
    }
    if (depth > target_depth || depth > MaxRootDepth) {
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
          search<true>(alpha, beta, depth, false, position, thread_info, table);

      while (score <= alpha || score >= beta || thread_data.stop) {

        if (thread_data.stop) {
          goto finish;
        }

        if (thread_info.thread_id == 0 &&
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
                            : static_cast<int64_t>(nodes) * 1000;

          Action move = score <= alpha
                            ? prev_best
                            : thread_info.best_moves[thread_info.multipv_index];

          std::string pv_suffix = "";
          if (move != MoveNone) {
            std::string pv_str = internal_to_uci(position, move);
            if (pv_str != "0000") {
              pv_suffix = " pv " + pv_str;
            }
          }

          if (abs(score) < MateScore - MaxSearchPly) {
            safe_printf(
                "info multipv %i depth %i seldepth %i score cp %i %s nodes "
                "%" PRIu64 " nps %" PRIi64 " time %" PRIi64 "%s\n",
                thread_info.multipv_index + 1, depth, thread_info.seldepth,
                score * 100 / NormalizationFactor, bound_string.c_str(),
                nodes, nps, search_time, pv_suffix.c_str());
          } else if (score > 0) {
            int dist = std::max(1, (MateScore - score + 1) / 2);
            safe_printf("info multipv %i depth %i seldepth %i score mate %i "
                        "%s nodes %" PRIu64 " nps %" PRIi64 " time %" PRIi64
                        "%s\n",
                        thread_info.multipv_index + 1, depth,
                        thread_info.seldepth, dist, bound_string.c_str(),
                        nodes, nps, search_time, pv_suffix.c_str());
          } else {
            int dist = std::max(1, (MateScore + score + 1) / 2);
            safe_printf("info multipv %i depth %i seldepth %i score mate %i "
                        "%s nodes %" PRIu64 " nps %" PRIi64 " time %" PRIi64
                        "%s\n",
                        thread_info.multipv_index + 1, depth,
                        thread_info.seldepth, -dist, bound_string.c_str(),
                        nodes, nps, search_time, pv_suffix.c_str());
          }
        }

        if (score <= alpha) {
          beta = (alpha + beta) / 2;
          alpha -= delta;
          if (score <= -MateScore + MaxSearchPly) alpha = ScoreNone;
          temp_depth = depth;
        } else if (score >= beta) {
          beta += delta;
          if (score >= MateScore - MaxSearchPly) beta = -ScoreNone;
          temp_depth = std::max(temp_depth - 1, 1);
        }
        delta += delta / 3;

        score = search<true>(alpha, beta, temp_depth, false, position,
                             thread_info, table);
      }

      if (score == ScoreNone) {
        break;
      }

      std::string eval_string;

      if (abs(score) < MateScore - MaxSearchPly) {
        eval_string = "cp " + std::to_string(score * 100 / NormalizationFactor);
      } else if (score > 0) {
        int dist = (MateScore - score + 1) / 2;
        eval_string = "mate " + std::to_string(std::max(1, dist));
      } else {
        int dist = (MateScore + score + 1) / 2;
        eval_string = "mate -" + std::to_string(std::max(1, dist));
      }

      thread_info.best_moves[thread_info.multipv_index] = thread_info.pv[0];
      completed_moves[thread_info.multipv_index] = thread_info.pv[0];
      completed_scores[thread_info.multipv_index] = score;
      if (thread_info.multipv_index == 0) {
        std::copy_n(thread_info.pv.begin(), MaxSearchPly, completed_pv.begin());
        last_completed_depth = depth;
      }

      if (thread_info.thread_id == 0) {

        uint64_t nodes = thread_info.nodes.load();

        for (auto &td : thread_data.thread_infos) {
          nodes += td.nodes.load();
        }

        int64_t search_time = time_elapsed(thread_info.start_time);
        int64_t nps = static_cast<int64_t>(nodes) * 1000 / std::max<int64_t>(1, search_time);

        safe_printf(
            "info multipv %i depth %i seldepth %i score %s nodes %" PRIu64
            " nps %" PRIi64 " time %" PRIi64 " tbhits %" PRIu64 " pv %s\n",
            thread_info.multipv_index + 1, depth, thread_info.seldepth,
            eval_string.c_str(), nodes, nps, search_time, thread_data.tb_hits.load(),
            format_pv(position, thread_info).c_str());

        if ((!thread_info.infinite_search && !thread_data.pondering &&
             static_cast<uint64_t>(search_time) > thread_info.opt_time) ||
            (!thread_data.pondering && nodes > thread_info.opt_nodes_searched)) {
          thread_data.stop = true;
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

      if (thread_data.stop) {
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

    if (thread_info.mate_search > 0 && completed_scores[0] >= MateScore - MaxSearchPly) {
      int dist = (MateScore - completed_scores[0] + 1) / 2;
      if (dist <= thread_info.mate_search) {
        thread_data.stop = true;
        break;
      }
    }

    if (abs(completed_scores[0]) >= MateScore - MaxSearchPly) {
      int mate_plies = MateScore - abs(completed_scores[0]);
      if (depth >= mate_plies + 2) {
        if (!thread_info.infinite_search && !thread_data.pondering) {
          thread_data.stop = true;
        }
        break;
      }
    }
  }

finish:

  if (completed_moves[0] != MoveNone) {
    thread_info.best_moves = completed_moves;
    thread_info.best_scores = completed_scores;
    std::copy(completed_pv.begin(), completed_pv.end(), thread_info.pv.begin());
  }
  update_attack_mode(thread_info, position);

  if (thread_info.thread_id == 0) {
    if (!thread_data.stop && (thread_info.infinite_search || thread_data.pondering)) {
      std::unique_lock lock(thread_data.control_mutex);
      thread_data.control_cv.wait(lock, [&] {
        return thread_data.stop || (!thread_info.infinite_search && !thread_data.pondering);
      });
    }
    thread_data.stop = true;
  }

  auto validate_ponder_move = [&](const BoardState &root_position,
                                  Action best_move,
                                  Action ponder_candidate) noexcept -> Action {
    if (ponder_candidate == MoveNone || best_move == MoveNone) {
      return MoveNone;
    }

    std::array<Action, MaxActions> root_legal{};
    const int root_count = legal_movegen(root_position, root_legal.data());
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

    BoardState ponder_position = root_position;
    make_move(ponder_position, best_move);

    std::array<Action, MaxActions> response_legal{};
    const int response_count = legal_movegen(ponder_position, response_legal.data());
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
      BoardState temp_pos = position;
      make_move(temp_pos, thread_info.best_moves[0]);

      bool tt_hit = false;
      TTEntry tt_entry =
          probe_entry(temp_pos.zobrist_key, tt_hit, thread_info.searches, table);
      if (tt_hit && tt_entry.best_move != MoveNone) {
        thread_info.ponder_move = tt_entry.best_move;
      } else {
        thread_info.ponder_move = MoveNone;
      }
    }
  }

  if (thread_info.thread_id == 0 && thread_info.variety > 0) {

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

      std::array<int, 32> promo_adjust{};
      for (int i = 0;
           i < variety_lines && thread_info.best_moves[i] != MoveNone; i++) {
        Action move = thread_info.best_moves[i];
        if (extract_type(move) == MoveTypes::Promotion &&
            extract_promo(move) != Promos::Queen) {
          BoardState temp_pos = position;
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

  if (thread_info.thread_id == 0 && thread_info.is_human) {

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
    if (thread_info.root_moves_limited) {
      if (!thread_info.root_moves.empty()) {
        thread_info.best_moves[0] = thread_info.root_moves[0].move;
        thread_info.best_scores[0] = 0;
      }
    } else {
      std::array<Action, MaxActions> legal_moves;
      int num_legal = legal_movegen(position, legal_moves.data());
      if (num_legal > 0) {
        thread_info.best_moves[0] = legal_moves[0];
        thread_info.best_scores[0] = 0;
      }
    }
  }

  if (thread_info.thread_id == 0 &&
      (!thread_info.infinite_search || thread_data.stop)) {
    if (thread_info.best_moves[0] == MoveNone) {
      safe_printf("bestmove 0000\n");
    } else {
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
        BoardState ponder_pos = position;
        make_move(ponder_pos, thread_info.best_moves[0]);
        std::string pd = internal_to_uci(ponder_pos, validated_ponder);
        safe_printf("bestmove %s ponder %s\n", bm.c_str(), pd.c_str());
      } else {
        safe_printf("bestmove %s\n", bm.c_str());
      }
    }
    }
  }
}

inline void filter_root_tablebase(const BoardState &position, ThreadInfo &thread_info) {
  if (!tb_initialized || !thread_info.use_syzygy || !TB_LARGEST ||
      pop_count(position.colors_bb[0] | position.colors_bb[1]) >
          std::min<int>(TB_LARGEST, thread_info.syzygy_probe_limit)) return;
  for (const auto &rights : position.castling_squares)
    for (int rook : rights) if (rook != SquareNone) return;
  bool repeated = false;
  for (int i = 0; i < thread_info.game_ply; ++i) {
    if (thread_info.game_hist[i].position_key == position.zobrist_key) {
      repeated = true;
      break;
    }
  }
  auto results = std::make_unique<TbRootMoves>();
  const unsigned ep = position.ep_square == SquareNone ? 0 : position.ep_square;
  const unsigned rule50 = thread_info.syzygy_50_move_rule ? std::min<int>(position.halfmoves, 100) : 0;
  auto probe = [&](bool dtz) {
    const auto &bb = position.pieces_bb;
    if (dtz) return tb_probe_root_dtz(position.colors_bb[0], position.colors_bb[1],
        bb[PieceTypes::King], bb[PieceTypes::Queen], bb[PieceTypes::Rook], bb[PieceTypes::Bishop],
        bb[PieceTypes::Knight], bb[PieceTypes::Pawn], rule50, 0, ep,
        position.color == Colors::White, repeated, thread_info.syzygy_50_move_rule, results.get());
    return tb_probe_root_wdl(position.colors_bb[0], position.colors_bb[1],
        bb[PieceTypes::King], bb[PieceTypes::Queen], bb[PieceTypes::Rook], bb[PieceTypes::Bishop],
        bb[PieceTypes::Knight], bb[PieceTypes::Pawn], rule50, 0, ep,
        position.color == Colors::White, thread_info.syzygy_50_move_rule, results.get());
  };

  if (!probe(true) && (rule50 || !probe(false))) { ++thread_data.tb_fails; return; }
  ++thread_data.tb_hits;
  std::vector<RootAction> best;
  int best_rank = INT32_MIN;
  for (unsigned i = 0; i < results->size; ++i) {
    const auto &result = results->moves[i];
    for (const auto &legal : thread_info.root_moves) {
      const Action move = legal.move;
      const unsigned promo = TB_MOVE_PROMOTES(result.move);
      if (extract_from(move) != TB_MOVE_FROM(result.move) || extract_to(move) != TB_MOVE_TO(result.move)) continue;
      if (promo ? (extract_type(move) != MoveTypes::Promotion || extract_promo(move) != 4 - promo)
                : extract_type(move) == MoveTypes::Promotion) continue;

      if (result.tbRank > best_rank) { best_rank = result.tbRank; best.clear(); }
      if (result.tbRank == best_rank) best.push_back(legal);
      break;
    }
  }
  if (!best.empty()) {
    thread_info.root_moves = std::move(best);
    thread_info.root_moves_limited = true;
  }
}

inline void search_position(BoardState &position, ThreadInfo &thread_info,
                            std::vector<TTBucket> &table) {
  prepare_search_evaluator(position, thread_info, table);
  thread_info.position = position;
  thread_info.thread_id = 0;
  thread_info.nodes.store(0);
  thread_data.tb_hits = 0;
  thread_data.tb_fails = 0;
  if (!thread_info.root_moves_limited) {
    thread_info.root_moves.clear();
    std::array<Action, MaxActions> moves{};
    const int count = legal_movegen(position, moves.data());
    for (int i = 0; i < count; ++i) thread_info.root_moves.push_back({moves[i], 0});
  }
  filter_root_tablebase(position, thread_info);
  for (size_t i = 0; i < thread_data.thread_infos.size(); ++i) {
    thread_data.thread_infos[i] = thread_info;
    thread_data.thread_infos[i].thread_id = static_cast<uint16_t>(i + 1);
  }
  std::atomic<bool> start_workers{false};
  for (size_t i = 0; i < thread_data.thread_infos.size(); ++i) {
    try {
      thread_data.threads.emplace_back([i, &table, &start_workers] {
        start_workers.wait(false);
        auto &worker = thread_data.thread_infos[i];
        iterative_deepen(worker.position, worker, table);
      });
    } catch (const std::system_error &) {
      safe_printf("info string Could not start all requested threads\n");
      break;
    }
  }
  start_workers.store(true);
  start_workers.notify_all();
  iterative_deepen(position, thread_info, table);
  thread_data.stop = true;
  for (auto &worker : thread_data.threads) worker.join();
  thread_data.threads.clear();
  thread_info.searches = (thread_info.searches + 1) % MaxAge;
}

inline int analyze_sacrifice(BoardState &position, ThreadInfo &thread_info, int depth,
                            int ply, int sacrificer_color) {
  auto value = [&] {
    int score = (material_eval(position) * 3 + eval(position, thread_info)) / 4;
    return position.color == sacrificer_color ? score : -score;
  };
  const uint64_t budget = thread_info.opt_time / 100 * thread_info.sacrifice_lookahead_time_multiplier;
  if (depth <= 0 || ply >= 10 || thread_info.game_ply >= MaxGameLen - 2 ||
      thread_info.search_ply >= MaxSearchPly - 2 || out_of_time(thread_info) ||
      (!thread_info.infinite_search && !thread_data.pondering &&
       static_cast<uint64_t>(time_elapsed(thread_info.start_time)) > budget)) return value();
  ++thread_info.nodes;
  std::array<Action, MaxActions> moves{};
  const int count = legal_movegen(position, moves.data());
  if (!count) {
    if (!attacks_square(position, get_king_pos(position, position.color), position.color ^ 1)) return 0;
    return position.color == sacrificer_color ? -MateThreshold : MateThreshold;
  }
  const bool maximize = position.color == sacrificer_color;
  int best = maximize ? -MateScore : MateScore;
  for (int i = 0; i < count && i < 16; ++i) {
    if (thread_data.stop) break;
    BoardState child = position;
    make_move(child, moves[i]);
    update_nnue_state(thread_info, moves[i], position, child);
    ss_push(position, thread_info, moves[i]);
    const int score = analyze_sacrifice(child, thread_info, depth - 1, ply + 1, sacrificer_color);
    ss_pop(thread_info);
    best = maximize ? std::max(best, score) : std::min(best, score);
    if (thread_data.stop) break;
  }
  return best;
}

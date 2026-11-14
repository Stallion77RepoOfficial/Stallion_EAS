#pragma once

#include "bitboard.h"
#include "utils.h"

#include <algorithm>
#include <cctype>
#include <sstream>
#include <string>
#include <string_view>

namespace detail_pos {

inline uint64_t piece_key(int piece, int sq) noexcept {
  return zobrist_keys[get_zobrist_key(piece, sq)];
}

inline void revoke_castle_wing(BoardState &pos, int side_color, int wing,
                               uint64_t &hash_acc) noexcept {
  const int rook_sq = pos.castling_squares[side_color][wing];
  if (rook_sq == SquareNone) return;
  hash_acc ^= castling_key(side_color, wing, rook_sq);
  pos.castling_squares[side_color][wing] = SquareNone;
}

inline bool parse_unsigned_dec(std::string_view token, uint64_t &out) noexcept {
  if (token.empty()) return false;
  uint64_t acc = 0;
  for (char ch : token) {
    if (ch < '0' || ch > '9') return false;
    const uint64_t digit = static_cast<uint64_t>(ch - '0');
    if (acc > (UINT64_MAX - digit) / 10ULL) return false;
    acc = acc * 10ULL + digit;
  }
  out = acc;
  return true;
}

inline bool apply_fen_castling(BoardState &parsed, const std::string &rights) noexcept {
  if (rights == "-") return true;

  for (unsigned char raw_ch : rights) {
    const int stm = std::islower(raw_ch) ? Colors::Black : Colors::White;
    const char token = static_cast<char>(std::tolower(raw_ch));
    const int king_sq = get_lsb(parsed.colors_bb[stm] & parsed.pieces_bb[PieceTypes::King]);
    const int back_rank_base = 56 * stm;
    if (get_rank(king_sq) != 7 * stm) return false;

    int rook_sq = SquareNone;
    if (token >= 'a' && token <= 'h') {
      if (!thread_data.is_frc) return false;
      rook_sq = back_rank_base + (token - 'a');
    } else if (token == 'k' || token == 'q') {
      if (!thread_data.is_frc) {
        if (king_sq != back_rank_base + 4) return false;
        rook_sq = back_rank_base + (token == 'k' ? 7 : 0);
      } else {
        for (int sq = back_rank_base; sq < back_rank_base + 8; ++sq) {
          if (parsed.board[sq] != Pieces::WRook + stm) continue;
          if (token == 'k' && sq > king_sq) rook_sq = sq;
          if (token == 'q' && sq < king_sq && rook_sq == SquareNone) rook_sq = sq;
        }
      }
    } else {
      return false;
    }

    if (!is_valid_square(rook_sq) || parsed.board[rook_sq] != Pieces::WRook + stm ||
        rook_sq == king_sq) {
      return false;
    }
    if (!thread_data.is_frc &&
        (king_sq != back_rank_base + 4 ||
         (rook_sq != back_rank_base && rook_sq != back_rank_base + 7))) {
      return false;
    }

    const int wing = (rook_sq > king_sq) ? Sides::Kingside : Sides::Queenside;
    if (parsed.castling_squares[stm][wing] != SquareNone) return false;
    parsed.castling_squares[stm][wing] = rook_sq;
  }

  return true;
}

inline void execute_null_move(BoardState &pos) noexcept {
  if (pos.halfmoves < UINT16_MAX) ++pos.halfmoves;
  pos.zobrist_key ^= ep_key(pos);
  pos.ep_square = SquareNone;
  pos.color ^= 1;
  pos.zobrist_key ^= zobrist_keys[side_index];
}

inline void execute_castle_move(BoardState &pos, int stm, int king_from,
                                int rook_from) noexcept {
  const int wing = (rook_from > king_from) ? Sides::Kingside : Sides::Queenside;
  const int king_dest = 56 * stm + 2 + 4 * wing;
  const int rook_dest = 56 * stm + 3 + 2 * wing;
  const uint64_t cleared_pair = (1ULL << king_from) | (1ULL << rook_from);

  pos.board[king_from] = Pieces::Blank;
  pos.board[rook_from] = Pieces::Blank;
  pos.board[king_dest] = Pieces::WKing + stm;
  pos.board[rook_dest] = Pieces::WRook + stm;

  pos.colors_bb[stm] =
      (pos.colors_bb[stm] & ~cleared_pair) | (1ULL << king_dest) | (1ULL << rook_dest);
  pos.pieces_bb[PieceTypes::King] =
      (pos.pieces_bb[PieceTypes::King] & ~(1ULL << king_from)) | (1ULL << king_dest);
  pos.pieces_bb[PieceTypes::Rook] =
      (pos.pieces_bb[PieceTypes::Rook] & ~(1ULL << rook_from)) | (1ULL << rook_dest);

  pos.castling_squares[stm].fill(SquareNone);
  pos.ep_square = SquareNone;
  pos.color ^= 1;
  calculate(pos);
}

}  // namespace detail_pos

inline std::string internal_to_uci(const BoardState &, Action move) {
  if (move == MoveNone) return "0000";

  const int src = extract_from(move);
  int dst = extract_to(move);
  if (!is_valid_square(src) || !is_valid_square(dst)) return "0000";

  const int kind = extract_type(move);
  if (kind == MoveTypes::Castling && !thread_data.is_frc) {
    dst += (get_file(dst) == 0) ? 2 : -1;
  }

  std::string text;
  text.reserve(5);
  text.push_back(static_cast<char>('a' + get_file(src)));
  text.push_back(static_cast<char>('1' + get_rank(src)));
  text.push_back(static_cast<char>('a' + get_file(dst)));
  text.push_back(static_cast<char>('1' + get_rank(dst)));

  if (kind == MoveTypes::Promotion) {
    text.push_back("nbrq"[extract_promo(move)]);
  }
  return text;
}

inline int get_king_pos(const BoardState &pos, int side_color) noexcept {
  return get_lsb(pos.colors_bb[side_color] & pos.pieces_bb[PieceTypes::King]);
}

inline void print_board(const BoardState &pos) {
  constexpr std::string_view kGlyphs = "  PpNnBbRrQqKk";
  constexpr std::string_view kSep = "+---+---+---+---+---+---+---+---+\n";

  std::string buf;
  buf.reserve(512);
  for (int row = 7; row >= 0; --row) {
    buf += kSep;
    for (int col = 0; col < 8; ++col) {
      const int pc = pos.board[row * 8 + col];
      buf += "| ";
      if (pc >= Pieces::WPawn && pc <= Pieces::BKing) {
        buf.push_back(kGlyphs[pc]);
        buf.push_back(' ');
      } else if (pc == Pieces::Blank) {
        buf += "  ";
      } else {
        buf += "# ";
      }
    }
    buf += "|\n";
  }
  buf += kSep;
  buf.push_back('\n');
  safe_printf("%s", buf.c_str());
}

inline uint64_t attacks_square(const BoardState &pos, int sq, int color) noexcept {
  return attackers_to(pos, sq, color, pos.colors_bb[0] | pos.colors_bb[1]);
}

inline uint64_t attacks_square(const BoardState &pos, int sq, int color,
                               uint64_t occ) noexcept {
  return attackers_to(pos, sq, color, occ) & occ;
}

inline bool set_board(BoardState &position, ThreadInfo &thread_info,
                      const std::string &fen_str) {
  std::istringstream parser(fen_str);
  std::string placement, turn_str, castle_str, ep_str;
  std::string half_str = "0", full_str = "1", trailing;
  if (!(parser >> placement >> turn_str >> castle_str >> ep_str)) return false;
  if (parser >> half_str) {
    if (!(parser >> full_str) || (parser >> trailing)) return false;
  }

  BoardState parsed{};
  parsed.ep_square = SquareNone;
  for (auto &row : parsed.castling_squares) row.fill(SquareNone);

  constexpr std::string_view kPieceChars = "PpNnBbRrQqKk";
  int rank = 7;
  int file = 0;
  for (char ch : placement) {
    if (ch == '/') {
      if (file != 8 || rank == 0) return false;
      --rank;
      file = 0;
      continue;
    }
    if (ch >= '1' && ch <= '8') {
      file += ch - '0';
      if (file > 8) return false;
      continue;
    }

    const size_t idx = kPieceChars.find(ch);
    if (idx == std::string_view::npos || file >= 8) return false;

    const int pc = static_cast<int>(idx) + 2;
    const int sq = rank * 8 + file++;
    parsed.board[sq] = pc;
    parsed.colors_bb[get_color(pc)] |= 1ULL << sq;
    parsed.pieces_bb[get_piece_type(pc)] |= 1ULL << sq;
    if (get_piece_type(pc) != PieceTypes::King) ++parsed.material_count[pc - 2];
  }

  if (rank != 0 || file != 8 || (turn_str != "w" && turn_str != "b")) return false;
  parsed.color = (turn_str == "b") ? Colors::Black : Colors::White;

  for (int side = 0; side < 2; ++side) {
    if (pop_count(parsed.colors_bb[side] & parsed.pieces_bb[PieceTypes::King]) != 1) {
      return false;
    }
  }
  if (parsed.pieces_bb[PieceTypes::Pawn] & (Ranks[0] | Ranks[7])) return false;
  if (attacks_square(parsed, get_king_pos(parsed, parsed.color ^ 1), parsed.color)) {
    return false;
  }

  if (!detail_pos::apply_fen_castling(parsed, castle_str)) return false;

  if (ep_str != "-") {
    if (ep_str.size() != 2 || ep_str[0] < 'a' || ep_str[0] > 'h' ||
        ep_str[1] != (parsed.color ? '3' : '6')) {
      return false;
    }
    parsed.ep_square = (ep_str[1] - '1') * 8 + (ep_str[0] - 'a');
    const int victim_sq = parsed.ep_square + (parsed.color ? 8 : -8);
    const int push_origin = parsed.ep_square + (parsed.color ? -8 : 8);
    if (parsed.board[parsed.ep_square] || parsed.board[push_origin] ||
        parsed.board[victim_sq] != Pieces::WPawn + (parsed.color ^ 1)) {
      return false;
    }
  }

  uint64_t half_val = 0;
  uint64_t full_val = 1;
  if (!detail_pos::parse_unsigned_dec(half_str, half_val) ||
      !detail_pos::parse_unsigned_dec(full_str, full_val) ||
      full_val == 0 || full_val > UINT32_MAX) {
    return false;
  }

  parsed.halfmoves = static_cast<uint16_t>(std::min<uint64_t>(half_val, UINT16_MAX));
  parsed.fullmove = static_cast<uint32_t>(full_val);
  calculate(parsed);

  position = parsed;
  thread_info.game_ply = 0;
  thread_info.search_ply = 0;
  thread_info.game_hist.fill({});
  return true;
}

inline std::string export_fen(const BoardState &pos, const ThreadInfo &) {
  constexpr std::string_view kGlyphs = "  PpNnBbRrQqKk";
  std::string out;
  out.reserve(80);

  for (int row = 7; row >= 0; --row) {
    int blanks = 0;
    for (int col = 0; col < 8; ++col) {
      const int pc = pos.board[row * 8 + col];
      if (!pc) {
        ++blanks;
        continue;
      }
      if (blanks) {
        out.push_back(static_cast<char>('0' + blanks));
        blanks = 0;
      }
      out.push_back(kGlyphs[pc]);
    }
    if (blanks) out.push_back(static_cast<char>('0' + blanks));
    if (row) out.push_back('/');
  }

  out += pos.color ? " b " : " w ";

  std::string castle_text;
  for (int side = 0; side < 2; ++side) {
    for (int wing : {Sides::Kingside, Sides::Queenside}) {
      const int rook_sq = pos.castling_squares[side][wing];
      if (rook_sq == SquareNone) continue;
      if (thread_data.is_frc) {
        castle_text.push_back(static_cast<char>((side ? 'a' : 'A') + get_file(rook_sq)));
      } else {
        const char ch = (wing == Sides::Kingside) ? 'k' : 'q';
        castle_text.push_back(side ? ch : static_cast<char>(std::toupper(ch)));
      }
    }
  }
  out += castle_text.empty() ? "-" : castle_text;
  out.push_back(' ');

  if (pos.ep_square == SquareNone) {
    out.push_back('-');
  } else {
    out.push_back(static_cast<char>('a' + get_file(pos.ep_square)));
    out.push_back(static_cast<char>('1' + get_rank(pos.ep_square)));
  }

  out += ' ';
  out += std::to_string(pos.halfmoves);
  out += ' ';
  out += std::to_string(pos.fullmove);
  return out;
}

constexpr inline bool is_cap(const BoardState &pos, Action move) noexcept {
  if (move == MoveNone || extract_type(move) == MoveTypes::Castling) return false;
  const int dst = extract_to(move);
  const int src = extract_from(move);
  if (!is_valid_square(dst) || !is_valid_square(src)) return false;

  if (pos.board[dst] != Pieces::Blank) return true;
  if (extract_type(move) == MoveTypes::Promotion) return true;
  return dst == pos.ep_square && pos.board[src] == Pieces::WPawn + pos.color;
}

inline bool can_castle(const BoardState &pos, int king_sq, int rook_sq) noexcept {
  const int stm = pos.color;
  const int wing = (rook_sq > king_sq) ? Sides::Kingside : Sides::Queenside;
  if (pos.board[king_sq] != Pieces::WKing + stm ||
      pos.castling_squares[stm][wing] != rook_sq ||
      !is_valid_square(rook_sq) || pos.board[rook_sq] != Pieces::WRook + stm) {
    return false;
  }

  const int king_dest = 56 * stm + 2 + 4 * wing;
  const int rook_dest = 56 * stm + 3 + 2 * wing;
  const uint64_t occ = pos.colors_bb[0] | pos.colors_bb[1];
  const uint64_t without_pair = occ & ~(1ULL << king_sq) & ~(1ULL << rook_sq);

  if (without_pair & (BetweenBBs[king_sq][king_dest] | BetweenBBs[rook_sq][rook_dest])) {
    return false;
  }
  if (attacks_square(pos, king_sq, stm ^ 1)) return false;

  if (king_sq != king_dest) {
    const int delta = (king_dest > king_sq) ? 1 : -1;
    const uint64_t no_king = occ & ~(1ULL << king_sq);
    for (int sq = king_sq + delta; sq != king_dest; sq += delta) {
      if (attacks_square(pos, sq, stm ^ 1, no_king)) return false;
    }
  }

  return !attacks_square(pos, king_dest, stm ^ 1, without_pair | (1ULL << rook_dest));
}

inline void make_move(BoardState &pos, Action move) noexcept {
  if (move == MoveNone) {
    detail_pos::execute_null_move(pos);
    return;
  }

  const int src = extract_from(move);
  const int dst = extract_to(move);
  const int moved_pc = pos.board[src];
  const int stm = pos.color;
  const int kind = extract_type(move);
  const int moved_type = get_piece_type(moved_pc);

  if (moved_pc == Pieces::Blank || get_color(moved_pc) != stm) std::exit(EXIT_FAILURE);
  if (kind != MoveTypes::Castling &&
      (src == dst || (pos.board[dst] && get_color(pos.board[dst]) == stm) ||
       pos.board[dst] == Pieces::WKing + (stm ^ 1))) {
    std::exit(EXIT_FAILURE);
  }
  if (kind == MoveTypes::Castling && !can_castle(pos, src, dst)) std::exit(EXIT_FAILURE);
  if (kind == MoveTypes::Promotion &&
      (moved_type != PieceTypes::Pawn || get_rank(dst) != (stm ? 0 : 7))) {
    std::exit(EXIT_FAILURE);
  }
  if (moved_type == PieceTypes::Pawn && kind != MoveTypes::Promotion &&
      get_rank(dst) == (stm ? 0 : 7)) {
    std::exit(EXIT_FAILURE);
  }
  if (kind == MoveTypes::EnPassant) {
    const int ep_victim = dst + (stm ? Directions::North : Directions::South);
    if (dst != pos.ep_square || pos.board[dst] != Pieces::Blank ||
        !is_valid_square(ep_victim) ||
        pos.board[ep_victim] != Pieces::WPawn + (stm ^ 1)) {
      std::exit(EXIT_FAILURE);
    }
  }

  if (pos.halfmoves < UINT16_MAX) ++pos.halfmoves;
  if (stm && pos.fullmove < UINT32_MAX) ++pos.fullmove;

  if (kind == MoveTypes::Castling) {
    detail_pos::execute_castle_move(pos, stm, src, dst);
    return;
  }

  uint64_t next_hash = pos.zobrist_key ^ ep_key(pos);
  uint64_t next_pawn_hash = pos.pawn_key;
  const int opp = stm ^ 1;
  const int own_king = get_king_pos(pos, stm);

  int captured_pc = Pieces::Blank;
  int captured_sq = SquareNone;
  int next_ep = SquareNone;

  if (pos.board[dst] != Pieces::Blank) {
    pos.halfmoves = 0;
    captured_pc = pos.board[dst];
    captured_sq = dst;
    --pos.material_count[captured_pc - 2];

    const uint64_t cap_zob = detail_pos::piece_key(captured_pc, captured_sq);
    next_hash ^= cap_zob;
    if (get_piece_type(captured_pc) == PieceTypes::Pawn) {
      next_pawn_hash ^= cap_zob;
    } else {
      pos.non_pawn_key[opp] ^= cap_zob;
    }
  } else if (kind == MoveTypes::EnPassant) {
    --pos.material_count[opp];
    captured_sq = dst + (stm ? Directions::North : Directions::South);
    captured_pc = pos.board[captured_sq];

    const uint64_t cap_zob = detail_pos::piece_key(captured_pc, captured_sq);
    next_hash ^= cap_zob;
    next_pawn_hash ^= cap_zob;
    pos.board[captured_sq] = Pieces::Blank;
  }

  pos.board[src] = Pieces::Blank;
  int placed_pc = moved_pc;

  if (moved_type == PieceTypes::Pawn) {
    pos.halfmoves = 0;
    if (kind == MoveTypes::Promotion) {
      placed_pc = extract_promo(move) * 2 + 4 + stm;
      --pos.material_count[stm];
      ++pos.material_count[placed_pc - 2];
    } else if (dst == src + Directions::North * 2 ||
               dst == src + Directions::South * 2) {
      next_ep = (dst + src) >> 1;
    }
  } else if (moved_type == PieceTypes::King) {
    detail_pos::revoke_castle_wing(pos, stm, Sides::Queenside, next_hash);
    detail_pos::revoke_castle_wing(pos, stm, Sides::Kingside, next_hash);
  }

  pos.board[dst] = placed_pc;

  if (src == pos.castling_squares[stm][Sides::Queenside] ||
      src == pos.castling_squares[stm][Sides::Kingside]) {
    const int wing = (src < own_king) ? Sides::Queenside : Sides::Kingside;
    detail_pos::revoke_castle_wing(pos, stm, wing, next_hash);
  }

  if (dst == pos.castling_squares[opp][Sides::Queenside] ||
      dst == pos.castling_squares[opp][Sides::Kingside]) {
    const int wing =
        (dst < get_king_pos(pos, opp)) ? Sides::Queenside : Sides::Kingside;
    detail_pos::revoke_castle_wing(pos, opp, wing, next_hash);
  }

  const uint64_t src_zob = detail_pos::piece_key(moved_pc, src);
  const uint64_t dst_zob = detail_pos::piece_key(placed_pc, dst);
  next_hash ^= src_zob ^ dst_zob ^ zobrist_keys[side_index];

  if (moved_type == PieceTypes::Pawn) {
    next_pawn_hash ^= src_zob;
    if (get_piece_type(placed_pc) == PieceTypes::Pawn) {
      next_pawn_hash ^= dst_zob;
    } else {
      pos.non_pawn_key[stm] ^= dst_zob;
    }
  } else {
    pos.non_pawn_key[stm] ^= src_zob ^ dst_zob;
  }

  update_bb(pos, moved_pc, src, placed_pc, dst, captured_pc, captured_sq);

  pos.color = opp;
  pos.ep_square = next_ep;
  next_hash ^= ep_key(pos);
  pos.zobrist_key = next_hash;
  pos.pawn_key = next_pawn_hash;

  safe_TT_prefetch(next_hash);
}

inline bool is_pseudo_legal(const BoardState &pos, Action move,
                            uint64_t checkers) noexcept {
  if (move == MoveNone) return false;
  const int src = extract_from(move);
  const int dst = extract_to(move);
  if (!is_valid_square(src) || !is_valid_square(dst)) return false;

  const int stm = pos.color;
  const int kind = extract_type(move);
  const uint64_t own_occ = pos.colors_bb[stm];
  if (kind != MoveTypes::Castling && ((1ULL << dst) & own_occ)) return false;

  const int pc = pos.board[src];
  if (pc == Pieces::Blank || get_color(pc) != stm) return false;
  const int ptype = get_piece_type(pc);

  // Double check permits only a non-castling king step.
  if (checkers & (checkers - 1)) {
    return kind == MoveTypes::Normal && ptype == PieceTypes::King &&
           (KING_ATK_SAFE(src) & (1ULL << dst));
  }

  if (kind == MoveTypes::Castling) return !checkers && can_castle(pos, src, dst);

  if (kind == MoveTypes::EnPassant) {
    if (pos.ep_square == SquareNone || !is_valid_square(pos.ep_square)) return false;
    const int ep_victim = dst + (stm ? Directions::North : Directions::South);
    return dst == pos.ep_square && ptype == PieceTypes::Pawn &&
           (PAWN_ATK_SAFE(stm, src) & (1ULL << dst)) &&
           is_valid_square(ep_victim) &&
           pos.board[ep_victim] == Pieces::WPawn + (stm ^ 1);
  }

  if (kind == MoveTypes::Promotion &&
      (ptype != PieceTypes::Pawn || get_rank(dst) != (stm ? 0 : 7))) {
    return false;
  }

  if (ptype == PieceTypes::King) return (KING_ATK_SAFE(src) & (1ULL << dst)) != 0;

  if (checkers) {
    const int king_sq = get_king_pos(pos, stm);
    if (!is_valid_square(king_sq)) return false;
    const int checker_sq = get_lsb(checkers);
    const uint64_t evasion_mask = BetweenBBs[king_sq][checker_sq] | (1ULL << checker_sq);
    if (!(evasion_mask & (1ULL << dst))) return false;
  }

  const uint64_t occ = pos.colors_bb[0] | pos.colors_bb[1];
  if (ptype == PieceTypes::Pawn) {
    const int push_dir = (stm == Colors::White) ? Directions::North : Directions::South;
    const uint64_t third_rank = (stm == Colors::White) ? Ranks[2] : Ranks[5];
    const uint64_t empty = ~occ;

    const uint64_t step1 = shift_pawns(1ULL << src, push_dir) & empty;
    const uint64_t step2 = shift_pawns(step1 & third_rank, push_dir) & empty;
    const uint64_t caps = PAWN_ATK_SAFE(stm, src) & pos.colors_bb[stm ^ 1];
    uint64_t reach = step1 | step2 | caps;
    if (kind != MoveTypes::Promotion) reach &= ~(Ranks[0] | Ranks[7]);
    return ((reach >> dst) & 1ULL) != 0;
  }

  uint64_t atk = 0;
  switch (ptype) {
    case PieceTypes::Knight:
      atk = KNIGHT_ATK_SAFE(src);
      break;
    case PieceTypes::Bishop:
      atk = get_bishop_attacks(src, occ);
      break;
    case PieceTypes::Rook:
      atk = get_rook_attacks(src, occ);
      break;
    case PieceTypes::Queen:
      atk = get_bishop_attacks(src, occ) | get_rook_attacks(src, occ);
      break;
    default:
      break;
  }
  return (atk & (1ULL << dst)) != 0;
}

inline bool is_legal(const BoardState &pos, Action move,
                     uint64_t checkers = ~0ULL) noexcept {
  if (move == MoveNone) return false;
  const int src = extract_from(move);
  const int dst = extract_to(move);
  if (!is_valid_square(src) || !is_valid_square(dst)) return false;

  const int stm = pos.color;
  const int pc = pos.board[src];
  if (!pc || get_color(pc) != stm) return false;

  const int kind = extract_type(move);
  if (kind == MoveTypes::Castling) return can_castle(pos, src, dst);
  if (pos.board[dst] == Pieces::WKing + (stm ^ 1) ||
      (pos.board[dst] && get_color(pos.board[dst]) == stm)) {
    return false;
  }

  const int ptype = get_piece_type(pc);
  const int king_sq = (ptype == PieceTypes::King) ? dst : get_king_pos(pos, stm);
  if (!is_valid_square(king_sq)) return false;

  if (!checkers && ptype != PieceTypes::King && kind != MoveTypes::EnPassant &&
      !((BishopAttacks[king_sq][0] | RookAttacks[king_sq][0]) & (1ULL << src))) {
    return true;
  }

  uint64_t occ = ((pos.colors_bb[0] | pos.colors_bb[1]) & ~(1ULL << src)) | (1ULL << dst);
  if (kind == MoveTypes::EnPassant) {
    const int ep_victim = dst + (stm ? Directions::North : Directions::South);
    if (!is_valid_square(ep_victim) ||
        pos.board[ep_victim] != Pieces::WPawn + (stm ^ 1)) {
      return false;
    }
    occ &= ~(1ULL << ep_victim);
  }

  return !(attacks_square(pos, king_sq, stm ^ 1, occ) & ~(1ULL << dst));
}

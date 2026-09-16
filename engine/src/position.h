#pragma once
#include "bitboard.h"
#include "utils.h"
#include <cctype>
#include <sstream>
#include <string_view>

struct ThreadInfo;

constexpr inline int16_t total_mat(const BoardState &position) noexcept {
  const int m = (position.material_count[0] + position.material_count[1]) *
                    MaterialValues[PieceTypes::Pawn] +
                (position.material_count[2] + position.material_count[3]) *
                    MaterialValues[PieceTypes::Knight] +
                (position.material_count[4] + position.material_count[5]) *
                    MaterialValues[PieceTypes::Bishop] +
                (position.material_count[6] + position.material_count[7]) *
                    MaterialValues[PieceTypes::Rook] +
                (position.material_count[8] + position.material_count[9]) *
                    MaterialValues[PieceTypes::Queen];

  return static_cast<int16_t>(m);
}

inline std::string internal_to_uci(const BoardState &, Action move) {
  if (move == MoveNone) return "0000";
  const int from = extract_from(move);
  int to = extract_to(move);
  const int promo = extract_promo(move);

  if (!is_valid_square(from) || !is_valid_square(to)) {
    return "0000";
  }

  if (extract_type(move) == MoveTypes::Castling && !thread_data.is_frc) {
    if (get_file(to) == 0) {
      to += 2;
    } else {
      to--;
    }
  }

  std::string uci;
  uci.reserve(5);
  uci += static_cast<char>(get_file(from) + 'a');
  uci += static_cast<char>(get_rank(from) + '1');
  uci += static_cast<char>(get_file(to) + 'a');
  uci += static_cast<char>(get_rank(to) + '1');

  if (extract_type(move) == MoveTypes::Promotion) {
    uci += "nbrq"[promo];
  }

  return uci;
}

inline int get_king_pos(const BoardState &position, int color) noexcept {
  return get_lsb(position.colors_bb[color] &
                 position.pieces_bb[PieceTypes::King]);
}

inline void print_board(const BoardState &position) {
  std::string out;
  out.reserve(512);
  for (int i = 56; i >= 0; i -= 8) {
    out += "+---+---+---+---+---+---+---+---+\n";
    for (int n = i; n != i + 8; n++) {
      out += "| ";
      if (position.board[n] == Pieces::Blank)
        out += "  ";
      else if (position.board[n] >= Pieces::WPawn &&
               position.board[n] <= Pieces::BKing) {
        static constexpr std::string_view piece_display = "P p N n B b R r Q q K k ";
        out.append(piece_display.data() + (position.board[n] - 2) * 2, 2);
      } else
        out += "# ";
    }
    out += "|\n";
  }
  out += "+---+---+---+---+---+---+---+---+\n\n";
  safe_printf("%s", out.c_str());
}

inline uint64_t attacks_square(const BoardState &position, int sq, int color) noexcept;

inline bool set_board(BoardState &position, ThreadInfo &thread_info, const std::string &f) {
  std::istringstream fen(f);
  std::string placement, turn, rights, ep, half = "0", full = "1", extra;
  if (!(fen >> placement >> turn >> rights >> ep)) return false;
  if (fen >> half) {
    if (!(fen >> full) || (fen >> extra)) return false;
  }
  BoardState parsed{};
  parsed.ep_square = SquareNone;
  for (auto &row : parsed.castling_squares) row.fill(SquareNone);
  int rank = 7, file = 0;
  constexpr std::string_view symbols = "PpNnBbRrQqKk";
  for (char c : placement) {
    if (c == '/') {
      if (file != 8 || rank == 0) return false;
      --rank; file = 0;
    } else if (c >= '1' && c <= '8') {
      file += c - '0';
      if (file > 8) return false;
    } else {
      const auto index = symbols.find(c);
      if (index == std::string_view::npos || file >= 8) return false;
      const int piece = static_cast<int>(index) + 2;
      const int sq = rank * 8 + file++;
      parsed.board[sq] = piece;
      parsed.colors_bb[get_color(piece)] |= 1ULL << sq;
      parsed.pieces_bb[get_piece_type(piece)] |= 1ULL << sq;
      if (get_piece_type(piece) != PieceTypes::King) ++parsed.material_count[piece - 2];
    }
  }
  if (rank != 0 || file != 8 || (turn != "w" && turn != "b")) return false;
  parsed.color = (turn == "b") ? Colors::Black : Colors::White;
  for (int c = 0; c < 2; ++c) {
    if (pop_count(parsed.colors_bb[c] & parsed.pieces_bb[PieceTypes::King]) != 1 ||
        pop_count(parsed.colors_bb[c]) > 16 || parsed.material_count[c] > 8) return false;
  }
  if (parsed.pieces_bb[PieceTypes::Pawn] & (Ranks[0] | Ranks[7])) return false;
  if (attacks_square(parsed, get_king_pos(parsed, parsed.color ^ 1), parsed.color)) return false;
  if (rights != "-") {
    for (unsigned char symbol : rights) {
      const int c = std::islower(symbol) ? Colors::Black : Colors::White;
      const char right = static_cast<char>(std::tolower(symbol));
      const int king = get_king_pos(parsed, c), base = 56 * c;
      if (get_rank(king) != 7 * c) return false;
      int rook = SquareNone;

      if (right >= 'a' && right <= 'h') {
        if (!thread_data.is_frc) return false;
        rook = base + right - 'a';
      }
      else if (right == 'k' || right == 'q') {
        if (!thread_data.is_frc) {
          if (king != base + 4) return false;
          rook = base + (right == 'k' ? 7 : 0);
        } else {

          for (int sq = base; sq < base + 8; ++sq) {
            if (parsed.board[sq] != Pieces::WRook + c) continue;
            if (right == 'k' && sq > king) rook = sq;
            if (right == 'q' && sq < king && rook == SquareNone) rook = sq;
          }
        }
      } else return false;
      if (!is_valid_square(rook) || parsed.board[rook] != Pieces::WRook + c || rook == king) return false;
      if (!thread_data.is_frc && (king != base + 4 || (rook != base && rook != base + 7))) return false;
      const int side = rook > king ? Sides::Kingside : Sides::Queenside;
      if (parsed.castling_squares[c][side] != SquareNone) return false;
      parsed.castling_squares[c][side] = rook;
    }
  }
  if (ep != "-") {
    if (ep.size() != 2 || ep[0] < 'a' || ep[0] > 'h' || ep[1] != (parsed.color ? '3' : '6')) return false;
    parsed.ep_square = (ep[1] - '1') * 8 + ep[0] - 'a';
    const int captured = parsed.ep_square + (parsed.color ? 8 : -8);
    const int origin = parsed.ep_square + (parsed.color ? -8 : 8);
    if (parsed.board[parsed.ep_square] || parsed.board[origin] ||
        parsed.board[captured] != Pieces::WPawn + (parsed.color ^ 1)) return false;
  }
  auto number = [](const std::string &text, uint64_t &out) noexcept {
    if (text.empty()) return false;
    uint64_t val = 0;
    for (char c : text) {
      if (c < '0' || c > '9') return false;
      if (val > (UINT64_MAX - static_cast<uint64_t>(c - '0')) / 10) return false;
      val = val * 10 + static_cast<uint64_t>(c - '0');
    }
    out = val;
    return true;
  };
  uint64_t halfmoves = 0, fullmove = 1;
  if (!number(half, halfmoves) || !number(full, fullmove) || fullmove == 0 || fullmove > UINT32_MAX) return false;
  parsed.halfmoves = static_cast<uint16_t>(std::min<uint64_t>(halfmoves, UINT16_MAX));
  parsed.fullmove = static_cast<uint32_t>(fullmove);
  calculate(parsed);
  position = parsed;
  thread_info.game_ply = thread_info.search_ply = 0;
  thread_info.game_hist.fill({});
  return true;
}

inline std::string export_fen(const BoardState &position, const ThreadInfo &) {
  std::string fen;
  fen.reserve(80);
  constexpr std::string_view pieces = "  PpNnBbRrQqKk";
  for (int rank = 7; rank >= 0; --rank) {
    int empty = 0;
    for (int file = 0; file < 8; ++file) {
      const int piece = position.board[rank * 8 + file];
      if (!piece) { ++empty; continue; }
      if (empty) { fen += char('0' + empty); empty = 0; }
      fen += pieces[piece];
    }
    if (empty) fen += char('0' + empty);
    if (rank) fen += '/';
  }
  fen += position.color ? " b " : " w ";
  std::string rights;
  for (int c = 0; c < 2; ++c) {
    for (int side : {Sides::Kingside, Sides::Queenside}) {
      const int rook = position.castling_squares[c][side];
      if (rook == SquareNone) continue;
      rights += thread_data.is_frc ? char((c ? 'a' : 'A') + get_file(rook))
                                  : (c ? (side ? 'k' : 'q') : (side ? 'K' : 'Q'));
    }
  }
  fen += rights.empty() ? "- " : rights + " ";
  if (position.ep_square == SquareNone) fen += "-";
  else {
    fen += char('a' + get_file(position.ep_square));
    fen += char('1' + get_rank(position.ep_square));
  }
  return fen + " " + std::to_string(position.halfmoves) + " " + std::to_string(position.fullmove);
}

inline uint64_t attacks_square(const BoardState &position, int sq, int color) noexcept {
  return attackers_to(position, sq, color,
                      position.colors_bb[0] | position.colors_bb[1]);
}

inline uint64_t attacks_square(const BoardState &position, int sq, int color,
                               uint64_t occ) noexcept {
  return attackers_to(position, sq, color, occ) & occ;
}

inline uint64_t attacks_square(const BoardState &position, int sq, uint64_t occ) noexcept {

  if (!is_valid_square(sq))
    return 0ULL;

  uint64_t bishops = position.pieces_bb[PieceTypes::Bishop] |
                     position.pieces_bb[PieceTypes::Queen];
  uint64_t rooks = position.pieces_bb[PieceTypes::Rook] |
                   position.pieces_bb[PieceTypes::Queen];

  return (PAWN_ATK_SAFE(Colors::White, sq) & position.colors_bb[Colors::Black] &
          position.pieces_bb[PieceTypes::Pawn]) |
         (PAWN_ATK_SAFE(Colors::Black, sq) & position.colors_bb[Colors::White] &
          position.pieces_bb[PieceTypes::Pawn]) |
         (KNIGHT_ATK_SAFE(sq) & position.pieces_bb[PieceTypes::Knight]) |
         (get_bishop_attacks(sq, occ) & bishops) |
         (get_rook_attacks(sq, occ) & rooks) |
         (KING_ATK_SAFE(sq) & position.pieces_bb[PieceTypes::King]);
}

constexpr inline bool is_cap(const BoardState &position, Action move) noexcept {
  if (move == MoveNone || extract_type(move) == MoveTypes::Castling) {
    return false;
  }
  const int to = extract_to(move);
  if (!is_valid_square(to)) {
    return false;
  }
  const int from_sq = extract_from(move);
  if (!is_valid_square(from_sq)) {
    return false;
  }
  return (position.board[to] ||
          (to == position.ep_square &&
           position.board[from_sq] == Pieces::WPawn + position.color) ||
          extract_type(move) == MoveTypes::Promotion);
}

inline void update_nnue_state(ThreadInfo &thread_info, Action move,
                              const BoardState &position, const BoardState & /*new_position*/) noexcept {
  if (!use_nnue || !nnue_loaded) return;

  if (move == MoveNone) {
    thread_info.nnue_state.push_null();
    return;
  }

  int from = extract_from(move), to = extract_to(move);
  if (!is_valid_square(from) || !is_valid_square(to)) return;

  const int from_piece = position.board[from];
  if (from_piece == Pieces::Blank) return;
  int to_piece = from_piece;
  const int color = position.color;

  if (extract_type(move) == MoveTypes::Promotion) {
    to_piece = (extract_promo(move) + 2) * 2 + color;
  }

  int captured_piece = Pieces::Blank, captured_square = SquareNone;
  if (position.board[to]) {
    captured_piece = position.board[to];
    captured_square = to;
  } else if (extract_type(move) == MoveTypes::EnPassant) {
    captured_square = to + (color ? Directions::North : Directions::South);
    if (is_valid_square(captured_square)) {
      captured_piece = position.board[captured_square];
    }
  }

  if (extract_type(move) == MoveTypes::Castling) {
    const int indx = color ? 56 : 0;
    const int side = to > from ? Sides::Kingside : Sides::Queenside;
    if (side == Sides::Kingside) {
      to = indx + 6;
      thread_info.nnue_state.add_add_sub_sub(
          from_piece, from, to, Pieces::WRook + color,
          position.castling_squares[color][side], indx + 5);
    } else {
      to = indx + 2;
      thread_info.nnue_state.add_add_sub_sub(
          from_piece, from, to, Pieces::WRook + color,
          position.castling_squares[color][side], indx + 3);
    }
  } else if (captured_piece != Pieces::Blank && is_valid_square(captured_square)) {
    thread_info.nnue_state.add_sub_sub(from_piece, from, to_piece, to, captured_piece, captured_square);
  } else {
    thread_info.nnue_state.add_sub(from_piece, from, to_piece, to);
  }
}

inline bool can_castle(const BoardState &position, int from, int rook) noexcept {
  const int color = position.color;
  const int side = rook > from ? Sides::Kingside : Sides::Queenside;
  if (position.board[from] != Pieces::WKing + color ||
      position.castling_squares[color][side] != rook ||
      !is_valid_square(rook) || position.board[rook] != Pieces::WRook + color) return false;
  const int king_to = 56 * color + 2 + 4 * side;
  const int rook_to = 56 * color + 3 + 2 * side;
  const uint64_t occupied = position.colors_bb[0] | position.colors_bb[1];
  const uint64_t cleared = occupied & ~(1ULL << from) & ~(1ULL << rook);
  if (cleared & (BetweenBBs[from][king_to] | BetweenBBs[rook][rook_to])) return false;
  if (attacks_square(position, from, color ^ 1)) return false;
  if (from != king_to) {
    const int step = king_to > from ? 1 : -1;
    for (int sq = from + step; sq != king_to; sq += step) {
      if (attacks_square(position, sq, color ^ 1, occupied & ~(1ULL << from))) return false;
    }
  }

  return !attacks_square(position, king_to, color ^ 1, cleared | (1ULL << rook_to));
}

inline void make_move(BoardState &position, Action move) {
  if (move == MoveNone) {
    if (position.halfmoves < UINT16_MAX) ++position.halfmoves;
    position.zobrist_key ^= ep_key(position);
    position.ep_square = SquareNone;
    position.color ^= 1;

    position.zobrist_key ^= zobrist_keys[side_index];
    return;
  }

  const int from = extract_from(move), to = extract_to(move);
  if (!is_valid_square(from) || !is_valid_square(to)) {
    return;
  }

  const int from_piece = position.board[from];
  if (from_piece == Pieces::Blank || get_color(from_piece) != position.color) {
    return;
  }

  const int color = position.color;
  const int move_type = extract_type(move);
  const int from_type = get_piece_type(from_piece);
  if (move_type != MoveTypes::Castling &&
      (from == to || (position.board[to] && get_color(position.board[to]) == color))) return;
  if (move_type == MoveTypes::Castling && !can_castle(position, from, to)) return;
  if (move_type == MoveTypes::Promotion &&
      (from_type != PieceTypes::Pawn || get_rank(to) != (color ? 0 : 7))) return;
  if (from_type == PieceTypes::Pawn && move_type != MoveTypes::Promotion &&
      get_rank(to) == (color ? 0 : 7)) return;
  if (move_type == MoveTypes::EnPassant) {
    const int captured = to + (color ? Directions::North : Directions::South);
    if (to != position.ep_square ||
        position.board[to] != Pieces::Blank || !is_valid_square(captured) ||
        position.board[captured] != Pieces::WPawn + (color ^ 1)) return;
  }
  if (position.halfmoves < UINT16_MAX) ++position.halfmoves;
  if (color && position.fullmove < UINT32_MAX) ++position.fullmove;

  if (move_type == MoveTypes::Castling) {
    const int side = to > from ? Sides::Kingside : Sides::Queenside;
    const int king_to = 56 * color + 2 + 4 * side;
    const int rook_to = 56 * color + 3 + 2 * side;
    const uint64_t old_squares = (1ULL << from) | (1ULL << to);
    position.board[from] = position.board[to] = Pieces::Blank;
    position.board[king_to] = Pieces::WKing + color;
    position.board[rook_to] = Pieces::WRook + color;
    position.colors_bb[color] = (position.colors_bb[color] & ~old_squares) |
                                (1ULL << king_to) | (1ULL << rook_to);
    position.pieces_bb[PieceTypes::King] = (position.pieces_bb[PieceTypes::King] & ~(1ULL << from)) | (1ULL << king_to);
    position.pieces_bb[PieceTypes::Rook] = (position.pieces_bb[PieceTypes::Rook] & ~(1ULL << to)) | (1ULL << rook_to);
    position.castling_squares[color].fill(SquareNone);
    position.ep_square = SquareNone;
    position.color ^= 1;
    calculate(position);
    return;
  }

  uint64_t temp_hash = position.zobrist_key ^ ep_key(position);
  uint64_t temp_pawns = position.pawn_key;

  const int opp_color = color ^ 1;
  int captured_piece = Pieces::Blank, captured_square = SquareNone;
  int ep_square = SquareNone;

  const int king_pos = get_king_pos(position, color);
  if (!is_valid_square(king_pos)) {
    return;
  }
  if (position.board[to]) {
    position.halfmoves = 0;
    position.material_count[position.board[to] - 2]--;
    captured_piece = position.board[to];
    captured_square = to;

    temp_hash ^= zobrist_keys[get_zobrist_key(captured_piece, captured_square)];

    if (get_piece_type(captured_piece) == PieceTypes::Pawn) {
      temp_pawns ^=
          zobrist_keys[get_zobrist_key(captured_piece, captured_square)];
    } else {
      position.non_pawn_key[color ^ 1] ^=
          zobrist_keys[get_zobrist_key(captured_piece, captured_square)];
    }
  } else if (extract_type(move) == MoveTypes::EnPassant) {
    position.material_count[opp_color]--;
    captured_square = to + (color ? Directions::North : Directions::South);
    captured_piece = position.board[captured_square];

    temp_hash ^= zobrist_keys[get_zobrist_key(position.board[captured_square],
                                              captured_square)];
    temp_pawns ^= zobrist_keys[get_zobrist_key(position.board[captured_square],
                                               captured_square)];

    position.board[captured_square] = Pieces::Blank;
  }

  position.board[from] = Pieces::Blank;
  position.board[to] = from_piece;

  int to_piece = position.board[to];

  if (from_type == PieceTypes::Pawn) {
    position.halfmoves = 0;

    if (extract_type(move) == MoveTypes::Promotion) {
      to_piece = extract_promo(move) * 2 + 4 + color;
      position.board[to] = to_piece;
      position.material_count[color]--;
      position.material_count[to_piece - 2]++;
    } else if (to == from + Directions::North * 2 ||
               to == from + Directions::South * 2) {
      ep_square = (to + from) >> 1;
    }
  } else if (from_type == PieceTypes::King) {
    if (position.castling_squares[color][Sides::Queenside] != SquareNone) {
      temp_hash ^= castling_key(color, Sides::Queenside, position.castling_squares[color][Sides::Queenside]);
      position.castling_squares[color][Sides::Queenside] = SquareNone;
    }

    if (position.castling_squares[color][Sides::Kingside] != SquareNone) {
      temp_hash ^= castling_key(color, Sides::Kingside, position.castling_squares[color][Sides::Kingside]);
      position.castling_squares[color][Sides::Kingside] = SquareNone;
    }
  }

  if (from == position.castling_squares[color][Sides::Queenside] ||
      from == position.castling_squares[color][Sides::Kingside]) {
    const int side = from < king_pos ? Sides::Queenside : Sides::Kingside;

    if (position.castling_squares[color][side] != SquareNone) {
      temp_hash ^= castling_key(color, side, position.castling_squares[color][side]);
      position.castling_squares[color][side] = SquareNone;
    }
  }

  if (to == position.castling_squares[opp_color][Sides::Queenside] ||
      to == position.castling_squares[opp_color][Sides::Kingside]) {
    const int side = to < get_king_pos(position, opp_color) ? Sides::Queenside
                                                            : Sides::Kingside;
    if (position.castling_squares[opp_color][side] != SquareNone) {
      temp_hash ^= castling_key(opp_color, side, position.castling_squares[opp_color][side]);
      position.castling_squares[opp_color][side] = SquareNone;
    }
  }

  temp_hash ^= zobrist_keys[get_zobrist_key(from_piece, from)];
  temp_hash ^= zobrist_keys[get_zobrist_key(to_piece, to)];

  if (get_piece_type(from_piece) == PieceTypes::Pawn) {
    temp_pawns ^= zobrist_keys[get_zobrist_key(from_piece, from)];
    if (get_piece_type(to_piece) == PieceTypes::Pawn) {
      temp_pawns ^= zobrist_keys[get_zobrist_key(to_piece, to)];
    } else {
      position.non_pawn_key[color] ^=
          zobrist_keys[get_zobrist_key(to_piece, to)];
    }
  } else {
    position.non_pawn_key[color] ^=
        zobrist_keys[get_zobrist_key(from_piece, from)];
    position.non_pawn_key[color] ^= zobrist_keys[get_zobrist_key(to_piece, to)];
  }

  temp_hash ^= zobrist_keys[side_index];

  update_bb(position, from_piece, from, to_piece, to, captured_piece,
            captured_square);

  position.color ^= 1;

  position.ep_square = ep_square;
  temp_hash ^= ep_key(position);
  position.zobrist_key = temp_hash;
  position.pawn_key = temp_pawns;

  safe_TT_prefetch(temp_hash);
}

inline bool is_pseudo_legal(const BoardState &position, Action move,
                            uint64_t checkers) noexcept {
  if (move == MoveNone) {
    return false;
  }
  const int from = extract_from(move), to = extract_to(move);
  if (!is_valid_square(from) || !is_valid_square(to)) {
    return false;
  }

  const int color = position.color;
  const uint64_t us = position.colors_bb[color];
  const uint64_t occ = (position.colors_bb[0] | position.colors_bb[1]);
  const uint64_t empty_squares = ~occ;

  if (extract_type(move) != MoveTypes::Castling && ((1ull << to) & us)) {
    return false;
  }

  const int piece = position.board[from];
  if (piece == Pieces::Blank || get_color(piece) != color) {
    return false;
  }

  const int piece_type = get_piece_type(piece);
  const int type = extract_type(move);

  if (checkers & (checkers - 1)) {
    return (type == MoveTypes::Normal && piece_type == PieceTypes::King &&
            (KING_ATK_SAFE(from) & (1ull << to)));
  }

  if (type == MoveTypes::Castling) return !checkers && can_castle(position, from, to);

  if (type == MoveTypes::EnPassant) {
    if (position.ep_square == SquareNone ||
        !is_valid_square(position.ep_square)) {
      return false;
    }
    const int captured = to + (color ? Directions::North : Directions::South);
    return to == position.ep_square && piece_type == PieceTypes::Pawn &&
           (PAWN_ATK_SAFE(color, from) & (1ULL << to)) &&
           is_valid_square(captured) &&
           position.board[captured] == Pieces::WPawn + (color ^ 1);
  }

  if (type == MoveTypes::Promotion &&
      (piece_type != PieceTypes::Pawn || get_rank(to) != (color ? 0 : 7))) {
    return false;
  }

  if (piece_type == PieceTypes::King) {
    return (KING_ATK_SAFE(from) & (1ull << to));
  }

  if (checkers) {
    const int checker_sq = get_lsb(checkers);
    const uint64_t single_check_filter =
        BetweenBBs[get_king_pos(position, color)][checker_sq] |
        (1ULL << checker_sq);
    if (!(single_check_filter & (1ULL << to)))
      return false;
  }

  if (piece_type == PieceTypes::Pawn) {
    const uint64_t square = (1ull << from);
    uint64_t legal_to = 0;

    const int dir = color == Colors::White ? Directions::North : Directions::South;
    const uint64_t start_rank =
        color == Colors::White ? Ranks[2] : Ranks[5];

    legal_to |= (shift_pawns(square, dir) & empty_squares);
    legal_to |= (shift_pawns(legal_to & start_rank, dir) & empty_squares);

    legal_to |= (((shift_pawns(square & ~Files[0], dir - 1)) |
                  (shift_pawns(square & ~Files[7], dir + 1))) &
                 position.colors_bb[color ^ 1]);

    if (type != MoveTypes::Promotion) {
      legal_to &= ~(Ranks[0] | Ranks[7]);
    }
    return (legal_to >> to) & 1;
  }

  uint64_t attacks = 0;
  if (piece_type == PieceTypes::Knight) {
    attacks = KNIGHT_ATK_SAFE(from);
  } else if (piece_type == PieceTypes::Bishop) {
    attacks = get_bishop_attacks(from, occ);
  } else if (piece_type == PieceTypes::Rook) {
    attacks = get_rook_attacks(from, occ);
  } else if (piece_type == PieceTypes::Queen) {
    attacks = get_bishop_attacks(from, occ) | get_rook_attacks(from, occ);
  }

  return (attacks & (1ull << to));
}

inline bool is_legal(const BoardState &position, Action move) noexcept {
  if (move == MoveNone) return false;
  const int from = extract_from(move), to = extract_to(move), color = position.color;
  if (!is_valid_square(from) || !is_valid_square(to)) return false;
  const int piece = position.board[from];
  if (!piece || get_color(piece) != color) return false;
  if (extract_type(move) == MoveTypes::Castling) return can_castle(position, from, to);
  if (position.board[to] == Pieces::WKing + (color ^ 1) ||
      (position.board[to] && get_color(position.board[to]) == color)) return false;
  uint64_t occupied = (position.colors_bb[0] | position.colors_bb[1]) & ~(1ULL << from);
  occupied |= 1ULL << to;
  if (extract_type(move) == MoveTypes::EnPassant) {
    const int captured = to + (color ? Directions::North : Directions::South);
    if (!is_valid_square(captured) || position.board[captured] != Pieces::WPawn + (color ^ 1)) return false;
    occupied &= ~(1ULL << captured);
  }
  const int king = get_piece_type(piece) == PieceTypes::King ? to : get_king_pos(position, color);
  return !(attacks_square(position, king, color ^ 1, occupied) & ~(1ULL << to));
}


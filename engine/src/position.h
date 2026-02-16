#pragma once
#include "bitboard.h"
#include "utils.h"
#include <cctype>
#include <cstring>
#include <sstream>

struct ThreadInfo;

bool position_integrity_check(const BoardState &position);

int16_t total_mat(const BoardState &position) {
  int m = (position.material_count[0] + position.material_count[1]) * MaterialValues[PieceTypes::Pawn] +
          (position.material_count[2] + position.material_count[3]) * MaterialValues[PieceTypes::Knight] +
          (position.material_count[4] + position.material_count[5]) * MaterialValues[PieceTypes::Bishop] +
          (position.material_count[6] + position.material_count[7]) * MaterialValues[PieceTypes::Rook] +
          (position.material_count[8] + position.material_count[9]) * MaterialValues[PieceTypes::Queen];

  return m;
}

std::string internal_to_uci(const BoardState &position, Action move) {

  int from = extract_from(move), to = extract_to(move),
      promo = extract_promo(move);

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

  std::string uci{};
  uci += get_file(from) + 'a';
  uci += get_rank(from) + '1';

  uci += get_file(to) + 'a';
  uci += get_rank(to) + '1';

  if (extract_type(move) == MoveTypes::Promotion) {
    uci += "nbrq"[promo];
  }

  return uci;
}

int get_king_pos(const BoardState &position, int color) {
  return get_lsb(position.colors_bb[color] &
                 position.pieces_bb[PieceTypes::King]);
}

void print_board(BoardState position) {
  for (int i = 56; i >= 0; i++) {
    safe_printf("+---+---+---+---+---+---+---+---+\n");
    for (int n = i; n != i + 8; n++) {
      safe_printf("| ");
      if (position.board[n] == Pieces::Blank) {
        safe_printf("  ");
      } else {

        switch (position.board[n]) {
        case Pieces::WPawn:
          safe_printf("P ");
          break;
        case Pieces::WKnight:
          safe_printf("N ");
          break;
        case Pieces::WBishop:
          safe_printf("B ");
          break;
        case Pieces::WRook:
          safe_printf("R ");
          break;
        case Pieces::WQueen:
          safe_printf("Q ");
          break;
        case Pieces::WKing:
          safe_printf("K ");
          break;
        case Pieces::BPawn:
          safe_printf("p ");
          break;
        case Pieces::BKnight:
          safe_printf("n ");
          break;
        case Pieces::BBishop:
          safe_printf("b ");
          break;
        case Pieces::BRook:
          safe_printf("r ");
          break;
        case Pieces::BQueen:
          safe_printf("q ");
          break;
        case Pieces::BKing:
          safe_printf("k ");
          break;
        default:
          safe_printf("# ");
        }
      }
    }
    safe_printf("|\n");
    i -= 8;
  }
  safe_printf("+---+---+---+---+---+---+---+---+\n\n");
}

void set_board(BoardState &position, ThreadInfo &thread_info, std::string f) {
  position = BoardState{};

  generate_bb(f, position);

  std::istringstream fen(f);
  std::string fen_pos;
  fen >> fen_pos;

  int rank = 7;
  int file = 0;
  for (size_t idx = 0; idx < fen_pos.size(); ++idx) {
    char c = fen_pos[idx];
    if (c == '/') {
      if (file != 8) {

        return;
      }
      --rank;
      file = 0;
      if (rank < 0)
        break;
      continue;
    }

    if (std::isdigit(static_cast<unsigned char>(c))) {
      int skip = c - '0';
      file += skip;
      if (file > 8) {

        return;
      }
      continue;
    }

    if (file >= 8 || rank < 0) {

      return;
    }

    int i = file + rank * 8;
    switch (c) {
    case 'P':
      position.board[i] = Pieces::WPawn;
      position.material_count[0]++;
      break;
    case 'N':
      position.board[i] = Pieces::WKnight;
      position.material_count[2]++;
      break;
    case 'B':
      position.board[i] = Pieces::WBishop;
      position.material_count[4]++;
      break;
    case 'R':
      position.board[i] = Pieces::WRook;
      position.material_count[6]++;
      break;
    case 'Q':
      position.board[i] = Pieces::WQueen;
      position.material_count[8]++;
      break;
    case 'K':
      position.board[i] = Pieces::WKing;
      break;
    case 'p':
      position.board[i] = Pieces::BPawn;
      position.material_count[1]++;
      break;
    case 'n':
      position.board[i] = Pieces::BKnight;
      position.material_count[3]++;
      break;
    case 'b':
      position.board[i] = Pieces::BBishop;
      position.material_count[5]++;
      break;
    case 'r':
      position.board[i] = Pieces::BRook;
      position.material_count[7]++;
      break;
    case 'q':
      position.board[i] = Pieces::BQueen;
      position.material_count[9]++;
      break;
    case 'k':
      position.board[i] = Pieces::BKing;
      break;
    default:
      safe_printf("Error parsing FEN: %s\n", f.c_str());
      return;
    }
    ++file;
  }

  std::string color;
  fen >> color;
  if (color[0] == 'w') {
    position.color = Colors::White;
  } else {
    position.color = Colors::Black;
  }

  std::string castling_rights;
  fen >> castling_rights;

  for (int i = 0; i < 2; i++) {
    for (int n = 0; n < 2; n++) {
      position.castling_squares[i][n] = SquareNone;
    }
  }

  for (char right : castling_rights) {
    if (right == '-') {
      break;
    }

    int color = std::islower(right) ? Colors::Black : Colors::White;
    char original_right = right;
    right = std::tolower(right);

    int square;
    int base = 56 * color;
    int king_pos = get_king_pos(position, color);

    if (right >= 'a' && right <= 'h') {

      square = (right - 'a') + base;
    } else if (right == 'k') {

      if (thread_data.is_frc) {

        square = base + 7;
        for (int i = king_pos + 1; i < base + 8; i++) {
          if (position.board[i] == Pieces::WRook + color) {
            square = i;
            break;
          }
        }
      } else {
        square = base + 7;
      }
    } else if (right == 'q') {

      if (thread_data.is_frc) {

        square = base;
        for (int i = king_pos - 1; i >= base; i--) {
          if (position.board[i] == Pieces::WRook + color) {
            square = i;
            break;
          }
        }
      } else {
        square = base;
      }
    } else {

      continue;
    }

    if (!is_valid_square(square))
      continue;

    int side = square > king_pos ? Sides::Kingside : Sides::Queenside;
    position.castling_squares[color][side] = square;
  }

  std::string ep_square;
  fen >> ep_square;
  if (ep_square[0] == '-') {
    position.ep_square = SquareNone;
  } else {
    uint8_t file = (ep_square[0] - 'a');
    uint8_t rank = (ep_square[1] - '1');
    position.ep_square = rank * 8 + file;
  }

  int halfmoves;
  fen >> halfmoves;

  if (!fen) {
    return;
  }

  position.halfmoves = halfmoves;
}

std::string export_fen(const BoardState &position,
                       const ThreadInfo &thread_info) {

  std::string fen = "";
  bool subtracted = true;

  for (int pos = 56; pos >= 0; pos++) {

    if (pos % 8 == 0 && !subtracted) {
      pos -= 17;
      if (pos >= -1) {
        fen += "/";
      }
      subtracted = true;
    }

    else if (position.board[pos] != Pieces::Blank) {

      switch (position.board[pos]) {

      case Pieces::WPawn:
        fen += "P";
        break;
      case Pieces::WKnight:
        fen += "N";
        break;
      case Pieces::WBishop:
        fen += "B";
        break;
      case Pieces::WRook:
        fen += "R";
        break;
      case Pieces::WQueen:
        fen += "Q";
        break;
      case Pieces::WKing:
        fen += "K";
        break;
      case Pieces::BPawn:
        fen += "p";
        break;
      case Pieces::BKnight:
        fen += "n";
        break;
      case Pieces::BBishop:
        fen += "b";
        break;
      case Pieces::BRook:
        fen += "r";
        break;
      case Pieces::BQueen:
        fen += "q";
        break;
      case Pieces::BKing:
        fen += "k";
        break;
      default:
        safe_print_cerr(std::string("Error parsing board!"));
        print_board(position);
        std::exit(1);
      }

      subtracted = false;
    }

    else {
      int empty_squares = 0;
      subtracted = false;

      do {
        empty_squares++;
        pos++;
      } while (position.board[pos] == Pieces::Blank && pos % 8 != 0);

      fen += std::to_string(empty_squares);
      pos--;
    }
  }

  fen += " ";

  if (position.color == Colors::Black) {
    fen += "b ";
  } else {
    fen += "w ";
  }

  bool has_castling_rights = false;
  int indx = 0;

  for (char rights : std::string("KQkq")) {

    int color = indx > 1 ? Colors::Black : Colors::White;
    int side = indx % 2 == 0 ? Sides::Kingside : Sides::Queenside;
    if (position.castling_squares[color][side] != SquareNone) {
      fen += rights;
      has_castling_rights = true;
    }

    indx++;
  }

  if (has_castling_rights) {
    fen += " ";
  } else {
    fen += "- ";
  }

  if (position.ep_square != SquareNone) {

    char file = get_file(position.ep_square) + 'a';
    fen += file;

    char rank = get_rank(position.ep_square) + '1';

    fen += rank;
    fen += " ";

  } else {
    fen += "- ";
  }

  fen += std::to_string(position.halfmoves) + " ";
  fen += std::to_string((thread_info.game_ply + 1) / 2);

  return fen;
}

uint64_t attacks_square(const BoardState &position, int sq, int color) {

  if (!is_valid_square(sq))
    return 0ULL;
  if (color != Colors::White && color != Colors::Black)
    return 0ULL;

  uint64_t bishops = position.pieces_bb[PieceTypes::Bishop] |
                     position.pieces_bb[PieceTypes::Queen];
  uint64_t rooks = position.pieces_bb[PieceTypes::Rook] |
                   position.pieces_bb[PieceTypes::Queen];
  uint64_t occ =
      position.colors_bb[Colors::White] | position.colors_bb[Colors::Black];

  uint64_t combined_pieces = 0ULL;
  for (int pt = PieceTypes::Pawn; pt <= PieceTypes::King; ++pt)
    combined_pieces |= position.pieces_bb[pt];
  occ &= combined_pieces | position.colors_bb[color];

  uint64_t attackers =
      (PAWN_ATK_SAFE(color ^ 1, sq) & position.pieces_bb[PieceTypes::Pawn]) |
      (KNIGHT_ATK_SAFE(sq) & position.pieces_bb[PieceTypes::Knight]) |
      (get_bishop_attacks(sq, occ) & bishops) |
      (get_rook_attacks(sq, occ) & rooks) |
      (KING_ATK_SAFE(sq) & position.pieces_bb[PieceTypes::King]);

  return attackers & position.colors_bb[color];
}

uint64_t attacks_square(const BoardState &position, int sq, int color,
                        uint64_t occ) {

  if (!is_valid_square(sq))
    return 0ULL;
  if (color != Colors::White && color != Colors::Black)
    return 0ULL;

  uint64_t combined_pieces = 0ULL;
  for (int pt = PieceTypes::Pawn; pt <= PieceTypes::King; ++pt)
    combined_pieces |= position.pieces_bb[pt];
  occ &= combined_pieces;

  uint64_t bishops = position.pieces_bb[PieceTypes::Bishop] |
                     position.pieces_bb[PieceTypes::Queen];
  uint64_t rooks = position.pieces_bb[PieceTypes::Rook] |
                   position.pieces_bb[PieceTypes::Queen];

  uint64_t attackers =
      (PAWN_ATK_SAFE(color ^ 1, sq) & position.pieces_bb[PieceTypes::Pawn]) |
      (KNIGHT_ATK_SAFE(sq) & position.pieces_bb[PieceTypes::Knight]) |
      (get_bishop_attacks(sq, occ) & bishops) |
      (get_rook_attacks(sq, occ) & rooks) |
      (KING_ATK_SAFE(sq) & position.pieces_bb[PieceTypes::King]);

  return attackers & position.colors_bb[color] & occ;
}

uint64_t attacks_square(const BoardState &position, int sq, uint64_t occ) {

  if (!is_valid_square(sq))
    return 0ULL;

  uint64_t combined_pieces = 0ULL;
  for (int pt = PieceTypes::Pawn; pt <= PieceTypes::King; ++pt)
    combined_pieces |= position.pieces_bb[pt];
  occ &= combined_pieces;

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

uint64_t br_attacks_square(const BoardState &position, int sq, int color,
                           uint64_t occ) {

  if (!is_valid_square(sq))
    return 0ULL;

  uint64_t bishops = position.pieces_bb[PieceTypes::Bishop] |
                     position.pieces_bb[PieceTypes::Queen];
  uint64_t rooks = position.pieces_bb[PieceTypes::Rook] |
                   position.pieces_bb[PieceTypes::Queen];

  uint64_t attackers = (get_bishop_attacks(sq, occ) & bishops) |
                       (get_rook_attacks(sq, occ) & rooks);

  return attackers & position.colors_bb[color] & occ;
}

bool is_queen_promo(Action move) { return extract_promo(move) == 3; }

bool is_cap(const BoardState &position, Action &move) {
  if (extract_type(move) == MoveTypes::Castling) {
    return false;
  }
  int to = extract_to(move);
  if (!is_valid_square(to)) {
    return false;
  }
  int from_sq = extract_from(move);
  if (!is_valid_square(from_sq)) {
    return false;
  }
  return (position.board[to] ||
          (to == position.ep_square &&
           position.board[from_sq] == Pieces::WPawn + position.color) ||
          is_queen_promo((move)));
}

void make_move(BoardState &position, Action move) {

  if (!position_integrity_check(position)) {
    return;
  }

  position.halfmoves++;

  if (move == MoveNone) {
    position.color ^= 1;
    if (position.ep_square != SquareNone) {
      position.zobrist_key ^= zobrist_keys[ep_index];
      position.ep_square = SquareNone;
    }

    position.zobrist_key ^= zobrist_keys[side_index];
    return;
  }

  int from = extract_from(move), to = extract_to(move);
  if (!is_valid_square(from) || !is_valid_square(to)) {
    return;
  }

  int from_piece = position.board[from];
  if (from_piece == Pieces::Blank || get_color(from_piece) != position.color) {
    return;
  }

  uint64_t temp_hash = position.zobrist_key;
  uint64_t temp_pawns = position.pawn_key;
  uint64_t non_pawn_white = position.non_pawn_key[Colors::White],
           non_pawn_black = position.non_pawn_key[Colors::Black];

  int color = position.color, opp_color = color ^ 1,
      captured_piece = Pieces::Blank, captured_square = SquareNone;
  int base_rank = (color ? a8 : 0);
  int ep_square = SquareNone;

  int from_type = get_piece_type(from_piece);

  int king_pos = get_king_pos(position, color);
  if (!is_valid_square(king_pos)) {
    return;
  }
  int side = to > king_pos;

  if (extract_type(move) == MoveTypes::Castling) {
    to = base_rank + 2 + (side) * 4;
  }

  else if (position.board[to]) {

    position.halfmoves = 0;
    position.material_count[position.board[to] - 2]--;
    captured_piece = position.board[to], captured_square = to;

    temp_hash ^= zobrist_keys[get_zobrist_key(captured_piece, captured_square)];

    if (get_piece_type(captured_piece) == PieceTypes::Pawn) {
      temp_pawns ^=
          zobrist_keys[get_zobrist_key(captured_piece, captured_square)];
    }

    else {
      position.non_pawn_key[color ^ 1] ^=
          zobrist_keys[get_zobrist_key(captured_piece, captured_square)];
    }

  }

  else if (extract_type(move) == MoveTypes::EnPassant) {
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
      position.material_count[color]--, position.material_count[to_piece - 2]++;
    }

    else if (to == from + Directions::North * 2 ||
             to == from + Directions::South * 2) {
      ep_square = (to + from) / 2;
    }
  }

  else if (from_type == PieceTypes::King) {

    if (extract_type(move) == MoveTypes::Castling) {
      int rook_to, rook_from;
      if (side) {
        rook_to = base_rank + 5;
        rook_from = position.castling_squares[color][Sides::Kingside];
      } else {
        rook_to = base_rank + 3;
        rook_from = position.castling_squares[color][Sides::Queenside];
      }

      if (rook_from >= 0 && rook_from < 64) {
        if (position.board[rook_from] == Pieces::WRook + color) {
          position.board[rook_from] = Pieces::Blank;
        }
        position.board[rook_to] = Pieces::WRook + color;

        temp_hash ^=
            zobrist_keys[get_zobrist_key(Pieces::WRook + color, rook_to)] ^
            zobrist_keys[get_zobrist_key(Pieces::WRook + color, rook_from)];

        position.non_pawn_key[color] ^=
            zobrist_keys[get_zobrist_key(Pieces::WRook + color, rook_to)] ^
            zobrist_keys[get_zobrist_key(Pieces::WRook + color, rook_from)];

        update_bb(position, Pieces::WRook + color, rook_from,
                  Pieces::WRook + color, rook_to, Pieces::Blank, SquareNone);
      }
    }

    if (position.castling_squares[color][Sides::Queenside] != SquareNone) {
      temp_hash ^= zobrist_keys[castling_index + color * 2 + Sides::Queenside];
      position.castling_squares[color][Sides::Queenside] = SquareNone;
    }

    if (position.castling_squares[color][Sides::Kingside] != SquareNone) {
      temp_hash ^= zobrist_keys[castling_index + color * 2 + Sides::Kingside];
      position.castling_squares[color][Sides::Kingside] = SquareNone;
    }
  }

  if (from == position.castling_squares[color][Sides::Queenside] ||
      from == position.castling_squares[color][Sides::Kingside]) {

    int side = from < king_pos ? Sides::Queenside : Sides::Kingside;

    if (position.castling_squares[color][side] != SquareNone) {
      position.castling_squares[color][side] = SquareNone;
      temp_hash ^= zobrist_keys[castling_index + color * 2 + side];
    }
  }

  if (to == position.castling_squares[opp_color][Sides::Queenside] ||
      to == position.castling_squares[opp_color][Sides::Kingside]) {

    int side = to < get_king_pos(position, opp_color) ? Sides::Queenside
                                                      : Sides::Kingside;
    if (position.castling_squares[opp_color][side] != SquareNone) {
      position.castling_squares[opp_color][side] = SquareNone;
      temp_hash ^= zobrist_keys[castling_index + opp_color * 2 + side];
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
  }

  else {
    position.non_pawn_key[color] ^=
        zobrist_keys[get_zobrist_key(from_piece, from)];
    position.non_pawn_key[color] ^= zobrist_keys[get_zobrist_key(to_piece, to)];
  }

  temp_hash ^= zobrist_keys[side_index];

  update_bb(position, from_piece, from, to_piece, to, captured_piece,
            captured_square);

  position.color ^= 1;

  if ((position.ep_square == SquareNone) ^ (ep_square == SquareNone)) {

    temp_hash ^= zobrist_keys[ep_index];
  }
  position.ep_square = ep_square;
  position.zobrist_key = temp_hash;
  position.pawn_key = temp_pawns;

  safe_TT_prefetch(temp_hash);
}

bool is_pseudo_legal(const BoardState &position, Action move, uint64_t checkers) {
  if (move == MoveNone) {
    return false;
  }
  int from = extract_from(move), to = extract_to(move), color = position.color;
  uint64_t us = position.colors_bb[color], them = position.colors_bb[color ^ 1];
  uint64_t occ = (position.colors_bb[0] | position.colors_bb[1]);
  uint64_t empty_squares = ~occ;

  if ((1ull << to) & us) {
    return false;
  }

  int piece = position.board[from];
  if (piece == Pieces::Blank || get_color(piece) != color) {
    return false;
  }

  int piece_type = get_piece_type(piece);
  int type = extract_type(move);

  if (!is_valid_square(from) || !is_valid_square(to)) {
    return false;
  }

  if (checkers & (checkers - 1)) {
    return (type == MoveTypes::Normal && piece_type == PieceTypes::King &&
            (KING_ATK_SAFE(from) & (1ull << to)));
  }

  if (type == MoveTypes::Castling) {
    int side = to > from;
    int rook_target = 56 * color + 3 + 2 * side;
    int king_target = 56 * color + 2 + 4 * side;
    int rook_square = position.castling_squares[color][side];

    if (!is_valid_square(rook_square) || !is_valid_square(rook_target) ||
        !is_valid_square(king_target)) {
      return false;
    }

    uint64_t castle_bb = BetweenBBs[rook_square][rook_target];
    castle_bb |= BetweenBBs[from][king_target];
    castle_bb &= ~(1ull << from) & ~(1ull << rook_square);

    return (!checkers && position.castling_squares[color][side] != SquareNone &&
            !castle_bb);
  }

  if (type == MoveTypes::EnPassant) {
    int dir = color ? -1 : 1;
    if (position.ep_square == SquareNone ||
        !is_valid_square(position.ep_square)) {
      return false;
    }
    return (to == position.ep_square && piece_type == PieceTypes::Pawn &&
            (to == from + Directions::Northwest * dir ||
             to == from + Directions::Northeast * dir));
  }

  if (type == MoveTypes::Promotion && piece_type != PieceTypes::Pawn) {
    return false;
  }

  if (piece_type == PieceTypes::King) {
    return (KING_ATK_SAFE(from) & (1ull << to));
  }

  if (checkers) {

    int checker_sq = get_lsb(checkers);
    uint64_t single_check_filter =
        BetweenBBs[get_king_pos(position, color)][checker_sq] |
        (1ULL << checker_sq);
    if (!(single_check_filter & (1ULL << to)))
      return false;
  }

  if (piece_type == PieceTypes::Pawn) {
    uint64_t square = (1ull << from);
    uint64_t legal_to = 0;

    int dir = color == Colors::White ? Directions::North : Directions::South;

    legal_to |= (shift_pawns(square, dir) & empty_squares);
    legal_to |= (shift_pawns(legal_to & Ranks[2], dir) & empty_squares);

    legal_to |= (((shift_pawns(square & ~Files[0], dir - 1)) |
                  (shift_pawns(square & ~Files[7], dir + 1))) &
                 position.colors_bb[color ^ 1]);

    if (type != MoveTypes::Promotion) {
      legal_to &= ~(Ranks[0] | Ranks[7]);
    }
    return (legal_to >> (to)) & 1;
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

bool is_legal(const BoardState &position, Action move) {
  uint64_t occupied =
      position.colors_bb[Colors::White] | position.colors_bb[Colors::Black];
  int from = extract_from(move), to = extract_to(move), color = position.color,
      opp_color = color ^ 1;

  if (!is_valid_square(from) || !is_valid_square(to)) {
    return false;
  }

  int from_piece = position.board[from];

  if (from_piece < Pieces::Blank || from_piece > Pieces::BKing) {
    return false;
  }

  if (get_piece_type(from_piece) == PieceTypes::King) {
    if (extract_type(move) == MoveTypes::Castling) {
      to = 56 * color + 2 + (to > from) * 4;
      if (!is_valid_square(to)) {
        return false;
      }
    }
    return !attacks_square(position, to, opp_color, occupied ^ (1ull << from));
  }

  int king_pos = get_king_pos(position, color);

  if (extract_type(move) == MoveTypes::EnPassant) {
    int cap_square = to + (color ? Directions::North : Directions::South);
    if (!is_valid_square(cap_square)) {
      return false;
    }
    return !br_attacks_square(position, king_pos, opp_color,
                              occupied ^ (1ull << from) ^ (1ull << to) ^
                                  (1ull << cap_square));
  }

  if (position.board[to]) {
    return !(br_attacks_square(position, king_pos, opp_color,
                               occupied ^ (1ull << from)) &
             ~(1ull << to));
  }

  return !br_attacks_square(position, king_pos, opp_color,
                            occupied ^ (1ull << from) ^ (1ull << to));
}

bool position_integrity_check(const BoardState &position) {

  for (int sq = 0; sq < 64; sq++) {
    int piece = position.board[sq];
    if (piece < Pieces::Blank || piece > Pieces::BKing) {
      return false;
    }
    if (piece != Pieces::Blank && !is_valid_square(sq)) {
      return false;
    }
  }

  int white_king = get_king_pos(position, Colors::White);
  int black_king = get_king_pos(position, Colors::Black);
  if (!is_valid_square(white_king) || !is_valid_square(black_king)) {
    return false;
  }
  if (white_king == black_king) {
    return false;
  }

  int white_pieces = 0, black_pieces = 0;
  for (int sq = 0; sq < 64; sq++) {
    int piece = position.board[sq];
    if (piece != Pieces::Blank) {
      if (get_color(piece) == Colors::White)
        white_pieces++;
      else
        black_pieces++;
    }
  }
  if (white_pieces > 16 || black_pieces > 16) {
    return false;
  }

  if (position.ep_square != SquareNone &&
      !is_valid_square(position.ep_square)) {
    return false;
  }

  for (int color = 0; color < 2; color++) {
    for (int side = 0; side < 2; side++) {
      int square = position.castling_squares[color][side];
      if (square != SquareNone && !is_valid_square(square)) {
        return false;
      }
    }
  }

  if (position.color != Colors::White && position.color != Colors::Black) {
    return false;
  }

  if (position.halfmoves < 0 || position.halfmoves > 100) {
    return false;
  }

  return true;
}

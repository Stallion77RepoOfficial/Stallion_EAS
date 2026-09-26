#include "sbin.h"
#include "../engine/src/bitboard.h"
#include "../engine/src/nnue.h"
#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <sstream>
#include <vector>

static inline uint8_t char_to_piece_code(char c) {
    switch (c) {
        case 'P': return 2;  case 'p': return 3;
        case 'N': return 4;  case 'n': return 5;
        case 'B': return 6;  case 'b': return 7;
        case 'R': return 8;  case 'r': return 9;
        case 'Q': return 10; case 'q': return 11;
        case 'K': return 12; case 'k': return 13;
        default: return 0;
    }
}

static inline char piece_code_to_char(uint8_t code) {
    switch (code) {
        case 2: return 'P';  case 3: return 'p';
        case 4: return 'N';  case 5: return 'n';
        case 6: return 'B';  case 7: return 'b';
        case 8: return 'R';  case 9: return 'r';
        case 10: return 'Q'; case 11: return 'q';
        case 12: return 'K'; case 13: return 'k';
        default: return '?';
    }
}

static bool decode_position(const PackedPosition* in, uint8_t board[64], int& count) {
    if (!in || !board) return false;
    std::memset(board, 0, 64);
    const int occupied_count = __builtin_popcountll(in->occupied);
    if (occupied_count < 2 || occupied_count > 32) return false;

    const uint16_t metadata = uint16_t(in->reserved[0]) |
                              (uint16_t(in->reserved[1]) << 8);
    if ((metadata & 0x7fff) == 0) return false;
    const uint8_t ep_code = (in->flags >> 5) & 0x07;
    const bool h_file_ep = (metadata & 0x8000) != 0;
    if (h_file_ep && ep_code != 0) return false;

    uint64_t occ = in->occupied;
    int kings[2]{}, pawns[2]{}, colors[2]{};
    int pieces[2][7]{}, bishops[2][2]{};
    count = 0;
    while (occ) {
        const int sq = __builtin_ctzll(occ);
        occ &= occ - 1;
        const uint8_t byte_val = in->pieces[count / 2];
        const uint8_t code = (count % 2 == 0) ? (byte_val & 0x0f) : (byte_val >> 4);
        if (code < 2 || code > 13) return false;
        board[sq] = code;
        ++colors[code & 1];
        ++pieces[code & 1][code / 2];
        if (code / 2 == 3) ++bishops[code & 1][(sq + (sq >> 3)) & 1];
        if (code >= 12) ++kings[code & 1];
        if (code <= 3) {
            ++pawns[code & 1];
            if (sq < 8 || sq >= 56) return false;
        }
        ++count;
    }
    if (count != occupied_count || kings[0] != 1 || kings[1] != 1 ||
        pawns[0] > 8 || pawns[1] > 8 || colors[0] > 16 || colors[1] > 16) {
        return false;
    }
    for (int c = 0; c < 2; ++c) {
        const int excess = std::max(0, pieces[c][2] - 2) + std::max(0, pieces[c][3] - 2) +
                           std::max(0, pieces[c][4] - 2) + std::max(0, pieces[c][5] - 1);
        if (pawns[c] + excess > 8) return false;
        const int promoted_bishops =
            std::max(0, bishops[c][0] - 1) + std::max(0, bishops[c][1] - 1);
        if (promoted_bishops > 8 - pawns[c]) return false;
    }

    if ((count & 1) && (in->pieces[count / 2] & 0xf0)) return false;
    for (int i = (count + 1) / 2; i < 16; ++i) {
        if (in->pieces[i] != 0) return false;
    }

    auto piece_at = [&](int sq) -> uint8_t {
        return (sq >= 0 && sq < 64) ? board[sq] : uint8_t(0);
    };

    if ((in->flags & (1 << 1)) && (piece_at(4) != 12 || piece_at(7) != 8)) return false;
    if ((in->flags & (1 << 2)) && (piece_at(4) != 12 || piece_at(0) != 8)) return false;
    if ((in->flags & (1 << 3)) && (piece_at(60) != 13 || piece_at(63) != 9)) return false;
    if ((in->flags & (1 << 4)) && (piece_at(60) != 13 || piece_at(56) != 9)) return false;

    const bool black_turn = (in->flags & 1) != 0;
    const int ep_file = h_file_ep ? 8 : int(ep_code);
    int ep_captured = -1;
    if (ep_file != 0) {
        const int target = (black_turn ? 2 : 5) * 8 + (ep_file - 1);
        const int captured = target + (black_turn ? 8 : -8);
        const int origin = target + (black_turn ? -8 : 8);
        if (target < 0 || target >= 64 || captured < 0 || captured >= 64 ||
            origin < 0 || origin >= 64 || piece_at(target) != 0 ||
            piece_at(origin) != 0 ||
            piece_at(captured) != uint8_t(2 + (black_turn ? 0 : 1))) {
            return false;
        }
        ep_captured = captured;
    }

    Position position{};
    int king_squares[2]{};
    occ = in->occupied;
    while (occ) {
        const int sq = __builtin_ctzll(occ);
        occ &= occ - 1;
        const int code = board[sq];
        position.colors_bb[code & 1] |= uint64_t(1) << sq;
        position.pieces_bb[code / 2] |= uint64_t(1) << sq;
        if (code >= 12) king_squares[code & 1] = sq;
    }
    const int turn = in->flags & 1;
    if (attackers_to(position, king_squares[turn ^ 1], turn, in->occupied)) return false;
    const uint64_t checkers =
        attackers_to(position, king_squares[turn], turn ^ 1, in->occupied);
    const int ncheckers = pop_count(checkers);
    if (ncheckers > 2) return false;
    auto is_slider = [&](int sq) {
        const int type = board[sq] / 2;
        return type >= 3 && type <= 5;
    };
    if (ncheckers == 2) {
        uint64_t c = checkers;
        bool slider = false;
        while (c) {
            const int sq = __builtin_ctzll(c);
            c &= c - 1;
            if (is_slider(sq)) { slider = true; break; }
        }
        if (!slider) return false;
    }
    if (ep_file != 0 && checkers) {
        uint64_t c = checkers;
        while (c) {
            const int sq = __builtin_ctzll(c);
            c &= c - 1;
            if (sq != ep_captured && !is_slider(sq)) return false;
        }
    }
    return true;
}

// ---- Shared record helpers (everything below builds on decode_position) ----

// Unpack a record that already passed decode_position into engine bitboards.
static void unpack_board(const PackedPosition& in, uint8_t board[64], uint64_t colors_bb[2],
                         uint64_t pieces_bb[7]) {
    std::memset(board, 0, 64);
    colors_bb[0] = colors_bb[1] = 0;
    std::fill_n(pieces_bb, 7, 0);
    uint64_t occ = in.occupied;
    for (int k = 0; occ; ++k) {
        const int sq = __builtin_ctzll(occ);
        occ &= occ - 1;
        const uint8_t code = (k % 2 == 0) ? (in.pieces[k / 2] & 0x0f) : (in.pieces[k / 2] >> 4);
        board[sq] = code;
        colors_bb[code & 1] |= uint64_t(1) << sq;
        pieces_bb[code / 2] |= uint64_t(1) << sq;
    }
}

static int ep_file_of(const PackedPosition& in) {
    const uint16_t metadata = in.reserved[0] | (uint16_t(in.reserved[1]) << 8);
    return (metadata & 0x8000) ? 8 : ((in.flags >> 5) & 0x07);
}

static Position engine_position(const PackedPosition& in) {
    Position pos{};
    unpack_board(in, pos.board.data(), pos.colors_bb.data(), pos.pieces_bb.data());
    pos.color = in.flags & 1;
    const int ep_file = ep_file_of(in);
    pos.ep_square = ep_file ? uint8_t((pos.color ? 2 : 5) * 8 + ep_file - 1) : uint8_t(SquareNone);
    return pos;
}

static inline uint64_t mix64(uint64_t x) {
    x ^= x >> 30;
    x *= 0xbf58476d1ce4e5b9ULL;
    x ^= x >> 27;
    x *= 0x94d049bb133111ebULL;
    return x ^ (x >> 31);
}

static uint64_t group_key(const PackedPosition& in) {
    const int stm = in.flags & 1;
    uint64_t occ = in.occupied, h = 0;
    for (int k = 0; occ; ++k) {
        const int sq = __builtin_ctzll(occ);
        occ &= occ - 1;
        const int code = (k % 2 == 0) ? (in.pieces[k / 2] & 0x0f) : (in.pieces[k / 2] >> 4);
        const int view_sq = stm ? (sq ^ 56) : sq;
        const int view_code = stm ? (code ^ 1) : code;
        h ^= mix64(uint64_t(view_sq) * 16 + uint64_t(view_code) + 1);
    }
    const uint64_t key = mix64(h ^ uint64_t(__builtin_popcountll(in.occupied)));
    return key ? key : 1;
}

// Side-to-move and opponent feature lists; the same indices the engine uses.
static void position_features(const PackedPosition& in, int32_t* us, int& n_us,
                              int32_t* them, int& n_them) {
    uint8_t board[64];
    uint64_t colors_bb[2], pieces_bb[7];
    unpack_board(in, board, colors_bb, pieces_bb);
    const bool white_turn = (in.flags & 1) == 0;
    const int wking = __builtin_ctzll(colors_bb[0] & pieces_bb[PieceTypes::King]);
    const int bking = __builtin_ctzll(colors_bb[1] & pieces_bb[PieceTypes::King]);
    const size_t w_bucket = static_cast<size_t>(KingBucketTable[wking]);
    const size_t b_bucket = static_cast<size_t>(KingBucketTable[bking ^ 56]);
    int32_t* white = white_turn ? us : them;
    int32_t* black = white_turn ? them : us;
    int n = 0;
    uint64_t occ = in.occupied;
    while (occ) {
        const int sq = __builtin_ctzll(occ);
        occ &= occ - 1;
        const auto [white_idx, black_idx] = feature_indices(board[sq], sq, w_bucket, b_bucket);
        white[n] = static_cast<int32_t>(white_idx);
        black[n] = static_cast<int32_t>(black_idx);
        ++n;
    }
    int extra_w[NNUE_EXTRA_SLOTS], extra_b[NNUE_EXTRA_SLOTS];
    const int nw = collect_extra_features(board, colors_bb, pieces_bb, false, extra_w, NNUE_EXTRA_SLOTS);
    const int nb = mirror_extra_features(extra_w, nw, extra_b, NNUE_EXTRA_SLOTS);
    if (nw < 0 || nb < 0) std::abort();
    for (int i = 0; i < nw; ++i) white[n + i] = extra_w[i];
    for (int i = 0; i < nb; ++i) black[n + i] = extra_b[i];
    n_us = n + (white_turn ? nw : nb);
    n_them = n + (white_turn ? nb : nw);
}

static bool is_mate_record(const PackedPosition& in) {
    return (in.eval == 2000 || in.eval == -2000) && (in.wdl == 0 || in.wdl == 65535);
}

static uint16_t calibrated_wdl(const PackedPosition& in, double lambda) {
    if (is_mate_record(in)) return in.eval > 0 ? 65535 : 0;
    const double cp = std::clamp(static_cast<double>(in.eval), -1500.0, 1500.0);
    const double cp_wdl = 1.0 / (1.0 + std::pow(10.0, -cp / 400.0));
    const double value = (1.0 - lambda) * cp_wdl + lambda * (in.wdl / 65535.0);
    return static_cast<uint16_t>(std::lrint(value * 65535.0));
}

static bool in_check(const Position& pos) {
    const int king = __builtin_ctzll(pos.colors_bb[pos.color] & pos.pieces_bb[PieceTypes::King]);
    return attackers_to(pos, king, pos.color ^ 1, pos.colors_bb[0] | pos.colors_bb[1]) != 0;
}

// A capture or promotion that wins material by static exchange.
static bool has_winning_tactic(const Position& pos) {
    const int stm = pos.color;
    const uint64_t own = pos.colors_bb[stm];
    const uint64_t occ = pos.colors_bb[0] | pos.colors_bb[1];
    const uint64_t targets = pos.colors_bb[stm ^ 1] & ~pos.pieces_bb[PieceTypes::King];
    const uint64_t promo_rank = Ranks[stm ? 0 : 7];
    uint64_t pieces = own;
    while (pieces) {
        const int from = pop_lsb(pieces);
        const int type = get_piece_type(pos.board[from]);
        uint64_t attacks = 0;
        switch (type) {
            case PieceTypes::Pawn: attacks = PAWN_ATK_SAFE(stm, from); break;
            case PieceTypes::Knight: attacks = KNIGHT_ATK_SAFE(from); break;
            case PieceTypes::Bishop: attacks = get_bishop_attacks(from, occ); break;
            case PieceTypes::Rook: attacks = get_rook_attacks(from, occ); break;
            case PieceTypes::Queen: attacks = get_bishop_attacks(from, occ) | get_rook_attacks(from, occ); break;
            default: attacks = KING_ATK_SAFE(from); break;
        }
        uint64_t captures = attacks & targets;
        while (captures) {
            const int to = pop_lsb(captures);
            const Move move = (type == PieceTypes::Pawn && ((uint64_t(1) << to) & promo_rank))
                                  ? pack_move_promo(from, to, Promos::Queen)
                                  : pack_move(from, to, MoveTypes::Normal);
            if (SEE(pos, move, 1)) return true;
        }
        if (type == PieceTypes::Pawn) {
            const int to = from + (stm ? Directions::South : Directions::North);
            if (((uint64_t(1) << to) & promo_rank) && !((uint64_t(1) << to) & occ) &&
                SEE(pos, pack_move_promo(from, to, Promos::Queen), 1))
                return true;
        }
    }
    if (pos.ep_square != SquareNone) {
        uint64_t pawns = PAWN_ATK_SAFE(stm ^ 1, pos.ep_square) & own & pos.pieces_bb[PieceTypes::Pawn];
        while (pawns) {
            if (SEE(pos, pack_move(pop_lsb(pawns), pos.ep_square, MoveTypes::EnPassant), 1)) return true;
        }
    }
    return false;
}

// Phase: 0 endgame, 1 late middlegame, 2 middlegame, 3 opening.
static int position_phase(const PackedPosition& in, const uint8_t board[64]) {
    int total = 0, white_home = 0, black_home = 0;
    for (int sq = 0; sq < 64; ++sq) {
        const uint8_t code = board[sq];
        if (!code) continue;
        if (code < 12) total += MaterialValues[code / 2];
        if (sq < 8 && (code == 4 || code == 6 || code == 8 || code == 10 || code == 12)) ++white_home;
        else if (sq >= 56 && (code == 5 || code == 7 || code == 9 || code == 11 || code == 13)) ++black_home;
    }
    if (total <= 3000) return 0;
    if (total <= 4200) return 1;
    const int fullmove = (in.reserved[0] | (int(in.reserved[1]) << 8)) & 0x7fff;
    if (fullmove > 1) return 2 * (fullmove - 1) + (in.flags & 1) < 20 ? 3 : 2;
    return white_home >= 6 && black_home >= 6 ? 3 : 2;
}

// Material sacrifice class seen from the side to move (0 = none).
static int sacrifice_type(const uint8_t board[64], int stm, int side_cp) {
    int queens[2] = {}, rooks[2] = {}, minors[2] = {}, pawns[2] = {};
    for (int sq = 0; sq < 64; ++sq) {
        const uint8_t code = board[sq];
        if (!code) continue;
        const int color = code & 1;
        switch (code / 2) {
            case PieceTypes::Pawn: ++pawns[color]; break;
            case PieceTypes::Knight:
            case PieceTypes::Bishop: ++minors[color]; break;
            case PieceTypes::Rook: ++rooks[color]; break;
            case PieceTypes::Queen: ++queens[color]; break;
            default: break;
        }
    }
    auto material = [&](int c) {
        return queens[c] * MaterialValues[PieceTypes::Queen] + rooks[c] * MaterialValues[PieceTypes::Rook] +
               minors[c] * MaterialValues[PieceTypes::Knight] + pawns[c] * MaterialValues[PieceTypes::Pawn];
    };
    const int me = stm, opp = stm ^ 1;
    const int down = material(opp) - material(me);
    if (down < 80 || side_cp < -30) return 0;
    if (queens[me] < queens[opp]) return 9;
    if (rooks[me] < rooks[opp]) return 5;
    if (minors[me] < minors[opp]) return down >= 350 ? 4 : 3;
    if (pawns[me] < pawns[opp]) return down >= 180 ? 2 : 1;
    return down >= 100 ? 1 : 0;
}

// Horizontal (a<->h) mirror; only defined without castling rights.
static PackedPosition mirrored_record(const PackedPosition& in) {
    uint8_t board[64];
    uint64_t colors_bb[2], pieces_bb[7];
    unpack_board(in, board, colors_bb, pieces_bb);
    PackedPosition out = in;
    std::memset(out.pieces, 0, sizeof(out.pieces));
    out.occupied = 0;
    uint8_t mirrored[64];
    for (int sq = 0; sq < 64; ++sq) mirrored[sq ^ 7] = board[sq];
    int k = 0;
    for (int sq = 0; sq < 64; ++sq) {
        if (!mirrored[sq]) continue;
        out.occupied |= uint64_t(1) << sq;
        out.pieces[k / 2] |= (k % 2 == 0) ? mirrored[sq] : uint8_t(mirrored[sq] << 4);
        ++k;
    }
    uint16_t metadata = in.reserved[0] | (uint16_t(in.reserved[1]) << 8);
    const int ep_file = ep_file_of(in);
    metadata &= 0x7fff;
    out.flags &= 0x1f;
    if (ep_file) {
        const int mirrored_file = 9 - ep_file;
        if (mirrored_file == 8) metadata |= 0x8000;
        else out.flags |= uint8_t(mirrored_file << 5);
    }
    out.reserved[0] = metadata & 255;
    out.reserved[1] = metadata >> 8;
    return out;
}

// Open-addressing set of non-zero 64-bit keys.
class KeySet {
public:
    explicit KeySet(size_t expected) {
        size_t capacity = 16;
        while (capacity < expected * 2 + 16) capacity <<= 1;
        slots_.assign(capacity, 0);
        mask_ = capacity - 1;
    }
    bool insert(uint64_t key) {
        for (size_t i = key & mask_;; i = (i + 1) & mask_) {
            if (slots_[i] == key) return false;
            if (!slots_[i]) {
                slots_[i] = key;
                return true;
            }
        }
    }
private:
    std::vector<uint64_t> slots_;
    size_t mask_;
};

// Values are part of the sbin_screen_batch API.
enum class Verdict : uint8_t { Accept = 0, Invalid = 1, Mate = 2, Check = 3, Tactical = 4 };

// Filters of a record that already passed decode_position.
static Verdict filter_verdict(const PackedPosition& in, uint32_t filters) {
    if ((filters & SBIN_SKIP_MATE) && is_mate_record(in)) return Verdict::Mate;
    if (filters & (SBIN_SKIP_CHECK | SBIN_SKIP_TACTICAL)) {
        const Position pos = engine_position(in);
        if ((filters & SBIN_SKIP_CHECK) && in_check(pos)) return Verdict::Check;
        if ((filters & SBIN_SKIP_TACTICAL) && has_winning_tactic(pos)) return Verdict::Tactical;
    }
    return Verdict::Accept;
}

static Verdict screen(const PackedPosition& in, uint32_t filters, uint8_t board[64]) {
    int count = 0;
    if (!decode_position(&in, board, count)) return Verdict::Invalid;
    return filter_verdict(in, filters);
}

static void count_rejection(Verdict verdict, uint64_t* stats) {
    switch (verdict) {
        case Verdict::Invalid: ++stats[SBIN_STAT_INVALID]; break;
        case Verdict::Mate: ++stats[SBIN_STAT_MATE]; break;
        case Verdict::Check: ++stats[SBIN_STAT_CHECK]; break;
        case Verdict::Tactical: ++stats[SBIN_STAT_TACTICAL]; break;
        case Verdict::Accept: break;
    }
}

// Visit [start, count) then [0, start) until visit() returns false.
template <typename Visit>
static void scan_wrapped(size_t count, size_t start, uint64_t* stats, Visit visit) {
    for (size_t step = 0; step < count; ++step) {
        const size_t row = (start + step) % count;
        ++stats[SBIN_STAT_SCANNED];
        stats[SBIN_STAT_NEXT_OFFSET] = (row + 1) % count;
        if (!visit(row)) return;
    }
}

extern "C" {

int sbin_format_version() { return 2; }

int sbin_pack_fen(const char* fen, float wdl, int16_t eval, PackedPosition* out) {
    if (!fen || !out || !std::isfinite(wdl) || wdl < 0 || wdl > 1) return -1;
    std::memset(out, 0, sizeof(PackedPosition));

    std::istringstream ss(fen);
    std::string board_str, turn_str, castling_str, ep_str;
    int halfmove = 0, fullmove = 1;

    if (!(ss >> board_str >> turn_str >> castling_str >> ep_str) ||
        (turn_str != "w" && turn_str != "b")) return -2;
    ss >> std::ws;
    if (!ss.eof()) {
        std::string extra;
        if (!(ss >> halfmove >> fullmove) || (ss >> extra)) return -2;
    }
    if (halfmove < 0 || halfmove > 255 || fullmove < 1 || fullmove > 32767) return -2;

    uint8_t square_pieces[64];
    std::memset(square_pieces, 0, sizeof(square_pieces));

    int row = 0, col = 0;
    for (char c : board_str) {
        if (c == '/') {
            if (col != 8) return -3;
            row++;
            col = 0;
            if (row > 7) return -3;
        } else if (c >= '1' && c <= '8') {
            col += (c - '0');
            if (col > 8) return -3;
        } else {
            uint8_t code = char_to_piece_code(c);
            if (!code || col >= 8 || row >= 8) return -4;
            int sq = (7 - row) * 8 + col;
            square_pieces[sq] = code;
            col++;
        }
    }
    if (row != 7 || col != 8) return -3;

    uint64_t occ = 0;
    int k = 0;
    for (int sq = 0; sq < 64; ++sq) {
        if (square_pieces[sq]) {
            if (k >= 32) return -5;
            occ |= (1ULL << sq);
            uint8_t code = square_pieces[sq];
            if (k % 2 == 0) {
                out->pieces[k / 2] = code & 0x0F;
            } else {
                out->pieces[k / 2] |= ((code & 0x0F) << 4);
            }
            k++;
        }
    }
    out->occupied = occ;

    uint8_t flags = 0;
    if (turn_str == "b") flags |= 1;

    for (char c : castling_str == "-" ? "" : castling_str) {
        uint8_t before = flags;
        if (c == 'K') flags |= (1 << 1);
        else if (c == 'Q') flags |= (1 << 2);
        else if (c == 'k') flags |= (1 << 3);
        else if (c == 'q') flags |= (1 << 4);
        else return -7;
        if (before == flags) return -7;
    }

    uint16_t metadata = static_cast<uint16_t>(fullmove);
    if (ep_str != "-") {
        if (ep_str.size() != 2 || ep_str[0] < 'a' || ep_str[0] > 'h' ||
            ep_str[1] != (turn_str == "b" ? '3' : '6')) return -7;
        uint8_t ep_file = static_cast<uint8_t>(ep_str[0] - 'a' + 1);
        flags |= ((ep_file & 0x07) << 5);
        if (ep_file == 8) metadata |= 0x8000;
    }
    out->flags = flags;

    out->eval = eval;
    out->wdl = static_cast<uint16_t>(std::round(wdl * 65535.0f));
    out->halfmove = static_cast<uint8_t>(halfmove);
    out->reserved[0] = metadata & 255;
    out->reserved[1] = metadata >> 8;

    uint8_t decoded[64];
    int decoded_count = 0;
    if (!decode_position(out, decoded, decoded_count)) return -8;

    return 0;
}

int sbin_unpack_fen(const PackedPosition* in, char* fen_buf, size_t buf_len, float* out_wdl, int16_t* out_eval) {
    if (!in || !fen_buf || buf_len == 0) return -1;
    uint8_t square_pieces[64];
    int k = 0;
    if (!decode_position(in, square_pieces, k)) return -3;

    std::string fen;
    for (int row = 0; row < 8; ++row) {
        int empty = 0;
        for (int col = 0; col < 8; ++col) {
            int sq = (7 - row) * 8 + col;
            uint8_t code = square_pieces[sq];
            if (code == 0) {
                empty++;
            } else {
                if (empty > 0) {
                    fen += std::to_string(empty);
                    empty = 0;
                }
                fen += piece_code_to_char(code);
            }
        }
        if (empty > 0) fen += std::to_string(empty);
        if (row < 7) fen += '/';
    }

    fen += ((in->flags & 1) ? " b " : " w ");

    std::string castling;
    if (in->flags & (1 << 1)) castling += 'K';
    if (in->flags & (1 << 2)) castling += 'Q';
    if (in->flags & (1 << 3)) castling += 'k';
    if (in->flags & (1 << 4)) castling += 'q';
    if (castling.empty()) castling = "-";
    fen += castling + " ";

    uint8_t ep_file = (in->flags >> 5) & 0x07;
    const uint16_t metadata = in->reserved[0] | (uint16_t(in->reserved[1]) << 8);
    if (metadata & 0x8000) ep_file = 8;
    if (ep_file >= 1 && ep_file <= 8) {
        char file_ch = 'a' + ep_file - 1;
        char rank_ch = (in->flags & 1) ? '3' : '6';
        fen += file_ch;
        fen += rank_ch;
    } else {
        fen += "-";
    }

    fen += " " + std::to_string(in->halfmove) + " " + std::to_string(metadata & 0x7fff);

    if (fen.size() >= buf_len) return -2;
    std::memcpy(fen_buf, fen.c_str(), fen.size() + 1);

    if (out_wdl) *out_wdl = static_cast<float>(in->wdl) / 65535.0f;
    if (out_eval) *out_eval = in->eval;

    return 0;
}

int sbin_nnue_slots() { return NNUE_FEATURE_SLOTS; }

int sbin_nnue_features() { return static_cast<int>(NNUE_INPUT_SIZE); }

int sbin_nnue_base_features() { return static_cast<int>(NNUE_BASE_FEATURES); }

int sbin_feature_block(int index, int* out) {
    if (index < 0 || index >= static_cast<int>(Feature::Count)) return -1;
    const auto& block = FeatureBlocks[index];
    out[0] = static_cast<int>(feature_offset(static_cast<Feature>(index)));
    out[1] = block.outer;
    out[2] = block.inner;
    out[3] = block.cells;
    return 0;
}

int sbin_stat_count() { return SBIN_STAT_COUNT; }

int sbin_extract_nnue(const PackedPosition* in, int16_t* us, int16_t* them, int* out_white_turn) {
    if (!in || !us || !them) return -1;
    uint8_t board[64];
    int count = 0;
    if (!decode_position(in, board, count)) return -1;
    int32_t us_buf[NNUE_FEATURE_SLOTS], them_buf[NNUE_FEATURE_SLOTS];
    int n_us = 0, n_them = 0;
    position_features(*in, us_buf, n_us, them_buf, n_them);
    std::fill_n(us, NNUE_FEATURE_SLOTS, -1);
    std::fill_n(them, NNUE_FEATURE_SLOTS, -1);
    for (int i = 0; i < n_us; ++i) us[i] = static_cast<int16_t>(us_buf[i]);
    for (int i = 0; i < n_them; ++i) them[i] = static_cast<int16_t>(them_buf[i]);
    if (out_white_turn) *out_white_turn = (in->flags & 1) == 0;
    return std::max(n_us, n_them);
}

size_t sbin_validate_batch(const PackedPosition* positions, size_t count, uint8_t* status) {
    if (!positions || !status) return 0;
    size_t valid = 0;
    for (size_t i = 0; i < count; ++i) {
        const auto& record = positions[i];
        uint8_t flags = 0;
        PackedPosition canonical = record;
        const int pieces = __builtin_popcountll(record.occupied);
        const uint16_t metadata = record.reserved[0] | (uint16_t(record.reserved[1]) << 8);
        if (!(metadata & 0x7fff)) {
            flags |= SBIN_MISSING_FULLMOVE;
            canonical.reserved[0] = 1;
        }
        if (pieces <= 32) {
            if ((pieces & 1) && (record.pieces[pieces / 2] & 0xf0)) {
                flags |= SBIN_NONZERO_PADDING;
                canonical.pieces[pieces / 2] &= 0x0f;
            }
            for (int n = (pieces + 1) / 2; n < 16; ++n) {
                if (record.pieces[n]) flags |= SBIN_NONZERO_PADDING;
                canonical.pieces[n] = 0;
            }
        }
        uint8_t board[64];
        int decoded = 0;
        if (!decode_position(&canonical, board, decoded)) flags |= SBIN_INVALID_POSITION;
        status[i] = flags;
        valid += flags == 0;
    }
    return valid;
}

void sbin_screen_batch(const PackedPosition* positions, size_t count, uint32_t filters, uint8_t* verdicts) {
    uint8_t board[64];
    for (size_t i = 0; i < count; ++i) verdicts[i] = static_cast<uint8_t>(screen(positions[i], filters, board));
}

void sbin_group_keys(const PackedPosition* positions, size_t count, uint64_t* keys) {
    for (size_t i = 0; i < count; ++i) keys[i] = group_key(positions[i]);
}

void sbin_calibrate_labels(PackedPosition* positions, size_t count, double lambda) {
    for (size_t i = 0; i < count; ++i) positions[i].wdl = calibrated_wdl(positions[i], lambda);
}

long long sbin_build_batch(const PackedPosition* records, size_t record_count,
                           const int64_t* rows, size_t count, float dropout, uint64_t seed,
                           int32_t* indices, int32_t* offsets,
                           int32_t* t_bags, int32_t* t_offsets,
                           int64_t* buckets, float* targets) {
    std::vector<int32_t> them(count * NNUE_FEATURE_SLOTS);
    std::vector<int> them_count(count);
    const uint64_t keep = dropout > 0.0f ? uint64_t((1.0 - double(dropout)) * 18446744073709551615.0) : 0;
    size_t nnz = 0;
    auto keep_feature = [&](uint64_t& state) {
        state += 0x9e3779b97f4a7c15ULL;
        return mix64(state) <= keep;
    };
    for (size_t i = 0; i < count; ++i) {
        const int64_t row = rows[i];
        if (row < 0 || static_cast<size_t>(row) >= record_count) return -1;
        const PackedPosition& record = records[row];
        int32_t us[NNUE_FEATURE_SLOTS];
        int n_us = 0, n_them = 0;
        position_features(record, us, n_us, them.data() + i * NNUE_FEATURE_SLOTS, n_them);
        offsets[i] = static_cast<int32_t>(nnz);
        uint64_t state = mix64(seed ^ (uint64_t(row) * 0x9e3779b97f4a7c15ULL));
        for (int k = 0; k < n_us; ++k)
            if (!keep || keep_feature(state)) indices[nnz++] = us[k];
        int kept = 0;
        int32_t* other = them.data() + i * NNUE_FEATURE_SLOTS;
        for (int k = 0; k < n_them; ++k)
            if (!keep || keep_feature(state)) other[kept++] = other[k];
        them_count[i] = kept;
        const int pieces = __builtin_popcountll(record.occupied);
        buckets[i] = std::clamp((pieces - 1) / 2, 0, static_cast<int>(OUTPUT_BUCKETS) - 1);
        const float wdl = record.wdl / 65535.0f;
        targets[i] = (record.flags & 1) ? 1.0f - wdl : wdl;
    }
    for (size_t i = 0; i < count; ++i) {
        offsets[count + i] = static_cast<int32_t>(nnz);
        std::copy_n(them.data() + i * NNUE_FEATURE_SLOTS, them_count[i], indices + nnz);
        nnz += them_count[i];
    }
    offsets[2 * count] = static_cast<int32_t>(nnz);
    std::fill_n(t_offsets, NNUE_INPUT_SIZE + 1, 0);
    for (size_t e = 0; e < nnz; ++e) ++t_offsets[indices[e] + 1];
    for (size_t f = 0; f < NNUE_INPUT_SIZE; ++f) t_offsets[f + 1] += t_offsets[f];
    std::vector<int32_t> cursor(t_offsets, t_offsets + NNUE_INPUT_SIZE);
    for (size_t bag = 0; bag < 2 * count; ++bag)
        for (int32_t e = offsets[bag]; e < offsets[bag + 1]; ++e)
            t_bags[cursor[indices[e]]++] = static_cast<int32_t>(bag);
    return static_cast<long long>(nnz);
}

long long sbin_select_base(const PackedPosition* source, size_t count, size_t start,
                           const uint64_t quotas[4], uint32_t filters, int labels_cp,
                           double lambda, PackedPosition* out, uint64_t* stats) {
    std::fill_n(stats, SBIN_STAT_COUNT, 0);
    const uint64_t target = quotas[0] + quotas[1] + quotas[2] + quotas[3];
    if (!count || start >= count) return -1;
    KeySet seen(target);
    uint64_t taken[4] = {};
    uint64_t selected = 0;
    scan_wrapped(count, start, stats, [&](size_t row) {
        const PackedPosition& record = source[row];
        uint8_t board[64];
        int pieces = 0;
        if (!decode_position(&record, board, pieces)) {
            ++stats[SBIN_STAT_INVALID];
            return true;
        }
        const int phase = position_phase(record, board);
        if (taken[phase] >= quotas[phase]) return true;
        const Verdict verdict = filter_verdict(record, filters);
        if (verdict != Verdict::Accept) {
            count_rejection(verdict, stats);
            return true;
        }
        if (!seen.insert(group_key(record))) {
            ++stats[SBIN_STAT_DUPLICATE];
            return true;
        }
        out[selected] = record;
        if (labels_cp) out[selected].wdl = calibrated_wdl(record, lambda);
        ++selected;
        ++taken[phase];
        return selected < target;
    });
    stats[SBIN_STAT_SELECTED] = selected;
    for (int p = 0; p < 4; ++p) stats[SBIN_STAT_ENDGAME + p] = taken[p];
    return static_cast<long long>(selected);
}

long long sbin_select_aggressive(const PackedPosition* puzzles, const int64_t* puzzle_rows,
                                 size_t puzzle_count, const PackedPosition* source, size_t count,
                                 size_t start, uint64_t target, double sac_ratio,
                                 int augment_mirror, uint32_t filters, int labels_cp,
                                 double lambda, PackedPosition* out, uint64_t* stats) {
    std::fill_n(stats, SBIN_STAT_COUNT, 0);
    if (!target || (count && start >= count)) return -1;
    KeySet seen(target);
    uint64_t selected = 0;
    auto add = [&](const PackedPosition& record) {
        if (selected >= target || !seen.insert(group_key(record))) return false;
        out[selected++] = record;
        return true;
    };
    for (size_t i = 0; i < puzzle_count && selected < target; ++i) {
        const PackedPosition& record = puzzles[puzzle_rows[i]];
        uint8_t board[64];
        const Verdict verdict = screen(record, filters, board);
        if (verdict != Verdict::Accept) {
            count_rejection(verdict, stats);
            continue;
        }
        if (add(record)) ++stats[SBIN_STAT_PUZZLES];
        else ++stats[SBIN_STAT_DUPLICATE];
    }
    const uint64_t puzzles_added = stats[SBIN_STAT_PUZZLES];
    const uint64_t target_sacs = std::min<uint64_t>(
        target, std::max<uint64_t>(uint64_t(double(target) * sac_ratio), puzzles_added));
    const uint64_t target_sharp = target - target_sacs;
    uint64_t sacs = puzzles_added, sharp = 0;
    const int sac_slot[10] = {-1, SBIN_STAT_SAC1, SBIN_STAT_SAC2, SBIN_STAT_SAC3, SBIN_STAT_SAC4,
                              SBIN_STAT_SAC5, -1, -1, -1, SBIN_STAT_SAC9};
    if (count && selected < target) {
        scan_wrapped(count, start, stats, [&](size_t row) {
            PackedPosition record = source[row];
            uint8_t board[64];
            int pieces = 0;
            if (!decode_position(&record, board, pieces)) {
                ++stats[SBIN_STAT_INVALID];
                return true;
            }
            const int stm = record.flags & 1;
            const int side_cp = stm ? -record.eval : record.eval;
            const int type = sacrifice_type(board, stm, side_cp);
            const bool is_sharp = position_phase(record, board) != 0 && std::abs(side_cp) > 50;
            uint64_t* bucket = nullptr;
            uint64_t limit = 0;
            if (type && sacs < target_sacs) {
                bucket = &sacs;
                limit = target_sacs;
            } else if (is_sharp && sharp < target_sharp) {
                bucket = &sharp;
                limit = target_sharp;
            } else {
                return true;
            }
            const Verdict verdict = filter_verdict(record, filters);
            if (verdict != Verdict::Accept) {
                count_rejection(verdict, stats);
                return true;
            }
            if (labels_cp) record.wdl = calibrated_wdl(record, lambda);
            if (!add(record)) {
                ++stats[SBIN_STAT_DUPLICATE];
                return true;
            }
            ++*bucket;
            if (bucket == &sacs) ++stats[sac_slot[type]];
            if (augment_mirror && *bucket < limit && !(record.flags & 0x1e)) {
                const PackedPosition mirror = mirrored_record(record);
                uint8_t mirror_board[64];
                int mirror_count = 0;
                if (!decode_position(&mirror, mirror_board, mirror_count)) std::abort();
                if (add(mirror)) {
                    ++*bucket;
                    ++stats[SBIN_STAT_MIRRORED];
                }
            }
            return selected < target;
        });
    }
    stats[SBIN_STAT_SELECTED] = selected;
    stats[SBIN_STAT_SACRIFICES] = sacs;
    stats[SBIN_STAT_SHARP] = sharp;
    return static_cast<long long>(selected);
}

}

#include "sbin.h"
#include "../engine/src/bitboard.h"
#include "../engine/src/nnue.h"
static_assert(SBIN_NNUE_SLOTS == NNUE_EXTRA_SLOTS);
#include <algorithm>
#include <cmath>
#include <sstream>

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

// King buckets and feature indices come straight from the engine (nnue.h),
// so the native decoder cannot drift out of sync with training or search.
int sbin_extract_nnue(const PackedPosition* in, int16_t* us, int16_t* them, int* out_white_turn) {
    if (!in || !us || !them) return -1;
    uint8_t board[64];
    int occupied_count = 0;
    if (!decode_position(in, board, occupied_count)) return -1;
    uint64_t occ = in->occupied;
    int count = 0;
    bool white_turn = ((in->flags & 1) == 0);
    if (out_white_turn) *out_white_turn = white_turn ? 1 : 0;

    std::fill_n(us, SBIN_NNUE_SLOTS, -1);
    std::fill_n(them, SBIN_NNUE_SLOTS, -1);

    int wking_sq = -1, bking_sq = -1;
    uint64_t occ_scan = occ;
    while (occ_scan) {
        int sq = __builtin_ctzll(occ_scan);
        occ_scan &= occ_scan - 1;
        uint8_t code = board[sq];
        if (code == 12) wking_sq = sq;
        else if (code == 13) bking_sq = sq;
    }
    if (wking_sq < 0 || bking_sq < 0) return -1;

    size_t w_bucket = static_cast<size_t>(KingBucketTable[wking_sq]);
    size_t b_bucket = static_cast<size_t>(KingBucketTable[bking_sq ^ 56]);

    while (occ) {
        int sq = __builtin_ctzll(occ);
        occ &= occ - 1;
        const uint8_t code = board[sq];

        const auto [white_idx, black_idx] =
            feature_indices(code, sq, static_cast<int>(w_bucket), static_cast<int>(b_bucket));

        if (white_turn) {
            us[count] = static_cast<int16_t>(white_idx);
            them[count] = static_cast<int16_t>(black_idx);
        } else {
            us[count] = static_cast<int16_t>(black_idx);
            them[count] = static_cast<int16_t>(white_idx);
        }
        count++;
    }

    uint64_t colors_bb[2] = {0, 0};
    uint64_t pieces_bb[7] = {0, 0, 0, 0, 0, 0, 0};
    for (int sq = 0; sq < 64; ++sq) {
        const uint8_t code = board[sq];
        if (code < 2 || code > 13) continue;
        colors_bb[code & 1] |= (1ULL << sq);
        pieces_bb[code / 2] |= (1ULL << sq);
    }
    int extra_w[SBIN_NNUE_SLOTS], extra_b[SBIN_NNUE_SLOTS];
    const int nw = collect_extra_features(board, colors_bb, pieces_bb, false, extra_w, SBIN_NNUE_SLOTS);
    const int nb = mirror_extra_features(extra_w, nw, extra_b, SBIN_NNUE_SLOTS);
    if (nw < 0 || nb < 0) return -1;
    if (count + nw > SBIN_NNUE_SLOTS || count + nb > SBIN_NNUE_SLOTS) return -1;
    const int* first = white_turn ? extra_w : extra_b;
    const int* second = white_turn ? extra_b : extra_w;
    const int n_first = white_turn ? nw : nb;
    const int n_second = white_turn ? nb : nw;
    for (int i = 0; i < n_first; ++i) us[count + i] = static_cast<int16_t>(first[i]);
    for (int i = 0; i < n_second; ++i) them[count + i] = static_cast<int16_t>(second[i]);
    return count + (n_first > n_second ? n_first : n_second);
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

size_t sbin_batch_decode_indexed(
    const PackedPosition* in_positions,
    size_t count,
    int16_t* out_features,
    float* out_targets,
    uint32_t* out_indices
) {
    if (!in_positions || !out_features || !out_targets) return 0;

    size_t decoded = 0;
    for (size_t i = 0; i < count; ++i) {
        const PackedPosition& pos = in_positions[i];
        int16_t* feat_us = out_features + (decoded * SBIN_NNUE_SLOTS * 2);
        int16_t* feat_them = feat_us + SBIN_NNUE_SLOTS;

        int white_turn = 1;
        int pieces = sbin_extract_nnue(&pos, feat_us, feat_them, &white_turn);
        if (pieces < 2) continue;

        float wdl_val = static_cast<float>(pos.wdl) / 65535.0f;
        out_targets[decoded] = white_turn ? wdl_val : (1.0f - wdl_val);
        if (out_indices) out_indices[decoded] = static_cast<uint32_t>(i);
        decoded++;
    }
    return decoded;
}

size_t sbin_batch_decode(
    const PackedPosition* in_positions,
    size_t count,
    int16_t* out_features,
    float* out_targets
) {
    return sbin_batch_decode_indexed(in_positions, count, out_features, out_targets, nullptr);
}

int sbin_classify_phase(const PackedPosition* pos) {
    uint8_t board[64];
    int piece_count = 0;
    if (!decode_position(pos, board, piece_count)) return -1;
    int total_mat = 0;
    int white_home = 0;
    int black_home = 0;
    uint64_t occ = pos->occupied;
    while (occ) {
        int sq = __builtin_ctzll(occ);
        occ &= occ - 1;
        uint8_t code = board[sq];
        if (code < 12) total_mat += MaterialValues[code / 2];
        if (sq < 8 && (code == 4 || code == 6 || code == 8 || code == 10 || code == 12)) {
            white_home++;
        } else if (sq >= 56 && (code == 5 || code == 7 || code == 9 || code == 11 || code == 13)) {
            black_home++;
        }
    }
    if (total_mat <= 3000) return 0;
    if (total_mat <= 4200) return 1;
    const int fullmove = (pos->reserved[0] | (int(pos->reserved[1]) << 8)) & 0x7fff;
    if (fullmove > 1) return 2 * (fullmove - 1) + (pos->flags & 1) < 20 ? 3 : 2;
    return white_home >= 6 && black_home >= 6 ? 3 : 2;
}

void sbin_classify_phases_batch(const PackedPosition* positions, size_t count, uint8_t* out_phases) {
    if (!positions || !out_phases) return;
    for (size_t i = 0; i < count; ++i) {
        out_phases[i] = static_cast<uint8_t>(sbin_classify_phase(&positions[i]));
    }
}

}

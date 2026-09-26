#pragma once
#include <cstdint>
#include <cstddef>
#include <cstring>
#include <string>

#pragma pack(push, 1)
struct PackedPosition {
    uint64_t occupied;
    uint8_t  pieces[16];
    int16_t  eval;
    uint16_t wdl;
    uint8_t  flags;
    uint8_t  halfmove;
    uint8_t  reserved[2];
};
#pragma pack(pop)

static_assert(sizeof(PackedPosition) == 32, "PackedPosition must be exactly 32 bytes");

enum SbinValidationStatus {
    SBIN_MISSING_FULLMOVE = 1,
    SBIN_NONZERO_PADDING = 2,
    SBIN_INVALID_POSITION = 4,
};

// Candidate filters for the selection functions (bit mask).
enum SbinFilter {
    SBIN_SKIP_MATE = 1,      // cp == +-2000 mate sentinel with a decisive stored WDL
    SBIN_SKIP_CHECK = 2,     // side to move is in check
    SBIN_SKIP_TACTICAL = 4,  // side to move has a capture or promotion with SEE >= 1
};

// Layout of the uint64_t stats[SBIN_STAT_COUNT] array filled by selection.
enum SbinStat {
    SBIN_STAT_SELECTED,
    SBIN_STAT_INVALID,
    SBIN_STAT_DUPLICATE,
    SBIN_STAT_MATE,
    SBIN_STAT_CHECK,
    SBIN_STAT_TACTICAL,
    SBIN_STAT_SCANNED,
    SBIN_STAT_NEXT_OFFSET,
    SBIN_STAT_ENDGAME,
    SBIN_STAT_LATE_MIDDLE,
    SBIN_STAT_MIDGAME,
    SBIN_STAT_OPENING,
    SBIN_STAT_PUZZLES,
    SBIN_STAT_SACRIFICES,
    SBIN_STAT_SHARP,
    SBIN_STAT_MIRRORED,
    SBIN_STAT_SAC1,
    SBIN_STAT_SAC2,
    SBIN_STAT_SAC3,
    SBIN_STAT_SAC4,
    SBIN_STAT_SAC5,
    SBIN_STAT_SAC9,
    SBIN_STAT_COUNT
};

#ifdef __cplusplus
extern "C" {
#endif

int sbin_format_version();
int sbin_nnue_slots();
int sbin_nnue_features();
int sbin_nnue_base_features();
// Extra feature block i: {offset, outer, inner, cells}; returns -1 past the end.
int sbin_feature_block(int index, int* out);
int sbin_stat_count();

int sbin_pack_fen(const char* fen, float wdl, int16_t eval, PackedPosition* out);

int sbin_unpack_fen(const PackedPosition* in, char* fen_buf, size_t buf_len, float* out_wdl, int16_t* out_eval);

// Padded [sbin_nnue_slots()] feature lists of one validated record (-1 = empty slot).
int sbin_extract_nnue(const PackedPosition* in, int16_t* us, int16_t* them, int* out_white_turn);

size_t sbin_validate_batch(const PackedPosition* positions, size_t count, uint8_t* status);

// Per-record verdict: 0 accept, 1 invalid, 2 mate, 3 check, 4 tactical.
void sbin_screen_batch(const PackedPosition* positions, size_t count, uint32_t filters, uint8_t* verdicts);

// Position identity as seen by the network: the board from the side to move's
// point of view. Color-flipped twins share a key.
void sbin_group_keys(const PackedPosition* positions, size_t count, uint64_t* keys);

// Rewrite the WDL field from the cp field (400 scale) blended with the stored WDL.
void sbin_calibrate_labels(PackedPosition* positions, size_t count, double lambda);

// One training batch of validated records in CSR form. Bags [0, count) hold the
// side-to-move features, bags [count, 2 * count) the other side. t_bags/t_offsets
// is the same matrix grouped by feature for the gradient. Returns nnz or -1.
long long sbin_build_batch(const PackedPosition* records, size_t record_count,
                           const int64_t* rows, size_t count, float dropout, uint64_t seed,
                           int32_t* indices, int32_t* offsets,
                           int32_t* t_bags, int32_t* t_offsets,
                           int64_t* buckets, float* targets);

// Phase-quota selection (endgame, late_middle, midgame, opening) scanning
// [start, count) then [0, start). Returns the number of records written to out.
long long sbin_select_base(const PackedPosition* source, size_t count, size_t start,
                           const uint64_t quotas[4], uint32_t filters, int labels_cp,
                           double lambda, PackedPosition* out, uint64_t* stats);

// Puzzle rows first, then sacrifice/sharp positions from the source (count may be 0).
long long sbin_select_aggressive(const PackedPosition* puzzles, const int64_t* puzzle_rows,
                                 size_t puzzle_count, const PackedPosition* source, size_t count,
                                 size_t start, uint64_t target, double sac_ratio,
                                 int augment_mirror, uint32_t filters, int labels_cp,
                                 double lambda, PackedPosition* out, uint64_t* stats);

#ifdef __cplusplus
}
#endif

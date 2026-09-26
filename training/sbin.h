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

enum { SBIN_NNUE_SLOTS = 192 };

#ifdef __cplusplus
extern "C" {
#endif

int sbin_format_version();

int sbin_pack_fen(const char* fen, float wdl, int16_t eval, PackedPosition* out);

int sbin_unpack_fen(const PackedPosition* in, char* fen_buf, size_t buf_len, float* out_wdl, int16_t* out_eval);

int sbin_extract_nnue(const PackedPosition* in, int16_t* us, int16_t* them, int* out_white_turn);

size_t sbin_batch_decode(
    const PackedPosition* in_positions,
    size_t count,
    int16_t* out_features,
    float* out_targets
);

size_t sbin_batch_decode_indexed(
    const PackedPosition* in_positions,
    size_t count,
    int16_t* out_features,
    float* out_targets,
    uint32_t* out_indices
);

enum SbinValidationStatus {
    SBIN_MISSING_FULLMOVE = 1,
    SBIN_NONZERO_PADDING = 2,
    SBIN_INVALID_POSITION = 4,
};

size_t sbin_validate_batch(const PackedPosition* positions, size_t count, uint8_t* status);

int sbin_classify_phase(const PackedPosition* pos);
void sbin_classify_phases_batch(const PackedPosition* positions, size_t count, uint8_t* out_phases);

#ifdef __cplusplus
}
#endif

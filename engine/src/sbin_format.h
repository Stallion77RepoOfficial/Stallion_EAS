#pragma once
#include <cstdint>

#pragma pack(push, 1)
struct PackedPosition {
    uint64_t occupied;
    uint8_t pieces[32]; // two codes per byte, enough for every board square
    int64_t eval;
    int64_t wdl;
    int64_t flags;
    int64_t halfmove;
    int64_t fullmove;
    int64_t ep_file;
    int64_t stored_wdl; // original label retained for optional CP/result blends
};
#pragma pack(pop)

static_assert(sizeof(PackedPosition) == 96);


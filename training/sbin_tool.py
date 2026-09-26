#!/usr/bin/env python3
"""Stallion SBIN: 32-byte binary chess positions and their native codec.

- Fixed 32 bytes per position, O(1) random access, zero-copy mmap reads.
- Native C++ library (sbin.cpp) built from the engine headers: validation,
  NNUE feature batches, selection and label calibration.
- A pure Python feature oracle to cross-check the native decoder.
"""

from __future__ import annotations

import argparse
import ctypes
from ctypes import POINTER, Structure, c_char_p, c_double, c_float, c_int, c_int16, c_longlong, c_size_t, c_uint32, c_uint64, c_void_p
import math
import operator
from pathlib import Path
import sys
import time
import uuid
from functools import lru_cache

import numpy as np

if __package__:
    from . import stallion as workflow
else:
    import stallion as workflow

ROOT = Path(__file__).resolve().parent
FORMAT_VERSION = 2
DYLIB_PATH = ROOT / ("libstallion_sbin.dylib" if sys.platform == "darwin" else "libstallion_sbin.so")

# Mirrors of the enums in sbin.h.
SBIN_MISSING_FULLMOVE = 1
SBIN_NONZERO_PADDING = 2
SBIN_INVALID_POSITION = 4
SBIN_STATUS_FLAGS = ((SBIN_MISSING_FULLMOVE, "missing_fullmove"),
                     (SBIN_NONZERO_PADDING, "nonzero_padding"),
                     (SBIN_INVALID_POSITION, "invalid_position"))
FILTERS = {"mate": 1, "check": 2, "tactical": 4}
RECORD = np.dtype((np.void, 32))
STAT_NAMES = (
    "selected", "invalid", "duplicate", "mate", "check", "tactical", "scanned", "next_offset",
    "endgame", "late_middle", "midgame", "opening", "puzzles", "sacrifices", "sharp", "mirrored",
    "sac1", "sac2", "sac3", "sac4", "sac5", "sac9",
)


class PackedPosition(Structure):
    _pack_ = 1
    _fields_ = [
        ("occupied", ctypes.c_uint64),
        ("pieces", ctypes.c_uint8 * 16),
        ("eval", ctypes.c_int16),
        ("wdl", ctypes.c_uint16),
        ("flags", ctypes.c_uint8),
        ("halfmove", ctypes.c_uint8),
        ("reserved", ctypes.c_uint8 * 2),
    ]


assert ctypes.sizeof(PackedPosition) == 32, "PackedPosition must be exactly 32 bytes"


@lru_cache(maxsize=1)
def load_native_lib():
    sources = [ROOT / name for name in ("sbin.cpp", "sbin.h", "Makefile")]
    sources += [ROOT.parent / "engine" / "src" / name for name in ("bitboard.h", "nnue.h", "defs.h")]
    if not DYLIB_PATH.is_file() or any(p.stat().st_mtime_ns > DYLIB_PATH.stat().st_mtime_ns for p in sources):
        workflow.run_checked(["make", "-C", str(ROOT)])
    lib = ctypes.CDLL(str(DYLIB_PATH))
    signatures = {
        "sbin_format_version": (c_int, []),
        "sbin_nnue_slots": (c_int, []),
        "sbin_nnue_features": (c_int, []),
        "sbin_nnue_base_features": (c_int, []),
        "sbin_feature_block": (c_int, [c_int, POINTER(c_int)]),
        "sbin_stat_count": (c_int, []),
        "sbin_pack_fen": (c_int, [c_char_p, c_float, c_int16, POINTER(PackedPosition)]),
        "sbin_unpack_fen": (c_int, [POINTER(PackedPosition), c_char_p, c_size_t, POINTER(c_float), POINTER(c_int16)]),
        "sbin_extract_nnue": (c_int, [POINTER(PackedPosition), POINTER(c_int16), POINTER(c_int16), POINTER(c_int)]),
        "sbin_validate_batch": (c_size_t, [c_void_p, c_size_t, c_void_p]),
        "sbin_screen_batch": (None, [c_void_p, c_size_t, c_uint32, c_void_p]),
        "sbin_group_keys": (None, [c_void_p, c_size_t, c_void_p]),
        "sbin_calibrate_labels": (None, [c_void_p, c_size_t, c_double]),
        "sbin_build_batch": (c_longlong, [c_void_p, c_size_t, c_void_p, c_size_t, c_float, c_uint64,
                                          c_void_p, c_void_p, c_void_p, c_void_p, c_void_p, c_void_p]),
        "sbin_select_base": (c_longlong, [c_void_p, c_size_t, c_size_t, c_void_p, c_uint32, c_int,
                                          c_double, c_void_p, c_void_p]),
        "sbin_select_aggressive": (c_longlong, [c_void_p, c_void_p, c_size_t, c_void_p, c_size_t, c_size_t,
                                                c_uint64, c_double, c_int, c_uint32, c_int, c_double,
                                                c_void_p, c_void_p]),
    }
    for name, (restype, argtypes) in signatures.items():
        function = getattr(lib, name)
        function.restype = restype
        function.argtypes = argtypes
    if lib.sbin_format_version() != FORMAT_VERSION:
        raise RuntimeError("SBIN kütüphane sürümü uyuşmuyor; make -C training çalıştırın.")
    if lib.sbin_stat_count() != len(STAT_NAMES):
        raise RuntimeError("SBIN kütüphanesi ile sbin_tool.py istatistik düzeni uyuşmuyor.")
    return lib


def nnue_slots() -> int:
    return load_native_lib().sbin_nnue_slots()


def pointer(array) -> int:
    """Address of a C-contiguous numpy array for the native calls."""
    if not array.flags["C_CONTIGUOUS"]:
        raise ValueError("Native çağrı için C-bitişik dizi gerekli.")
    return array.ctypes.data


def filter_mask(names) -> int:
    mask = 0
    for name in names:
        if name not in FILTERS:
            raise ValueError(f"Bilinmeyen filtre: {name}; geçerli: {', '.join(FILTERS)}")
        mask |= FILTERS[name]
    return mask


def stats_dict(stats) -> dict[str, int]:
    return {name: int(value) for name, value in zip(STAT_NAMES, stats)}


class SbinDataset:
    """Read-only memory-mapped SBIN dataset; views stay valid after close()."""

    def __init__(self, sbin_path: Path):
        self.path = Path(sbin_path).resolve()
        file_size = self.path.stat().st_size
        if not file_size or file_size % 32 != 0:
            raise ValueError(f"Geçersiz SBIN dosya boyutu: {file_size} (32'nin katı olmalı)")
        self.count = file_size // 32
        self.lib = load_native_lib()
        self._records = np.memmap(self.path, dtype=RECORD, mode="r")

    def __len__(self) -> int:
        return self.count

    def records(self):
        """One 32-byte void record per row."""
        return self._records

    def _position(self, index: int) -> PackedPosition:
        index = operator.index(index)
        if index < 0 or index >= self.count:
            raise IndexError(f"SBIN indeks aralık dışında: {index}")
        return PackedPosition.from_buffer_copy(self._records[index].tobytes())

    def get_fen(self, index: int) -> tuple[str, float, int]:
        pos = self._position(index)
        buf = ctypes.create_string_buffer(128)
        wdl = c_float()
        eval_cp = c_int16()
        ret = self.lib.sbin_unpack_fen(ctypes.byref(pos), buf, 128, ctypes.byref(wdl), ctypes.byref(eval_cp))
        if ret != 0:
            raise RuntimeError(f"Unpack hatası (code {ret}); kayıt={index}")
        return buf.value.decode("utf-8"), wdl.value, eval_cp.value

    def eval_view(self):
        """int16 view of the packed white-POV cp field (offset 24, stride 32)."""
        return np.ndarray((self.count,), dtype="<i2", buffer=self._records, offset=24, strides=(32,))

    def get_nnue_features(self, index: int) -> tuple[list[int], list[int], bool]:
        pos = self._position(index)
        slots = self.lib.sbin_nnue_slots()
        us = (c_int16 * slots)()
        them = (c_int16 * slots)()
        white_turn = c_int()
        total = self.lib.sbin_extract_nnue(ctypes.byref(pos), us, them, ctypes.byref(white_turn))
        if total < 0:
            raise RuntimeError(f"NNUE extract hatası; kayıt={index}")
        return ([x for x in us if x >= 0], [x for x in them if x >= 0], bool(white_turn.value))

    def close(self):
        self._records = None

    def __enter__(self):
        return self

    def __exit__(self, *_):
        self.close()


def convert_parquet_to_sbin(parquet_path: Path, sbin_path: Path) -> dict:
    """Pack a (fen, wdl) Parquet table; any invalid row stops the conversion."""
    import pyarrow.parquet as pq
    lib = load_native_lib()
    parquet_path = Path(parquet_path).resolve()
    sbin_path = Path(sbin_path).resolve()
    if parquet_path == sbin_path:
        raise ValueError("Parquet ve SBIN çıktı yolları farklı olmalı.")
    started = time.perf_counter()
    source_before = workflow.file_identity(parquet_path)
    packed = PackedPosition()
    rows = 0
    sbin_path.parent.mkdir(parents=True, exist_ok=True)
    temporary = sbin_path.with_name(f".{sbin_path.name}.{uuid.uuid4().hex}.tmp")
    try:
        with open(temporary, "wb") as out_f:
            for batch in pq.ParquetFile(parquet_path).iter_batches(batch_size=65536, columns=["fen", "wdl"]):
                data = bytearray()
                for fen, wdl in zip(batch.column(0).to_pylist(), batch.column(1).to_pylist()):
                    value = float(wdl)
                    if not math.isfinite(value) or not 0.0 <= value <= 1.0:
                        raise ValueError(f"Geçersiz WDL; satır={rows}")
                    fen = workflow.normalize_engine_fen(fen)
                    status = lib.sbin_pack_fen(fen.encode("utf-8"), c_float(value), c_int16(0), ctypes.byref(packed))
                    if status:
                        raise ValueError(f"Paketlenemeyen FEN (kod {status}); satır={rows}: {fen}")
                    data.extend(bytes(packed))
                    rows += 1
                out_f.write(data)
        if not rows:
            raise ValueError("Parquet dosyasında satır yok.")
        if workflow.file_identity(parquet_path) != source_before:
            raise RuntimeError("Parquet dönüşüm sırasında değişti; çıktı yayımlanmadı.")
        temporary.replace(sbin_path)
    finally:
        temporary.unlink(missing_ok=True)
    report = {"version": FORMAT_VERSION, "source": source_before,
              "output": workflow.file_identity(sbin_path), "rows": rows,
              "seconds": round(time.perf_counter() - started, 3)}
    workflow.atomic_json(sbin_path.with_suffix(".sbin.json"), report)
    return report


# ---- Pure Python feature oracle (independent of the native decoder) ----

KING_BUCKET_TABLE = [
    0, 1, 2, 3, 3, 2, 1, 0,
    0, 1, 2, 3, 3, 2, 1, 0,
    4, 5, 6, 7, 7, 6, 5, 4,
    4, 5, 6, 7, 7, 6, 5, 4,
    8, 9, 10, 11, 11, 10, 9, 8,
    8, 9, 10, 11, 11, 10, 9, 8,
    12, 13, 14, 15, 15, 14, 13, 12,
    12, 13, 14, 15, 15, 14, 13, 12,
]
PIECE_CODES = {"P": 2, "p": 3, "N": 4, "n": 5, "B": 6, "b": 7,
               "R": 8, "r": 9, "Q": 10, "q": 11, "K": 12, "k": 13}
FEATURES_PER_KING_BUCKET = 12 * 64
FEATURES_PER_COLOR = 6 * 64
MATERIAL, ZONE_OCC, ZONE_ATK, PAWN, ROOKFILE, COMPLEX = range(6)


@lru_cache(maxsize=1)
def feature_blocks() -> list[tuple[int, int, int, int]]:
    """(offset, outer, inner, cells) of each extra block, from the engine layout."""
    lib = load_native_lib()
    blocks = []
    out = (c_int * 4)()
    while lib.sbin_feature_block(len(blocks), out) == 0:
        blocks.append(tuple(out))
    return blocks


def _extra(block: int, outer: int, inner: int, cell: int) -> int:
    offset, _, inner_count, cells = feature_blocks()[block]
    return offset + (outer * inner_count + inner) * cells + cell


def _step_table(steps) -> list[int]:
    table = [0] * 64
    for sq in range(64):
        f, r = sq & 7, sq >> 3
        for df, dr in steps:
            tf, tr = f + df, r + dr
            if 0 <= tf < 8 and 0 <= tr < 8:
                table[sq] |= 1 << (tr * 8 + tf)
    return table


_PAWN_ATK = [_step_table(((-1, 1), (1, 1))), _step_table(((-1, -1), (1, -1)))]
_KNIGHT_ATK = _step_table(((-2, -1), (-2, 1), (-1, 2), (1, 2), (2, 1), (2, -1), (1, -2), (-1, -2)))
_KING_ATK = _step_table([(df, dr) for df in (-1, 0, 1) for dr in (-1, 0, 1) if df or dr])


def _slider_attacked(sq: int, occ: int, pieces: list[int], colors: list[int],
                     color: int, diagonal: bool) -> bool:
    f, r = sq & 7, sq >> 3
    if diagonal:
        dirs = ((1, 1), (-1, 1), (1, -1), (-1, -1))
        want = (pieces[3] | pieces[5]) & colors[color]
    else:
        dirs = ((1, 0), (-1, 0), (0, 1), (0, -1))
        want = (pieces[4] | pieces[5]) & colors[color]
    for df, dr in dirs:
        tf, tr = f + df, r + dr
        while 0 <= tf < 8 and 0 <= tr < 8:
            t = tr * 8 + tf
            if (occ >> t) & 1:
                if (want >> t) & 1:
                    return True
                break
            tf += df
            tr += dr
    return False


def _attacked_by(sq: int, color: int, occ: int, colors: list[int], pieces: list[int]) -> bool:
    return bool(_PAWN_ATK[color ^ 1][sq] & pieces[1] & colors[color] or
                _KNIGHT_ATK[sq] & pieces[2] & colors[color] or
                _slider_attacked(sq, occ, pieces, colors, color, True) or
                _slider_attacked(sq, occ, pieces, colors, color, False) or
                _KING_ATK[sq] & pieces[6] & colors[color])


def _collect_extra_view(board: list[int], colors: list[int], pieces: list[int],
                        wking: int, bking: int, flip: bool) -> list[int]:
    """Python rendering of engine collect_extra_features()."""
    out: list[int] = []
    kings = (wking, bking)
    mat_count = [[0] * 5 for _ in range(2)]
    complex_count = [[0] * 2 for _ in range(2)]
    for sq in range(64):
        piece = board[sq]
        if piece < 2 or piece > 13:
            continue
        color = piece & 1
        base = (piece >> 1) - 1
        if base <= 4:
            mat_count[color][base] += 1
        if base == 0:
            complex_count[color][((sq & 7) + (sq >> 3)) & 1] += 1
    for slot_side in range(2):
        real_side = slot_side ^ 1 if flip else slot_side
        for typ in range(5):
            out.append(_extra(MATERIAL, slot_side, typ, min(mat_count[real_side][typ], 9)))
    for slot_king in range(2):
        real_king = slot_king ^ 1 if flip else slot_king
        king_view = kings[real_king] ^ 56 if flip else kings[real_king]
        kf, kr = king_view & 7, king_view >> 3
        for off in range(9):
            tf, tr = kf + (off % 3) - 1, kr + (off // 3) - 1
            if tf < 0 or tf > 7 or tr < 0 or tr > 7:
                continue
            target_real = ((tr * 8 + tf) ^ 56) if flip else (tr * 8 + tf)
            piece = board[target_real]
            occ = 0 if piece < 2 or piece > 13 else (piece ^ 1 if flip else piece) - 1
            out.append(_extra(ZONE_OCC, slot_king, off, occ))
    occ_bb = colors[0] | colors[1]
    for slot_king in range(2):
        real_king = slot_king ^ 1 if flip else slot_king
        king_view = kings[real_king] ^ 56 if flip else kings[real_king]
        kf, kr = king_view & 7, king_view >> 3
        enemy_real = slot_king if flip else slot_king ^ 1
        for off in range(9):
            tf, tr = kf + (off % 3) - 1, kr + (off // 3) - 1
            if tf < 0 or tf > 7 or tr < 0 or tr > 7:
                continue
            target_real = ((tr * 8 + tf) ^ 56) if flip else (tr * 8 + tf)
            if _attacked_by(target_real, enemy_real, occ_bb, colors, pieces):
                out.append(_extra(ZONE_ATK, slot_king, off, 0))
    for slot_color in range(2):
        real_color = slot_color ^ 1 if flip else slot_color
        pawn_code = 2 + real_color
        enemy_pawn = 3 - real_color
        for state in range(3):
            for sq in range(64):
                real = sq ^ 56 if flip else sq
                if board[real] != pawn_code:
                    continue
                f, r = real & 7, real >> 3
                if state == 0:
                    has = not any(board[rr * 8 + ff] == enemy_pawn
                                  for ff in (f - 1, f, f + 1) if 0 <= ff < 8
                                  for rr in range(8) if (rr > r if real_color == 0 else rr < r))
                elif state == 1:
                    has = not any(board[rr * 8 + ff] == pawn_code
                                  for ff in (f - 1, f + 1) if 0 <= ff < 8 for rr in range(8))
                else:
                    has = any(rr * 8 + f != real and board[rr * 8 + f] == pawn_code for rr in range(8))
                if has:
                    out.append(_extra(PAWN, slot_color, state, sq))
    for slot_color in range(2):
        real_color = slot_color ^ 1 if flip else slot_color
        rook_code = 8 + real_color
        for kind in range(2):
            for sq in range(64):
                real = sq ^ 56 if flip else sq
                if board[real] != rook_code:
                    continue
                f = real & 7
                file_pawns = [board[rr * 8 + f] for rr in range(8) if board[rr * 8 + f] in (2, 3)]
                ok = not file_pawns if kind == 0 else all((p & 1) != real_color for p in file_pawns)
                if ok:
                    out.append(_extra(ROOKFILE, slot_color, kind, sq))
    for slot_side in range(2):
        for sc in range(2):
            real_side = slot_side ^ 1 if flip else slot_side
            real_sc = sc ^ 1 if flip else sc
            out.append(_extra(COMPLEX, slot_side, sc, min(complex_count[real_side][real_sc], 8)))
    return out


def oracle_features(fen: str) -> tuple[list[int], list[int], bool]:
    """Feature lists of a FEN computed without the native library."""
    placement, turn = fen.split()[:2]
    board = [0] * 64
    colors = [0, 0]
    pieces = [0] * 7
    for row, rank_text in enumerate(placement.split("/")):
        file_index = 0
        for ch in rank_text:
            if ch.isdigit():
                file_index += int(ch)
                continue
            square = (7 - row) * 8 + file_index
            code = PIECE_CODES[ch]
            board[square] = code
            colors[code & 1] |= 1 << square
            pieces[code // 2] |= 1 << square
            file_index += 1
    wking = (colors[0] & pieces[6]).bit_length() - 1
    bking = (colors[1] & pieces[6]).bit_length() - 1
    w_bucket = KING_BUCKET_TABLE[wking]
    b_bucket = KING_BUCKET_TABLE[bking ^ 56]
    white: list[int] = []
    black: list[int] = []
    for square, code in enumerate(board):
        if not code:
            continue
        color, base = code & 1, code // 2 - 1
        white.append(w_bucket * FEATURES_PER_KING_BUCKET + color * FEATURES_PER_COLOR + base * 64 + square)
        black.append(b_bucket * FEATURES_PER_KING_BUCKET + (color ^ 1) * FEATURES_PER_COLOR + base * 64 + (square ^ 56))
    white += _collect_extra_view(board, colors, pieces, wking, bking, False)
    black += _collect_extra_view(board, colors, pieces, wking, bking, True)
    return (white, black, True) if turn == "w" else (black, white, False)


def verify_sbin(sbin_path: Path, samples: int = 10000, *, full: bool = False,
                check_eval_labels: bool = False) -> dict:
    import chess
    samples = int(samples)
    if samples < 1:
        raise ValueError("samples pozitif olmalı.")
    before = workflow.file_identity(Path(sbin_path))
    report = {"file": before, "scope": "all" if full else "sample", "checked": 0,
              "invalid_records": 0, "missing_fullmove": 0, "nonzero_padding": 0,
              "invalid_position": 0, "first_invalid_indices": [],
              "oracle_checked": 0, "oracle_failures": 0}
    started = time.perf_counter()
    with SbinDataset(sbin_path) as ds:
        records = ds.records()
        rng = np.random.default_rng(42)
        test_indices = np.sort(rng.choice(ds.count, size=min(samples, ds.count), replace=False))
        scope = [np.arange(begin, min(begin + 262144, ds.count)) for begin in range(0, ds.count, 262144)] \
            if full else [test_indices]
        labels = {"checked": 0, "expected_encoding": "elo400_white", "elo400_mismatches": 0}
        for rows in scope:
            chunk = np.ascontiguousarray(records[rows])
            status = np.zeros(len(rows), dtype=np.uint8)
            valid = ds.lib.sbin_validate_batch(pointer(chunk), len(rows), pointer(status))
            report["checked"] += len(rows)
            report["invalid_records"] += len(rows) - valid
            for flag, name in SBIN_STATUS_FLAGS:
                report[name] += int(np.count_nonzero(status & flag))
            room = 20 - len(report["first_invalid_indices"])
            if room > 0:
                report["first_invalid_indices"].extend(rows[np.flatnonzero(status)[:room]].tolist())
            if check_eval_labels:
                stored = chunk.view(np.uint16).reshape(-1, 16)[:, 13].astype(np.int64)
                ds.lib.sbin_calibrate_labels(pointer(chunk), len(chunk), 0.0)
                expected = chunk.view(np.uint16).reshape(-1, 16)[:, 13]
                labels["checked"] += len(chunk)
                labels["elo400_mismatches"] += int(np.count_nonzero(np.abs(stored - expected) > 2))
        if check_eval_labels:
            report["eval_label_calibration"] = labels
        test_records = np.ascontiguousarray(records[test_indices])
        test_status = np.zeros(len(test_indices), dtype=np.uint8)
        ds.lib.sbin_validate_batch(pointer(test_records), len(test_indices), pointer(test_status))
        for index in test_indices[test_status == 0].tolist():
            fen, wdl, _ = ds.get_fen(index)
            us, them, white = ds.get_nnue_features(index)
            expected_us, expected_them, expected_white = oracle_features(fen)
            report["oracle_checked"] += 1
            if (not chess.Board(fen).is_valid() or not 0.0 <= wdl <= 1.0 or
                    sorted(us) != sorted(expected_us) or sorted(them) != sorted(expected_them) or
                    white != expected_white):
                report["oracle_failures"] += 1
    if workflow.file_identity(Path(sbin_path)) != before:
        raise RuntimeError("SBIN doğrulama sırasında değişti; sonuç geçersiz.")
    report["seconds"] = round(time.perf_counter() - started, 3)
    report["passed"] = report["invalid_records"] == 0 and report["oracle_failures"] == 0
    if check_eval_labels:
        report["passed"] = report["passed"] and report["eval_label_calibration"]["elo400_mismatches"] == 0
    return report


def benchmark_batches(sbin_path: Path, batch_size: int) -> dict:
    if batch_size < 1:
        raise ValueError("batch-size pozitif olmalı.")
    lib = load_native_lib()
    with SbinDataset(sbin_path) as ds:
        records = np.ascontiguousarray(ds.records()[:min(ds.count, 4_000_000)])
    rows = np.random.default_rng(0).permutation(len(records)).astype(np.int64)
    buffers = workflow.BatchBuffers(batch_size, lib.sbin_nnue_slots())
    started = time.perf_counter()
    for begin in range(0, len(rows) - batch_size + 1, batch_size):
        buffers.build(lib, records, rows[begin:begin + batch_size], 0.0, 0)
    positions = (len(rows) // batch_size) * batch_size
    seconds = time.perf_counter() - started
    return {"positions": positions, "seconds": round(seconds, 3),
            "positions_per_second": round(positions / max(seconds, 1e-9))}


def main():
    parser = argparse.ArgumentParser(description="Stallion SBIN 32-byte binary format aracı")
    parser.add_argument("command", choices=["convert", "verify", "benchmark"])
    parser.add_argument("--parquet", type=Path)
    parser.add_argument("--sbin", type=Path, required=True)
    parser.add_argument("--samples", type=int, default=10000)
    parser.add_argument("--batch-size", type=int, default=16384)
    parser.add_argument("--all", dest="full", action="store_true", help="Tüm kayıtları native doğrulayıcıyla tara")
    parser.add_argument("--check-eval-labels", action="store_true", help="CP etiketli eval verisinde WDL ölçeğini karşılaştır")
    parser.add_argument("--report", type=Path, help="JSON rapor çıktısı")
    args = parser.parse_args()
    if args.command == "convert":
        if args.parquet is None:
            parser.error("convert için --parquet gerekli.")
        report = convert_parquet_to_sbin(args.parquet, args.sbin)
    elif args.command == "verify":
        report = verify_sbin(args.sbin, args.samples, full=args.full, check_eval_labels=args.check_eval_labels)
    else:
        report = benchmark_batches(args.sbin, args.batch_size)
    if args.report:
        workflow.atomic_json(args.report, report)
    summary = {key: value for key, value in report.items() if key not in ("file", "first_invalid_indices", "output", "source")}
    print(" ".join(f"{key}={value}" for key, value in summary.items()), flush=True)
    if args.command == "verify" and not report["passed"]:
        sys.exit(2)


if __name__ == "__main__":
    main()

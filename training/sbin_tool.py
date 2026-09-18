#!/usr/bin/env python3
"""Stallion SBIN: 32-byte ultra-compact binary chess dataset tool.

Features:
- Fixed 32 bytes per position (zero index overhead, O(1) random access).
- Direct zero-copy mmap streaming from SSD.
- Native C++ feature decoder matching Stallion NNUE architecture.
"""

from __future__ import annotations

import argparse
import ctypes
from ctypes import c_char_p, c_float, c_int16, c_int, c_size_t, POINTER, Structure
import math
import json
import operator
from pathlib import Path
import subprocess
import sys
import time
import uuid
from functools import lru_cache

if __package__:
    from . import stallion as workflow
else:
    import stallion as workflow

source_identity = workflow.file_identity

ROOT = Path(__file__).resolve().parent
FORMAT_VERSION = 2
WRITER_VERSION = 3
NNUE_SLOTS = 256
DYLIB_PATH = ROOT / ("libstallion_sbin.dylib" if sys.platform == "darwin" else "libstallion_sbin.so")

# 32-byte PackedPosition struct in Python ctypes
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

# Mirror of SbinValidationStatus in sbin.h.
SBIN_MISSING_FULLMOVE = 1
SBIN_NONZERO_PADDING = 2
SBIN_INVALID_POSITION = 4
SBIN_STATUS_FLAGS = ((SBIN_MISSING_FULLMOVE, "missing_fullmove"),
                     (SBIN_NONZERO_PADDING, "nonzero_padding"),
                     (SBIN_INVALID_POSITION, "invalid_position"))


@lru_cache(maxsize=1)
def load_native_lib():
    sources = [ROOT / name for name in ("sbin.cpp", "sbin.h", "Makefile")]
    sources += [ROOT.parent / "engine" / "src" / name for name in ("bitboard.h", "nnue.h", "defs.h")]
    if not DYLIB_PATH.is_file() or any(p.stat().st_mtime_ns > DYLIB_PATH.stat().st_mtime_ns for p in sources):
        subprocess.run(["make", "-C", str(ROOT)], check=True, capture_output=True, text=True)
    lib = ctypes.CDLL(str(DYLIB_PATH))
    lib.sbin_format_version.restype = c_int
    if lib.sbin_format_version() != FORMAT_VERSION:
        raise RuntimeError("SBIN kütüphane sürümü uyuşmuyor; make -C training çalıştırın.")

    lib.sbin_pack_fen.argtypes = [c_char_p, c_float, c_int16, POINTER(PackedPosition)]
    lib.sbin_pack_fen.restype = c_int

    lib.sbin_unpack_fen.argtypes = [POINTER(PackedPosition), c_char_p, c_size_t, POINTER(c_float), POINTER(c_int16)]
    lib.sbin_unpack_fen.restype = c_int

    lib.sbin_extract_nnue.argtypes = [POINTER(PackedPosition), POINTER(c_int16), POINTER(c_int16), POINTER(c_int)]
    lib.sbin_extract_nnue.restype = c_int

    lib.sbin_batch_decode.argtypes = [POINTER(PackedPosition), c_size_t, POINTER(c_int16), POINTER(c_float)]
    lib.sbin_batch_decode.restype = c_size_t

    lib.sbin_batch_decode_indexed.argtypes = [POINTER(PackedPosition), c_size_t, POINTER(c_int16), POINTER(c_float), POINTER(ctypes.c_uint32)]
    lib.sbin_batch_decode_indexed.restype = c_size_t

    lib.sbin_validate_batch.argtypes = [POINTER(PackedPosition), c_size_t, POINTER(ctypes.c_uint8)]
    lib.sbin_validate_batch.restype = c_size_t

    lib.sbin_classify_phase.argtypes = [POINTER(PackedPosition)]
    lib.sbin_classify_phase.restype = c_int

    lib.sbin_classify_phases_batch.argtypes = [POINTER(PackedPosition), c_size_t, POINTER(ctypes.c_uint8)]
    lib.sbin_classify_phases_batch.restype = None

    return lib


def sbin_is_current(parquet_path: Path, sbin_path: Path) -> bool:
    try:
        saved = json.loads(sbin_path.with_suffix(".sbin.json").read_text())
        return (saved["version"] == FORMAT_VERSION and
                saved.get("writer_version") == WRITER_VERSION and
                saved["source"] == source_identity(parquet_path) and
                saved["output"] == source_identity(sbin_path))
    except (OSError, ValueError, KeyError, TypeError):
        return False


class SbinDataset:
    """Zero-copy memory-mapped Stallion SBIN dataset."""
    def __init__(self, sbin_path: Path):
        self.path = Path(sbin_path).resolve()
        self.file_size = self.path.stat().st_size
        if not self.file_size or self.file_size % 32 != 0:
            raise ValueError(f"Geçersiz SBIN dosya boyutu: {self.file_size} (32'nin katı olmalı)")
        self.count = self.file_size // 32
        self.lib = load_native_lib()

        # mmap the binary file directly
        import mmap
        self._f = open(self.path, "rb")
        try:
            self._mmap = mmap.mmap(self._f.fileno(), 0, access=mmap.ACCESS_COPY)
            # Create ctypes array over mmap buffer (ZERO COPY!)
            self._array = (PackedPosition * self.count).from_buffer(self._mmap)
        except Exception:
            if getattr(self, "_mmap", None) is not None:
                self._mmap.close()
            self._f.close()
            raise

    def __len__(self) -> int:
        return self.count

    def _position(self, index: int) -> PackedPosition:
        if self._mmap is None:
            raise ValueError("SBIN veri seti kapalı.")
        index = operator.index(index)
        if index < 0 or index >= self.count:
            raise IndexError(f"SBIN indeks aralık dışında: {index}")
        return PackedPosition.from_buffer_copy(self._mmap[index * 32:(index + 1) * 32])

    def get_fen(self, index: int) -> tuple[str, float, int]:
        pos = self._position(index)
        buf = ctypes.create_string_buffer(128)
        wdl = c_float()
        eval_cp = c_int16()
        ret = self.lib.sbin_unpack_fen(ctypes.byref(pos), buf, 128, ctypes.byref(wdl), ctypes.byref(eval_cp))
        if ret != 0:
            raise RuntimeError(f"Unpack hatası (code {ret})")
        return buf.value.decode("utf-8"), wdl.value, eval_cp.value

    def eval_view(self):
        """Read-only int16 view of the packed cp-eval field (offset 24, stride 32)."""
        import numpy as np
        return np.ndarray((self.count,), dtype="<i2", buffer=self._mmap, offset=24, strides=(32,))

    def records_ptr(self, index: int):
        """Record at index for batch native calls (shares the mmap buffer)."""
        return self._array[index]

    def record_bytes(self, index: int) -> bytes:
        """Raw 32 bytes of the record at index."""
        return self._mmap[index * 32:(index + 1) * 32]

    def get_nnue_features(self, index: int) -> tuple[list[int], list[int], bool]:
        pos = self._position(index)
        us = (c_int16 * NNUE_SLOTS)()
        them = (c_int16 * NNUE_SLOTS)()
        white_turn = c_int()
        total = self.lib.sbin_extract_nnue(ctypes.byref(pos), us, them, ctypes.byref(white_turn))
        if total < 0:
            raise RuntimeError("NNUE extract hatası")
        return ([x for x in us[:total] if x >= 0], [x for x in them[:total] if x >= 0],
                bool(white_turn.value))

    def close(self):
        if getattr(self, "_array", None) is not None:
            self._array = None
        if getattr(self, "_mmap", None) is not None:
            self._mmap.close()
            self._mmap = None
        if getattr(self, "_f", None) is not None:
            self._f.close()
            self._f = None

    def __enter__(self):
        return self

    def __exit__(self, *_):
        self.close()


def convert_parquet_to_sbin(parquet_path: Path, sbin_path: Path) -> None:
    import pyarrow.parquet as pq
    lib = load_native_lib()
    parquet_path = Path(parquet_path).resolve()
    sbin_path = Path(sbin_path).resolve()
    if parquet_path == sbin_path:
        raise ValueError("Parquet ve SBIN çıktı yolları farklı olmalı.")
    if sbin_is_current(parquet_path, sbin_path):
        return
    sbin_path.parent.mkdir(parents=True, exist_ok=True)

    print(f"Dönüştürülüyor: {parquet_path} -> {sbin_path}")
    t0 = time.time()
    source_before = source_identity(parquet_path)
    pf = pq.ParquetFile(parquet_path)
    total_rows = pf.metadata.num_rows

    packed_buf = PackedPosition()
    success = 0
    skipped = 0

    temporary = sbin_path.with_name(f".{sbin_path.name}.{uuid.uuid4().hex}.tmp")
    try:
        with open(temporary, "wb") as out_f:
            for batch in pf.iter_batches(batch_size=65536, columns=["fen", "wdl"]):
                fens = batch.column(0).to_pylist()
                wdls = batch.column(1).to_pylist()
                batch_bytes = bytearray()
                for fen, wdl in zip(fens, wdls):
                    try:
                        w_val = float(wdl)
                        if not math.isfinite(w_val) or not 0 <= w_val <= 1:
                            raise ValueError("invalid target")
                        fen = workflow.normalize_engine_fen(fen)
                        ret = lib.sbin_pack_fen(
                            fen.encode("utf-8"), c_float(w_val), c_int16(0),
                            ctypes.byref(packed_buf),
                        )
                    except (TypeError, ValueError, OverflowError, UnicodeError):
                        ret = -1
                    if ret == 0:
                        batch_bytes.extend(bytes(packed_buf))
                        success += 1
                    else:
                        skipped += 1
                out_f.write(batch_bytes)
                print(f"\rİşlenen: {success:,} / {total_rows:,} (Atlanan: {skipped})", end="", flush=True)

        if not success:
            raise ValueError("Geçerli SBIN kaydı bulunamadı.")
        if source_identity(parquet_path) != source_before:
            raise RuntimeError("Parquet dönüşüm sırasında değişti; çıktı yayımlanmadı.")
        temporary.replace(sbin_path)
    except Exception:
        temporary.unlink(missing_ok=True)
        raise
    workflow.atomic_json(sbin_path.with_suffix(".sbin.json"), {
        "version": FORMAT_VERSION, "writer_version": WRITER_VERSION, "source": source_before,
        "output": source_identity(sbin_path), "rows": success, "invalid_rows": skipped,
    })

    elapsed = time.time() - t0
    out_size = sbin_path.stat().st_size
    ratio = (out_size / parquet_path.stat().st_size) * 100
    print(f"\n[BAŞARILI] {success:,} pozisyon {elapsed:.2f} saniyede dönüştürüldü ({success/elapsed:,.0f} pos/sn).")
    print(f"Parquet boyutu: {parquet_path.stat().st_size / 1024 / 1024:.2f} MB")
    print(f"SBIN boyutu   : {out_size / 1024 / 1024:.2f} MB (Sıkıştırma oranı: %{ratio:.1f})")


def eval_targets(cp, wdl):
    import numpy as np
    cp = np.asarray(cp, dtype=np.float64)
    expected = 1.0 / (1.0 + 10.0 ** (-np.clip(cp, -1500, 1500) / 400.0))
    mate = (np.abs(cp) == 2000) & ((wdl == 0) | (wdl == 65535))
    return np.where(mate, (cp > 0).astype(float), expected)


def calibrate_eval_records(data: bytes) -> bytes:
    import numpy as np
    if len(data) % 32:
        raise ValueError("SBIN kayıt boyutu 32'nin katı olmalı.")
    if not data:
        return data
    result = bytearray(data)
    count = len(data) // 32
    cp = np.ndarray((count,), dtype="<i2", buffer=result, offset=24, strides=(32,))
    wdl = np.ndarray((count,), dtype="<u2", buffer=result, offset=26, strides=(32,))
    wdl[:] = np.rint(eval_targets(cp, wdl) * 65535).astype("<u2")
    return bytes(result)


def verify_sbin(sbin_path: Path, samples: int = 10000, *, full: bool = False,
                check_eval_labels: bool = False) -> dict:
    samples = int(samples)
    if samples < 1:
        raise ValueError("samples pozitif olmalı.")
    import random
    import numpy as np
    import chess
    before = source_identity(Path(sbin_path))
    report = {"file": before, "scope": "all" if full else "sample", "checked": 0,
              "invalid_records": 0, "missing_fullmove": 0, "nonzero_padding": 0,
              "invalid_position": 0, "first_invalid_indices": [],
              "oracle_checked": 0, "oracle_failures": 0}
    with SbinDataset(sbin_path) as ds:
        print(f"Doğrulanıyor: {sbin_path} ({ds.count:,} kayıt, kapsam={report['scope']})", flush=True)
        rng = random.Random(42)
        test_indices = rng.sample(range(ds.count), min(samples, ds.count))
        t0 = time.time()
        last_progress = t0
        if full:
            for begin in range(0, ds.count, 262144):
                count = min(262144, ds.count - begin)
                status = np.zeros(count, dtype=np.uint8)
                ptr = ctypes.byref(ds._array[begin])
                valid = ds.lib.sbin_validate_batch(ptr, count, status.ctypes.data_as(POINTER(ctypes.c_uint8)))
                del ptr
                report["checked"] += count
                report["invalid_records"] += count - valid
                for flag, name in SBIN_STATUS_FLAGS:
                    report[name] += int(np.count_nonzero(status & flag))
                room = 20 - len(report["first_invalid_indices"])
                if room > 0:
                    report["first_invalid_indices"].extend((begin + np.flatnonzero(status)[:room]).tolist())
                if time.time() - last_progress >= 10:
                    print(f"Taranan={report['checked']:,}/{ds.count:,} geçersiz={report['invalid_records']:,}", flush=True)
                    last_progress = time.time()

        if check_eval_labels:
            label_counts = {"checked": 0, "elo400_mismatches": 0,
                            "expected_encoding": "elo400_white", "quantization_tolerance": 2 / 65535}
            for begin in range(0, ds.count, 1048576):
                end = min(begin + 1048576, ds.count)
                raw = np.frombuffer(ds._mmap[begin * 32:end * 32], dtype=np.dtype([
                    ("board", "V24"), ("cp", "<i2"), ("wdl", "<u2"), ("flags", "V4")]))
                cp = raw["cp"].astype(np.float64)
                target = raw["wdl"].astype(np.float64) / 65535.0
                expected = eval_targets(cp, raw["wdl"])
                label_counts["checked"] += len(cp)
                label_counts["elo400_mismatches"] += int(np.count_nonzero(np.abs(target - expected) > 2 / 65535))
            report["eval_label_calibration"] = label_counts

        for idx in test_indices:
            packed = ds._position(idx)
            status = ctypes.c_uint8()
            ds.lib.sbin_validate_batch(ctypes.byref(packed), 1, ctypes.byref(status))
            if not full:
                report["checked"] += 1
                report["invalid_records"] += bool(status.value)
                for flag, name in SBIN_STATUS_FLAGS:
                    report[name] += bool(status.value & flag)
                if status.value and len(report["first_invalid_indices"]) < 20:
                    report["first_invalid_indices"].append(idx)
            if status.value:
                continue
            fen, wdl, _ = ds.get_fen(idx)
            us, them, white = ds.get_nnue_features(idx)
            board = chess.Board(fen)
            expected_us, expected_them, expected_white = workflow.parse_fen_fast(fen)
            report["oracle_checked"] += 1
            if (not board.is_valid() or not 0.0 <= wdl <= 1.0 or
                    sorted(us) != sorted(expected_us) or sorted(them) != sorted(expected_them) or
                    white != expected_white):
                report["oracle_failures"] += 1
        elapsed = time.time() - t0
    if source_identity(Path(sbin_path)) != before:
        raise RuntimeError("SBIN doğrulama sırasında değişti; sonuç geçersiz.")
    report["seconds"] = round(elapsed, 3)
    report["passed"] = report["invalid_records"] == 0 and report["oracle_failures"] == 0
    if check_eval_labels:
        report["passed"] = report["passed"] and report["eval_label_calibration"]["elo400_mismatches"] == 0
    print(f"Kontrol={report['checked']:,} geçersiz={report['invalid_records']:,} "
          f"oracle_hatası={report['oracle_failures']:,} süre={elapsed:.1f} sn", flush=True)
    return report


def benchmark_read_speed(sbin_path: Path, batch_size: int = 1024) -> None:
    batch_size = int(batch_size)
    if batch_size < 1:
        raise ValueError("batch-size pozitif olmalı.")
    lib = load_native_lib()
    with SbinDataset(sbin_path) as ds:
        print("\n=== BENCHMARK: C++ NATIVE BATCH DECODE ===")
        print(f"Veri Seti: {sbin_path} ({ds.count:,} pozisyon)")
        print(f"Batch Boyutu: {batch_size}")

        out_features = (c_int16 * (batch_size * NNUE_SLOTS * 2))()
        out_targets = (c_float * batch_size)()

        t0 = time.perf_counter()
        total_decoded = 0
        for begin in range(0, ds.count, batch_size):
            count = min(batch_size, ds.count - begin)
            ptr = ctypes.byref(ds._array[begin])
            n = lib.sbin_batch_decode(ptr, count, out_features, out_targets)
            del ptr
            total_decoded += n

        elapsed = time.perf_counter() - t0
        speed = total_decoded / max(elapsed, 1e-9)
        print(f"Çözülen Pozisyon : {total_decoded:,}")
        print(f"Toplam Süre      : {elapsed:.3f} saniye")
        print(f"Saf Okuma Hızı   : {speed:,.0f} pozisyon/saniye")


def main():
    parser = argparse.ArgumentParser(description="Stallion SBIN 32-byte binary format aracı")
    parser.add_argument("command", choices=["convert", "verify", "benchmark"])
    parser.add_argument("--parquet", default=None)
    parser.add_argument("--sbin", default=None)
    parser.add_argument("--samples", type=int, default=10000)
    parser.add_argument("--batch-size", type=int, default=1024)
    parser.add_argument("--all", dest="full", action="store_true", help="Tüm kayıtları native doğrulayıcıyla tara")
    parser.add_argument("--check-eval-labels", action="store_true", help="CP etiketli eval verisinde WDL ölçeğini karşılaştır")
    parser.add_argument("--report", type=Path, help="JSON doğrulama raporu")
    args = parser.parse_args()

    if args.command == "convert":
        if not args.parquet or not args.sbin:
            print("Hata: --parquet ve --sbin gerekli.")
            sys.exit(1)
        convert_parquet_to_sbin(Path(args.parquet), Path(args.sbin))
    elif args.command == "verify":
        if not args.sbin:
            print("Hata: --sbin gerekli.")
            sys.exit(1)
        report = verify_sbin(Path(args.sbin), args.samples, full=args.full,
                             check_eval_labels=args.check_eval_labels)
        if args.report:
            workflow.atomic_json(args.report, report)
        if not report["passed"]:
            sys.exit(2)
    elif args.command == "benchmark":
        if not args.sbin:
            print("Hata: --sbin gerekli.")
            sys.exit(1)
        benchmark_read_speed(Path(args.sbin), args.batch_size)


if __name__ == "__main__":
    main()

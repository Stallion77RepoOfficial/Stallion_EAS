#!/usr/bin/env python3
"""Assemble the 100M Master Hybrid Dataset for Stallion NNUE training."""

import json
import os
import sys
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parent
DATA_DIR = ROOT / "data"
EVALS_PATH = DATA_DIR / "evals.sbin"
BRILLIANT_PATH = DATA_DIR / "brilliant.sbin"
PUZZLES_PATH = DATA_DIR / "puzzle_sacrifices.sbin"
OUTPUT_PATH = DATA_DIR / "master_100m.sbin"


def main():
    target_total = 100_000_000
    if len(sys.argv) > 1 and sys.argv[1].isdigit():
        target_total = int(sys.argv[1])

    for p, name in [(EVALS_PATH, "Evals"), (BRILLIANT_PATH, "Brilliant"), (PUZZLES_PATH, "Puzzles")]:
        if not p.is_file():
            print(f"[HATA] {name} dosyası bulunamadı: {p}", file=sys.stderr)
            return 1

    brilliant_count = BRILLIANT_PATH.stat().st_size // 32
    puzzles_count = PUZZLES_PATH.stat().st_size // 32
    evals_total = EVALS_PATH.stat().st_size // 32

    # Target allocations
    # Brilliant: take all available or up to 12M
    brilliant_take = min(brilliant_count, int(target_total * 0.12))
    # Puzzles: ~3.5%
    puzzles_take = int(target_total * 0.035)
    # Evals: remainder
    evals_take = target_total - brilliant_take - puzzles_take

    print(f"=== 100M Master Hibrit Veri Seti Oluşturuluyor ===")
    print(f"Toplam Hedef: {target_total:,} pozisyon")
    print(f"1. Temel Konumsal (evals.sbin): {evals_take:,} pozisyon (%{evals_take / target_total * 100:.1f})")
    print(f"2. Süper-GM Fedalar (brilliant.sbin): {brilliant_take:,} pozisyon (%{brilliant_take / target_total * 100:.1f})")
    print(f"3. Taktik Bulmacalar (puzzle_sacrifices.sbin): {puzzles_take:,} pozisyon (%{puzzles_take / target_total * 100:.1f})")
    print(f"Hedef Dosya: {OUTPUT_PATH} ({target_total * 32 / (1024*1024):,.1f} MB)\n")

    started = time.perf_counter()
    temp_output = OUTPUT_PATH.with_suffix(".tmp")
    written = 0

    with open(temp_output, "wb") as out_f:
        print("[*] Veri setleri eşzamanlı harmanlanıyor (Interleaved 500k bloklar)...", flush=True)
        t0 = time.perf_counter()
        try:
            from training.sbin_tool import SbinDataset, load_native_lib
        except ImportError:
            from sbin_tool import SbinDataset, load_native_lib
        import ctypes
        import numpy as np

        lib = load_native_lib()
        with SbinDataset(EVALS_PATH) as d_ev, \
             SbinDataset(BRILLIANT_PATH) as d_br, \
             SbinDataset(PUZZLES_PATH) as d_pz:

            pz_data = np.frombuffer(d_pz._mmap, dtype="V32")
            pz_len = len(pz_data)

            status = np.zeros(262144, dtype=np.uint8)
            status_ptr = status.ctypes.data_as(ctypes.POINTER(ctypes.c_uint8))

            ev_cur = 0
            br_cur = 0
            pz_cur = 0
            eval_buf = np.empty(0, dtype="V32")

            def fetch_evals(needed):
                nonlocal ev_cur, eval_buf
                chunks = [eval_buf]
                got = len(eval_buf)
                while got < needed and ev_cur < d_ev.count:
                    count = min(262144, d_ev.count - ev_cur)
                    pos_ptr = ctypes.byref(d_ev._array[ev_cur])
                    lib.sbin_validate_batch(pos_ptr, count, status_ptr)
                    del pos_ptr
                    valid = np.frombuffer(d_ev._mmap[ev_cur * 32 : (ev_cur + count) * 32], dtype="V32")[status[:count] == 0]
                    chunks.append(valid)
                    got += len(valid)
                    ev_cur += count
                all_v = np.concatenate(chunks)
                res = all_v[:needed]
                eval_buf = all_v[needed:]
                return res

            num_blocks = 200
            ev_per_block = evals_take // num_blocks
            br_per_block = brilliant_take // num_blocks
            pz_per_block = puzzles_take // num_blocks

            ev_accum = 0
            br_accum = 0
            pz_accum = 0

            rng = np.random.default_rng(42)

            for b in range(num_blocks):
                is_last = (b == num_blocks - 1)
                b_ev = (evals_take - ev_accum) if is_last else ev_per_block
                b_br = (brilliant_take - br_accum) if is_last else br_per_block
                b_pz = (puzzles_take - pz_accum) if is_last else pz_per_block

                ev_records = fetch_evals(b_ev)
                br_records = np.frombuffer(d_br._mmap[br_cur * 32 : (br_cur + b_br) * 32], dtype="V32")
                br_cur += b_br

                pz_records = np.empty(b_pz, dtype="V32")
                pz_filled = 0
                while pz_filled < b_pz:
                    take = min(b_pz - pz_filled, pz_len - pz_cur)
                    pz_records[pz_filled : pz_filled + take] = pz_data[pz_cur : pz_cur + take]
                    pz_filled += take
                    pz_cur = (pz_cur + take) % pz_len

                block_records = np.concatenate([ev_records, br_records, pz_records])
                perm = rng.permutation(len(block_records))
                out_f.write(block_records[perm].tobytes())

                ev_accum += b_ev
                br_accum += b_br
                pz_accum += b_pz
                written += len(block_records)

                if (b + 1) % 20 == 0 or is_last:
                    pct = (written / target_total) * 100
                    speed = written / max(0.1, time.perf_counter() - t0)
                    print(f"      Blok {b+1}/{num_blocks} ({written:,} / {target_total:,}, %{pct:.1f}) [{speed:,.0f} pos/s]...", flush=True)

            del status_ptr, status, pz_data, eval_buf
            if "block_records" in locals():
                del block_records, ev_records, br_records, pz_records
        print(f"      Tamamlandı ({written:,} pozisyon, {time.perf_counter() - t0:.1f} sn).", flush=True)

    temp_output.replace(OUTPUT_PATH)
    total_sec = time.perf_counter() - started
    final_mb = OUTPUT_PATH.stat().st_size / (1024 * 1024)

    metadata = {
        "output": str(OUTPUT_PATH),
        "total_rows": written,
        "evals_rows": evals_take,
        "brilliant_rows": brilliant_take,
        "puzzles_rows": puzzles_take,
        "file_size_bytes": OUTPUT_PATH.stat().st_size,
        "elapsed_seconds": round(total_sec, 2),
        "created_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
    }
    with open(OUTPUT_PATH.with_suffix(".extract.json"), "w", encoding="utf-8") as f:
        json.dump(metadata, f, indent=2)

    print(f"\n[BAŞARILI] {written:,} pozisyonluk {OUTPUT_PATH.name} oluşturuldu ({final_mb:,.1f} MB, {total_sec:.1f} sn)!")
    return 0


if __name__ == "__main__":
    sys.exit(main())

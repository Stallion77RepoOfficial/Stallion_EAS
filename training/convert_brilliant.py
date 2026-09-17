#!/usr/bin/env python3
"""High-speed multi-core converter from Brilliant PGN to Stallion SBIN format."""

import ctypes
import json
import multiprocessing as mp
import os
import sys
import time
from pathlib import Path

import chess.pgn

ROOT = Path(__file__).resolve().parent
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))
if str(ROOT.parent) not in sys.path:
    sys.path.insert(0, str(ROOT.parent))

DATA_DIR = ROOT / "data"
PGN_PATH = DATA_DIR / "nnue_games_live.pgn"
OUTPUT_PATH = DATA_DIR / "brilliant.sbin"


def find_chunk_boundaries(pgn_path: Path, num_workers: int) -> list[int]:
    size = pgn_path.stat().st_size
    offsets = [0]
    with open(pgn_path, "rb") as f:
        for i in range(1, num_workers):
            target = i * (size // num_workers)
            f.seek(target)
            chunk = f.read(65536)
            idx = chunk.find(b"\n[Event ")
            if idx != -1:
                offsets.append(target + idx + 1)
            else:
                raise RuntimeError(f"Could not find [Event boundary for worker {i}")
    offsets.append(size)
    return offsets


def worker_convert(worker_id: int, pgn_path: str, start_offset: int, end_offset: int,
                   temp_output: str) -> tuple[int, int]:
    try:
        from sbin_tool import PackedPosition, load_native_lib
    except ImportError:
        from training.sbin_tool import PackedPosition, load_native_lib
    lib = load_native_lib()
    buf = PackedPosition()

    games_processed = 0
    positions_extracted = 0
    batch_bytes = bytearray()

    with open(temp_output, "wb") as out_f, open(pgn_path, "r", encoding="utf-8", errors="replace") as pgn_f:
        pgn_f.seek(start_offset)
        while True:
            current_pos = pgn_f.tell()
            if current_pos >= end_offset:
                break
            game = chess.pgn.read_game(pgn_f)
            if game is None:
                break
            games_processed += 1

            result = game.headers.get("Result")
            if result == "1-0":
                white_wdl = 1.0
            elif result == "0-1":
                white_wdl = 0.0
            elif result == "1/2-1/2":
                white_wdl = 0.5
            else:
                continue

            bply_str = game.headers.get("BrilliantPly", "")
            plies = [int(p.strip()) for p in bply_str.split(",") if p.strip().isdigit()]
            if not plies:
                continue

            target_plies = set()
            for p in plies:
                target_plies.add(p)      # Fedadan hemen önce (Karar anı)
                target_plies.add(p + 1)  # Feda anı (Fedanın tahtadaki hali)
                target_plies.add(p + 2)  # Fedadan hemen sonrası (Taktik devam hamlesi)
            max_target = max(target_plies)

            board = game.board()
            for ply, move in enumerate(game.mainline_moves(), start=1):
                if ply in target_plies:
                    fen = board.fen()
                    ret = lib.sbin_pack_fen(
                        fen.encode("utf-8"), ctypes.c_float(white_wdl),
                        ctypes.c_int16(0), ctypes.byref(buf)
                    )
                    if ret == 0:
                        batch_bytes.extend(bytes(buf))
                        positions_extracted += 1
                        if len(batch_bytes) >= 65536 * 32:
                            out_f.write(batch_bytes)
                            batch_bytes.clear()
                if ply > max_target:
                    break
                board.push(move)

            if games_processed % 50000 == 0:
                print(f"[Çekirdek {worker_id}] {games_processed:,} oyun tarandı ({positions_extracted:,} pozisyon)...", flush=True)

        if batch_bytes:
            out_f.write(batch_bytes)
            batch_bytes.clear()

    return games_processed, positions_extracted


def main():
    if not PGN_PATH.is_file():
        print(f"[HATA] PGN dosyası bulunamadı: {PGN_PATH}", file=sys.stderr)
        return 1

    num_workers = min(10, os.cpu_count() or 4)
    file_size_mb = PGN_PATH.stat().st_size / (1024 * 1024)
    print(f"=== Brilliant PGN -> SBIN Dönüştürme Başlatılıyor ===")
    print(f"Kaynak: {PGN_PATH} ({file_size_mb:,.1f} MB)")
    print(f"Hedef: {OUTPUT_PATH}")
    print(f"İş Parçacığı: {num_workers} çekirdek paralel")

    started = time.perf_counter()
    boundaries = find_chunk_boundaries(PGN_PATH, num_workers)
    temp_files = [DATA_DIR / f".brilliant_part_{i}.sbin" for i in range(num_workers)]

    tasks = [
        (i, str(PGN_PATH), boundaries[i], boundaries[i + 1], str(temp_files[i]))
        for i in range(num_workers)
    ]

    print("İş parçacıkları başlatılıyor...")
    with mp.Pool(processes=num_workers) as pool:
        results = pool.starmap(worker_convert, tasks)

    total_games = sum(r[0] for r in results)
    total_positions = sum(r[1] for r in results)
    parse_elapsed = time.perf_counter() - started

    print(f"\nTüm çekirdekler tamamlandı ({parse_elapsed:.1f} sn).")
    print(f"Toplam İşlenen Oyun: {total_games:,} ({total_games / parse_elapsed:,.0f} oyun/s)")
    print(f"Toplam Çıkarılan Pozisyon: {total_positions:,} ({total_positions / parse_elapsed:,.0f} pos/s)")

    print(f"\nParçalar birleştiriliyor -> {OUTPUT_PATH}...")
    temp_target = OUTPUT_PATH.with_suffix(".tmp")
    with open(temp_target, "wb") as out_f:
        for temp_file in temp_files:
            if temp_file.is_file():
                with open(temp_file, "rb") as in_f:
                    while True:
                        buf = in_f.read(1024 * 1024 * 16)
                        if not buf:
                            break
                        out_f.write(buf)
                temp_file.unlink(missing_ok=True)

    temp_target.replace(OUTPUT_PATH)
    final_size_mb = OUTPUT_PATH.stat().st_size / (1024 * 1024)
    total_elapsed = time.perf_counter() - started

    metadata = {
        "source": str(PGN_PATH),
        "output": str(OUTPUT_PATH),
        "total_games": total_games,
        "total_positions": total_positions,
        "file_size_bytes": OUTPUT_PATH.stat().st_size,
        "elapsed_seconds": round(total_elapsed, 2),
        "games_per_second": round(total_games / total_elapsed, 1),
        "positions_per_second": round(total_positions / total_elapsed, 1),
        "created_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
    }
    with open(OUTPUT_PATH.with_suffix(".extract.json"), "w", encoding="utf-8") as f:
        json.dump(metadata, f, indent=2)

    print(f"[BAŞARILI] {total_positions:,} pozisyonluk {OUTPUT_PATH.name} oluşturuldu ({final_size_mb:,.1f} MB, {total_elapsed:.1f} sn)!")
    return 0


if __name__ == "__main__":
    sys.exit(main())

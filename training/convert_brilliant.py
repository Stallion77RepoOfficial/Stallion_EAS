#!/usr/bin/env python3
"""High-speed multi-core converter from Brilliant PGN to Stallion SBIN format."""

import argparse
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


def worker_convert(pgn_path: str, start_offset: int, end_offset: int,
                   temp_output: str) -> tuple[int, int, int]:
    if __package__:
        from .sbin_tool import PackedPosition, load_native_lib
    else:
        from sbin_tool import PackedPosition, load_native_lib
    lib = load_native_lib()
    buf = PackedPosition()

    games_processed = 0
    games_without_result = 0
    positions_extracted = 0
    batch_bytes = bytearray()

    def pack(fen: str, wdl: float) -> None:
        nonlocal positions_extracted
        ret = lib.sbin_pack_fen(fen.encode("utf-8"), ctypes.c_float(wdl), ctypes.c_int16(0), ctypes.byref(buf))
        if ret != 0:
            raise ValueError(f"Paketlenemeyen konum (kod {ret}): {fen}")
        batch_bytes.extend(bytes(buf))
        positions_extracted += 1

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

            white_wdl = {"1-0": 1.0, "0-1": 0.0, "1/2-1/2": 0.5}.get(game.headers.get("Result"))
            if white_wdl is None:
                games_without_result += 1
                continue

            bply_str = game.headers.get("BrilliantPly", "")
            plies = [int(p.strip()) for p in bply_str.split(",") if p.strip().isdigit()]
            if not plies:
                continue

            # Feda çevresindeki altı konumu gerçek oyun sonucuyla etiketle.
            # BrilliantPly hangi tarafın kazandığını belirlemez.
            target_plies: dict[int, float] = {}
            for p in plies:
                for offset in range(6):  # p .. p+5
                    target_plies[p + offset] = white_wdl

            max_target = max(target_plies.keys())
            board = game.board()
            seen_fens = set()

            for ply, move in enumerate(game.mainline_moves(), start=1):
                if ply in target_plies:
                    fen = board.fen()
                    if fen not in seen_fens:
                        seen_fens.add(fen)
                        pack(fen, target_plies[ply])

                board.push(move)

                # Dinamik Mat Kesme Kuralı: Mat görüldüğü anda zincir o pozisyonda kesilir!
                if board.is_checkmate():
                    fen = board.fen()
                    if fen not in seen_fens:
                        seen_fens.add(fen)
                        # Mat eden tarafın WDL skoru (Sıra siyahtaysa Beyaz mat etmiştir -> 1.0)
                        pack(fen, 1.0 if board.turn == chess.BLACK else 0.0)
                    break

                if ply >= max_target:
                    break
            if len(batch_bytes) >= 65536 * 32:
                out_f.write(batch_bytes)
                batch_bytes.clear()

        out_f.write(batch_bytes)

    return games_processed, games_without_result, positions_extracted


def main(argv=None):
    parser = argparse.ArgumentParser(description="BrilliantPly PGN -> WDL-etiketli SBIN feda havuzu")
    parser.add_argument("--pgn", default=str(PGN_PATH))
    parser.add_argument("--output", default=str(OUTPUT_PATH))
    parser.add_argument("--data-dir", default=str(DATA_DIR))
    parser.add_argument("--workers", type=int, default=10)
    args = parser.parse_args(argv)
    pgn_path = Path(args.pgn)
    output_path = Path(args.output)
    data_dir = Path(args.data_dir)
    if not pgn_path.is_file():
        print(f"[HATA] PGN dosyası bulunamadı: {pgn_path}", file=sys.stderr)
        return 1

    num_workers = min(max(1, args.workers), os.cpu_count())
    started = time.perf_counter()
    boundaries = find_chunk_boundaries(pgn_path, num_workers)
    temp_files = [data_dir / f".brilliant_part_{i}.sbin" for i in range(num_workers)]

    tasks = [
        (str(pgn_path), boundaries[i], boundaries[i + 1], str(temp_files[i]))
        for i in range(num_workers)
    ]
    with mp.Pool(processes=num_workers) as pool:
        results = pool.starmap(worker_convert, tasks)

    total_games = sum(r[0] for r in results)
    games_without_result = sum(r[1] for r in results)
    total_positions = sum(r[2] for r in results)
    temp_target = output_path.with_suffix(".tmp")
    with open(temp_target, "wb") as out_f:
        for temp_file in temp_files:
            with open(temp_file, "rb") as in_f:
                while buf := in_f.read(1024 * 1024 * 16):
                    out_f.write(buf)
            temp_file.unlink()

    temp_target.replace(output_path)
    total_elapsed = time.perf_counter() - started

    metadata = {
        "source": str(pgn_path),
        "output": str(output_path),
        "total_games": total_games,
        "games_without_result": games_without_result,
        "total_positions": total_positions,
        "label_source": "game_result",
        "file_size_bytes": output_path.stat().st_size,
        "elapsed_seconds": round(total_elapsed, 2),
        "games_per_second": round(total_games / total_elapsed, 1),
        "positions_per_second": round(total_positions / total_elapsed, 1),
        "created_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
    }
    with open(output_path.with_suffix(".extract.json"), "w", encoding="utf-8") as f:
        json.dump(metadata, f, indent=2)

    print(f"[BAŞARILI] {output_path}: {total_positions:,} pozisyon, {total_games:,} oyun ({total_elapsed:.1f} sn)", flush=True)
    return 0


if __name__ == "__main__":
    sys.exit(main())

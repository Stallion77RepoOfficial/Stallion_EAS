#!/usr/bin/env python3
"""Single entry point for all Stallion EAS training workflows.

Commands:
  extract      phase-balanced or aggressive SBIN extraction
  prepare      validated aggressive puzzle-position preparation
  datagen      labelled self-play data generation through one UCI session/game
  train        engine-compatible NNUE training and export
  match        candidate/baseline cutechess comparison
  eas          local aggressiveness report from a PGN
  sacrifices   SGS sacrifice scan from a PGN
  pipeline     run a selected subset with --steps

All Python workflow code is intentionally kept in this file. Native assets
(the engine, Stockfish, cutechess and pgn-extract pattern files) remain files.
"""

from __future__ import annotations

import argparse
from contextlib import contextmanager
from dataclasses import dataclass, field
from datetime import datetime
import hashlib
import importlib
import json
import math
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import tempfile
import time
import uuid
from typing import Any, Iterator, Sequence


ROOT = Path(__file__).resolve().parent
REPOSITORY = ROOT.parent
ENGINE_ROOT = REPOSITORY / "engine"
NETS = ENGINE_ROOT / "nets"
DEFAULT_ASSETS = ROOT / "assets"
DEFAULT_EVAL = ROOT / "data" / "evals.sbin"
DEFAULT_PUZZLES = ROOT / "data" / "puzzle_sacrifices.sbin"
DEFAULT_BOOK = ROOT / "openings.epd"
NNUE_FEATURES = 768
NNUE_ACCUMULATOR = 1024
NNUE_PAYLOAD_SIZE = 2 * (NNUE_FEATURES * NNUE_ACCUMULATOR + 3 * NNUE_ACCUMULATOR + 1)
NNUE_FILE_SIZE = (NNUE_PAYLOAD_SIZE + 63) // 64 * 64
CACHE_VERSION = 7
SCALE = 400
QA = 255
QB = 64
QAB = QA * QB

PIECE_CODES = {
    "P": 2, "p": 3, "N": 4, "n": 5, "B": 6, "b": 7,
    "R": 8, "r": 9, "Q": 10, "q": 11, "K": 12, "k": 13,
}
PIECE_VALUES = {"p": 105, "n": 320, "b": 330, "r": 520, "q": 950}


def dependency(name: str):
    try:
        return importlib.import_module(name)
    except ImportError as exc:
        raise RuntimeError(
            f"Eksik Python bağımlılığı: {name}. "
            f"{sys.executable} -m pip install -r training/requirements.txt çalıştırın."
        ) from exc


def resolve_path(value: str | os.PathLike[str] | None,
                default: Path | None = None) -> Path | None:
    if value is None:
        return default.expanduser().resolve() if default else None
    raw = Path(value).expanduser()
    if raw.is_absolute():
        return raw.resolve()
    caller = (Path.cwd() / raw).resolve()
    training_relative = (ROOT / raw).resolve()
    return caller if caller.exists() or not training_relative.exists() else training_relative


def require_file(value: str | os.PathLike[str] | None, label: str) -> Path:
    path = resolve_path(value)
    if path is None or not path.is_file():
        raise RuntimeError(f"{label} bulunamadı: {path or value}")
    return path


def atomic_bytes(path: Path, value: bytes) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(f".{path.name}.{uuid.uuid4().hex}.tmp")
    try:
        temporary.write_bytes(value)
        temporary.replace(path)
    finally:
        temporary.unlink(missing_ok=True)


def atomic_text(path: Path, value: str) -> None:
    atomic_bytes(path, value.encode("utf-8"))


def atomic_json(path: Path, value: Any) -> None:
    atomic_text(path, json.dumps(value, indent=2, ensure_ascii=False))


def run_checked(command: Sequence[str | os.PathLike[str]], cwd: Path | None = None,
                capture: bool = True) -> subprocess.CompletedProcess[str]:
    cmd = [str(item) for item in command]
    result = subprocess.run(
        cmd, cwd=str(cwd) if cwd else None, capture_output=capture,
        text=True, check=False,
    )
    if result.returncode:
        detail = (result.stderr or result.stdout or "").strip()
        raise RuntimeError(
            f"Komut başarısız ({result.returncode}): {' '.join(cmd)}"
            + (f"\n{detail}" if detail else "")
        )
    return result


def file_identity(path: Path) -> dict[str, Any]:
    path = path.resolve()
    try:
        stat = path.stat()
        return {"path": str(path), "size": stat.st_size, "mtime_ns": stat.st_mtime_ns}
    except OSError:
        return {"path": str(path), "size": 0, "mtime_ns": 0}


def file_sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def require_network(path: Any, label: str = "NNUE") -> Path:
    network = require_file(path, label)
    if network.stat().st_size not in (NNUE_PAYLOAD_SIZE, NNUE_FILE_SIZE):
        raise ValueError(f"{label} motorun 768x1024 NNUE biçiminde değil: {network}")
    return network


def cp_to_wdl(cp: float) -> float:
    cp = float(cp)
    if not math.isfinite(cp):
        raise ValueError("Eval skoru sonlu olmalı.")
    cp = max(-1500.0, min(1500.0, cp))
    return 1.0 / (1.0 + 10.0 ** (-cp / 400.0))


def parse_fen_fast(fen: str) -> tuple[list[int], list[int], bool]:
    """Parse the feature indices used by the C++ NNUE evaluator."""
    if not isinstance(fen, str):
        raise ValueError("FEN text olmalı")
    parts = fen.split()
    if len(parts) not in (4, 6) or parts[1] not in ("w", "b"):
        raise ValueError(f"Geçersiz FEN alanları: {fen}")
    if parts[2] != "-" and not re.fullmatch(r"[KQkqA-Ha-h]+", parts[2]):
        raise ValueError(f"Geçersiz rok alanı: {fen}")
    if parts[3] != "-" and not re.fullmatch(r"[a-h][36]", parts[3]):
        raise ValueError(f"Geçersiz geçerken alma alanı: {fen}")
    if len(parts) == 6 and (not parts[4].isdigit() or not parts[5].isdigit() or int(parts[5]) < 1):
        raise ValueError(f"Geçersiz FEN sayacı: {fen}")
    if parts[3] != "-" and parts[3][1] != ("6" if parts[1] == "w" else "3"):
        raise ValueError(f"Geçersiz geçerken alma sırası: {fen}")
    ranks = parts[0].split("/")
    if len(ranks) != 8:
        raise ValueError(f"Geçersiz FEN tahtası: {fen}")

    us: list[int] = []
    them: list[int] = []
    kings = {"K": 0, "k": 0}
    colors = [0, 0]
    pawns = [0, 0]
    total = 0
    for row, rank_text in enumerate(ranks):
        file_index = 0
        for ch in rank_text:
            if ch in "12345678":
                file_index += int(ch)
                continue
            if ch not in PIECE_CODES or file_index >= 8:
                raise ValueError(f"Geçersiz taş veya sıra: {fen}")
            if ch in ("P", "p") and row in (0, 7):
                raise ValueError(f"Terfi sırasındaki piyon: {fen}")
            code = PIECE_CODES[ch]
            color = code & 1
            colors[color] += 1
            if ch in ("P", "p"):
                pawns[color] += 1
            base = code // 2 - 1
            square = (7 - row) * 8 + file_index
            white_index = color * 384 + base * 64 + square
            black_index = (color ^ 1) * 384 + base * 64 + (square ^ 56)
            if parts[1] == "w":
                us.append(white_index)
                them.append(black_index)
            else:
                us.append(black_index)
                them.append(white_index)
            if ch in kings:
                kings[ch] += 1
            total += 1
            file_index += 1
        if file_index != 8:
            raise ValueError(f"FEN sırası sekiz kare değil: {fen}")
    if kings["K"] != 1 or kings["k"] != 1:
        raise ValueError(f"FEN birer şah içermeli: {fen}")
    if not 2 <= total <= 32:
        raise ValueError(f"Geçersiz taş sayısı: {fen}")
    if max(colors) > 16 or max(pawns) > 8:
        raise ValueError(f"Geçersiz taraf başına taş sayısı: {fen}")
    return us, them, parts[1] == "w"


def mirror_fen(fen: str) -> str | None:
    """A-H horizontal mirror for FEN positions where castling rights no longer exist."""
    parts = fen.split()
    if len(parts) < 4 or parts[2] != "-":
        return None
    board, turn, _, ep = parts[0], parts[1], parts[2], parts[3]
    ranks = board.split("/")
    if len(ranks) != 8:
        return None
    mirrored_board = "/".join("".join(reversed(r)) for r in ranks)
    mirrored_ep = "-"
    if ep != "-" and len(ep) == 2:
        mirrored_file = chr(ord("a") + (7 - (ord(ep[0]) - ord("a"))))
        mirrored_ep = mirrored_file + ep[1]
    halfmove = parts[4] if len(parts) > 4 else "0"
    fullmove = parts[5] if len(parts) > 5 else "1"
    return f"{mirrored_board} {turn} - {mirrored_ep} {halfmove} {fullmove}"


def normalize_engine_fen(fen: str) -> str:
    chess = dependency("chess")
    try:
        board = chess.Board(fen)
    except Exception as exc:
        raise ValueError(f"Geçersiz FEN: {fen}") from exc
    if not board.is_valid():
        raise ValueError(f"Yasal olmayan FEN: {fen}")
    normalized = board.fen()
    parse_fen_fast(normalized)
    return normalized


def parse_position_metrics(fen: str) -> dict[str, Any]:
    parts = fen.split()
    board_part = parts[0]
    turn = parts[1]
    has_fullmove = len(parts) >= 6 and parts[5].isdigit()
    fullmove = int(parts[5]) if has_fullmove else 1
    # Plies: 2 * (fullmove - 1) + (0 if turn == 'w' else 1), exactly matching C++ search.h:2068
    plies = 2 * max(0, fullmove - 1) + (0 if turn == "w" else 1)

    white_material = black_material = 0
    total_pieces = 0
    for ch in board_part:
        lower = ch.lower()
        if lower not in PIECE_VALUES and lower != "k":
            continue
        total_pieces += 1
        if lower in PIECE_VALUES:
            val = PIECE_VALUES[lower]
            if ch.isupper():
                white_material += val
            else:
                black_material += val

    total_material = white_material + black_material
    my_material = white_material if turn == "w" else black_material
    opponent_material = black_material if turn == "w" else white_material

    # C++ Engine phase thresholds (params.h: LatePhaseMaterial=4200, EndgameMaterial=3000, OpeningMinPly=20)
    if total_material <= 3000:
        phase = "endgame"
    elif total_material <= 4200:
        phase = "late_middle"
    else:
        if has_fullmove and fullmove > 1:
            is_opening = (plies < 20)
        else:
            # Missing/default fullmove counters use the same home-rank heuristic as SBIN.
            ranks = board_part.split("/")
            white_home = sum(1 for ch in ranks[7] if ch in "RNBQK")
            black_home = sum(1 for ch in ranks[0] if ch in "rnbqk")
            is_opening = (white_home >= 6 and black_home >= 6)
        phase = "opening" if is_opening else "midgame"

    return {
        "phase": phase,
        "total_material": total_material,
        "white_material": white_material,
        "black_material": black_material,
        "plies": plies,
        "mat_down": opponent_material - my_material,
        "turn_white": turn == "w",
        "total_pieces": total_pieces,
    }


def add_dataset_record(records: list[tuple[str, float]], seen: set[str],
                       fen: str, wdl: float) -> bool:
    try:
        normalized = normalize_engine_fen(fen)
        value = float(wdl)
    except (TypeError, ValueError, OverflowError):
        return False
    if not math.isfinite(value) or not 0 <= value <= 1:
        return False
    key = " ".join(normalized.split()[:2])
    if key in seen:
        return False
    seen.add(key)
    records.append((normalized, value))
    return True


def write_dataset(records: Sequence[tuple[str, float]], output: Path) -> None:
    if not records:
        raise RuntimeError("Uygun veri bulunamadı.")
    if output.suffix.lower() not in (".sbin", ".parquet"):
        raise ValueError("Dataset çıktı uzantısı .sbin veya .parquet olmalı.")
    output.parent.mkdir(parents=True, exist_ok=True)
    temporary = output.with_name(f".{output.name}.{uuid.uuid4().hex}.tmp")

    try:
        if output.suffix.lower() == ".sbin":
            try:
                from training.sbin_tool import PackedPosition, load_native_lib
            except ImportError:
                from sbin_tool import PackedPosition, load_native_lib  # type: ignore[no-redef]
            import ctypes
            lib = load_native_lib()
            packed_buf = PackedPosition()
            batch_bytes = bytearray()
            with open(temporary, "wb") as out_f:
                for index, (fen, wdl) in enumerate(records):
                    w_val = float(wdl)
                    if not math.isfinite(w_val) or not 0.0 <= w_val <= 1.0:
                        raise ValueError(f"Geçersiz WDL; kayıt={index}")
                    fen_norm = normalize_engine_fen(fen)
                    ret = lib.sbin_pack_fen(
                        fen_norm.encode("utf-8"), ctypes.c_float(w_val),
                        ctypes.c_int16(0), ctypes.byref(packed_buf),
                    )
                    if ret != 0:
                        raise ValueError(f"SBIN yazılamadı; kayıt={index}, kod={ret}")
                    batch_bytes.extend(bytes(packed_buf))
                    if len(batch_bytes) >= 65536 * 32:
                        out_f.write(batch_bytes)
                        batch_bytes.clear()
                if batch_bytes:
                    out_f.write(batch_bytes)
        else:
            pa = dependency("pyarrow")
            pq = dependency("pyarrow.parquet")
            fens = []
            targets = []
            for index, (fen, wdl) in enumerate(records):
                value = float(wdl)
                if not math.isfinite(value) or not 0 <= value <= 1:
                    raise ValueError(f"Geçersiz WDL; kayıt={index}")
                fens.append(normalize_engine_fen(fen))
                targets.append(value)
            table = pa.Table.from_arrays(
                [pa.array(fens), pa.array(targets, type=pa.float32())],
                names=["fen", "wdl"],
            )
            pq.write_table(table, str(temporary), compression="zstd")
        temporary.replace(output)
    finally:
        temporary.unlink(missing_ok=True)


def fast_sgs_classify(fen: str, side_cp: float = 0.0) -> int | None:
    """Classify 6 material-imbalance categories (1, 2, 3, 4, 5, 9).

    1: 1-Pawn sacrifice
    2: 2-Pawn sacrifice
    3: Minor piece sacrifice (Knight / Bishop)
    4: Minor piece + pawn sacrifice
    5: Rook / exchange sacrifice
    9: Queen sacrifice
    """
    board_part, turn = fen.split()[:2]
    w_q, b_q = board_part.count("Q"), board_part.count("q")
    w_r, b_r = board_part.count("R"), board_part.count("r")
    w_min = board_part.count("B") + board_part.count("N")
    b_min = board_part.count("b") + board_part.count("n")
    w_p, b_p = board_part.count("P"), board_part.count("p")

    if turn == "w":
        my_q, opp_q = w_q, b_q
        my_r, opp_r = w_r, b_r
        my_min, opp_min = w_min, b_min
        my_p, opp_p = w_p, b_p
    else:
        my_q, opp_q = b_q, w_q
        my_r, opp_r = b_r, w_r
        my_min, opp_min = b_min, w_min
        my_p, opp_p = b_p, w_p

    my_mat = my_q * PIECE_VALUES["q"] + my_r * PIECE_VALUES["r"] + my_min * PIECE_VALUES["n"] + my_p * PIECE_VALUES["p"]
    opp_mat = opp_q * PIECE_VALUES["q"] + opp_r * PIECE_VALUES["r"] + opp_min * PIECE_VALUES["n"] + opp_p * PIECE_VALUES["p"]
    mat_down = opp_mat - my_mat

    # Yeterli materyal fedası ve dinamik kompanzasyon kontrolü
    if mat_down < 80 or side_cp < -30:
        return None

    # Vezir Fedası (SGS Tip 9)
    if my_q < opp_q:
        return 9
    # Kale / Kalite Fedası (SGS Tip 5)
    if my_r < opp_r:
        return 5
    # Hafif Taş Fedası (SGS Tip 3 & 4)
    if my_min < opp_min:
        return 4 if mat_down >= 350 else 3
    # Piyon Fedası (SGS Tip 1 & 2)
    if my_p < opp_p:
        return 2 if mat_down >= 180 else 1
    if mat_down >= 100:
        return 1
    return None


def phase_distribution(args: argparse.Namespace, target: int, rng: Any) -> dict[str, int]:
    np = dependency("numpy")
    names = ("endgame", "late_middle", "midgame", "opening")
    if getattr(args, "random_dist", False):
        floors = np.array([0.15, 0.15, 0.10, 0.05])
        weights = floors + (1.0 - floors.sum()) * rng.dirichlet([2.0, 2.0, 1.5, 1.0])
    else:
        text = getattr(args, "phase_dist", None) or "35,35,20,10"
        try:
            weights = np.array([float(value.strip()) for value in text.split(",")])
        except ValueError as exc:
            raise ValueError("phase-dist dört sayısal oran içermeli.") from exc
        if (len(weights) != 4 or not np.isfinite(weights).all() or
                (weights < 0).any() or not math.isfinite(float(weights.sum())) or weights.sum() <= 0):
            raise ValueError("phase-dist sonlu, negatif olmayan ve toplamı pozitif dört oran içermeli.")
        weights /= weights.sum()
    quotas = weights * target
    counts = np.floor(quotas).astype(np.int64)
    for index in np.argsort(-(quotas - counts), kind="stable")[:target - int(counts.sum())]:
        counts[index] += 1
    result = dict(zip(names, map(int, counts)))
    print("Faz hedefleri: " + ", ".join(f"{name}={count:,}" for name, count in result.items()), flush=True)
    return result


def sbin_depth(source: Path, requested: int) -> int | None:
    if requested < 0:
        raise ValueError("min-depth negatif olamaz.")
    for suffix in (".sbin.json", ".extract.json"):
        try:
            metadata = json.loads(source.with_suffix(suffix).read_text(encoding="utf-8"))
            depth = metadata.get("source_min_depth", metadata.get("min_depth"))
            if (metadata.get("output") == file_identity(source) and
                    isinstance(depth, int) and depth > 0):
                if requested > depth:
                    raise ValueError(f"SBIN yalnızca en az depth {depth} garantisi veriyor; depth {requested} filtresi için ham eval kaynağı gerekli.")
                return depth
        except (OSError, json.JSONDecodeError, AttributeError, TypeError):
            continue
    if requested:
        print(f"[BİLGİ] {source.name} derinlik manifesti içermiyor; depth={requested} varsayılıyor.", flush=True)
        return requested
    return None


def eval_wdl(white_cp: float, stored_wdl: float, lambda_val: float = 0.25) -> float:
    if abs(white_cp) == 2000 and stored_wdl in (0.0, 1.0):
        return 1.0 if white_cp > 0 else 0.0
    cp_wdl = cp_to_wdl(white_cp)
    if math.isfinite(stored_wdl) and 0.0 <= stored_wdl <= 1.0:
        return (1.0 - lambda_val) * cp_wdl + lambda_val * stored_wdl
    return cp_wdl


def extract_sbin_base(args: argparse.Namespace, source: Path, output: Path,
                      skip: int, distribution: dict[str, int], rng: Any,
                      source_before: dict[str, Any]) -> dict[str, Any]:
    np = dependency("numpy")
    sbin = dependency("training.sbin_tool" if __package__ else "sbin_tool")
    import ctypes
    target = sum(distribution.values())
    counts = {name: 0 for name in distribution}
    selected: list[int] = []
    seen: set[bytes] = set()
    invalid = duplicates = scanned = 0
    next_offset = skip
    started = last_print = time.monotonic()
    phases = np.empty(262144, dtype=np.uint8)
    labels = getattr(args, "sbin_labels", "cp")
    with sbin.SbinDataset(source) as ds:
        split = min(skip, ds.count)
        for begin, end in ((split, ds.count), (0, split)):
            for cur in range(begin, end, len(phases)):
                if len(selected) >= target:
                    break
                count = min(len(phases), end - cur)
                ptr = ctypes.byref(ds._array[cur])
                try:
                    ds.lib.sbin_classify_phases_batch(ptr, count, phases.ctypes.data_as(ctypes.POINTER(ctypes.c_uint8)))
                finally:
                    del ptr
                eligible = np.zeros(count, dtype=bool)
                for p, name in enumerate(distribution):
                    if counts[name] < distribution[name]:
                        eligible |= phases[:count] == p
                consumed = count
                names = tuple(distribution)
                for relative in np.flatnonzero(eligible):
                    index = cur + int(relative)
                    name = names[int(phases[relative])]
                    if counts[name] >= distribution[name]:
                        continue
                    raw = ds._mmap[index * 32:(index + 1) * 32]
                    key = raw[:24] + bytes([raw[28] & 1])
                    if key in seen:
                        duplicates += 1
                        continue
                    seen.add(key)
                    selected.append(index)
                    counts[name] += 1
                    if len(selected) >= target:
                        consumed = int(relative) + 1
                        break
                scanned += consumed
                invalid += int(np.count_nonzero(phases[:consumed] == 255))
                next_offset = cur + consumed
                if time.monotonic() - last_print >= 5:
                    print(f"Taranan={scanned:,} toplanan={len(selected):,}/{target:,} geçersiz={invalid:,} tekrar={duplicates:,}", flush=True)
                    last_print = time.monotonic()
            if len(selected) >= target:
                break
        if not selected:
            raise RuntimeError("Uygun SBIN pozisyonu bulunamadı.")
        del seen
        rng.shuffle(selected)
        output.parent.mkdir(parents=True, exist_ok=True)
        temporary = output.with_name(f".{output.name}.{uuid.uuid4().hex}.tmp")
        try:
            with temporary.open("wb") as stream:
                for begin in range(0, len(selected), 65536):
                    data = b"".join(ds._mmap[index * 32:(index + 1) * 32] for index in selected[begin:begin + 65536])
                    stream.write(sbin.calibrate_eval_records(data) if labels == "cp" else data)
            if file_identity(source) != source_before:
                raise RuntimeError("Eval kaynağı çıkarma sırasında değişti; çıktı yayımlanmadı.")
            temporary.replace(output)
        finally:
            temporary.unlink(missing_ok=True)
    print(f"SBIN çıkarma: {len(selected):,} kayıt, {time.monotonic() - started:.1f} sn; geçersiz={invalid:,}, tekrar={duplicates:,}", flush=True)
    return {"rows": len(selected), "counts": counts, "invalid_rows": invalid,
            "duplicate_rows": duplicates, "next_offset": next_offset}


def mine_hard_dataset(args: argparse.Namespace) -> Path:
    """Evaluate positions with a champion NNUE model on GPU and extract the hardest examples."""
    torch = dependency("torch")
    np = dependency("numpy")
    sbin = dependency("training.sbin_tool" if __package__ else "sbin_tool")
    import ctypes

    source = resolve_path(getattr(args, "source", None) or getattr(args, "zst", None), DEFAULT_EVAL)
    source = require_file(source, "Eval SBIN kaynağı")
    model_path = resolve_path(getattr(args, "model", None), NETS / "base.nnue")
    model_path = require_network(model_path, "Madencilik Modeli")
    output = resolve_path(args.output, ROOT / "data" / "base_hard.sbin")
    output.parent.mkdir(parents=True, exist_ok=True)
    target = int(args.target or 5_000_000)
    pool_size = int(getattr(args, "pool_size", 0) or max(target * 3, 15_000_000))
    device_name = getattr(args, "device", "auto")
    device = _torch_device(torch, device_name)

    print(f"\n=== Zor Örnek Madenciliği (Hard Example Mining) Başlatılıyor ===", flush=True)
    print(f"Referans Model: {model_path} ({file_sha256(model_path)[:16]})", flush=True)
    print(f"Kaynak Veri: {source} ({source.stat().st_size // (1024*1024):,} MB)", flush=True)
    print(f"Tarama Havuzu: {pool_size:,} pozisyon", flush=True)
    print(f"Hedeflenen Zor Pozisyon: {target:,}", flush=True)
    print(f"İşlem Cihazı: {device}\n", flush=True)

    model = _make_nnue_model().to(device)
    load_nnue(model, model_path)
    model.eval()

    offset_file = resolve_path(args.offset_file)
    skip = 0
    if offset_file and offset_file.exists() and not getattr(args, "no_offset", False):
        try:
            state = json.loads(offset_file.read_text(encoding="utf-8"))
            skip = int(state.get("hard_offset", state.get("base_offset", 0)) or 0)
        except Exception:
            skip = 0

    chunk_size = 65536
    all_errors = np.empty(pool_size, dtype=np.float32)
    start_indices = np.empty(pool_size, dtype=np.int64)

    started = time.perf_counter()
    scanned = 0
    features_buf = np.empty((chunk_size, 2, 32), dtype=np.int16)
    targets_buf = np.empty(chunk_size, dtype=np.float32)
    indices_buf = np.empty(chunk_size, dtype=np.uint32)
    f_ptr = features_buf.ctypes.data_as(ctypes.POINTER(ctypes.c_int16))
    t_ptr = targets_buf.ctypes.data_as(ctypes.POINTER(ctypes.c_float))
    i_ptr = indices_buf.ctypes.data_as(ctypes.POINTER(ctypes.c_uint32))

    with sbin.SbinDataset(source) as ds:
        total_in_ds = ds.count
        if skip >= total_in_ds:
            skip = 0
        cur = skip
        while scanned < pool_size:
            count = min(chunk_size, pool_size - scanned)
            if cur + count > total_in_ds:
                cur = 0
            count = min(count, total_in_ds - cur)

            pos_ptr = ctypes.byref(ds._array[cur])
            try:
                decoded = ds.lib.sbin_batch_decode_indexed(pos_ptr, count, f_ptr, t_ptr, i_ptr)
            finally:
                del pos_ptr

            if decoded == 0:
                cur += count
                continue

            tensor = torch.as_tensor(features_buf[:decoded], device=device, dtype=torch.int64)
            mask = (tensor >= 0).to(dtype=torch.float32)
            tensor = tensor.clamp_min(0)
            labels = torch.as_tensor(targets_buf[:decoded], device=device)

            with torch.no_grad():
                logits = model(tensor[:, 0], tensor[:, 1], mask[:, 0], mask[:, 1])
                preds = torch.sigmoid(logits)
                errors = torch.abs(preds - labels)
                err_np = errors.cpu().numpy()

            take = min(decoded, pool_size - scanned)
            all_errors[scanned:scanned + take] = err_np[:take]
            start_indices[scanned:scanned + take] = cur + indices_buf[:take].astype(np.int64)

            scanned += take
            cur += count

            if scanned % (chunk_size * 16) == 0 or scanned >= pool_size:
                elapsed = time.perf_counter() - started
                rate = scanned / elapsed if elapsed > 0 else 0
                mean_err = float(np.mean(all_errors[:scanned]))
                print(f"Tarandı: {scanned:,}/{pool_size:,} ({rate:,.0f} pos/s) - Ortalama Hata: {mean_err:.4f}", flush=True)

        if offset_file and not getattr(args, "no_offset", False):
            try:
                state = json.loads(offset_file.read_text(encoding="utf-8")) if offset_file.exists() else {}
                state["hard_offset"] = cur
                atomic_json(offset_file, state)
            except Exception:
                pass

        total_elapsed = time.perf_counter() - started
        print(f"\n--- Tarama Tamamlandı ({total_elapsed:.1f} sn, {scanned / max(0.001, total_elapsed):,.0f} pos/s). En Zor {target:,} Pozisyon Seçiliyor... ---", flush=True)
        if target >= pool_size:
            selected_local = np.arange(pool_size)
        else:
            top_k_local = np.argpartition(all_errors, -target)[-target:]
            selected_local = top_k_local

        mined_indices = start_indices[selected_local]
        mined_errors = all_errors[selected_local]
        mined_indices.sort()

        mean_pool_error = float(np.mean(all_errors))
        mean_hard_error = float(np.mean(mined_errors))
        min_hard_error = float(np.min(mined_errors))
        max_hard_error = float(np.max(mined_errors))

        print(f"Genel Havuz Ortalama Hata: {mean_pool_error:.4f}", flush=True)
        print(f"Seçilen Zor Pozisyonlar Ortalama Hata: {mean_hard_error:.4f} (Min: {min_hard_error:.4f}, Max: {max_hard_error:.4f})", flush=True)
        print(f"Zorluk Artış Katsayısı: {mean_hard_error / max(1e-5, mean_pool_error):.2f}x daha zor!", flush=True)

        print(f"Zor pozisyonlar diske yazılıyor: {output}...", flush=True)
        temporary = output.with_name(f".{output.name}.{uuid.uuid4().hex}.tmp")
        try:
            with temporary.open("wb") as stream:
                for begin in range(0, len(mined_indices), 65536):
                    data = b"".join(ds._mmap[idx * 32:(idx + 1) * 32] for idx in mined_indices[begin:begin + 65536])
                    stream.write(sbin.calibrate_eval_records(data))
            temporary.replace(output)
        finally:
            temporary.unlink(missing_ok=True)

        atomic_json(output.with_suffix(".extract.json"), {
            "source": str(source), "output": str(output), "model": str(model_path),
            "model_sha256": file_sha256(model_path),
            "pool_size": pool_size, "target": target, "rows": len(mined_indices),
            "mean_pool_error": mean_pool_error, "mean_hard_error": mean_hard_error,
            "min_hard_error": min_hard_error, "max_hard_error": max_hard_error,
            "difficulty_multiplier": mean_hard_error / max(1e-5, mean_pool_error),
            "created_at": datetime.now().isoformat(),
        })

    print(f"[BAŞARILI] {len(mined_indices):,} zor pozisyon {output} dosyasına kaydedildi ({output.stat().st_size:,} byte).\n", flush=True)
    return output


def extract_dataset(args: argparse.Namespace, phase: str | None = None,
                    output: Path | None = None) -> Path:
    np = dependency("numpy")
    phase = phase or args.phase
    if phase not in ("base", "aggressive"):
        raise RuntimeError("extract için phase base veya aggressive olmalı.")
    output = output or resolve_path(args.output)
    output = output or ROOT / "data" / f"{phase}.sbin"
    output = output.resolve()
    if output.suffix.lower() not in (".sbin", ".parquet"):
        raise ValueError("Dataset çıktı uzantısı .sbin veya .parquet olmalı.")

    if phase == "aggressive" and getattr(args, "aggressive_source", "simple") == "puzzles":
        puzzle_sbin = resolve_path(args.puzzles, DEFAULT_PUZZLES)
        if not puzzle_sbin or not puzzle_sbin.is_file():
            raise RuntimeError(f"Feda bulmaca havuzu bulunamadı: {puzzle_sbin}")
        target_limit = getattr(args, "target", None)
        if target_limit is not None and target_limit < 2:
            raise ValueError("target en az 2 olmalı.")
        records: list[tuple[str, float]] = []
        seen: set[str] = set()

        if output == puzzle_sbin.resolve():
            raise ValueError("Puzzle havuzu ve çıktı aynı dosya olamaz.")
        try:
            from training.sbin_tool import SbinDataset
        except ImportError:
            from sbin_tool import SbinDataset  # type: ignore[no-redef]
        with SbinDataset(puzzle_sbin) as p_ds:
            p_len = len(p_ds)
            seed_val = getattr(args, "seed", None)
            p_rng = np.random.default_rng(int(seed_val) if seed_val is not None else int(time.time()))
            indices = p_rng.permutation(p_len)
            for idx in indices:
                p_fen, p_wdl, _ = p_ds.get_fen(int(idx))
                add_dataset_record(records, seen, p_fen, p_wdl)
                if target_limit and len(records) >= target_limit:
                    break
        print(f"[BAŞARILI] {len(records):,} saf feda pozisyonu doğrudan puzzle_sacrifices.sbin havuzundan çekildi.", flush=True)
        write_dataset(records, output)
        if target_limit and len(records) < target_limit and not getattr(args, "allow_short_dataset", False):
            raise RuntimeError(f"Hedef tamamlanmadı: {len(records):,}/{target_limit:,}; kısmi veri {output} içinde korundu.")
        return output

    source = resolve_path(getattr(args, "source", None) or getattr(args, "zst", None), DEFAULT_EVAL)
    source = require_file(source, "Eval SBIN kaynağı")
    if source.suffix.lower() != ".sbin":
        raise ValueError(f"Yalnızca .sbin eval arşivleri desteklenir: {source}")

    seed = getattr(args, "seed", None)
    if seed is not None:
        try:
            seed = int(seed)
        except (TypeError, ValueError) as exc:
            raise ValueError("seed tam sayı olmalı.") from exc
    target = int(args.target if args.target is not None else (
        args.base_target if phase == "base" else args.aggressive_target
    ))
    if source.resolve() == output:
        raise ValueError("Eval kaynağı ve çıktı aynı dosya olamaz.")
    if target < 2 or args.min_depth < 0 or (args.skip_lines or 0) < 0:
        raise RuntimeError("target en az 2, min-depth ve skip-lines negatif olmayan sayılar olmalı.")

    puzzle_ratio = float(getattr(args, "puzzle_ratio", 0.20))
    sac_ratio = float(getattr(args, "sac_ratio", 0.50))
    if not 0.0 <= puzzle_ratio <= 1.0:
        raise RuntimeError("puzzle-ratio 0 ile 1 arasında olmalı.")
    if not 0.0 <= sac_ratio <= 1.0:
        raise RuntimeError("sac-ratio 0 ile 1 arasında olmalı.")

    offset_file = resolve_path(args.offset_file)
    state: dict[str, Any] = {}
    if offset_file and offset_file.exists():
        try:
            state = json.loads(offset_file.read_text(encoding="utf-8"))
            if not isinstance(state, dict):
                state = {}
        except (OSError, ValueError, TypeError):
            state = {}

    rng = np.random.default_rng(seed if seed is not None else int(time.time()))
    if getattr(args, "no_offset", False):
        upper = source.stat().st_size // 32
        skip_lines = int(rng.integers(0, max(1, upper)))
        print(f"Rastgele veri modu aktif: Rastgele başlangıç satırı = {skip_lines:,}")
    elif args.reset_offset:
        skip_lines = 0
        if offset_file:
            state[f"{phase}_offset"] = 0
    else:
        if (args.skip_lines is None and state.get("source") is not None and
                state["source"] != file_identity(source)):
            raise ValueError("Eval kaynağı değişti; --reset-offset veya açık --skip-lines kullanın.")
        state["source"] = file_identity(source)
        skip_lines = args.skip_lines if args.skip_lines is not None else int(state.get(f"{phase}_offset", 0) or 0)

    print(f"Veri çıkarma: {phase.upper()} hedef={target:,} kaynak={source}")

    distribution = phase_distribution(args, target, rng) if phase == "base" else {}
    source_before = file_identity(source)
    source_min_depth = sbin_depth(source, args.min_depth)
    if phase == "base":
        result = extract_sbin_base(args, source, output, skip_lines, distribution, rng, source_before)
        if offset_file and not getattr(args, "no_offset", False):
            state[f"{phase}_offset"] = result["next_offset"]
            state["source"] = source_before
            atomic_json(offset_file, state)
        atomic_json(output.with_suffix(".extract.json"), {
            "source": source_before, "output": file_identity(output), "phase": phase,
            "target": target, **result, "start_after": skip_lines,
            "min_depth_requested": args.min_depth, "source_min_depth": source_min_depth,
            "label_source": getattr(args, "sbin_labels", "cp"), "seed": seed,
            "phase_dist": distribution,
        })
        if result["rows"] < target and not getattr(args, "allow_short_dataset", False):
            raise RuntimeError(f"Hedef tamamlanmadı: {result['rows']:,}/{target:,}; kısmi veri {output} içinde korundu.")
        return output

    records: list[tuple[str, float]] = []
    seen: set[str] = set()
    total_scanned = 0
    invalid = 0
    started = time.time()

    puzzle_count = 0
    puzzle_target = 0
    if phase == "aggressive" and puzzle_ratio:
        puzzle_sbin = resolve_path(args.puzzles, DEFAULT_PUZZLES)
        puzzle_target = min(target, int(target * puzzle_ratio))
        if puzzle_sbin and puzzle_sbin.is_file():
            try:
                from training.sbin_tool import SbinDataset
            except ImportError:
                from sbin_tool import SbinDataset  # type: ignore[no-redef]
            with SbinDataset(puzzle_sbin) as p_ds:
                p_len = len(p_ds)
                sample_count = min(puzzle_target, p_len)
                puzzle_seed = (seed + (skip_lines % 100000)
                               if seed is not None else int(time.time()))
                puzzle_rng = np.random.default_rng(puzzle_seed)
                indices = puzzle_rng.choice(p_len, size=sample_count, replace=False)
                for idx in indices:
                    p_fen, p_wdl, _ = p_ds.get_fen(int(idx))
                    if add_dataset_record(records, seen, p_fen, p_wdl):
                        puzzle_count += 1
            print(f"SBIN feda havuzundan seçilen: {puzzle_count:,} (toplam havuz={p_len:,})", flush=True)

    target_sacs = min(target, max(int(target * sac_ratio), puzzle_count))
    target_sharp = target - target_sacs
    counts = {"sacrifices": puzzle_count, "sharp": 0}
    sac_counts: dict[int, int] = {1: 0, 2: 0, 3: 0, 4: 0, 5: 0, 9: 0}

    def accept_record(fen: str, white_wdl: float, white_cp: float) -> None:
        metrics = parse_position_metrics(fen)
        side_cp = white_cp if metrics["turn_white"] else -white_cp
        sharp = metrics["phase"] in ("midgame", "late_middle", "opening") and abs(side_cp) > 50
        sac_type = fast_sgs_classify(fen, side_cp)
        if sac_type and counts["sacrifices"] < target_sacs:
            if add_dataset_record(records, seen, fen, white_wdl):
                counts["sacrifices"] += 1
                sac_counts[sac_type] = sac_counts.get(sac_type, 0) + 1
                if getattr(args, "augment_mirror", False) and counts["sacrifices"] < target_sacs:
                    m_fen = mirror_fen(fen)
                    if m_fen and add_dataset_record(records, seen, m_fen, white_wdl):
                        counts["sacrifices"] += 1
        elif sharp and counts["sharp"] < target_sharp:
            if add_dataset_record(records, seen, fen, white_wdl):
                counts["sharp"] += 1
                if getattr(args, "augment_mirror", False) and counts["sharp"] < target_sharp:
                    m_fen = mirror_fen(fen)
                    if m_fen and add_dataset_record(records, seen, m_fen, white_wdl):
                        counts["sharp"] += 1

    def scan_file(start_after: int, stop_after: int | None = None) -> None:
        nonlocal total_scanned, invalid
        try:
            from training.sbin_tool import SbinDataset, load_native_lib
        except ImportError:
            from sbin_tool import SbinDataset, load_native_lib  # type: ignore[no-redef]
        import ctypes
        lib = load_native_lib()
        with SbinDataset(source) as ds:
            batch_size = 65536
            status = np.zeros(batch_size, dtype=np.uint8)
            cur = start_after
            limit = ds.count if stop_after is None else min(ds.count, stop_after)
            while cur < limit and len(records) < target:
                count = min(batch_size, limit - cur)
                ptr = ctypes.byref(ds._array[cur])
                try:
                    valid_count = lib.sbin_validate_batch(ptr, count, status[:count].ctypes.data_as(ctypes.POINTER(ctypes.c_uint8)))
                finally:
                    del ptr
                invalid += (count - valid_count)
                total_scanned = cur + count
                valid_idx = np.flatnonzero(status[:count] == 0)
                for v_idx in valid_idx:
                    g_idx = cur + int(v_idx)
                    try:
                        fen, white_wdl, white_cp = ds.get_fen(g_idx)
                        if getattr(args, "sbin_labels", "cp") == "cp":
                            white_wdl = eval_wdl(white_cp, white_wdl, getattr(args, "wdl_lambda", 0.25))
                        accept_record(fen, white_wdl, white_cp)
                    except (ValueError, TypeError, KeyError, OverflowError):
                        invalid += 1
                    if len(records) >= target:
                        total_scanned = g_idx + 1
                        break
                cur += count
                if total_scanned % 200000 < batch_size:
                    speed = (total_scanned - start_after) / max(1.0, time.time() - started)
                    detail = ", ".join(f"{key}={value:,}" for key, value in counts.items())
                    print(f"Taranan: {total_scanned:,} Toplanan: {len(records):,}/{target:,} "
                          f"({detail}) [{speed:.0f} pos/sn]", flush=True)

    scan_file(skip_lines)
    if len(records) < target and skip_lines > 0:
        print(f"\nDosya sonuna ulaşıldı ({total_scanned:,}). Hedef ({target:,}) için dosya başından devam ediliyor...", flush=True)
        scan_file(0, skip_lines)
    if not records:
        raise RuntimeError("Uygun engine pozisyonu bulunamadı.")
    order = np.random.default_rng(seed if seed is not None else int(time.time())).permutation(len(records))
    if file_identity(source) != source_before:
        raise RuntimeError("Eval kaynağı çıkarma sırasında değişti; çıktı yayımlanmadı.")
    write_dataset([records[int(index)] for index in order], output)
    if offset_file and not getattr(args, "no_offset", False):
        state[f"{phase}_offset"] = total_scanned
        state["source"] = file_identity(source)
        atomic_json(offset_file, state)
    atomic_json(output.with_suffix(".extract.json"), {
        "source": source_before, "output": file_identity(output), "phase": phase, "target": target,
        "rows": len(records), "counts": counts, "invalid_rows": invalid,
        "start_after": skip_lines, "next_offset": total_scanned,
        "min_depth_requested": args.min_depth, "source_min_depth": source_min_depth, "seed": seed,
        "label_source": getattr(args, "sbin_labels", "cp") if source.suffix.lower() == ".sbin" else "cp",
        "phase_dist": distribution if phase == "base" else None,
    })
    if len(records) < target and not getattr(args, "allow_short_dataset", False):
        raise RuntimeError(f"Hedef tamamlanmadı: {len(records):,}/{target:,}. Kısmi veri {output} içinde korundu; kullanmak için --allow-short-dataset gerekli.")
    if phase == "aggressive":
        print(f"SGS Feda Dağılımı: Vezir={sac_counts.get(9, 0):,}, Kale={sac_counts.get(5, 0):,}, HafifTaş={sac_counts.get(3, 0)+sac_counts.get(4, 0):,}, Piyon={sac_counts.get(1, 0)+sac_counts.get(2, 0):,}", flush=True)
    print(f"[BAŞARILI] {output}: {len(records):,} kayıt; atlanan={invalid:,}")
    return output


def _parse_range(value: str, label: str) -> tuple[int, int]:
    pieces = [part.strip() for part in str(value).split("-", 1)]
    try:
        bounds = tuple(int(part) for part in pieces)
    except (TypeError, ValueError) as exc:
        raise ValueError(f"{label} min-max biçiminde olmalı.") from exc
    if len(bounds) == 1:
        low = high = bounds[0]
    elif len(bounds) == 2:
        low, high = bounds
    else:
        raise ValueError(f"{label} min-max biçiminde olmalı.")
    if low < 0 or high < low:
        raise ValueError(f"{label} aralığı geçersiz.")
    return low, high


def _selfplay_game(engine_path: Path, depth: int, random_chance: float,
                   opening_moves: tuple[int, int], max_moves: int,
                   seed: int) -> list[tuple[str, float]]:
    """Play one fully adjudicated game and return outcome-labelled positions.

    A separate persistent UCI process is used per game.  Starting an engine
    for every ply (the old selfplay helper did this) loses state and makes the
    generated labels and timing incomparable.
    """
    chess = dependency("chess")
    try:
        engine_api = importlib.import_module("chess.engine")
    except ImportError as exc:
        raise RuntimeError("python-chess engine API gerekli.") from exc
    rng = __import__("random").Random(seed)
    process = None
    positions: list[str] = []
    try:
        process = engine_api.SimpleEngine.popen_uci(str(engine_path))
        try:
            process.configure({"Threads": 1})
        except Exception:
            # Some small UCI engines do not expose Threads; analysis remains
            # valid with their default setting.
            pass
        board = chess.Board()
        opening_plies = rng.randint(*opening_moves) * 2
        for _ in range(opening_plies):
            if board.is_game_over(claim_draw=True):
                break
            board.push(rng.choice(list(board.legal_moves)))

        max_plies = max_moves * 2
        while len(board.move_stack) < max_plies and not board.is_game_over(claim_draw=True):
            fen = board.fen()
            if rng.random() < random_chance:
                move = rng.choice(list(board.legal_moves))
            else:
                info = process.analyse(board, engine_api.Limit(depth=depth))
                pv = info.get("pv") or []
                move = pv[0] if pv and pv[0] in board.legal_moves else None
                if move is None:
                    move = process.play(board, engine_api.Limit(depth=depth)).move
            if move is None or move not in board.legal_moves:
                return []
            positions.append(fen)
            board.push(move)

        outcome = board.outcome(claim_draw=True)
        if outcome is None:
            # A truncated game has no ground-truth result and must not be
            # turned into a fabricated draw/win label.
            return []
        value = {"1-0": 1.0, "0-1": 0.0, "1/2-1/2": 0.5}.get(outcome.result())
        if value is None:
            return []
        return [(fen, value) for fen in positions]
    finally:
        if process is not None:
            try:
                process.quit()
            except Exception:
                process.close()


def run_datagen(args: argparse.Namespace) -> Path:
    """Generate a small, reproducible self-play dataset for smoke/training use."""
    engine = require_file(getattr(args, "engine", None), "Self-play motoru")
    games = int(getattr(args, "games", 0))
    depth = int(getattr(args, "depth", 8))
    max_moves = int(getattr(args, "max_moves", 80))
    concurrency = int(getattr(args, "concurrency", 1))
    random_chance = float(getattr(args, "random_chance", 0.05))
    opening_moves = _parse_range(getattr(args, "opening_moves", "4-8"), "opening-moves")
    if games < 1 or depth < 1 or max_moves < 1 or concurrency < 1:
        raise ValueError("datagen games, depth, max-moves ve concurrency pozitif olmalı.")
    if not math.isfinite(random_chance) or not 0.0 <= random_chance <= 1.0:
        raise ValueError("random-chance 0 ile 1 arasında olmalı.")
    output = (resolve_path(getattr(args, "output", None)) or
              ROOT / "data" / "selfplay.parquet").resolve()
    seed = int(getattr(args, "seed", 42))
    from concurrent.futures import ThreadPoolExecutor
    records: list[tuple[str, float]] = []
    seen: set[str] = set()
    completed = 0
    failed = 0
    discarded = 0
    print(f"Self-play: motor={engine} oyun={games} depth={depth} threads={concurrency}", flush=True)
    with ThreadPoolExecutor(max_workers=concurrency) as pool:
        futures = [pool.submit(
            _selfplay_game, engine, depth, random_chance, opening_moves,
            max_moves, seed + index,
        ) for index in range(games)]
        for future in futures:
            try:
                game_records = future.result()
            except Exception as exc:
                failed += 1
                print(f"[UYARI] Self-play oyunu atlandı: {exc}", flush=True)
                continue
            if not game_records:
                discarded += 1
                continue
            completed += 1
            for fen, value in game_records:
                add_dataset_record(records, seen, fen, value)
    if not records:
        raise RuntimeError("Self-play geçerli, sonuçlanmış pozisyon üretmedi.")
    write_dataset(records, output)
    atomic_json(output.with_suffix(".datagen.json"), {
        "engine": file_identity(engine), "games_requested": games,
        "games_completed": completed, "games_failed": failed,
        "games_discarded": discarded,
        "positions": len(records), "depth": depth, "max_moves": max_moves,
        "random_chance": random_chance, "opening_moves": list(opening_moves),
        "concurrency": concurrency, "seed": seed,
    })
    print(f"[BAŞARILI] Self-play: {len(records):,} benzersiz pozisyon -> {output}", flush=True)
    return output


def prepare_dataset(args: argparse.Namespace) -> Path:
    """Prepare an aggressive puzzle pool using the same validated writer."""
    if getattr(args, "phase", "aggressive") != "aggressive":
        raise ValueError("prepare yalnızca aggressive fazında kullanılabilir.")
    output = (resolve_path(getattr(args, "output", None)) or
              ROOT / "data" / "aggressive-prepared.sbin").resolve()
    prepared_args = _namespace_copy(
        args, phase="aggressive", aggressive_source="puzzles", output=output,
    )
    return extract_dataset(prepared_args, "aggressive", output)


@contextmanager
def advisory_lock(path: Path) -> Iterator[None]:
    path.parent.mkdir(parents=True, exist_ok=True)
    handle = path.open("a+")
    locked = False
    try:
        try:
            import fcntl
            fcntl.flock(handle.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
            locked = True
        except ImportError:
            pass
        except BlockingIOError as exc:
            raise RuntimeError(f"Çıktı başka bir işlem tarafından hazırlanıyor: {path}") from exc
        yield
    finally:
        if locked:
            import fcntl
            fcntl.flock(handle.fileno(), fcntl.LOCK_UN)
        handle.close()


def feature_group_ids(features: Any) -> Any:
    """Hash the complete NNUE input, independent of piece enumeration order.

    Mirrored positions with identical side-to-move features share a partition.
    Rights/clocks are excluded because this architecture cannot observe them.
    """
    np = dependency("numpy")
    values = np.asarray(features, dtype=np.int32)
    if values.ndim != 3 or values.shape[1:] != (2, 32):
        raise ValueError(f"NNUE özellik şekli [N,2,32] olmalı: {values.shape}")
    # Both accumulators are observable by the network.  Hashing only the
    # side-to-move half lets mirrored/opponent-swapped positions leak between
    # train and validation despite having different predictions.
    canonical = np.sort(values, axis=2)
    hashes = np.full(len(canonical), 2166136261, dtype=np.uint32)
    for side in range(2):
        for column in canonical[:, side, :].T:
            hashes = (hashes ^ (column + 1).astype(np.uint32)) * np.uint32(16777619)
    # Avalanche the FNV hash so a probability threshold is not occupancy-biased.
    hashes ^= hashes >> 16
    hashes *= np.uint32(0x7FEB352D)
    hashes ^= hashes >> 15
    hashes *= np.uint32(0x846CA68B)
    return hashes ^ (hashes >> 16)


def split_feature_groups(groups: Any, validation: float) -> tuple[Any, Any]:
    np = dependency("numpy")
    selected = groups.astype(np.float64) < validation * 2**32
    if not selected.any() or selected.all():
        unique = np.unique(groups)
        if len(unique) < 2:
            raise ValueError("Eğitim ve doğrulama için en az iki farklı pozisyon grubu gerekli.")
        # Small datasets still move whole groups, never individual duplicate rows.
        selected = groups == unique[0]
    return np.flatnonzero(~selected), np.flatnonzero(selected)


def prepare_feature_cache(dataset: Path) -> tuple[Any, Any, Any, dict[str, Any]]:
    """Decode Parquet or SBIN safely into a versioned, memory-mapped feature cache."""
    np = dependency("numpy")
    dataset = dataset.resolve()
    if not dataset.is_file():
        raise RuntimeError(f"Dataset bulunamadı: {dataset}")
    stat = dataset.stat()
    cache_id = hashlib.sha256(
        f"cache-v{CACHE_VERSION}:{dataset}:{stat.st_size}:{stat.st_mtime_ns}".encode()
    ).hexdigest()[:24]
    parent = dataset.parent / ".feature_cache"
    cache = parent / cache_id
    marker = cache / "complete.json"

    def load_complete() -> tuple[Any, Any, Any, dict[str, Any]] | None:
        if not marker.is_file():
            return None
        try:
            info = json.loads(marker.read_text(encoding="utf-8"))
            arrays = tuple(np.load(cache / name, mmap_mode="r")
                           for name in ("features.npy", "targets.npy", "groups.npy"))
            rows = int(info["rows"])
            if (info["version"] == CACHE_VERSION and rows >= 2 and
                    arrays[0].shape == (rows, 2, 32) and arrays[0].dtype == np.int16 and
                    arrays[1].shape == arrays[2].shape == (rows,) and
                    arrays[1].dtype == np.float32 and arrays[2].dtype == np.uint32):
                return arrays[0], arrays[1], arrays[2], info
        except (OSError, ValueError, KeyError, TypeError):
            return None
        return None

    complete = load_complete()
    if complete is not None:
        return complete
    parent.mkdir(parents=True, exist_ok=True)
    with advisory_lock(parent / f"{cache_id}.lock"):
        complete = load_complete()
        if complete is not None:
            return complete
        temporary = parent / f".{cache_id}.{uuid.uuid4().hex}.tmp"
        temporary.mkdir()
        invalid = valid = 0
        try:
            sbin_file = dataset if dataset.suffix.lower() == ".sbin" else temporary / "source.sbin"
            if dataset.suffix.lower() != ".sbin":
                try:
                    try:
                        from training.sbin_tool import convert_parquet_to_sbin
                    except ImportError:
                        from sbin_tool import convert_parquet_to_sbin
                    convert_parquet_to_sbin(dataset, sbin_file)
                except (OSError, RuntimeError, subprocess.SubprocessError) as exc:
                    print(f"[ÖNBELLEK] Native SBIN kullanılamıyor; Parquet okunuyor: {exc}", flush=True)
                    sbin_file = None

            if sbin_file is not None and sbin_file.is_file():
                try:
                    from training.sbin_tool import SbinDataset, load_native_lib
                except ImportError:
                    from sbin_tool import SbinDataset, load_native_lib  # type: ignore[no-redef]
                import ctypes
                lib = load_native_lib()
                with SbinDataset(sbin_file) as ds:
                    rows = ds.count
                    if rows < 2:
                        raise RuntimeError("Dataset en az iki satır içermeli.")
                    features = np.lib.format.open_memmap(
                        temporary / "features.npy", mode="w+", dtype=np.int16, shape=(rows, 2, 32)
                    )
                    targets = np.lib.format.open_memmap(
                        temporary / "targets.npy", mode="w+", dtype=np.float32, shape=(rows,)
                    )
                    groups = np.lib.format.open_memmap(
                        temporary / "groups.npy", mode="w+", dtype=np.uint32, shape=(rows,)
                    )
                    for begin in range(0, rows, 65536):
                        count = min(65536, rows - begin)
                        f_ptr = features[begin:].ctypes.data_as(ctypes.POINTER(ctypes.c_int16))
                        t_ptr = targets[begin:].ctypes.data_as(ctypes.POINTER(ctypes.c_float))
                        pos_ptr = ctypes.byref(ds._array[begin])
                        try:
                            decoded = lib.sbin_batch_decode(pos_ptr, count, f_ptr, t_ptr)
                        finally:
                            del pos_ptr, f_ptr, t_ptr
                        if decoded != count:
                            raise ValueError(f"SBIN {begin:,}..{begin + count - 1:,} aralığında {count - decoded:,} geçersiz veya eski biçimli kayıt var: {sbin_file}. sbin_tool.py verify --all ile kontrol edin.")
                        valid += decoded
            else:
                pq = dependency("pyarrow.parquet")
                parquet = pq.ParquetFile(dataset)
                rows = int(parquet.metadata.num_rows)
                if rows < 2:
                    raise RuntimeError("Dataset en az iki satır içermeli.")
                features = np.lib.format.open_memmap(
                    temporary / "features.npy", mode="w+", dtype=np.int16, shape=(rows, 2, 32)
                )
                targets = np.lib.format.open_memmap(
                    temporary / "targets.npy", mode="w+", dtype=np.float32, shape=(rows,)
                )
                groups = np.lib.format.open_memmap(
                    temporary / "groups.npy", mode="w+", dtype=np.uint32, shape=(rows,)
                )
                for batch in parquet.iter_batches(batch_size=65536, columns=["fen", "wdl"]):
                    for fen, wdl in zip(batch.column(0).to_pylist(), batch.column(1).to_pylist()):
                        try:
                            fen = normalize_engine_fen(fen)
                            us, them, white = parse_fen_fast(fen)
                            value = float(wdl)
                            if not math.isfinite(value) or not 0 <= value <= 1:
                                raise ValueError("invalid target")
                        except (TypeError, ValueError, OverflowError):
                            invalid += 1
                            continue
                        features[valid] = -1
                        features[valid, 0, :len(us)] = us
                        features[valid, 1, :len(them)] = them
                        targets[valid] = value if white else 1.0 - value
                        valid += 1
            for begin in range(0, valid, 65536):
                end = min(begin + 65536, valid)
                groups[begin:end] = feature_group_ids(features[begin:end])
            if file_identity(dataset) != {"path": str(dataset), "size": stat.st_size, "mtime_ns": stat.st_mtime_ns}:
                raise RuntimeError("Dataset önbellek hazırlanırken değişti.")
            features.flush()
            targets.flush()
            groups.flush()
            del features, targets, groups
            if valid < 2:
                raise RuntimeError(f"Geçerli NNUE kaydı yok; atlanan satır: {invalid:,}")
            if valid < rows:
                for name in ("features", "targets", "groups"):
                    source = np.load(temporary / f"{name}.npy", mmap_mode="r")[:valid]
                    trimmed = temporary / f"{name}.trim.npy"
                    np.save(trimmed, source)
                    trimmed.replace(temporary / f"{name}.npy")
            info = {
                "version": CACHE_VERSION,
                "source": str(dataset),
                "source_size": stat.st_size,
                "source_mtime_ns": stat.st_mtime_ns,
                "rows": valid,
                "invalid_rows": invalid,
            }
            atomic_json(temporary / "complete.json", info)
            if cache.exists():
                shutil.rmtree(cache)
            temporary.replace(cache)
        except Exception:
            shutil.rmtree(temporary, ignore_errors=True)
            raise
    complete = load_complete()
    if complete is None:
        raise RuntimeError("NNUE özellik önbelleği oluşturulamadı.")
    return complete


def _make_nnue_model() -> Any:
    """Create the PyTorch model whose tensor order matches engine/src/nnue.h."""
    torch = dependency("torch")
    nn = importlib.import_module("torch.nn")

    class Model(nn.Module):  # type: ignore[name-defined]
        def __init__(self) -> None:
            super().__init__()
            self.embedding = nn.Embedding(NNUE_FEATURES, NNUE_ACCUMULATOR)
            self.feature_bias = nn.Parameter(torch.zeros(NNUE_ACCUMULATOR))
            self.output = nn.Linear(NNUE_ACCUMULATOR * 2, 1)
            nn.init.normal_(self.embedding.weight, mean=0.0, std=0.02)
            nn.init.normal_(self.output.weight, mean=0.0, std=0.02)
            nn.init.zeros_(self.output.bias)

        def forward(self, us_idx: Any, them_idx: Any,
                    us_mask: Any, them_mask: Any) -> Any:
            batch = us_idx.shape[0]
            features = torch.zeros(
                (batch * 2, NNUE_FEATURES), device=us_idx.device,
                dtype=self.embedding.weight.dtype,
            )
            features.scatter_add_(
                1, torch.cat((us_idx, them_idx), dim=0).long(),
                torch.cat((us_mask, them_mask), dim=0).to(features.dtype),
            )
            accumulators = features @ self.embedding.weight + self.feature_bias
            us, them = accumulators.chunk(2)
            us = torch.clamp(us, 0.0, 1.0).square()
            them = torch.clamp(them, 0.0, 1.0).square()
            return self.output(torch.cat((us, them), dim=1)).squeeze(-1)

    return Model()


def export_nnue(model: Any, output: Path) -> Path:
    """Quantize a trained model into the exact little-endian engine format."""
    torch = dependency("torch")
    np = dependency("numpy")
    output = output.resolve()
    output.parent.mkdir(parents=True, exist_ok=True)
    for name, parameter in model.named_parameters():
        if not bool(torch.isfinite(parameter).all()):
            raise ValueError(f"NNUE parametresi sonlu değil: {name}")
    def quantize_i16(values: Any, scale: float, name: str) -> Any:
        raw = np.asarray(values)
        rounded = np.rint(raw * scale)
        if not np.isfinite(rounded).all():
            raise ValueError(f"NNUE parametresi sonlu değil: {name}")
        minimum = float(rounded.min()) if rounded.size else 0.0
        maximum = float(rounded.max()) if rounded.size else 0.0
        if minimum < -32768 or maximum > 32767:
            raise ValueError(
                f"NNUE parametresi int16 aralığını aşıyor: {name} "
                f"({minimum:.0f}..{maximum:.0f})"
            )
        return rounded.astype("<i2", copy=False)

    with torch.no_grad():
        feature = model.embedding.weight.detach().cpu().numpy()
        feature_i16 = quantize_i16(feature, QA, "embedding.weight")
        bias = model.feature_bias.detach().cpu().numpy()
        bias_i16 = quantize_i16(bias, QA, "feature_bias")
        output_weight = model.output.weight.detach().cpu().numpy().reshape(-1)
        output_scaled = output_weight * (400.0 / math.log(10.0)) / SCALE
        output_i16 = quantize_i16(output_scaled, QB, "output.weight")
        output_bias = float(model.output.bias.detach().cpu().numpy().reshape(-1)[0])
        output_bias_scaled = output_bias * (400.0 / math.log(10.0)) / SCALE
        output_bias_i16 = quantize_i16(
            np.asarray([output_bias_scaled]), QAB, "output.bias"
        )
    payload = b"".join(
        (feature_i16.tobytes(), bias_i16.tobytes(),
         output_i16.tobytes(), output_bias_i16.tobytes())
    )
    if len(payload) != NNUE_PAYLOAD_SIZE:
        raise RuntimeError(f"NNUE payload boyutu beklenmiyor: {len(payload)}")
    data = payload + bytes(NNUE_FILE_SIZE - len(payload))
    atomic_bytes(output, data)
    print(f"[BAŞARILI] NNUE yazıldı: {output} ({len(data):,} byte)")
    return output


def load_nnue(model: Any, network: Path) -> None:
    """Load an engine NNUE into a PyTorch model for fine tuning."""
    torch = dependency("torch")
    np = dependency("numpy")
    network = require_file(network, "NNUE")
    data = network.read_bytes()

    if len(data) not in (NNUE_PAYLOAD_SIZE, NNUE_FILE_SIZE):
        raise ValueError(f"NNUE boyutu geçersiz: {len(data)} (Beklenen: {NNUE_PAYLOAD_SIZE} veya {NNUE_FILE_SIZE})")
    offset = 0
    feature_bytes = NNUE_FEATURES * NNUE_ACCUMULATOR * 2
    feature = np.frombuffer(data[offset:offset + feature_bytes], dtype="<i2")
    feature = feature.reshape(NNUE_FEATURES, NNUE_ACCUMULATOR).astype(np.float32) / QA
    offset += feature_bytes
    bias_bytes = NNUE_ACCUMULATOR * 2
    bias = np.frombuffer(data[offset:offset + bias_bytes], dtype="<i2").astype(np.float32) / QA
    offset += bias_bytes
    output_bytes = NNUE_ACCUMULATOR * 2 * 2
    output = np.frombuffer(data[offset:offset + output_bytes], dtype="<i2")
    output = output.astype(np.float32) / QB * SCALE / (400.0 / math.log(10.0))
    offset += output_bytes
    output_bias = np.frombuffer(data[offset:offset + 2], dtype="<i2").astype(np.float32)
    output_bias = output_bias / QAB * SCALE / (400.0 / math.log(10.0))
    with torch.no_grad():
        model.embedding.weight.copy_(torch.from_numpy(feature))
        model.feature_bias.copy_(torch.from_numpy(bias))
        model.output.weight.copy_(torch.from_numpy(output.reshape(1, -1)))
        model.output.bias.copy_(torch.from_numpy(output_bias))


def _torch_device(torch: Any, requested: str) -> Any:
    if requested == "auto":
        cuda = bool(torch.cuda.is_available())
        mps_backend = getattr(getattr(torch, "backends", None), "mps", None)
        mps = bool(mps_backend and mps_backend.is_available())
        requested = "cuda" if cuda else "mps" if mps else "cpu"
    if requested == "cuda" and not torch.cuda.is_available():
        raise RuntimeError("CUDA kullanılamıyor; --device cpu veya auto seçin.")
    if requested == "mps":
        mps_backend = getattr(getattr(torch, "backends", None), "mps", None)
        if not mps_backend or not mps_backend.is_available():
            raise RuntimeError("MPS kullanılamıyor; --device cpu veya auto seçin.")
    return torch.device(requested)


def train_nnue(dataset: Path, output: Path, *, epochs: int, batch_size: int,
               lr: float, resume: Path | None, device_name: str, seed: int,
               patience: int, validation: float, swa: bool = True,
               feature_dropout: float = 0.0) -> Path:
    """Train and export one base or aggressive engine-compatible network."""
    torch = dependency("torch")
    np = dependency("numpy")
    functional = importlib.import_module("torch.nn.functional")
    if epochs < 1 or batch_size < 1 or not math.isfinite(lr) or lr <= 0 or patience < 1:
        raise ValueError("epochs, batch-size, lr ve patience pozitif olmalı.")
    if not 0.0 < validation < 1.0:
        raise ValueError("validation 0 ile 1 arasında olmalı.")
    dataset = require_file(dataset, "Eğitim Veri Seti")
    output = output.resolve()
    torch.manual_seed(seed)
    device = _torch_device(torch, device_name)
    features, targets, groups, cache_info = prepare_feature_cache(dataset)
    total = int(cache_info["rows"])
    if total < 2:
        raise RuntimeError("Eğitim için en az iki geçerli pozisyon gerekli.")
    training_ids, validation_ids = split_feature_groups(groups, validation)
    print(
        f"Device={device}; train={len(training_ids):,}; "
        f"validation={len(validation_ids):,}; cache={cache_info.get('version')}",
        flush=True,
    )
    model = _make_nnue_model().to(device)
    optimizer = torch.optim.AdamW(model.parameters(), lr=lr, weight_decay=1e-5)
    scheduler = torch.optim.lr_scheduler.CosineAnnealingLR(optimizer, T_max=epochs)
    start_epoch = 0
    best_validation = float("inf")
    best_model = None
    stale = 0
    swa_weights: dict[str, Any] | None = None
    swa_count = 0
    swa_start_epoch = max(1, int(epochs * 0.5)) if epochs >= 3 else 1
    if resume:
        resume = resume.resolve()
        if resume.suffix.lower() == ".nnue":
            load_nnue(model, resume)
        else:
            checkpoint_file = require_file(resume, "NNUE checkpoint")
            try:
                saved = torch.load(str(checkpoint_file), map_location=device, weights_only=True)
            except TypeError:
                saved = torch.load(str(checkpoint_file), map_location=device)
            if not isinstance(saved, dict) or "model" not in saved:
                raise ValueError("Checkpoint model alanı içermiyor.")
            if saved.get("dataset_identity", file_identity(dataset)) != file_identity(dataset):
                raise ValueError("Checkpoint başka veri sürümüne ait; yeni veri için --resume ile NNUE kullanın.")
            if saved.get("cache_version", CACHE_VERSION) != CACHE_VERSION:
                raise ValueError("Checkpoint doğrulama ayrımı eski; --resume ile NNUE kullanın.")
            saved_validation = saved.get("validation")
            if saved_validation is not None and not math.isclose(
                    float(saved_validation), float(validation), rel_tol=0.0, abs_tol=1e-12):
                raise ValueError("Checkpoint validation oranı değişmiş; aynı oranı kullanın.")
            saved_seed = saved.get("seed")
            if saved_seed is not None and int(saved_seed) != int(seed):
                raise ValueError("Checkpoint seed değeri değişmiş; deterministik devam için aynı seed gerekli.")
            model.load_state_dict(saved["model"])
            if "optimizer" in saved:
                optimizer.load_state_dict(saved["optimizer"])
            if "scheduler" in saved:
                scheduler.load_state_dict(saved["scheduler"])
                scheduler.T_max = epochs
            start_epoch = int(saved.get("epoch", 0))
            if start_epoch < 0:
                raise ValueError("Checkpoint epoch değeri negatif olamaz.")
            if "scheduler" in saved:
                for group, base_lr in zip(optimizer.param_groups, scheduler.base_lrs):
                    group["lr"] = scheduler.eta_min + (base_lr - scheduler.eta_min) * (
                        1.0 + math.cos(math.pi * start_epoch / epochs)) / 2.0
            stale = int(saved.get("stale_epochs", 0))
            best_model = saved.get("best_model")
            if best_model is not None:
                best_validation = float(saved["best_validation"])
    if start_epoch >= epochs:
        raise ValueError("--epochs checkpoint epoch değerinden büyük olmalı.")
    output.parent.mkdir(parents=True, exist_ok=True)
    if best_model is not None:
        restored_best = _make_nnue_model()
        restored_best.load_state_dict(best_model)
        export_nnue(restored_best, output)

    def chunked_batches(ids: Any, shuffle_chunks: bool = True, epoch_seed: int = 42) -> Iterator[tuple[Any, Any, Any, Any, Any]]:
        n = len(ids)
        if n == 0:
            return
        chunk_size = 262144
        num_chunks = (n + chunk_size - 1) // chunk_size
        rng = np.random.default_rng(epoch_seed)
        chunk_order = rng.permutation(num_chunks) if shuffle_chunks else np.arange(num_chunks)

        for c_idx in chunk_order:
            c_start = int(c_idx * chunk_size)
            c_end = min(c_start + chunk_size, n)
            chunk_ids = ids[c_start:c_end]
            if len(chunk_ids) == 0:
                continue

            w_start = int(chunk_ids[0])
            w_end = int(chunk_ids[-1]) + 1
            w_feat = np.array(features[w_start:w_end])
            w_targ = np.array(targets[w_start:w_end])

            local_idx = chunk_ids - w_start
            c_feat = w_feat[local_idx]
            c_targ = w_targ[local_idx]

            perm = rng.permutation(len(chunk_ids)) if shuffle_chunks else np.arange(len(chunk_ids))
            c_feat = c_feat[perm]
            c_targ = c_targ[perm]

            for b in range(0, len(perm), batch_size):
                b_feat = c_feat[b:b + batch_size]
                b_targ = c_targ[b:b + batch_size]
                tensor = torch.as_tensor(b_feat.astype(np.int64), device=device)
                mask = (tensor >= 0).to(dtype=torch.float32)
                tensor = tensor.clamp_min(0)
                labels = torch.as_tensor(b_targ, device=device)
                yield tensor[:, 0], tensor[:, 1], mask[:, 0], mask[:, 1], labels

    history: list[dict[str, Any]] = []
    if resume and resume.suffix.lower() != ".nnue":
        history = list(saved.get("history", []))
    for epoch in range(start_epoch, epochs):
        started = time.perf_counter()
        model.train()
        train_sum = 0.0
        train_processed = 0
        last_log_time = started
        last_log_pos = 0
        total_train = len(training_ids)

        for us, them, us_mask, them_mask, labels in chunked_batches(training_ids, shuffle_chunks=True, epoch_seed=seed + epoch):
            optimizer.zero_grad(set_to_none=True)
            if feature_dropout > 0.0:
                us_mask = us_mask * (torch.rand_like(us_mask) > feature_dropout).float()
                them_mask = them_mask * (torch.rand_like(them_mask) > feature_dropout).float()
            loss = functional.binary_cross_entropy_with_logits(
                model(us, them, us_mask, them_mask), labels
            )
            loss.backward()
            optimizer.step()
            batch_len = len(labels)
            train_sum += float(loss.detach().item()) * batch_len
            train_processed += batch_len

            if train_processed - last_log_pos >= 5_000_000:
                now = time.perf_counter()
                seg_speed = (train_processed - last_log_pos) / max(0.001, now - last_log_time)
                overall_speed = train_processed / max(0.001, now - started)
                current_loss = train_sum / train_processed
                remaining_pos = total_train - train_processed
                eta_min = (remaining_pos / overall_speed) / 60.0
                pct = (train_processed / total_train) * 100
                print(
                    f"  [Epoch {epoch + 1}/{epochs}] {train_processed:,}/{total_train:,} "
                    f"({pct:.1f}%) loss={current_loss:.6f} "
                    f"{seg_speed:,.0f} pos/s (ETA: {eta_min:.1f} dk)",
                    flush=True,
                )
                last_log_pos = train_processed
                last_log_time = now

        model.eval()
        validation_sum = 0.0
        with torch.no_grad():
            for us, them, us_mask, them_mask, labels in chunked_batches(validation_ids, shuffle_chunks=False, epoch_seed=seed):
                validation_sum += float(functional.binary_cross_entropy_with_logits(
                    model(us, them, us_mask, them_mask), labels, reduction="sum"
                ).item())
        train_loss = train_sum / len(training_ids)
        validation_loss = validation_sum / len(validation_ids)
        if not math.isfinite(train_loss + validation_loss):
            raise RuntimeError("NNUE eğitimi sonlu olmayan kayıp üretti.")
        scheduler.step()
        if validation_loss < best_validation:
            best_validation = validation_loss
            best_model = {name: value.detach().cpu().clone() for name, value in model.state_dict().items()}
            stale = 0
            export_nnue(model, output)
        else:
            stale += 1
        if swa and (epoch + 1) >= swa_start_epoch:
            swa_count += 1
            if swa_weights is None:
                swa_weights = {name: value.detach().cpu().clone().float() for name, value in model.state_dict().items()}
            else:
                for name, value in model.state_dict().items():
                    swa_weights[name] += value.detach().cpu().float()
        checkpoint_data = {
            "model": model.state_dict(),
            "optimizer": optimizer.state_dict(),
            "scheduler": scheduler.state_dict(),
            "epoch": epoch + 1,
            "best_validation": best_validation,
            "best_model": best_model,
            "dataset": str(dataset),
            "dataset_identity": file_identity(dataset),
            "cache_version": CACHE_VERSION,
            "seed": seed,
            "validation": validation,
            "stale_epochs": stale,
        }
        elapsed = time.perf_counter() - started
        row = {
            "epoch": epoch + 1,
            "train_loss": train_loss,
            "validation_loss": validation_loss,
            "seconds": elapsed,
            "positions_per_second": len(training_ids) / max(elapsed, 1e-9),
        }
        history.append(row)
        checkpoint_data["history"] = history
        checkpoint_path = output.with_suffix(".pt")
        temporary = checkpoint_path.with_name(f".{checkpoint_path.name}.{uuid.uuid4().hex}.tmp")
        try:
            torch.save(checkpoint_data, temporary)
            temporary.replace(checkpoint_path)
        finally:
            temporary.unlink(missing_ok=True)
        atomic_json(output.with_suffix(".metrics.json"), history)
        print(
            f"Epoch {epoch + 1}/{epochs} train={train_loss:.6f} "
            f"validation={validation_loss:.6f} "
            f"{row['positions_per_second']:.0f} pos/s",
            flush=True,
        )
        if stale >= patience:
            print("Erken durdurma: doğrulama kaybı iyileşmedi.", flush=True)
            break
    if not output.exists():
        export_nnue(model, output)
    if swa and swa_count > 1 and swa_weights is not None:
        avg_weights = {name: (val / swa_count).to(model.state_dict()[name].dtype) for name, val in swa_weights.items()}
        swa_model = _make_nnue_model().to(device)
        swa_model.load_state_dict(avg_weights)
        swa_model.eval()
        swa_val_sum = 0.0
        with torch.no_grad():
            for us, them, us_mask, them_mask, labels in chunked_batches(validation_ids, shuffle_chunks=False, epoch_seed=seed):
                swa_val_sum += float(functional.binary_cross_entropy_with_logits(
                    swa_model(us, them, us_mask, them_mask), labels, reduction="sum"
                ).item())
        swa_val_loss = swa_val_sum / len(validation_ids)
        swa_output = output.with_name(f"{output.stem}-swa{output.suffix}")
        export_nnue(swa_model, swa_output)
        print(
            f"SWA modeli üretildi ({swa_count} epoch ortalaması): "
            f"val_loss={swa_val_loss:.6f} (En iyi tekil epoch: {best_validation:.6f}) -> {swa_output.name}",
            flush=True,
        )
        if swa_val_loss < best_validation:
            print(f"SWA modeli daha iyi doğrulama kaybı sağladı ({swa_val_loss:.6f} < {best_validation:.6f}); birincil ağ yapıldı.", flush=True)
            export_nnue(swa_model, output)
    return output


@dataclass
class MatchResult:
    score_line: str
    wins: int
    losses: int
    draws: int
    elo_diff: float
    sprt_decision: str | None
    is_winner: bool
    returncode: int
    command: list[str]
    input_hashes: dict[str, str] = field(default_factory=dict)
    status: str = "INCONCLUSIVE"

    def as_dict(self, candidate: Path, baseline: Path, pgn: Path | None) -> dict[str, Any]:
        return {
            "score": self.score_line,
            "wins": self.wins,
            "losses": self.losses,
            "draws": self.draws,
            "elo_diff": self.elo_diff,
            "sprt_decision": self.sprt_decision,
            "is_winner": self.is_winner,
            "returncode": self.returncode,
            "candidate": str(candidate),
            "baseline": str(baseline),
            "pgn": str(pgn) if pgn else None,
            "command": self.command,
            "input_hashes": self.input_hashes,
            "status": self.status,
        }


def find_cutechess(assets: Path | None = None) -> Path | None:
    found = shutil.which("cutechess-cli")
    if found:
        return Path(found).resolve()
    candidates = [
        ROOT / "cutechess-cli",
        (assets or DEFAULT_ASSETS) / "cutechess-cli",
        REPOSITORY / "cutechess-cli",
    ]
    for candidate in candidates:
        if candidate.is_file() and os.access(candidate, os.X_OK):
            return candidate.resolve()
    return None


def _parse_match_score(line: str) -> tuple[int, int, int] | None:
    match = re.search(
        r"Score of Candidate vs Baseline:\s*(\d+)\s*-\s*(\d+)\s*-\s*(\d+)",
        line,
    )
    if not match:
        return None
    values = match.groups()
    return int(values[0]), int(values[1]), int(values[2])


def verify_engine_networks(engine: Path, options: Sequence[str]) -> None:
    """Require the UCI acknowledgements; an ignored EvalFile invalidates a match."""
    commands = ["uci", "setoption name Hash value 16"]
    expected = []
    for option in options:
        name, value = option.removeprefix("option.").split("=", 1)
        if "\n" in value or "\r" in value:
            raise ValueError("UCI dosya yolları satır sonu içeremez.")
        commands.append(f"setoption name {name} value {value}")
        if name in ("EvalFile", "EvalFileAggressive"):
            expected.append(f"info string {name} loaded")
    result = subprocess.run(
        [str(engine)], input="\n".join(commands + ["isready", "quit", ""]),
        capture_output=True, text=True, timeout=30,
    )
    lines = result.stdout.splitlines()
    if (result.returncode or "uciok" not in lines or "readyok" not in lines or
            any(line not in lines for line in expected)):
        raise RuntimeError(f"Motor NNUE yüklemesini doğrulamadı: {engine}\n{result.stdout[-2000:]}\n{result.stderr[-1000:]}")


def run_match(args: argparse.Namespace, phase: str | None = None) -> MatchResult:
    phase = phase or getattr(args, "phase", None) or getattr(args, "net_type", "base")
    if phase not in ("base", "aggressive"):
        raise RuntimeError("match için phase base veya aggressive olmalı.")
    candidate = require_network(args.candidate, "Aday NNUE")
    baseline = require_network(args.baseline, "Taban NNUE")
    engine = require_file(args.engine, "Stallion motoru")
    book = resolve_path(getattr(args, "book", None))
    if book and not book.is_file():
        raise RuntimeError(f"Açılış kitabı bulunamadı: {book}")
    games = int(getattr(args, "games", 0))
    if games < 2 or games % 2:
        raise ValueError("games pozitif ve çift olmalı.")
    concurrency = int(getattr(args, "concurrency", 1))
    if concurrency < 1:
        raise ValueError("concurrency pozitif olmalı.")
    assets = resolve_path(getattr(args, "assets", None), DEFAULT_ASSETS)
    cli = find_cutechess(assets)
    if not cli:
        raise RuntimeError("cutechess-cli bulunamadı.")
    fixed_base = require_network(getattr(args, "fixed_base", None), "Sabit base NNUE") if phase == "aggressive" else None
    fixed_aggressive = require_network(getattr(args, "fixed_aggressive", None), "Sabit aggressive NNUE") if phase == "base" else None
    original_candidate, original_baseline = candidate, baseline
    pgnout = resolve_path(getattr(args, "pgnout", None))
    jsonout = resolve_path(getattr(args, "json_out", None))
    if pgnout and pgnout.exists() and pgnout.stat().st_size:
        raise ValueError(f"PGN zaten dolu; yeni çıktı yolu kullanın: {pgnout}")
    artifact = jsonout or pgnout or ROOT / "runs" / f"match-{datetime.now():%Y%m%d-%H%M%S-%f}.json"
    snapshot = artifact.with_suffix(".inputs")
    snapshot.mkdir(parents=True, exist_ok=False)
    input_hashes: dict[str, str] = {}

    def freeze(path: Path, name: str) -> Path:
        target = snapshot / name
        shutil.copy2(path, target)
        input_hashes[name] = file_sha256(target)
        return target

    candidate = freeze(candidate, "candidate.nnue")
    baseline = freeze(baseline, "baseline.nnue")
    engine = freeze(engine, "engine" + engine.suffix)
    if fixed_base:
        fixed_base = freeze(fixed_base, "fixed-base.nnue")
    if fixed_aggressive:
        fixed_aggressive = freeze(fixed_aggressive, "fixed-aggressive.nnue")
    if book:
        book = freeze(book, "openings.epd")

    candidate_options: list[str]
    baseline_options: list[str]
    if phase == "base":
        candidate_options = [f"option.EvalFile={candidate}"]
        baseline_options = [f"option.EvalFile={baseline}"]
        fixed = fixed_aggressive
        if fixed:
            candidate_options.append(f"option.EvalFileAggressive={fixed}")
            baseline_options.append(f"option.EvalFileAggressive={fixed}")
    else:
        candidate_options = [f"option.EvalFileAggressive={candidate}"]
        baseline_options = [f"option.EvalFileAggressive={baseline}"]
        fixed = fixed_base
        if fixed:
            candidate_options.append(f"option.EvalFile={fixed}")
            baseline_options.append(f"option.EvalFile={fixed}")

    common_options = ["option.Use NNUE=true", "option.UseOpeningBook=false", "option.UseSyzygy=false",
                      "option.Threads=1", "option.Hash=64", "option.MultiPV=1", "option.Ponder=false",
                      "option.Variety=0", "option.UCI_LimitStrength=false"]
    verify_engine_networks(engine, candidate_options + common_options)
    verify_engine_networks(engine, baseline_options + common_options)
    command = [
        str(cli),
        "-engine", f"cmd={engine}", "name=Candidate", *candidate_options,
        "-engine", f"cmd={engine}", "name=Baseline", *baseline_options,
        "-each", "proto=uci", f"tc={getattr(args, 'tc', '5+0.05')}", *common_options,
        "-rounds", str(games // 2), "-games", "2", "-repeat", "-recover",
        "-concurrency", str(concurrency),
        "-draw", "movenumber=40", "movecount=8", "score=10",
        "-resign", "movecount=3", "score=600",
        "-srand", str(getattr(args, "seed", 42)),
    ]
    if book:
        command.extend(["-openings", f"file={book}", "format=epd", "order=random"])
    use_sprt = bool(getattr(args, "sprt", False))
    if use_sprt:
        command.extend(["-sprt", "elo0=0", "elo1=15", "alpha=0.05", "beta=0.05"])
    if pgnout:
        pgnout.parent.mkdir(parents=True, exist_ok=True)
        command.extend(["-pgnout", str(pgnout)])
    print(
        f"Maç: {phase.upper()} candidate={candidate.name} baseline={baseline.name} "
        f"games={games}",
        flush=True,
    )
    process = subprocess.Popen(
        command, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
        text=True, bufsize=1,
    )
    score_line = ""
    elo_diff = 0.0
    sprt_decision: str | None = None
    failures: list[str] = []
    log_lines: list[str] = []
    returncode = -1
    try:
        assert process.stdout is not None
        for line in process.stdout:
            print(line, end="", flush=True)
            log_lines.append(line)
            if re.search(
                    r"loses on time|disconnect|illegal move|illegal game|forfeit|"
                    r"connection stall|stalled connection|crash|segmentation|"
                    r"load failed|no move|aborted|error",
                    line, re.I):
                failures.append(line.strip())
            if "Score of Candidate vs Baseline:" in line:
                score_line = line.strip()
            if "Elo difference:" in line:
                if "-inf" in line:
                    elo_diff = -999.0
                elif "inf" in line:
                    elo_diff = 999.0
                else:
                    found = re.search(r"Elo difference:\s*([+-]?\d+(?:\.\d+)?)", line)
                    if found:
                        elo_diff = float(found.group(1))
            if "H1 was accepted" in line:
                sprt_decision = "PASSED"
            elif "H0 was accepted" in line:
                sprt_decision = "FAILED"
        returncode = process.wait()
    except BaseException:
        if process.poll() is None:
            process.terminate()
            try:
                process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait()
        raise
    finally:
        atomic_text(artifact.with_suffix(".log"), "".join(log_lines))
    if returncode:
        raise RuntimeError(f"cutechess-cli maçı başarısız oldu (exit {returncode}).")
    if failures:
        raise RuntimeError("Maçta motor/zaman hatası var; güç kararı verilmedi: " + "; ".join(failures[:5]))
    score = _parse_match_score(score_line)
    if not score:
        raise RuntimeError("cutechess-cli skor satırı üretmedi.")
    wins, losses, draws = score
    completed = wins + losses + draws
    if not completed or completed > games or (completed != games and sprt_decision is None):
        raise RuntimeError(
            f"Eksik maç: {completed}/{games} oyun tamamlandı."
        )
    winner = use_sprt and sprt_decision == "PASSED"
    status = (sprt_decision or "INCONCLUSIVE") if use_sprt else "ESTIMATE_ONLY"
    result = MatchResult(
        score_line=score_line,
        wins=wins,
        losses=losses,
        draws=draws,
        elo_diff=elo_diff,
        sprt_decision=sprt_decision,
        is_winner=winner,
        returncode=returncode,
        command=command,
        input_hashes=input_hashes,
        status=status,
    )
    atomic_json(jsonout or artifact.with_suffix(".json"), result.as_dict(original_candidate, original_baseline, pgnout))
    print(
        f"Maç sonucu: {wins}-{losses}-{draws}, Elo {elo_diff:+.1f}; "
        f"{status}",
        flush=True,
    )
    return result


def pgn_game_count(path: Path) -> int:
    """Count games without depending on pgn-extract's output formatting."""
    count = 0
    with path.open("r", encoding="latin-1", errors="ignore") as stream:
        for line in stream:
            if line.lstrip().startswith("[Event "):
                count += 1
    return count


def ply_count_total(path: Path) -> int:
    total = 0
    with path.open("r", encoding="latin-1", errors="ignore") as stream:
        for line in stream:
            match = re.match(r'\s*\[PlyCount\s+"(\d+)"\]', line)
            if match:
                total += int(match.group(1))
    return total


def combine_text_files(sources: Sequence[Path], target: Path) -> None:
    target.parent.mkdir(parents=True, exist_ok=True)
    with target.open("w", encoding="latin-1", errors="ignore") as destination:
        for source in sources:
            if not source.exists():
                continue
            text_value = source.read_text(encoding="latin-1", errors="ignore")
            if text_value:
                destination.write(text_value)
                if not text_value.endswith("\n"):
                    destination.write("\n")


def _pgn_extract(binary: Path, command: Sequence[str | os.PathLike[str]],
                 cwd: Path) -> None:
    run_checked([binary, *command], cwd=cwd)


def _pattern_path(assets: Path, number: int, color: str) -> Path:
    name = f"{number}_pawnsac_{color}" if number != 9 else f"queensac_{color}"
    return require_file(assets / name, "SGS pattern")


def run_eas(args: argparse.Namespace) -> dict[str, Any]:
    pgn = require_file(args.pgn, "PGN")
    assets = resolve_path(getattr(args, "assets", None), DEFAULT_ASSETS) or DEFAULT_ASSETS
    binary = require_file(assets / "pgn-extract", "pgn-extract")
    output = (resolve_path(args.output) or ROOT / "statistics_EAS_ratinglist.txt").resolve()
    jsonout = resolve_path(args.json_out)
    with tempfile.TemporaryDirectory(prefix="stallion-eas-") as temporary_name:
        work = Path(temporary_name)
        normalized = work / "source.pgn"
        _pgn_extract(binary, [
            "--quiet", "--fixresulttags", "-C", "-N", "-V", "--plycount",
            pgn, "--output", normalized,
        ], work)
        if not normalized.exists():
            normalized.write_text("", encoding="latin-1")
        engines: set[str] = set()
        with normalized.open("r", encoding="latin-1", errors="ignore") as stream:
            for line in stream:
                match = re.match(r'\[(?:White|Black)\s+"([^"]*)"\]', line)
                if match and match.group(1):
                    engines.add(match.group(1))
        wins_source = work / "all_wins.pgn"
        win_parts: list[Path] = []
        draw_parts: dict[str, list[Path]] = {}
        for engine_name in sorted(engines):
            white = work / f"{hashlib.md5(engine_name.encode()).hexdigest()}-w.pgn"
            black = work / f"{hashlib.md5(engine_name.encode()).hexdigest()}-b.pgn"
            _pgn_extract(binary, [
                "--quiet", f"-Tw{engine_name}", "-Tr1-0", normalized,
                "--output", white,
            ], work)
            _pgn_extract(binary, [
                "--quiet", f"-Tb{engine_name}", "-Tr0-1", normalized,
                "--output", black,
            ], work)
            win_parts.extend((white, black))
            draws_w = work / f"{hashlib.md5(engine_name.encode()).hexdigest()}-dw.pgn"
            draws_b = work / f"{hashlib.md5(engine_name.encode()).hexdigest()}-db.pgn"
            _pgn_extract(binary, [
                "--quiet", f"-Tw{engine_name}", "-Tr1/2-1/2", normalized,
                "--output", draws_w,
            ], work)
            _pgn_extract(binary, [
                "--quiet", f"-Tb{engine_name}", "-Tr1/2-1/2", normalized,
                "--output", draws_b,
            ], work)
            draw_parts[engine_name] = [draws_w, draws_b]
        combine_text_files(win_parts, wins_source)
        total_wins = pgn_game_count(wins_source)
        avg_all = (
            max(20, (ply_count_total(wins_source) + total_wins) // (2 * total_wins))
            if total_wins else 60
        )
        early_limit = max(10, avg_all // 2)
        short_limit = min(95, max(30, avg_all - 15))
        results: list[dict[str, Any]] = []
        for engine_name in sorted(engines):
            digest = hashlib.md5(engine_name.encode()).hexdigest()
            engine_wins = work / f"{digest}-wins.pgn"
            engine_draws = work / f"{digest}-draws.pgn"
            combine_text_files(
                [work / f"{digest}-w.pgn", work / f"{digest}-b.pgn"], engine_wins
            )
            combine_text_files(draw_parts[engine_name], engine_draws)
            wins = pgn_game_count(engine_wins)
            draws = pgn_game_count(engine_draws)
            if not wins:
                continue
            average_moves = max(1, (ply_count_total(engine_wins) + wins) // (2 * wins))
            sac_counts: dict[int, int] = {}
            sac_files: list[Path] = []
            for sac_type in (1, 2, 3, 4, 5, 9):
                white_pattern = _pattern_path(assets, sac_type, "white")
                black_pattern = _pattern_path(assets, sac_type, "black")
                white_sac = work / f"{digest}-{sac_type}-w.pgn"
                black_sac = work / f"{digest}-{sac_type}-b.pgn"
                merged_sac = work / f"{digest}-{sac_type}-merged.pgn"
                clean_sac = work / f"{digest}-{sac_type}-clean.pgn"
                _pgn_extract(binary, [
                    "--quiet", "-y" + str(white_pattern), work / f"{digest}-w.pgn",
                    "--output", white_sac,
                ], work)
                _pgn_extract(binary, [
                    "--quiet", "-y" + str(black_pattern), work / f"{digest}-b.pgn",
                    "--output", black_sac,
                ], work)
                combine_text_files((white_sac, black_sac), merged_sac)
                _pgn_extract(binary, [
                    "--quiet", "-D", merged_sac, "--output", clean_sac,
                ], work)
                sac_counts[sac_type] = pgn_game_count(clean_sac)
                sac_files.append(clean_sac)
            all_sacs = work / f"{digest}-all-sacs.pgn"
            all_sacs_clean = work / f"{digest}-all-sacs-clean.pgn"
            combine_text_files(sac_files, all_sacs)
            _pgn_extract(binary, [
                "--quiet", "-D", all_sacs, "--output", all_sacs_clean,
            ], work)
            shortwins = work / f"{digest}-shorts.pgn"
            earlysacs = work / f"{digest}-early.pgn"
            _pgn_extract(binary, [
                "--quiet", f"-bu{short_limit}", engine_wins, "--output", shortwins,
            ], work)
            # Limit the matching prefix, not the final length of the won game.
            early_parts: list[Path] = []
            for side, suffix in (("white", "w"), ("black", "b")):
                truncated = work / f"{digest}-early-prefix-{suffix}.pgn"
                _pgn_extract(binary, ["--quiet", "--plylimit", str(early_limit * 2),
                    work / f"{digest}-{suffix}.pgn", "--output", truncated], work)
                for sac_type in (1, 2, 3, 4, 5, 9):
                    matched = work / f"{digest}-early-{suffix}-{sac_type}.pgn"
                    _pgn_extract(binary, ["--quiet", "-y" + str(_pattern_path(assets, sac_type, side)),
                        truncated, "--output", matched], work)
                    early_parts.append(matched)
            early_combined = work / f"{digest}-early-combined.pgn"
            combine_text_files(early_parts, early_combined)
            _pgn_extract(binary, ["--quiet", "-D", early_combined, "--output", earlysacs], work)
            short_count = pgn_game_count(shortwins)
            early_count = pgn_game_count(earlysacs)
            sac_count = pgn_game_count(all_sacs_clean)
            sac_percent = sac_count * 100.0 / wins
            early_percent = early_count * 100.0 / wins
            short_percent = short_count * 100.0 / wins
            move_advantage = max(0, avg_all - average_moves) * 50
            score = int(
                sac_percent * 40 + early_percent * 50 +
                short_percent * 30 + move_advantage
            )
            results.append({
                "engine": engine_name,
                "eas": score,
                "wins": wins,
                "draws": draws,
                "avg_moves": average_moves,
                "total_sacs": sac_count,
                "perc_sacs": sac_percent,
                "perc_early_sacs": early_percent,
                "perc_shorts": short_percent,
                "sac_counts": sac_counts,
            })
    results.sort(key=lambda item: item["eas"], reverse=True)
    report = {
        "source": str(pgn),
        "avg_win_moves": avg_all,
        "early_sac_limit": early_limit,
        "short_win_limit": short_limit,
        "results": results,
        "stats": {item["engine"]: item["eas"] for item in results},
    }
    lines = [
        "*****************************************************************************",
        "*** Local approximate aggressiveness report (not official EAS V6)       ***",
        "*****************************************************************************",
        f"Kaynak Dosya: {pgn.name}",
        f"Ortalama Galibiyet Uzunluğu: {report['avg_win_moves']} hamle",
        f"Erken Feda Sınırı          : {report['early_sac_limit']} hamle",
        f"Kısa Galibiyet Sınırı      : {report['short_win_limit']} hamle",
        "-----------------------------------------------------------------------------",
        "Sıra  EAS-Puanı   Galibiyet  Ort.Hamle  Fedalar%  ErkenFeda%  KısaGalibiyet%  Motor",
        "-----------------------------------------------------------------------------",
    ]
    for rank, item in enumerate(results, 1):
        lines.append(
            f"{rank:3d}   {item['eas']:6d}      {item['wins']:5d}      "
            f"{item['avg_moves']:4d}      {item['perc_sacs']:5.1f}%    "
            f"{item['perc_early_sacs']:5.1f}%      {item['perc_shorts']:5.1f}%       "
            f"{item['engine']}"
        )
    lines.append("-----------------------------------------------------------------------------")
    report_text = "\n".join(lines) + "\n"
    atomic_text(output, report_text)
    if jsonout:
        atomic_json(jsonout, report)
    print(f"\n{report_text}", flush=True)
    stats = report.get("stats", {})
    cand_eas = stats.get("Candidate")
    base_eas = stats.get("Baseline")
    if cand_eas is not None and base_eas is not None:
        print(f"EAS sonucu: Candidate: {cand_eas} EAS, Baseline: {base_eas} EAS (Fark: {cand_eas - base_eas:+d} EAS)", flush=True)
    print(f"[BAŞARILI] EAS raporu: {output}")
    return report


def run_sacrifices(args: argparse.Namespace) -> Path:
    pgn = require_file(args.pgn, "PGN")
    assets = resolve_path(getattr(args, "assets", None), DEFAULT_ASSETS) or DEFAULT_ASSETS
    binary = require_file(assets / "pgn-extract", "pgn-extract")
    if args.max_moves < 1:
        raise ValueError("max-moves pozitif olmalı.")
    output = (resolve_path(args.output) or ROOT / "games_with_sacrifices.pgn").resolve()
    types = [1, 2, 3, 4, 5, 9] if args.sac_type == 0 else [args.sac_type]
    for sac_type in types:
        _pattern_path(assets, sac_type, "white")
        _pattern_path(assets, sac_type, "black")
    with tempfile.TemporaryDirectory(prefix="stallion-sgs-") as temporary_name:
        work = Path(temporary_name)
        white_wins = work / "white-wins.pgn"
        black_wins = work / "black-wins.pgn"
        _pgn_extract(binary, [
            "--quiet", "--fixresulttags", "-bl15", f"-bu{args.max_moves}",
            "-Tr1-0", pgn, "--output", white_wins,
        ], work)
        _pgn_extract(binary, [
            "--quiet", "--fixresulttags", "-bl15", f"-bu{args.max_moves}",
            "-Tr0-1", pgn, "--output", black_wins,
        ], work)
        parts: list[Path] = []
        for sac_type in types:
            white = work / f"{sac_type}-white.pgn"
            black = work / f"{sac_type}-black.pgn"
            merged = work / f"{sac_type}-merged.pgn"
            clean = work / f"{sac_type}-clean.pgn"
            _pgn_extract(binary, [
                "--quiet", "-y" + str(_pattern_path(assets, sac_type, "white")),
                white_wins, "--output", white,
            ], work)
            _pgn_extract(binary, [
                "--quiet", "-y" + str(_pattern_path(assets, sac_type, "black")),
                black_wins, "--output", black,
            ], work)
            combine_text_files((white, black), merged)
            _pgn_extract(binary, [
                "--quiet", "-D", merged, "--output", clean,
            ], work)
            parts.append(clean)
        combined = work / "combined.pgn"
        cleaned = work / "combined-clean.pgn"
        combine_text_files(parts, combined)
        _pgn_extract(binary, ["--quiet", "-D", combined, "--output", cleaned], work)
        data = cleaned.read_bytes() if cleaned.exists() else b""
    output.parent.mkdir(parents=True, exist_ok=True)
    atomic_bytes(output, data)
    print(f"[BAŞARILI] SGS çıktısı: {output} ({pgn_game_count(output):,} oyun)")
    return output


def promote_network(candidate: Path, baseline: Path, *, expected_baseline: str,
                    expected_candidate: str) -> None:
    candidate = require_network(candidate, "Aday NNUE")
    baseline = require_network(baseline, "Taban NNUE")
    if candidate == baseline:
        raise ValueError("Aday ve taban NNUE aynı dosya olamaz.")
    with advisory_lock(baseline.with_suffix(".promotion.lock")):
        # Read both files once while holding the promotion lock.  Hashing the
        # path and then copying it leaves a small TOCTOU window in which a
        # training process can replace the candidate between the two calls.
        baseline_data = baseline.read_bytes()
        candidate_data = candidate.read_bytes()
        baseline_digest = hashlib.sha256(baseline_data).hexdigest()
        candidate_digest = hashlib.sha256(candidate_data).hexdigest()
        if baseline_digest != expected_baseline or candidate_digest != expected_candidate:
            raise RuntimeError("Maçtan sonra NNUE dosyası değişti; terfi için yeni maç gerekli.")
        backup_dir = ROOT / "runs" / "backups"
        backup_dir.mkdir(parents=True, exist_ok=True)
        backup = backup_dir / f"{baseline.stem}.previous-{expected_baseline[:16]}.nnue"
        if not backup.exists():
            atomic_bytes(backup, baseline_data)
        atomic_bytes(baseline, candidate_data)
        atomic_json(backup_dir / f"{baseline.stem}.promotion.json", {
            "candidate": str(candidate), "candidate_sha256": expected_candidate,
            "previous_sha256": expected_baseline, "backup": str(backup),
            "promoted_at": datetime.now().isoformat(),
        })

    print(f"[TERFİ] {candidate.name} -> {baseline}; yedek={backup.name}")


def maybe_promote(args: argparse.Namespace, phase: str, result: MatchResult,
                  candidate: Path, baseline: Path, eas_report: dict | None = None) -> bool:
    stats = (eas_report or {}).get("stats", {})
    cand_eas = stats.get("Candidate")
    base_eas = stats.get("Baseline")
    eas_gain = (cand_eas - base_eas) if (cand_eas is not None and base_eas is not None) else None
    eas_info = f"EAS: Candidate={cand_eas} vs Baseline={base_eas} (Fark: {eas_gain:+d})" if eas_gain is not None else "EAS: N/A"

    if not args.promote:
        print(f"\n[TERFİ KAPALI] Elo: {result.elo_diff:+.1f} ({result.status}); {eas_info}; aday: {candidate}", flush=True)
        return False

    if (result.returncode != 0 or not result.is_winner or
            result.sprt_decision != "PASSED" or result.status != "PASSED"):
        print(f"[TERFİ YOK] SPRT H1 kabulü gerekli; sonuç={result.status}; aday={candidate}", flush=True)
        return False

    if phase == "base":
        promote_network(candidate, baseline,
                        expected_candidate=result.input_hashes["candidate.nnue"],
                        expected_baseline=result.input_hashes["baseline.nnue"])
        return True

    if eas_gain is not None and eas_gain >= args.min_eas_gain:
        promote_network(candidate, baseline,
                        expected_candidate=result.input_hashes["candidate.nnue"],
                        expected_baseline=result.input_hashes["baseline.nnue"])
        return True

    print(f"\n[TERFİ YOK] Elo: {result.elo_diff:+.1f} ({result.status}); {eas_info}; test={result.status}; aday korunuyor: {candidate}", flush=True)
    return False


def parse_steps(value: str | None) -> list[str]:
    aliases = {
        "data": "extract",
        "dataset": "extract",
        "extraction": "extract",
        "data-extract": "extract",
        "training": "train",
        "build": "prepare",
        "merge": "prepare",
        "comparison": "match",
        "match-compare": "match",
        "eas-compare": "eas",
        "eas-comparison": "eas",
        "sacrifice": "sacrifices",
        "sgs": "sacrifices",
        "selfplay": "datagen",
        "datagen": "datagen",
        "everything": "all",
    }
    order = ["prepare", "extract", "datagen", "train", "match", "eas", "sacrifices", "promote"]
    tokens = [
        aliases.get(token.lower(), token.lower())
        for token in re.split(r"[,\s]+", value or "all") if token
    ]
    unknown = [token for token in tokens if token not in order and token != "all"]
    if unknown:
        raise ValueError(
            f"Bilinmeyen pipeline adımı: {', '.join(unknown)}; "
            f"geçerli adımlar: {', '.join(order)}"
        )
    if not tokens or "all" in tokens:
        tokens = [token for token in tokens if token != "all"] + ["extract", "train", "match", "eas"]
    if "datagen" in tokens and any(step in tokens for step in ("extract", "prepare")):
        raise ValueError("datagen ile extract/prepare aynı fazda birlikte seçilemez.")
    if "prepare" in tokens and "extract" in tokens:
        raise ValueError("prepare ile extract aynı fazda birlikte seçilemez; tek bir veri kaynağı seçin.")
    return [step for step in order if step in tokens]


def _namespace_copy(args: argparse.Namespace, **changes: Any) -> argparse.Namespace:
    values = vars(args).copy()
    values.update(changes)
    return argparse.Namespace(**values)


def run_pipeline(args: argparse.Namespace) -> Path:
    steps = parse_steps(args.steps)
    args = _namespace_copy(args, promote=args.promote or "promote" in steps)
    if args.promote and not args.sprt:
        raise ValueError("Otomatik terfi için --sprt gerekli.")
    if args.promote and "promote" not in steps:
        steps.append("promote")
    if "promote" in steps and "match" not in steps:
        raise ValueError("promote için aynı pipeline içinde match gerekli.")
    phases = ["base", "aggressive"] if args.phase == "all" else [args.phase]
    if args.promote and "aggressive" in phases and "eas" not in steps:
        raise ValueError("Aggressive terfisi için --steps içinde eas da gerekli.")
    if "prepare" in steps and "aggressive" not in phases:
        raise ValueError("prepare yalnızca aggressive fazında kullanılabilir.")
    if args.max_iters < 0:
        raise ValueError("max-iters negatif olamaz.")
    if args.max_iters == 0:
        print("max-iters=0: pipeline durdurulana kadar yineleyecek.", flush=True)
    run_dir = resolve_path(args.run_dir)
    if run_dir is None:
        run_dir = ROOT / "runs" / datetime.now().strftime("%Y%m%d-%H%M%S")
    run_dir.mkdir(parents=True, exist_ok=True)
    print(
        f"Pipeline fazları={','.join(phases)} adımlar={','.join(steps)} "
        f"run={run_dir}",
        flush=True,
    )
    for phase in phases:
        baseline = NETS / f"{phase}.nnue"
        if args.baseline and len(phases) == 1:
            baseline = args.baseline
        if any(step in steps for step in ("train", "match", "promote")):
            require_file(baseline, f"{phase} taban NNUE")
        iteration = 0
        while True:
            iteration += 1
            if args.max_iters and iteration > args.max_iters:
                break
            prefix = run_dir / f"{phase}-{iteration}"
            dataset = (
                args.dataset if phase == "base" else
                (args.aggressive_dataset or (args.dataset if len(phases) == 1 else None))
            )
            if dataset:
                dataset_path = require_file(dataset, f"{phase} veri seti")
            else:
                dataset_path = prefix.with_suffix(".sbin")
            candidate = prefix.with_suffix(".nnue")
            if "train" in steps and candidate.exists():
                raise ValueError(f"Aday çıktı zaten var; yeni --run-dir kullanın: {candidate}")
            pgnout = prefix.with_suffix(".pgn")
            match_json = prefix.with_suffix(".match.json")
            eas_json = prefix.with_suffix(".eas.json")
            match_result: MatchResult | None = None
            eas_report: dict[str, Any] | None = None

            phase_steps = list(steps)
            print(
                f"\n[{phase.upper()} {iteration}] adımlar={','.join(phase_steps)}",
                flush=True,
            )
            for step in phase_steps:
                if step == "prepare":
                    dataset_path = prepare_dataset(_namespace_copy(
                        args, phase=phase, output=dataset_path,
                    ))
                elif step == "datagen":
                    dataset_path = run_datagen(_namespace_copy(
                        args, phase=phase, output=dataset_path,
                    ))
                elif step == "extract":
                    if dataset:
                        print(f"[{phase.upper()} {iteration}] Mevcut veri seti yeniden kullanılıyor: {dataset_path}", flush=True)
                    else:
                        dataset_path = extract_dataset(
                            _namespace_copy(args, phase=phase, output=dataset_path,
                                            skip_lines=args.skip_lines if iteration == 1 else None,
                                            reset_offset=args.reset_offset and iteration == 1,
                                            seed=args.seed + iteration - 1),
                            phase, dataset_path,
                        )
                elif step == "train":
                    dataset_path = require_file(dataset_path, f"{phase} veri seti")
                    train_nnue(
                        dataset_path, candidate,
                        epochs=args.epochs, batch_size=args.batch_size,
                        lr=args.lr, resume=baseline if args.resume is None else args.resume,
                        device_name=args.device, seed=args.seed,
                        patience=args.patience, validation=args.validation,
                        swa=getattr(args, "swa", True),
                    )
                elif step == "match":
                    if not candidate.is_file() and args.candidate:
                        candidate = args.candidate
                    if not candidate.is_file():
                        raise RuntimeError(
                            "match adımı için aday NNUE yok; train çalıştırın veya --candidate verin."
                        )
                    match_args = _namespace_copy(
                        args, phase=phase, candidate=candidate, baseline=baseline,
                        pgnout=pgnout, json_out=match_json,
                        games=(args.games_base or args.games) if phase == "base" else (args.games_aggressive or args.games),
                        fixed_base=args.fixed_base,
                        fixed_aggressive=args.fixed_aggressive,
                        engine=args.engine,
                    )
                    match_result = run_match(match_args, phase)
                elif step == "eas":
                    eas_input = pgnout if pgnout.is_file() else args.pgn
                    if eas_input is None:
                        raise RuntimeError(
                            "eas adımı için match PGN'i yok; match çalıştırın veya --pgn verin."
                        )
                    eas_args = _namespace_copy(
                        args, pgn=eas_input,
                        output=prefix.with_suffix(".eas.txt"),
                        json_out=eas_json,
                    )
                    eas_report = run_eas(eas_args)
                elif step == "sacrifices":
                    if args.pgn is None:
                        raise RuntimeError("sacrifices adımı için --pgn gerekli.")
                    run_sacrifices(_namespace_copy(
                        args, pgn=args.pgn, output=prefix.with_suffix(".sacrifices.pgn")
                    ))
                elif step == "promote":
                    if match_result is None:
                        raise RuntimeError("promote adımı match adımından sonra gelmeli.")
                    maybe_promote(args, phase, match_result, candidate, baseline, eas_report)
    print(f"Pipeline tamamlandı: {run_dir}")
    return run_dir


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description=(
            "Stallion EAS eğitim araçlarının tek giriş noktası. "
            "pipeline --phase base|aggressive|all --steps extract,train,match,eas"
        )
    )
    parser.add_argument(
        "command", nargs="?", default="pipeline",
        choices=["pipeline", "extract", "prepare", "datagen", "train", "match", "eas", "sacrifices", "mine"],
    )
    parser.add_argument("--phase", "--mode", "--net-type", dest="phase",
                        choices=["all", "base", "aggressive"], default="all")
    parser.add_argument("--steps", default="all",
                        help="pipeline adımları: extract,train,match,eas,sacrifices,promote,all")
    parser.add_argument("--run-dir", default=None)
    parser.add_argument("--output", default=None)
    parser.add_argument("--dataset", default=None)
    parser.add_argument("--aggressive-dataset", default=None)

    parser.add_argument("--source", "--sbin", "--zst", dest="source", default=str(DEFAULT_EVAL),
                        help="Eval SBIN veri kaynağı (varsayılan: training/data/evals.sbin)")
    parser.add_argument("--puzzles", default=str(DEFAULT_PUZZLES),
                        help="Feda bulmaca SBIN kaynağı (varsayılan: training/data/puzzle_sacrifices.sbin)")
    parser.add_argument("--target", type=int, default=None)
    parser.add_argument("--base-target", "--base-positions", dest="base_target",
                        type=int, default=500_000)
    parser.add_argument("--aggressive-target", "--agg-positions", dest="aggressive_target",
                        type=int, default=500_000)
    parser.add_argument("--min-depth", type=int, default=14,
                        help="Eval analiz derinliği; manifesti olmayan SBIN için 0 ile filtresiz kullan")
    parser.add_argument("--sbin-labels", choices=["cp", "wdl"], default="cp",
                        help="Eval SBIN etiket kaynağı: cp WDL'yi yeniden hesaplar; wdl mevcut etiketleri korur")
    parser.add_argument("--skip-lines", type=int, default=None)
    parser.add_argument("--allow-short-dataset", action="store_true",
                        help="Hedefin altında kalan kısmi veriyle devam et")
    parser.add_argument("--offset-file", default=str(ROOT / "data" / "stream_offset.json"))
    parser.add_argument("--reset-offset", "--reset-offsets", dest="reset_offset",
                        action="store_true")
    parser.add_argument("--no-offset", "--random", dest="no_offset", action="store_true",
                        help="Ofset dosyası kullanma, her seferinde rastgele konumdan başla")
    parser.add_argument("--phase-dist", type=str, default="35,35,20,10",
                        help="Base faz dağılım oranları: endgame,late_middle,midgame,opening (örn: 35,35,20,10)")
    parser.add_argument("--random-dist", action="store_true",
                        help="Faz oranlarını her iterasyonda tamamen rastgele belirle")
    parser.add_argument("--puzzle-ratio", type=float, default=0.20)
    parser.add_argument("--sac-ratio", type=float, default=0.50,
                        help="Aggressive fazında feda pozisyonu oranı (varsayılan: 0.50)")
    parser.add_argument("--wdl-lambda", type=float, default=0.25,
                        help="WDL hedef karışım oranı (0.0: saf eval, 1.0: saf oyun sonucu, varsayılan: 0.25)")
    parser.add_argument("--augment-mirror", action="store_true",
                        help="Rok hakkı kalmamış pozisyonlar için yatay ayna (a-h flip) artırımı uygula")
    parser.add_argument("--seed", type=int, default=42)

    parser.add_argument("--epochs", type=int, default=12)
    parser.add_argument("--batch-size", type=int, default=1024)
    parser.add_argument("--lr", type=float, default=2e-4)
    parser.add_argument("--resume", "--resume-net", dest="resume", default=None)
    parser.add_argument("--device", choices=["auto", "cpu", "mps", "cuda"], default="auto")
    parser.add_argument("--patience", type=int, default=8)
    parser.add_argument("--validation", type=float, default=0.05)
    parser.add_argument("--swa", action="store_true", default=True,
                        help="Son epoch'larda Stokastik Ağırlık Ortalaması (SWA) modeli üret (varsayılan: True)")
    parser.add_argument("--no-swa", dest="swa", action="store_false",
                        help="SWA model üretimini devre dışı bırak")
    parser.add_argument("--feature-dropout", type=float, default=0.0,
                        help="Eğitim sırasında rastgele özellik maskeleme oranı (varsayılan: 0.0)")
    parser.add_argument("--model", default=None,
                        help="Madencilik (mine) için referans şampiyon NNUE modeli")
    parser.add_argument("--pool-size", type=int, default=15_000_000,
                        help="Madencilikte taranacak pozisyon havuzu boyutu (varsayılan: 15,000,000)")

    parser.add_argument("--engine", default=None)
    parser.add_argument("--assets", default=str(DEFAULT_ASSETS),
                        help="cutechess/pgn-extract ve pattern dosyalarının bulunduğu klasör")
    parser.add_argument("--book", default=str(DEFAULT_BOOK))
    parser.add_argument("--candidate", default=None)
    parser.add_argument("--baseline", default=None)
    parser.add_argument("--fixed-base", default=None)
    parser.add_argument("--fixed-aggressive", default=None)
    parser.add_argument("--games", type=int, default=100)
    parser.add_argument("--games-base", type=int, default=None)
    parser.add_argument("--games-aggressive", "--games-agg", dest="games_aggressive",
                        type=int, default=None)
    parser.add_argument("--concurrency", type=int, default=4)
    parser.add_argument("--tc", default="5+0.05")
    parser.add_argument("--aggressive-source", choices=["simple", "puzzles"], default="simple",
                        help="aggressive extract kaynağı (puzzles yalnızca puzzle havuzunu kullanır)")
    parser.add_argument("--depth", type=int, default=8,
                        help="datagen self-play arama derinliği")
    parser.add_argument("--random-chance", type=float, default=0.05,
                        help="datagen rastgele hamle olasılığı")
    parser.add_argument("--opening-moves", default="4-8",
                        help="datagen rastgele açılış uzunluğu, örn. 4-8")
    parser.add_argument("--sprt", action="store_true")
    parser.add_argument("--promote", "--auto-filter", dest="promote", action="store_true")
    parser.add_argument("--min-eas-gain", type=int, default=0)
    parser.add_argument("--pgn", default=None)
    parser.add_argument("--pgnout", default=None)
    parser.add_argument("--json-out", "--json", dest="json_out", default=None)
    parser.add_argument("--sac-type", type=int, choices=[0, 1, 2, 3, 4, 5, 9], default=0)
    parser.add_argument("--max-moves", type=int, default=80)
    parser.add_argument("--max-iters", type=int, default=1)
    parser.add_argument("--version", action="version", version="stallion-training 1.0")
    return parser


def normalize_args(args: argparse.Namespace) -> argparse.Namespace:
    if args.promote and not args.sprt:
        raise ValueError("Otomatik terfi için --sprt gerekli.")
    if args.games_base is None:
        args.games_base = args.games
    if args.games_aggressive is None:
        args.games_aggressive = args.games
    args.assets = resolve_path(getattr(args, "assets", None), DEFAULT_ASSETS)
    args.source = resolve_path(getattr(args, "source", None) or getattr(args, "zst", None), DEFAULT_EVAL)
    args.zst = args.source
    args.puzzles = resolve_path(args.puzzles, DEFAULT_PUZZLES)
    args.book = resolve_path(args.book, DEFAULT_BOOK)
    args.offset_file = resolve_path(args.offset_file)
    args.dataset = resolve_path(args.dataset)
    args.aggressive_dataset = resolve_path(args.aggressive_dataset)
    args.run_dir = resolve_path(args.run_dir)
    args.resume = resolve_path(args.resume)
    args.candidate = resolve_path(args.candidate)
    args.baseline = resolve_path(args.baseline)
    args.fixed_base = resolve_path(args.fixed_base, NETS / "base.nnue")
    args.fixed_aggressive = resolve_path(args.fixed_aggressive, NETS / "aggressive.nnue")
    if args.engine is None:
        filename = {"darwin": "stallion_eas_mac", "win32": "stallion_eas_windows.exe"}.get(sys.platform, "stallion_eas_linux")
        preferred = ENGINE_ROOT / filename
        fallback = ROOT / filename
        args.engine = preferred if preferred.is_file() else fallback
    else:
        args.engine = resolve_path(args.engine)
    args.pgn = resolve_path(args.pgn)
    args.pgnout = resolve_path(args.pgnout)
    args.json_out = resolve_path(args.json_out)
    if args.command in ("extract", "prepare") and args.output is None:
        phase = args.phase if args.phase in ("base", "aggressive") else "base"
        args.output = ROOT / "data" / ("aggressive-prepared.sbin" if args.command == "prepare" else f"{phase}.sbin")
    elif args.command == "datagen" and args.output is None:
        args.output = ROOT / "data" / "selfplay.sbin"
    else:
        args.output = resolve_path(args.output)
    if args.command == "train" and args.output is None:
        phase = args.phase if args.phase in ("base", "aggressive") else "base"
        args.output = ROOT / "runs" / f"train-{datetime.now():%Y%m%d-%H%M%S-%f}" / f"{phase}-candidate.nnue"
    if args.command == "eas" and args.output is None:
        args.output = ROOT / "statistics_EAS_ratinglist.txt"
    if args.command == "sacrifices" and args.output is None:
        args.output = ROOT / "games_with_sacrifices.pgn"
    args.output = resolve_path(args.output)
    if args.command == "match":
        if args.candidate is None or args.baseline is None:
            raise ValueError("match için --candidate ve --baseline gerekli.")
        if args.pgnout is None:
            args.pgnout = ROOT / "runs" / f"match-{datetime.now():%Y%m%d-%H%M%S-%f}.pgn"
    return args


def main(argv: Sequence[str] | None = None) -> int:
    parser = build_parser()
    try:
        args = normalize_args(parser.parse_args(argv))
        if args.command in ("extract", "prepare", "datagen", "train", "match") and args.phase == "all":
            raise ValueError(f"{args.command} için --phase base veya aggressive seçin.")
        if args.command == "extract":
            extract_dataset(args, args.phase, args.output)
            return 0
        if args.command == "prepare":
            prepare_dataset(args)
            return 0
        if args.command == "datagen":
            run_datagen(args)
            return 0
        if args.command == "mine":
            mine_hard_dataset(args)
            return 0
        if args.command == "train":
            if args.dataset is None:
                raise ValueError("train için --dataset gerekli.")
            train_nnue(
                args.dataset, args.output, epochs=args.epochs,
                batch_size=args.batch_size, lr=args.lr, resume=args.resume,
                device_name=args.device, seed=args.seed, patience=args.patience,
                validation=args.validation, swa=getattr(args, "swa", True),
                feature_dropout=getattr(args, "feature_dropout", 0.0),
            )
            return 0
        if args.command == "match":
            result = run_match(args, args.phase)
            eas_report = None
            if args.phase == "aggressive":
                pgn_file = args.pgnout if (args.pgnout and Path(args.pgnout).is_file()) else args.pgn
                if pgn_file and Path(pgn_file).is_file():
                    eas_report = run_eas(_namespace_copy(
                        args, pgn=pgn_file,
                        output=Path(pgn_file).with_suffix(".eas.txt"),
                        json_out=Path(pgn_file).with_suffix(".eas.json"),
                    ))
            maybe_promote(args, args.phase, result, args.candidate, args.baseline, eas_report)
            return 0 if not args.sprt or result.is_winner else 1
        if args.command == "eas":
            if args.pgn is None:
                raise ValueError("eas için --pgn gerekli.")
            run_eas(args)
            return 0
        if args.command == "sacrifices":
            if args.pgn is None:
                raise ValueError("sacrifices için --pgn gerekli.")
            run_sacrifices(args)
            return 0
        run_pipeline(args)
        return 0
    except (RuntimeError, ValueError, OSError, subprocess.SubprocessError) as exc:
        print(f"[HATA] {exc}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    raise SystemExit(main())

#!/usr/bin/env python3
"""Single entry point for all Stallion EAS training workflows.

Commands:
  extract      phase-balanced or aggressive SBIN extraction
  prepare      validated aggressive puzzle-position preparation
  datagen      labelled self-play data generation through one UCI session/game
  train        engine-compatible NNUE training and export
  match        candidate/baseline cutechess comparison
  gauntlet     quick foreign-anchor gate vs Stockfish UCI_Elo
  eas          local aggressiveness report from a PGN
  sacrifices   SGS sacrifice scan from a PGN
  iwins        IWS interesting-wins filter from a PGN
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
import random
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
NNUE_OUTPUT_BUCKETS = 16
# 16 king buckets x 768 piece-square inputs + 1028 extra inputs (engine defs.h).
NNUE_FEATURES = 13316
NNUE_ACCUMULATOR = 1024
NNUE_PAYLOAD_SIZE = 2 * (NNUE_FEATURES * NNUE_ACCUMULATOR + NNUE_ACCUMULATOR + NNUE_OUTPUT_BUCKETS * (2 * NNUE_ACCUMULATOR) + NNUE_OUTPUT_BUCKETS)
NNUE_FILE_SIZE = (NNUE_PAYLOAD_SIZE + 63) // 64 * 64
# Checkpoints store the validation split scheme; a new scheme needs a new run.
SPLIT_VERSION = 2
SCALE = 400
QA = 255
QB = 64
QAB = QA * QB
PHASES = ("endgame", "late_middle", "midgame", "opening")


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
    """Relative paths are relative to the working directory."""
    if value is None:
        return default.expanduser().resolve() if default else None
    return Path(value).expanduser().resolve()


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


def run_checked(command: Sequence[str | os.PathLike[str]], cwd: Path | None = None) -> subprocess.CompletedProcess[str]:
    """Run quietly; the captured output is shown only when the command fails."""
    cmd = [str(item) for item in command]
    result = subprocess.run(cmd, cwd=str(cwd) if cwd else None, capture_output=True, text=True, check=False)
    if result.returncode:
        detail = (result.stderr or result.stdout or "").strip()
        raise RuntimeError(
            f"Komut başarısız ({result.returncode}): {' '.join(cmd)}"
            + (f"\n{detail}" if detail else "")
        )
    return result


def file_identity(path: Path) -> dict[str, Any]:
    path = path.resolve()
    stat = path.stat()
    return {"path": str(path), "size": stat.st_size, "mtime_ns": stat.st_mtime_ns}


def file_sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def _sbin() -> Any:
    """Resolve the native decoder for the current invocation style."""
    return importlib.import_module(".sbin_tool", __package__) if __package__ else importlib.import_module("sbin_tool")


def _require_nnue_size(size: int) -> None:
    if size not in (NNUE_PAYLOAD_SIZE, NNUE_FILE_SIZE):
        raise ValueError(
            f"NNUE boyutu geçersiz: {size} byte; yalnızca 16 giriş / 16 çıkış bucket "
            f"({NNUE_FEATURES}x{NNUE_ACCUMULATOR}, {NNUE_PAYLOAD_SIZE} veya {NNUE_FILE_SIZE} byte) desteklenir."
        )


def require_network(path: Any, label: str = "NNUE") -> Path:
    network = require_file(path, label)
    _require_nnue_size(network.stat().st_size)
    return network


def normalize_engine_fen(fen: str) -> str:
    chess = dependency("chess")
    board = chess.Board(fen)
    if not board.is_valid():
        raise ValueError(f"Yasal olmayan FEN: {fen}")
    return board.fen()


def add_dataset_record(records: list[tuple[str, float]], seen: set[str],
                       fen: str, wdl: float) -> bool:
    """Append a normalized (fen, white wdl) record; False for a repeated board."""
    normalized = normalize_engine_fen(fen)
    value = float(wdl)
    if not math.isfinite(value) or not 0.0 <= value <= 1.0:
        raise ValueError(f"Geçersiz WDL: {wdl}")
    key = " ".join(normalized.split()[:2])
    if key in seen:
        return False
    seen.add(key)
    records.append((normalized, value))
    return True


def atomic_records(path: Path, records: Any) -> None:
    """Write a numpy array of 32-byte SBIN records atomically."""
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(f".{path.name}.{uuid.uuid4().hex}.tmp")
    try:
        records.tofile(temporary)
        temporary.replace(path)
    finally:
        temporary.unlink(missing_ok=True)


def write_dataset(records: Sequence[tuple[str, float]], output: Path,
                  evals: Sequence[int] | None = None) -> None:
    """Pack normalized (fen, white wdl[, white cp]) records; every record must pack."""
    import ctypes
    if not records:
        raise RuntimeError("Uygun veri bulunamadı.")
    if output.suffix.lower() != ".sbin":
        raise ValueError("Dataset çıktı uzantısı .sbin olmalı.")
    sbin = _sbin()
    lib = sbin.load_native_lib()
    packed = sbin.PackedPosition()
    data = bytearray()
    for index, (fen, wdl) in enumerate(records):
        cp = 0 if evals is None else evals[index]
        status = lib.sbin_pack_fen(fen.encode("utf-8"), ctypes.c_float(wdl), ctypes.c_int16(cp),
                                   ctypes.byref(packed))
        if status:
            raise ValueError(f"Paketlenemeyen kayıt (kod {status}); kayıt={index}: {fen}")
        data.extend(bytes(packed))
    atomic_bytes(output, bytes(data))


def phase_distribution(args: argparse.Namespace, target: int, rng: Any) -> dict[str, int]:
    np = dependency("numpy")
    if args.random_dist:
        floors = np.array([0.15, 0.15, 0.10, 0.05])
        weights = floors + (1.0 - floors.sum()) * rng.dirichlet([2.0, 2.0, 1.5, 1.0])
    else:
        try:
            weights = np.array([float(value.strip()) for value in args.phase_dist.split(",")])
        except ValueError as exc:
            raise ValueError("phase-dist dört sayısal oran içermeli.") from exc
        if (len(weights) != 4 or not np.isfinite(weights).all() or
                (weights < 0).any() or weights.sum() <= 0):
            raise ValueError("phase-dist sonlu, negatif olmayan ve toplamı pozitif dört oran içermeli.")
        weights /= weights.sum()
    quotas = weights * target
    counts = np.floor(quotas).astype(np.int64)
    for index in np.argsort(-(quotas - counts), kind="stable")[:target - int(counts.sum())]:
        counts[index] += 1
    return dict(zip(PHASES, map(int, counts)))


def _require_cp_labels(source: Path, sample: int = 2048) -> None:
    """Fail fast when cp-derived labels are requested from a cp-less SBIN.

    Files written by this pipeline (self-play, puzzles, prepared sets) store
    eval=0 with WDL-only labels. Calibrating them as cp would silently rewrite
    every label to 0.5, so refuse and point at --sbin-labels wdl instead.
    """
    np = dependency("numpy")
    with _sbin().SbinDataset(source) as ds:
        count = min(max(1, sample), ds.count)
        stride = max(1, ds.count // count)
        nonzero = int(np.count_nonzero(ds.eval_view()[::stride][:count]))
    if nonzero < max(1, count // 100):
        raise ValueError(
            f"{source.name} cp etiketi taşımıyor (örneklemde {nonzero}/{count} sıfır-dışı); "
            "WDL-etiketli kaynak için --sbin-labels wdl kullanın."
        )


def _read_offsets(path: Path | None) -> dict[str, Any]:
    if path is None or not path.exists():
        return {}
    state = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(state, dict):
        raise ValueError(f"Ofset dosyası JSON sözlüğü olmalı: {path}")
    return state


def _label_settings(args: argparse.Namespace) -> tuple[bool, float]:
    labels_cp = args.sbin_labels == "cp"
    lam = float(args.wdl_lambda)
    if not 0.0 <= lam <= 1.0:
        raise ValueError("wdl-lambda 0 ile 1 arasında olmalı.")
    return labels_cp, lam


def extract_dataset(args: argparse.Namespace, phase: str | None = None,
                    output: Path | None = None) -> Path:
    """Native selection from eval/puzzle SBIN sources into one shuffled SBIN."""
    np = dependency("numpy")
    sbin = _sbin()
    lib = sbin.load_native_lib()
    phase = phase or args.phase
    if phase not in ("base", "aggressive"):
        raise ValueError("extract için phase base veya aggressive olmalı.")
    output = (output or args.output or ROOT / "data" / f"{phase}.sbin").resolve()
    if output.suffix.lower() != ".sbin":
        raise ValueError("Dataset çıktı uzantısı .sbin olmalı.")
    labels_cp, lam = _label_settings(args)
    filter_names = args.base_filters if phase == "base" else args.aggressive_filters
    filters = sbin.filter_mask(filter_names)
    rng = np.random.default_rng(args.seed)
    stats = np.zeros(len(sbin.STAT_NAMES), dtype=np.uint64)
    manifest: dict[str, Any] = {"phase": phase, "filters": list(filter_names), "seed": args.seed}

    if phase == "aggressive" and args.aggressive_source == "puzzles":
        puzzles = require_file(args.puzzles, "Feda bulmaca havuzu")
        if output == puzzles:
            raise ValueError("Puzzle havuzu ve çıktı aynı dosya olamaz.")
        with sbin.SbinDataset(puzzles) as p_ds:
            target = int(args.target if args.target is not None else p_ds.count)
            if target < 2:
                raise ValueError("target en az 2 olmalı.")
            rows = rng.permutation(p_ds.count).astype(np.int64)
            out = np.empty(target, dtype=sbin.RECORD)
            written = lib.sbin_select_aggressive(
                sbin.pointer(p_ds.records()), sbin.pointer(rows), len(rows), None, 0, 0,
                target, 1.0, 0, filters, 0, 0.0, sbin.pointer(out), sbin.pointer(stats))
        manifest.update({"source": file_identity(puzzles), "target": target, "label_source": "wdl"})
    else:
        source = require_file(args.source, "Eval SBIN kaynağı")
        if source.suffix.lower() != ".sbin":
            raise ValueError(f"Yalnızca .sbin eval arşivleri desteklenir: {source}")
        if source == output:
            raise ValueError("Eval kaynağı ve çıktı aynı dosya olamaz.")
        target = int(args.target if args.target is not None else
                     (args.base_target if phase == "base" else args.aggressive_target))
        if target < 2:
            raise ValueError("target en az 2 olmalı.")
        if labels_cp:
            _require_cp_labels(source)
        state = _read_offsets(args.offset_file)
        source_identity = file_identity(source)
        with sbin.SbinDataset(source) as ds:
            if args.no_offset:
                start = int(rng.integers(0, ds.count))
            elif args.reset_offset:
                start = 0
            else:
                if args.skip_lines is None and state.get("source") not in (None, source_identity):
                    raise ValueError("Eval kaynağı değişti; --reset-offset veya açık --skip-lines kullanın.")
                start = int(args.skip_lines if args.skip_lines is not None else state.get(f"{phase}_offset", 0))
            if not 0 <= start < ds.count:
                raise ValueError(f"Başlangıç satırı kaynak dışında: {start:,}/{ds.count:,}")
            out = np.empty(target, dtype=sbin.RECORD)
            if phase == "base":
                distribution = phase_distribution(args, target, rng)
                quotas = np.array([distribution[name] for name in PHASES], dtype=np.uint64)
                written = lib.sbin_select_base(
                    sbin.pointer(ds.records()), ds.count, start, sbin.pointer(quotas), filters,
                    int(labels_cp), lam, sbin.pointer(out), sbin.pointer(stats))
                manifest["phase_dist"] = distribution
            else:
                puzzle_ratio, sac_ratio = float(args.puzzle_ratio), float(args.sac_ratio)
                if not 0.0 <= puzzle_ratio <= 1.0 or not 0.0 <= sac_ratio <= 1.0:
                    raise ValueError("puzzle-ratio ve sac-ratio 0 ile 1 arasında olmalı.")
                puzzle_records = np.empty(0, dtype=sbin.RECORD)
                puzzle_rows = np.empty(0, dtype=np.int64)
                if puzzle_ratio:
                    puzzles = require_file(args.puzzles, "Feda bulmaca havuzu")
                    with sbin.SbinDataset(puzzles) as p_ds:
                        puzzle_records = np.array(p_ds.records())
                    puzzle_rows = rng.choice(len(puzzle_records), size=min(int(target * puzzle_ratio), len(puzzle_records)),
                                             replace=False).astype(np.int64)
                    manifest["puzzles"] = file_identity(puzzles)
                written = lib.sbin_select_aggressive(
                    sbin.pointer(puzzle_records), sbin.pointer(puzzle_rows), len(puzzle_rows),
                    sbin.pointer(ds.records()), ds.count, start, target, sac_ratio,
                    int(args.augment_mirror), filters, int(labels_cp), lam,
                    sbin.pointer(out), sbin.pointer(stats))
        if file_identity(source) != source_identity:
            raise RuntimeError("Eval kaynağı çıkarma sırasında değişti; çıktı yayımlanmadı.")
        manifest.update({"source": source_identity, "target": target, "start_after": start,
                         "label_source": args.sbin_labels, "wdl_lambda": lam if labels_cp else None})
    if written < 0:
        raise RuntimeError("Native seçim geçersiz argüman aldı.")
    if not written:
        raise RuntimeError("Uygun pozisyon bulunamadı.")
    summary = sbin.stats_dict(stats)
    if "start_after" in manifest and not args.no_offset and args.offset_file:
        state[f"{phase}_offset"] = summary["next_offset"]
        state["source"] = manifest["source"]
        atomic_json(args.offset_file, state)
    atomic_records(output, out[:written][rng.permutation(written)])
    atomic_json(output.with_suffix(".extract.json"), {**manifest, "output": file_identity(output), **summary})
    if written < manifest["target"] and not args.allow_short_dataset:
        raise RuntimeError(f"Hedef tamamlanmadı: {written:,}/{manifest['target']:,}; kısmi veri {output} içinde; "
                           "kullanmak için --allow-short-dataset gerekli.")
    skipped = ", ".join(f"{name}={summary[name]:,}" for name in ("invalid", "duplicate", "mate", "check", "tactical")
                        if summary[name])
    print(f"[BAŞARILI] {output}: {written:,} kayıt{'; atlanan ' + skipped if skipped else ''}", flush=True)
    return output


def mine_hard_dataset(args: argparse.Namespace) -> Path:
    """Keep the positions the reference network predicts worst."""
    torch = dependency("torch")
    np = dependency("numpy")
    sbin = _sbin()
    lib = sbin.load_native_lib()
    source = require_file(args.source, "Eval SBIN kaynağı")
    output = (args.output or ROOT / "data" / "base_hard.sbin").resolve()
    if output == source:
        raise ValueError("Madencilik kaynağı ve çıktı aynı dosya olamaz.")
    target = int(args.target if args.target is not None else 5_000_000)
    pool_size = int(args.pool_size)
    if target < 2 or pool_size < target:
        raise ValueError("target en az 2, pool-size en az target kadar olmalı.")
    labels_cp, lam = _label_settings(args)
    if not labels_cp:
        raise ValueError("mine cp etiketleriyle çalışır; --sbin-labels cp kullanın.")
    _require_cp_labels(source)
    filters = sbin.filter_mask(args.base_filters)
    model_path = require_network(args.model or NETS / "stallion.nnue", "Madencilik Modeli")
    device = _torch_device(torch, args.device)
    model = _make_nnue_model().to(device)
    load_nnue(model, model_path)
    model.eval()
    state = _read_offsets(args.offset_file) if not args.no_offset else {}
    source_identity = file_identity(source)
    if state.get("source") not in (None, source_identity):
        raise ValueError("Eval kaynağı değişti; ofset dosyasını sıfırlayın veya --no-offset kullanın.")
    errors = np.empty(pool_size, dtype=np.float32)
    pool_rows = np.empty(pool_size, dtype=np.int64)
    rejected = np.zeros(5, dtype=np.int64)
    started = time.perf_counter()
    with sbin.SbinDataset(source) as ds:
        records = ds.records()
        cursor = int(state.get("hard_offset", 0))
        if not 0 <= cursor < ds.count:
            raise ValueError(f"hard_offset kaynak dışında: {cursor:,}/{ds.count:,}")
        filled = scanned = 0
        while filled < pool_size:
            if scanned >= ds.count:
                raise RuntimeError(f"Kaynakta yalnızca {filled:,} uygun kayıt var; pool-size {pool_size:,}.")
            take = min(262144, ds.count - cursor, ds.count - scanned)
            chunk = np.array(records[cursor:cursor + take])
            verdicts = np.empty(take, dtype=np.uint8)
            lib.sbin_screen_batch(sbin.pointer(chunk), take, filters, sbin.pointer(verdicts))
            usable = np.flatnonzero(verdicts == 0)[:pool_size - filled].astype(np.int64)
            lib.sbin_calibrate_labels(sbin.pointer(chunk), take, lam)
            chunk_errors = []
            with torch.no_grad():
                for batch in batch_stream(lib, chunk, usable, 16384, 0.0, 0, drop_last=False):
                    *inputs, targets = batch.tensors(torch, device)
                    chunk_errors.append((torch.sigmoid(model(*inputs)) - targets).abs()[:batch.valid])
            if len(usable):
                errors[filled:filled + len(usable)] = torch.cat(chunk_errors).cpu().numpy()
                pool_rows[filled:filled + len(usable)] = cursor + usable
            filled += len(usable)
            consumed = int(usable[-1]) + 1 if filled == pool_size else take
            rejected += np.bincount(verdicts[:consumed], minlength=5)
            scanned += consumed
            cursor = (cursor + consumed) % ds.count
        selected = np.arange(pool_size) if target >= pool_size else np.argpartition(errors, -target)[-target:]
        mined = np.array(records[np.sort(pool_rows[selected])])
    if file_identity(source) != source_identity:
        raise RuntimeError("Eval kaynağı madencilik sırasında değişti; çıktı yayımlanmadı.")
    lib.sbin_calibrate_labels(sbin.pointer(mined), len(mined), lam)
    atomic_records(output, mined)
    if not args.no_offset and args.offset_file:
        state["hard_offset"] = cursor
        state["source"] = source_identity
        atomic_json(args.offset_file, state)
    atomic_json(output.with_suffix(".extract.json"), {
        "phase": "mine", "source": source_identity, "output": file_identity(output),
        "model": str(model_path), "model_sha256": file_sha256(model_path),
        "pool_size": pool_size, "target": target, "rows": len(mined), "wdl_lambda": lam,
        "filters": list(args.base_filters), "next_offset": cursor,
        "invalid": int(rejected[1]), "mate": int(rejected[2]), "check": int(rejected[3]), "tactical": int(rejected[4]),
        "mean_pool_error": float(errors.mean()), "mean_hard_error": float(errors[selected].mean()),
        "seconds": round(time.perf_counter() - started, 1),
    })
    print(f"[BAŞARILI] {output}: {len(mined):,} zor pozisyon (havuz {pool_size:,}; ortalama hata "
          f"{float(errors.mean()):.4f} -> {float(errors[selected].mean()):.4f})", flush=True)
    return output


def _parse_range(value: str, label: str) -> tuple[int, int]:
    pieces = [part.strip() for part in str(value).split("-", 1)]
    try:
        bounds = tuple(int(part) for part in pieces)
    except (TypeError, ValueError) as exc:
        raise ValueError(f"{label} min-max biçiminde olmalı.") from exc
    if len(bounds) == 1:
        low = high = bounds[0]
    else:
        low, high = bounds
    if low < 0 or high < low:
        raise ValueError(f"{label} aralığı geçersiz.")
    return low, high


def _load_epd_fens(book: Path) -> list[str]:
    """Opening FENs (first four EPD fields) of a required book file."""
    book = require_file(book, "Açılış kitabı")
    fens = [" ".join(fields[:4]) for fields in
            (line.split() for line in book.read_text(encoding="utf-8").splitlines())
            if len(fields) >= 4 and "/" in fields[0]]
    if not fens:
        raise ValueError(f"Açılış kitabında FEN yok: {book}")
    return fens


def _search_limit(args: argparse.Namespace) -> Any:
    """python-chess Limit from --depth/--nodes; each flag is its own limit."""
    engine_api = importlib.import_module("chess.engine")
    if args.depth is None and args.nodes is None:
        raise ValueError("Arama sınırı gerekli: --depth ve/veya --nodes verin.")
    if (args.depth is not None and args.depth < 1) or (args.nodes is not None and args.nodes < 1):
        raise ValueError("depth ve nodes pozitif olmalı.")
    return engine_api.Limit(depth=args.depth, nodes=args.nodes)


def _selfplay_game(engine_path: Path, limit: Any, random_chance: float,
                   opening_moves: tuple[int, int], max_moves: int,
                   seed: int, book_fens: Sequence[str]) -> list[tuple[str, float]]:
    """Play one game on a persistent UCI process; positions get the final result.

    Games start from a random book line, or from random opening moves when no
    book is used. A game cut at max-moves has no result and yields nothing.
    """
    chess = dependency("chess")
    engine_api = importlib.import_module("chess.engine")
    rng = random.Random(seed)
    board = chess.Board(rng.choice(book_fens)) if book_fens else chess.Board()
    if not book_fens:
        for _ in range(rng.randint(*opening_moves) * 2):
            if board.is_game_over(claim_draw=True):
                break
            board.push(rng.choice(list(board.legal_moves)))
    positions: list[str] = []
    with engine_api.SimpleEngine.popen_uci(str(engine_path)) as process:
        process.configure({"Threads": 1})
        while len(board.move_stack) < max_moves * 2 and not board.is_game_over(claim_draw=True):
            positions.append(board.fen())
            if rng.random() < random_chance:
                board.push(rng.choice(list(board.legal_moves)))
            else:
                board.push(process.play(board, limit).move)
    outcome = board.outcome(claim_draw=True)
    if outcome is None:
        return []
    value = {"1-0": 1.0, "0-1": 0.0, "1/2-1/2": 0.5}[outcome.result()]
    return [(fen, value) for fen in positions]


def run_datagen(args: argparse.Namespace) -> Path:
    """Self-play games labelled with their final result."""
    from concurrent.futures import ThreadPoolExecutor
    engine = require_file(args.engine, "Self-play motoru")
    limit = _search_limit(args)
    random_chance = float(args.random_chance)
    opening_moves = _parse_range(args.opening_moves, "opening-moves")
    if args.games < 1 or args.max_moves < 1 or args.concurrency < 1:
        raise ValueError("datagen games, max-moves ve concurrency pozitif olmalı.")
    if not math.isfinite(random_chance) or not 0.0 <= random_chance <= 1.0:
        raise ValueError("random-chance 0 ile 1 arasında olmalı.")
    output = (args.output or ROOT / "data" / "selfplay.sbin").resolve()
    book_fens = _load_epd_fens(args.book) if args.book else []
    records: list[tuple[str, float]] = []
    seen: set[str] = set()
    completed = discarded = 0
    with ThreadPoolExecutor(max_workers=args.concurrency) as pool:
        games = pool.map(
            lambda index: _selfplay_game(engine, limit, random_chance, opening_moves,
                                         args.max_moves, args.seed + index, book_fens),
            range(args.games))
        for game_records in games:
            if not game_records:
                discarded += 1
                continue
            completed += 1
            for fen, value in game_records:
                add_dataset_record(records, seen, fen, value)
    write_dataset(records, output)
    atomic_json(output.with_suffix(".datagen.json"), {
        "engine": file_identity(engine), "games_requested": args.games,
        "games_completed": completed, "games_discarded": discarded,
        "positions": len(records), "depth": args.depth, "nodes": args.nodes,
        "max_moves": args.max_moves, "random_chance": random_chance,
        "opening_moves": list(opening_moves), "concurrency": args.concurrency,
        "seed": args.seed, "book": str(args.book) if args.book else None,
        "book_lines": len(book_fens),
    })
    print(f"[BAŞARILI] {output}: {len(records):,} pozisyon, {completed:,} oyun "
          f"(sonuçsuz {discarded:,})", flush=True)
    return output


def _label_chunk(engine: Path, limit: Any, fens: Sequence[str]) -> list[int]:
    """White-POV cp of each FEN from one persistent engine; mates become +-2000."""
    chess = dependency("chess")
    engine_api = importlib.import_module("chess.engine")
    values: list[int] = []
    with engine_api.SimpleEngine.popen_uci(str(engine)) as process:
        process.configure({"Threads": 1})
        for fen in fens:
            score = process.analyse(chess.Board(fen), limit)["score"].white()
            mate = score.mate()
            values.append((2000 if mate > 0 else -2000) if mate is not None else max(-32000, min(32000, score.score())))
    return values


def run_label(args: argparse.Namespace) -> Path:
    """Add cp evals from an external UCI engine to a WDL-only SBIN."""
    from concurrent.futures import ThreadPoolExecutor
    source = require_file(args.dataset, "Etiketlenecek SBIN")
    engine = require_file(args.engine, "Etiket motoru")
    limit = _search_limit(args)
    if args.concurrency < 1:
        raise ValueError("label için concurrency pozitif olmalı.")
    output = (args.output or source.with_name(f"{source.stem}-lbl.sbin")).resolve()
    if output == source:
        raise ValueError("Etiket çıktısı girdi dosyasıyla aynı olamaz.")
    started = time.time()
    with _sbin().SbinDataset(source) as ds:
        inputs = [ds.get_fen(index) for index in range(len(ds))]
    fens = [fen for fen, _, _ in inputs]
    chunks = [fens[begin::args.concurrency] for begin in range(args.concurrency)]
    with ThreadPoolExecutor(max_workers=args.concurrency) as pool:
        labelled = list(pool.map(lambda chunk: _label_chunk(engine, limit, chunk), chunks))
    evals = [0] * len(fens)
    for begin, values in enumerate(labelled):
        evals[begin::args.concurrency] = values
    write_dataset([(fen, wdl) for fen, wdl, _ in inputs], output, evals)
    atomic_json(output.with_suffix(".label.json"), {
        "source": file_identity(source), "output": file_identity(output),
        "engine": file_identity(engine), "depth": args.depth, "nodes": args.nodes,
        "workers": args.concurrency, "positions": len(fens),
        "mean_abs_cp": sum(map(abs, evals)) / len(evals), "seconds": time.time() - started,
    })
    print(f"[BAŞARILI] {output}: {len(fens):,} pozisyon etiketlendi ({time.time() - started:.0f} sn)", flush=True)
    return output


def prepare_dataset(args: argparse.Namespace) -> Path:
    """Aggressive puzzle pool through the same native selection."""
    output = (args.output or ROOT / "data" / "aggressive-prepared.sbin").resolve()
    prepared_args = _namespace_copy(args, phase="aggressive", aggressive_source="puzzles", output=output)
    return extract_dataset(prepared_args, "aggressive", output)


@contextmanager
def advisory_lock(path: Path) -> Iterator[None]:
    """Exclusive cross-platform lock; fails fast if another process holds it."""
    path.parent.mkdir(parents=True, exist_ok=True)
    if os.name == "nt":
        import msvcrt
        path.touch(exist_ok=True)
        if path.stat().st_size == 0:
            path.write_bytes(b"\x00")
        handle = path.open("r+b")
        try:
            handle.seek(0)
            try:
                msvcrt.locking(handle.fileno(), msvcrt.LK_NBLCK, 1)
            except OSError as exc:
                raise RuntimeError(f"Çıktı başka bir işlem tarafından hazırlanıyor: {path}") from exc
            yield
        finally:
            try:
                handle.seek(0)
                msvcrt.locking(handle.fileno(), msvcrt.LK_UNLOCK, 1)
            finally:
                handle.close()
        return
    import fcntl
    handle = path.open("a+")
    try:
        try:
            fcntl.flock(handle.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError as exc:
            raise RuntimeError(f"Çıktı başka bir işlem tarafından hazırlanıyor: {path}") from exc
        yield
    finally:
        fcntl.flock(handle.fileno(), fcntl.LOCK_UN)
        handle.close()


class BatchBuffers:
    """Host buffers of one CSR batch produced by sbin_build_batch."""

    def __init__(self, batch_size: int, slots: int) -> None:
        np = dependency("numpy")
        self.indices = np.empty(2 * batch_size * slots, dtype=np.int32)
        self.offsets = np.empty(2 * batch_size + 1, dtype=np.int32)
        self.t_bags = np.empty(2 * batch_size * slots, dtype=np.int32)
        self.t_offsets = np.empty(NNUE_FEATURES + 1, dtype=np.int32)
        self.buckets = np.empty(batch_size, dtype=np.int64)
        self.targets = np.empty(batch_size, dtype=np.float32)
        self.count = self.valid = self.nnz = 0

    def build(self, lib: Any, records: Any, rows: Any, dropout: float, seed: int) -> None:
        sbin = _sbin()
        nnz = lib.sbin_build_batch(
            sbin.pointer(records), len(records), sbin.pointer(rows), len(rows), dropout, seed,
            sbin.pointer(self.indices), sbin.pointer(self.offsets), sbin.pointer(self.t_bags),
            sbin.pointer(self.t_offsets), sbin.pointer(self.buckets), sbin.pointer(self.targets))
        if nnz < 0:
            raise ValueError("Batch satırı veri seti dışında.")
        self.count, self.nnz = len(rows), nnz

    def tensors(self, torch: Any, device: Any) -> tuple[Any, ...]:
        """(indices, offsets, t_bags, t_offsets, buckets, targets) on the device."""
        count, nnz = self.count, self.nnz
        return tuple(torch.from_numpy(array).to(device) for array in (
            self.indices[:nnz], self.offsets[:2 * count + 1], self.t_bags[:nnz],
            self.t_offsets, self.buckets[:count], self.targets[:count]))


def batch_stream(lib: Any, records: Any, rows: Any, batch_size: int, dropout: float, seed: int,
                 *, drop_last: bool) -> Iterator[BatchBuffers]:
    """Batches built on a background thread (ctypes releases the GIL).

    Every batch has batch_size rows so compiled kernels keep one shape; a short
    final batch is padded with its first row and reports the real size in .valid.
    """
    import queue
    import threading
    np = dependency("numpy")
    rows = np.ascontiguousarray(rows, dtype=np.int64)
    ends = range(batch_size, len(rows) + (0 if drop_last else batch_size - 1) + 1, batch_size)
    slots = lib.sbin_nnue_slots()
    free: queue.Queue[BatchBuffers | None] = queue.Queue()
    ready: queue.Queue[BatchBuffers | None] = queue.Queue()
    for _ in range(3):
        free.put(BatchBuffers(batch_size, slots))
    failure: list[BaseException] = []

    def produce() -> None:
        try:
            for number, end in enumerate(ends):
                buffers = free.get()
                if buffers is None:
                    return
                part = rows[end - batch_size:end] if end <= len(rows) else np.concatenate(
                    (rows[end - batch_size:], np.full(end - len(rows), rows[end - batch_size], dtype=np.int64)))
                buffers.build(lib, records, part, dropout, seed + number)
                buffers.valid = min(end, len(rows)) - (end - batch_size)
                ready.put(buffers)
        except BaseException as exc:
            failure.append(exc)
        finally:
            ready.put(None)

    worker = threading.Thread(target=produce, daemon=True)
    worker.start()
    try:
        while (buffers := ready.get()) is not None:
            yield buffers
            free.put(buffers)
    finally:
        free.put(None)
        worker.join()
    if failure:
        raise failure[0]


def _make_nnue_model() -> Any:
    """PyTorch model whose tensor order matches engine/src/nnue.h."""
    torch = dependency("torch")
    nn = importlib.import_module("torch.nn")
    functional = importlib.import_module("torch.nn.functional")

    class FeatureTransformer(torch.autograd.Function):
        """Sum of active feature rows; the weight gradient reads the feature-major CSR."""

        @staticmethod
        def forward(ctx: Any, weight: Any, indices: Any, offsets: Any, t_bags: Any, t_offsets: Any) -> Any:
            ctx.save_for_backward(t_bags, t_offsets)
            return functional.embedding_bag(indices, weight, offsets, mode="sum", include_last_offset=True)

        @staticmethod
        def backward(ctx: Any, grad: Any) -> tuple[Any, ...]:
            t_bags, t_offsets = ctx.saved_tensors
            weight_grad = functional.embedding_bag(t_bags, grad.contiguous(), t_offsets,
                                                   mode="sum", include_last_offset=True)
            return weight_grad, None, None, None, None

    def head(accumulator: Any, bias: Any, weights: Any, biases: Any, buckets: Any) -> Any:
        hidden = (accumulator + bias).clamp(0.0, 1.0).square()
        count = buckets.shape[0]
        joined = torch.cat((hidden[:count], hidden[count:]), dim=1)
        return (joined * weights[buckets]).sum(dim=1) + biases[buckets]

    compiled_head = torch.compile(head, dynamic=False)

    class Model(nn.Module):  # type: ignore[name-defined]
        def __init__(self) -> None:
            super().__init__()
            self.embedding = nn.EmbeddingBag(NNUE_FEATURES, NNUE_ACCUMULATOR,
                                             mode="sum", include_last_offset=True)
            self.feature_bias = nn.Parameter(torch.zeros(NNUE_ACCUMULATOR))
            self.output_weights = nn.Parameter(torch.zeros(NNUE_OUTPUT_BUCKETS, NNUE_ACCUMULATOR * 2))
            self.output_biases = nn.Parameter(torch.zeros(NNUE_OUTPUT_BUCKETS))
            nn.init.normal_(self.embedding.weight, mean=0.0, std=0.02)
            nn.init.normal_(self.output_weights, mean=0.0, std=0.02)

        def forward(self, indices: Any, offsets: Any, t_bags: Any, t_offsets: Any, buckets: Any) -> Any:
            accumulator = FeatureTransformer.apply(self.embedding.weight, indices, offsets, t_bags, t_offsets)
            return compiled_head(accumulator, self.feature_bias, self.output_weights,
                                 self.output_biases, buckets)

    return Model()


def export_nnue(model: Any, output: Path) -> Path:
    """Quantize a trained model into the exact little-endian engine format."""
    torch = dependency("torch")
    np = dependency("numpy")
    output = output.resolve()
    output.parent.mkdir(parents=True, exist_ok=True)
    expected_shapes = {
        "embedding.weight": (NNUE_FEATURES, NNUE_ACCUMULATOR),
        "feature_bias": (NNUE_ACCUMULATOR,),
        "output_weights": (NNUE_OUTPUT_BUCKETS, NNUE_ACCUMULATOR * 2),
        "output_biases": (NNUE_OUTPUT_BUCKETS,),
    }
    if {name: tuple(value.shape) for name, value in model.named_parameters()} != expected_shapes:
        raise ValueError("NNUE modeli 16 giriş / 16 çıkış bucket mimarisiyle uyuşmuyor.")
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

        out_w = model.output_weights.detach().cpu().numpy()
        out_w_scaled = out_w * (400.0 / math.log(10.0)) / SCALE
        out_w_i16 = quantize_i16(out_w_scaled, QB, "output_weights")

        out_b = model.output_biases.detach().cpu().numpy()
        out_b_scaled = out_b * (400.0 / math.log(10.0)) / SCALE
        out_b_i16 = quantize_i16(out_b_scaled, QAB, "output_biases")

    payload = b"".join(
        (feature_i16.tobytes(), bias_i16.tobytes(),
         out_w_i16.tobytes(), out_b_i16.tobytes())
    )
    if len(payload) != NNUE_PAYLOAD_SIZE:
        raise RuntimeError(f"NNUE payload boyutu beklenmiyor: {len(payload)} != {NNUE_PAYLOAD_SIZE}")
    data = payload + bytes(NNUE_FILE_SIZE - len(payload))
    atomic_bytes(output, data)
    return output


def load_nnue(model: Any, network: Path) -> None:
    """Load only the current 16/16 engine format, without changing its shape."""
    torch = dependency("torch")
    np = dependency("numpy")
    network = require_network(network)
    data = network.read_bytes()

    _require_nnue_size(len(data))
    offset = NNUE_FEATURES * NNUE_ACCUMULATOR * 2
    feature = np.frombuffer(data[:offset], dtype="<i2").reshape(
        NNUE_FEATURES, NNUE_ACCUMULATOR).astype(np.float32) / QA
    bias = np.frombuffer(data[offset:offset + NNUE_ACCUMULATOR * 2], dtype="<i2").astype(np.float32) / QA
    offset += NNUE_ACCUMULATOR * 2
    out_w = np.frombuffer(data[offset:offset + NNUE_OUTPUT_BUCKETS * NNUE_ACCUMULATOR * 4], dtype="<i2").astype(np.float32)
    output = out_w.reshape(NNUE_OUTPUT_BUCKETS, NNUE_ACCUMULATOR * 2) / QB * SCALE / (400.0 / math.log(10.0))
    offset += NNUE_OUTPUT_BUCKETS * NNUE_ACCUMULATOR * 4
    out_b = np.frombuffer(data[offset:offset + NNUE_OUTPUT_BUCKETS * 2], dtype="<i2").astype(np.float32)
    output_bias = out_b / QAB * SCALE / (400.0 / math.log(10.0))

    with torch.no_grad():
        model.embedding.weight.copy_(torch.from_numpy(feature))
        model.feature_bias.copy_(torch.from_numpy(bias))
        model.output_weights.copy_(torch.from_numpy(output))
        model.output_biases.copy_(torch.from_numpy(output_bias))


def _torch_device(torch: Any, requested: str) -> Any:
    if requested == "auto":
        requested = "cuda" if torch.cuda.is_available() else "mps" if torch.backends.mps.is_available() else "cpu"
    if requested == "cuda" and not torch.cuda.is_available():
        raise RuntimeError("CUDA kullanılamıyor; --device cpu veya auto seçin.")
    if requested == "mps" and not torch.backends.mps.is_available():
        raise RuntimeError("MPS kullanılamıyor; --device cpu veya auto seçin.")
    return torch.device(requested)


def load_training_records(dataset: Path, validation: float) -> tuple[Any, Any, Any]:
    """All records in RAM plus a position-keyed train/validation split.

    Positions that look the same to the network (identical board from the side
    to move's view) always land on the same side of the split.
    """
    np = dependency("numpy")
    sbin = _sbin()
    lib = sbin.load_native_lib()
    if dataset.suffix.lower() != ".sbin":
        raise ValueError(f"Eğitim verisi .sbin olmalı (Parquet için sbin_tool.py convert): {dataset}")
    records = np.fromfile(dataset, dtype=sbin.RECORD)
    if len(records) < 2:
        raise RuntimeError("Eğitim için en az iki pozisyon gerekli.")
    status = np.empty(len(records), dtype=np.uint8)
    valid = lib.sbin_validate_batch(sbin.pointer(records), len(records), sbin.pointer(status))
    if valid != len(records):
        raise ValueError(f"{dataset}: {len(records) - valid:,} geçersiz kayıt; sbin_tool.py verify --all ile kontrol edin.")
    keys = np.empty(len(records), dtype=np.uint64)
    lib.sbin_group_keys(sbin.pointer(records), len(records), sbin.pointer(keys))
    in_validation = (keys >> np.uint64(32)) < np.uint64(int(validation * 2**32))
    training_rows = np.flatnonzero(~in_validation).astype(np.int64)
    validation_rows = np.flatnonzero(in_validation).astype(np.int64)
    if not len(validation_rows) or not len(training_rows):
        raise ValueError("Doğrulama ayrımı boş kaldı; --validation oranını veya veri boyutunu artırın.")
    return records, training_rows, validation_rows


def train_nnue(dataset: Path, output: Path, *, epochs: int, batch_size: int,
               lr: float, resume: Path | None, device_name: str, seed: int,
               patience: int, validation: float, swa: bool,
               feature_dropout: float, verbose: bool) -> Path:
    """Train and export one engine-compatible network."""
    torch = dependency("torch")
    np = dependency("numpy")
    functional = importlib.import_module("torch.nn.functional")
    if epochs < 1 or batch_size < 1 or not math.isfinite(lr) or lr <= 0 or patience < 1:
        raise ValueError("epochs, batch-size, lr ve patience pozitif olmalı.")
    if not 0.0 < validation < 1.0:
        raise ValueError("validation 0 ile 1 arasında olmalı.")
    if not 0.0 <= feature_dropout < 1.0:
        raise ValueError("feature-dropout 0 ile 1 arasında olmalı.")
    dataset = require_file(dataset, "Eğitim Veri Seti")
    output = output.resolve()
    torch.manual_seed(seed)
    device = _torch_device(torch, device_name)
    lib = _sbin().load_native_lib()
    records, training_rows, validation_rows = load_training_records(dataset, validation)
    steps_per_epoch = len(training_rows) // batch_size
    if not steps_per_epoch:
        raise ValueError(f"Eğitim satırı ({len(training_rows):,}) batch-size değerinden az.")
    total_steps = epochs * steps_per_epoch
    if verbose:
        print(f"Device={device}; train={len(training_rows):,}; validation={len(validation_rows):,}; "
              f"adım/epoch={steps_per_epoch:,}", flush=True)
    model = _make_nnue_model().to(device)
    optimizer = torch.optim.AdamW(model.parameters(), lr=lr, weight_decay=1e-5, fused=True)
    start_epoch = 0
    best_validation = float("inf")
    best_model = None
    stale = 0
    swa_weights: dict[str, Any] | None = None
    swa_count = 0
    swa_start_epoch = max(1, epochs // 2)
    history: list[dict[str, Any]] = []
    if resume and resume.suffix.lower() == ".nnue":
        load_nnue(model, resume)
    elif resume:
        saved = torch.load(str(require_file(resume, "NNUE checkpoint")), map_location="cpu", weights_only=True)
        required_fields = {
            "model", "optimizer", "epoch", "best_validation", "best_model", "dataset_identity",
            "split_version", "validation", "seed", "batch_size", "stale_epochs", "swa_weights",
            "swa_count", "history", "torch_rng_state",
        }
        missing = required_fields - saved.keys()
        if missing:
            raise ValueError(f"Checkpoint eksik alanlar içeriyor: {', '.join(sorted(missing))}; "
                             "fine-tune için --resume model.nnue kullanın.")
        expected = {"dataset_identity": file_identity(dataset), "split_version": SPLIT_VERSION,
                    "validation": validation, "seed": seed, "batch_size": batch_size}
        changed = [name for name, value in expected.items() if saved[name] != value]
        if changed:
            raise ValueError(f"Checkpoint farklı ayarlarla üretilmiş ({', '.join(changed)}); "
                             "yeni ayarlar için --resume model.nnue kullanın.")
        model.load_state_dict(saved["model"])
        optimizer.load_state_dict(saved["optimizer"])
        start_epoch = int(saved["epoch"])
        stale = int(saved["stale_epochs"])
        best_model = saved["best_model"]
        if best_model is not None:
            best_validation = float(saved["best_validation"])
        swa_weights = saved["swa_weights"]
        swa_count = int(saved["swa_count"])
        history = list(saved["history"])
        torch.set_rng_state(saved["torch_rng_state"])
    if start_epoch >= epochs:
        raise ValueError("--epochs checkpoint epoch değerinden büyük olmalı.")
    output.parent.mkdir(parents=True, exist_ok=True)
    if best_model is not None:
        restored_best = _make_nnue_model()
        restored_best.load_state_dict(best_model)
        export_nnue(restored_best, output)

    def validation_loss(network: Any) -> float:
        network.eval()
        total = torch.zeros((), device=device)
        with torch.no_grad():
            for batch in batch_stream(lib, records, validation_rows, batch_size, 0.0, seed, drop_last=False):
                *inputs, targets = batch.tensors(torch, device)
                losses = functional.binary_cross_entropy_with_logits(network(*inputs), targets, reduction="none")
                total += losses[:batch.valid].sum()
        return float(total.item()) / len(validation_rows)

    for epoch in range(start_epoch, epochs):
        started = time.perf_counter()
        model.train()
        order = training_rows[np.random.default_rng(seed + epoch).permutation(len(training_rows))]
        loss_sum = torch.zeros((), device=device)
        step = epoch * steps_per_epoch
        for number, batch in enumerate(batch_stream(lib, records, order[:steps_per_epoch * batch_size], batch_size,
                                                    feature_dropout, (seed + epoch) * steps_per_epoch,
                                                    drop_last=True)):
            # Per-step cosine decay from lr to zero over the whole run.
            for group in optimizer.param_groups:
                group["lr"] = lr * 0.5 * (1.0 + math.cos(math.pi * (step + number) / total_steps))
            *inputs, targets = batch.tensors(torch, device)
            loss = functional.binary_cross_entropy_with_logits(model(*inputs), targets)
            optimizer.zero_grad(set_to_none=True)
            loss.backward()
            optimizer.step()
            loss_sum += loss.detach()
            if verbose and (number + 1) % max(1, steps_per_epoch // 20) == 0:
                seconds = time.perf_counter() - started
                print(f"  [Epoch {epoch + 1}/{epochs}] {number + 1:,}/{steps_per_epoch:,} "
                      f"loss={float(loss_sum.item()) / (number + 1):.6f} "
                      f"{(number + 1) * batch_size / seconds:,.0f} pos/s", flush=True)
        train_loss = float(loss_sum.item()) / steps_per_epoch
        epoch_validation = validation_loss(model)
        if not math.isfinite(train_loss + epoch_validation):
            raise RuntimeError("NNUE eğitimi sonlu olmayan kayıp üretti.")
        if epoch_validation < best_validation:
            best_validation = epoch_validation
            best_model = {name: value.detach().cpu().clone() for name, value in model.state_dict().items()}
            stale = 0
            export_nnue(model, output)
        else:
            stale += 1
        if swa and epoch + 1 >= swa_start_epoch:
            swa_count += 1
            if swa_weights is None:
                swa_weights = {name: value.detach().cpu().clone().float() for name, value in model.state_dict().items()}
            else:
                for name, value in model.state_dict().items():
                    swa_weights[name] += value.detach().cpu().float()
        elapsed = time.perf_counter() - started
        history.append({"epoch": epoch + 1, "train_loss": train_loss, "validation_loss": epoch_validation,
                        "seconds": elapsed, "positions_per_second": steps_per_epoch * batch_size / elapsed})
        checkpoint = {
            "model": model.state_dict(), "optimizer": optimizer.state_dict(), "epoch": epoch + 1,
            "best_validation": best_validation, "best_model": best_model, "dataset": str(dataset),
            "dataset_identity": file_identity(dataset), "split_version": SPLIT_VERSION, "seed": seed,
            "validation": validation, "batch_size": batch_size, "stale_epochs": stale,
            "swa_weights": swa_weights, "swa_count": swa_count, "history": history,
            "torch_rng_state": torch.get_rng_state(),
        }
        checkpoint_path = output.with_suffix(".pt")
        temporary = checkpoint_path.with_name(f".{checkpoint_path.name}.{uuid.uuid4().hex}.tmp")
        try:
            torch.save(checkpoint, temporary)
            temporary.replace(checkpoint_path)
        finally:
            temporary.unlink(missing_ok=True)
        atomic_json(output.with_suffix(".metrics.json"), history)
        print(f"Epoch {epoch + 1}/{epochs} train={train_loss:.6f} validation={epoch_validation:.6f} "
              f"{history[-1]['positions_per_second']:,.0f} pos/s", flush=True)
        if stale >= patience:
            print("Erken durdurma: doğrulama kaybı iyileşmedi.", flush=True)
            break
    if swa and swa_count > 1:
        swa_model = _make_nnue_model().to(device)
        swa_model.load_state_dict({name: (value / swa_count).to(model.state_dict()[name].dtype)
                                   for name, value in swa_weights.items()})
        swa_loss = validation_loss(swa_model)
        export_nnue(swa_model, output.with_name(f"{output.stem}-swa{output.suffix}"))
        if swa_loss < best_validation:
            export_nnue(swa_model, output)
            best_validation = swa_loss
        print(f"SWA ({swa_count} epoch): validation={swa_loss:.6f}"
              f"{' (birincil ağ)' if swa_loss == best_validation else ''}", flush=True)
    print(f"NNUE: {output} (validation={best_validation:.6f})", flush=True)
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


STALLION_MATCH_OPTIONS = ("option.UseOpeningBook=false", "option.UseSyzygy=false",
                          "option.Threads=1", "option.Hash=64", "option.MultiPV=1")
CUTECHESS_FAILURE = re.compile(
    r"loses on time|disconnect|illegal move|illegal game|forfeit|connection stall|"
    r"stalled connection|crash|segmentation|load failed|no move|aborted|error", re.I)


def _parse_score(line: str) -> tuple[int, int, int]:
    match = re.search(r"Score of Candidate vs \S+:\s*(\d+)\s*-\s*(\d+)\s*-\s*(\d+)", line)
    if not match:
        raise RuntimeError("cutechess-cli skor satırı üretmedi.")
    return int(match.group(1)), int(match.group(2)), int(match.group(3))


def _prepare_tournament(args: argparse.Namespace, kind: str,
                        networks: dict[str, Path]) -> tuple[dict[str, Path], list[str], dict[str, str], Path]:
    """Freeze inputs, build one embedded engine per net and the shared cutechess arguments."""
    if args.games < 2 or args.games % 2:
        raise ValueError("games pozitif ve çift olmalı.")
    if args.concurrency < 1:
        raise ValueError("concurrency pozitif olmalı.")
    if not args.engine_src.is_dir():
        raise RuntimeError(f"Motor kaynağı bulunamadı: {args.engine_src}")
    if args.pgnout and args.pgnout.exists() and args.pgnout.stat().st_size:
        raise ValueError(f"PGN zaten dolu; yeni çıktı yolu kullanın: {args.pgnout}")
    artifact = args.json_out or args.pgnout or ROOT / "runs" / f"{kind}-{datetime.now():%Y%m%d-%H%M%S-%f}.json"
    snapshot = artifact.with_suffix(".inputs")
    snapshot.mkdir(parents=True, exist_ok=False)
    hashes: dict[str, str] = {}

    def freeze(path: Path, name: str) -> Path:
        target = snapshot / name
        shutil.copy2(path, target)
        hashes[name] = file_sha256(target)
        return target

    cache_dir = ROOT / "runs" / "engine-cache"
    cache_dir.mkdir(parents=True, exist_ok=True)
    engines: dict[str, Path] = {}
    for name, network in networks.items():
        frozen = freeze(network, f"{name}.nnue")
        binary = _build_embedded_engine(args.engine_src, frozen, cache_dir)
        verify_embedded_engine(binary)
        engines[name] = freeze(binary, f"{name}-engine{binary.suffix}")
    common = [
        "-each", "proto=uci", f"tc={args.tc}",
        "-rounds", str(args.games // 2), "-games", "2", "-repeat", "-recover",
        "-concurrency", str(args.concurrency),
        "-draw", "movenumber=40", "movecount=8", "score=10",
        "-resign", "movecount=3", "score=600",
        "-srand", str(args.seed),
    ]
    if args.book:
        book = freeze(require_file(args.book, "Açılış kitabı"), "openings.epd")
        common.extend(["-openings", f"file={book}", "format=epd", "order=random"])
    if args.pgnout:
        args.pgnout.parent.mkdir(parents=True, exist_ok=True)
        common.extend(["-pgnout", str(args.pgnout)])
    return engines, common, hashes, artifact


def _run_cutechess(command: list[str], artifact: Path, verbose: bool) -> tuple[str, float, str | None]:
    """Run cutechess-cli; any engine or clock failure invalidates the result."""
    process = subprocess.Popen(command, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True, bufsize=1)
    lines: list[str] = []
    failures: list[str] = []
    score_line = ""
    elo_diff = 0.0
    sprt_decision: str | None = None
    returncode = -1
    try:
        assert process.stdout is not None
        for line in process.stdout:
            if verbose:
                print(line, end="", flush=True)
            lines.append(line)
            if CUTECHESS_FAILURE.search(line):
                failures.append(line.strip())
            if line.startswith("Score of Candidate vs "):
                score_line = line.strip()
            if "Elo difference:" in line:
                found = re.search(r"Elo difference:\s*([+-]?(?:\d+(?:\.\d+)?|inf))", line)
                if found:
                    value = found.group(1)
                    elo_diff = 999.0 if value in ("inf", "+inf") else -999.0 if value == "-inf" else float(value)
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
        if verbose or returncode or failures:
            atomic_text(artifact.with_suffix(".log"), "".join(lines))
    if returncode:
        raise RuntimeError(f"cutechess-cli başarısız oldu (exit {returncode}).")
    if failures:
        raise RuntimeError("Motor/zaman hatası var; sonuç geçersiz: " + "; ".join(failures[:5]))
    return score_line, elo_diff, sprt_decision


def _engine_source_fingerprint(engine_src: Path) -> str:
    """Hash the engine sources so cached match binaries stay valid."""
    digest = hashlib.sha256()
    files = sorted((engine_src / "src").glob("*.cpp")) + sorted((engine_src / "src").glob("*.h"))
    files += sorted((engine_src / "fathom" / "src").glob("*.cpp")) + sorted((engine_src / "fathom" / "src").glob("*.h"))
    files += [engine_src / name for name in ("net_embed.S", "Makefile.rules", "Makefile.mac",
                                             "Makefile.linux", "Makefile.windows", "Makefile.android")]
    for path in files:
        if path.is_file():
            digest.update(path.name.encode())
            digest.update(path.read_bytes())
    return digest.hexdigest()[:16]


def _build_embedded_engine(engine_src: Path, network: Path, cache_dir: Path) -> Path:
    """Build (or reuse a cached) engine binary with the given net embedded."""
    engine_src = engine_src.resolve()
    network = require_network(network, "NNUE")
    for needed in ("src/stallion.cpp", "net_embed.S", "Makefile.rules"):
        if not (engine_src / needed).is_file():
            raise RuntimeError(f"Motor kaynağı eksik: {engine_src / needed}")
    key = f"{_engine_source_fingerprint(engine_src)}_{file_sha256(network)[:16]}"
    binary_name = {"darwin": "stallion_eas_mac", "win32": "stallion_eas_windows.exe"}.get(
        sys.platform, "stallion_eas_linux")
    build_dir = (cache_dir / key).resolve()
    binary = build_dir / binary_name
    marker = build_dir / "complete.json"
    if binary.is_file() and marker.is_file():
        return binary
    with advisory_lock(cache_dir / f"{key}.build.lock"):
        if binary.is_file() and marker.is_file():
            return binary
        if build_dir.exists():
            shutil.rmtree(build_dir)
        build_dir.mkdir(parents=True)
        for name in ("src", "fathom"):
            shutil.copytree(engine_src / name, build_dir / name)
        for name in ("Makefile", "Makefile.rules", "Makefile.mac", "Makefile.linux",
                     "Makefile.windows", "Makefile.android", "net_embed.S"):
            shutil.copy2(engine_src / name, build_dir / name)
        nets_dir = build_dir / "nets"
        nets_dir.mkdir()
        shutil.copy2(network, nets_dir / "stallion.nnue")
        run_checked(["make", "-C", str(build_dir)])
        if not binary.is_file():
            raise RuntimeError(f"Motor derlenemedi: {build_dir}")
        atomic_json(marker, {"network_sha256": file_sha256(network), "source": key})
    return binary


def verify_embedded_engine(binary: Path, timeout: float = 60.0) -> None:
    """Smoke-test a match binary: handshake plus a shallow fixed-depth search.

    The whole handshake/search read is bounded by `timeout`; a silent engine
    fails instead of hanging forever.
    """
    import queue
    import threading
    proc = subprocess.Popen(
        [str(binary)],
        stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
        text=True
    )
    lines: queue.Queue[str | None] = queue.Queue()

    def _drain() -> None:
        assert proc.stdout is not None
        try:
            for line in proc.stdout:
                lines.put(line)
        finally:
            lines.put(None)

    reader = threading.Thread(target=_drain, daemon=True)
    reader.start()
    stdout_lines = []
    deadline = time.monotonic() + timeout
    try:
        assert proc.stdin is not None
        proc.stdin.write("uci\nisready\nposition startpos\ngo depth 3\n")
        proc.stdin.flush()
        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                raise RuntimeError(f"Motor duman testi zaman aşımına uğradı ({timeout:.0f} sn): {binary}")
            try:
                line = lines.get(timeout=remaining)
            except queue.Empty:
                raise RuntimeError(f"Motor duman testi zaman aşımına uğradı ({timeout:.0f} sn): {binary}")
            if line is None:
                break
            stdout_lines.append(line)
            if "bestmove" in line:
                break
        proc.stdin.write("quit\n")
        proc.stdin.flush()
    finally:
        try:
            proc.wait(timeout=10)
        except subprocess.TimeoutExpired:
            proc.kill()
            proc.wait()
    out = "".join(stdout_lines)
    if "uciok" not in out or "readyok" not in out or "bestmove" not in out:
        raise RuntimeError(f"Motor duman testi geçemedi: {binary}\n{out}")


def run_match(args: argparse.Namespace, phase: str | None = None) -> MatchResult:
    phase = phase or args.phase
    if args.promote and not args.sprt:
        raise ValueError("Otomatik terfi için --sprt gerekli.")
    candidate = require_network(args.candidate, "Aday NNUE")
    baseline = require_network(args.baseline, "Taban NNUE")
    engines, common, hashes, artifact = _prepare_tournament(
        args, "match", {"candidate": candidate, "baseline": baseline})
    command = [
        str(require_file(args.cutechess, "cutechess-cli")),
        "-engine", f"cmd={engines['candidate']}", "name=Candidate", *STALLION_MATCH_OPTIONS,
        "-engine", f"cmd={engines['baseline']}", "name=Baseline", *STALLION_MATCH_OPTIONS,
        *common,
    ]
    if args.sprt:
        command.extend(["-sprt", "elo0=0", "elo1=15", "alpha=0.05", "beta=0.05"])
    score_line, elo_diff, sprt_decision = _run_cutechess(command, artifact, args.verbose)
    wins, losses, draws = _parse_score(score_line)
    completed = wins + losses + draws
    if not completed or completed > args.games or (completed != args.games and sprt_decision is None):
        raise RuntimeError(f"Eksik maç: {completed}/{args.games} oyun tamamlandı.")
    status = (sprt_decision or "INCONCLUSIVE") if args.sprt else "ESTIMATE_ONLY"
    result = MatchResult(
        score_line=score_line, wins=wins, losses=losses, draws=draws, elo_diff=elo_diff,
        sprt_decision=sprt_decision, is_winner=args.sprt and sprt_decision == "PASSED",
        returncode=0, command=command, input_hashes=hashes, status=status,
    )
    atomic_json(args.json_out or artifact.with_suffix(".json"), result.as_dict(candidate, baseline, args.pgnout))
    print(f"Maç sonucu: {wins}-{losses}-{draws}, Elo {elo_diff:+.1f}; {status}", flush=True)
    return result


def run_gauntlet(args: argparse.Namespace) -> bool:
    """Quick foreign-anchor gate: candidate vs Stockfish at fixed UCI_Elo.

    Self-matches cannot see shared blind spots; this catches collapses
    (like a -400 Elo net) in ~20 games before a full match. PASS means
    score% >= min_score, not an Elo claim — precision still needs SPRT.
    """
    candidate = require_network(args.candidate, "Aday NNUE")
    stockfish = require_file(args.stockfish, "Stockfish")
    if not 100 <= args.anchor_elo <= 4000:
        raise ValueError("anchor-elo 100 ile 4000 arasında olmalı.")
    if not 0.0 < args.min_score < 1.0:
        raise ValueError("min-score 0 ile 1 arasında olmalı.")
    anchor_name = f"SF{args.anchor_elo}"
    engines, common, hashes, artifact = _prepare_tournament(args, "gauntlet", {"candidate": candidate})
    command = [
        str(require_file(args.cutechess, "cutechess-cli")),
        "-engine", f"cmd={engines['candidate']}", "name=Candidate", *STALLION_MATCH_OPTIONS,
        "-engine", f"cmd={stockfish}", f"name={anchor_name}", "option.Threads=1", "option.Hash=64",
        "option.UCI_LimitStrength=true", f"option.UCI_Elo={args.anchor_elo}",
        *common,
    ]
    score_line, elo_diff, _ = _run_cutechess(command, artifact, args.verbose)
    wins, losses, draws = _parse_score(score_line)
    completed = wins + losses + draws
    if completed != args.games:
        raise RuntimeError(f"Eksik gauntlet: {completed}/{args.games} oyun tamamlandı.")
    score_pct = (wins + draws / 2) / completed
    passed = score_pct >= args.min_score
    record = {
        "time": datetime.now().isoformat(timespec="seconds"), "candidate": str(candidate),
        "candidate_sha256": hashes["candidate.nnue"], "anchor": anchor_name, "games": args.games,
        "wins": wins, "losses": losses, "draws": draws, "score_pct": round(score_pct, 4),
        "elo_diff": elo_diff, "min_score": args.min_score, "verdict": "PASS" if passed else "FAIL",
        "input_hashes": hashes, "pgn": str(args.pgnout) if args.pgnout else None,
    }
    atomic_json(args.json_out or artifact.with_suffix(".json"), record)
    with (ROOT / "runs" / "gauntlet.jsonl").open("a", encoding="utf-8") as ledger:
        ledger.write(json.dumps(record, ensure_ascii=False) + "\n")
    print(f"Gauntlet sonucu: {wins}-{losses}-{draws} ({score_pct:.1%}), Elo {elo_diff:+.1f} vs {anchor_name}; "
          f"{'PASS' if passed else 'FAIL'}", flush=True)
    return passed


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
    assets = args.assets
    binary = require_file(assets / "pgn-extract", "pgn-extract")
    output = (args.output or ROOT / "statistics_EAS_ratinglist.txt").resolve()
    jsonout = args.json_out
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
    stats = report.get("stats", {})
    cand_eas = stats.get("Candidate")
    base_eas = stats.get("Baseline")
    if cand_eas is not None and base_eas is not None:
        print(f"EAS: Candidate={cand_eas}, Baseline={base_eas}, fark={cand_eas - base_eas:+d}; rapor={output}", flush=True)
    else:
        print(f"EAS raporu: {output}", flush=True)
    return report


def run_sacrifices(args: argparse.Namespace) -> Path:
    pgn = require_file(args.pgn, "PGN")
    assets = args.assets
    binary = require_file(assets / "pgn-extract", "pgn-extract")
    if args.max_moves < 1:
        raise ValueError("max-moves pozitif olmalı.")
    output = (args.output or ROOT / "games_with_sacrifices.pgn").resolve()
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
    print(f"[BAŞARILI] SGS çıktısı: {output} ({pgn_game_count(output):,} oyun)", flush=True)
    return output


def _iws_annotate(source: Path, tag_file: Path, output: Path) -> None:
    """Insert the IWS Annotator tag into every game (replaces tagCreate.exe)."""
    tag_line = tag_file.read_text(encoding="latin-1", errors="ignore").strip()
    if not tag_line:
        raise ValueError(f"Boş IWS etiket dosyası: {tag_file}")
    games: list[str] = []
    current: list[str] = []
    in_tags = True
    for line in source.read_text(encoding="latin-1", errors="ignore").splitlines():
        if line.lstrip().startswith("[") and not in_tags and current:
            games.append("\n".join(current))
            current = []
            in_tags = True
        current.append(line)
        if line.strip() and not line.lstrip().startswith("["):
            in_tags = False
    if current:
        games.append("\n".join(current))
    annotated: list[str] = []
    for game in games:
        if "[White " not in game:
            continue
        lines = [line for line in game.splitlines()
                 if not line.lstrip().startswith("[Annotator ")]
        insert_at = next((i for i, line in enumerate(lines)
                          if line.strip() and not line.lstrip().startswith("[")), len(lines))
        lines.insert(insert_at, tag_line)
        annotated.append("\n".join(lines).strip())
    output.write_text("\n\n".join(annotated) + ("\n" if annotated else ""), encoding="latin-1")


def run_iwins(args: argparse.Namespace) -> Path:
    """Port of the IWS-Tool V4.1 batch pipeline (interesting wins filter)."""
    pgn = require_file(args.pgn, "PGN")
    assets = args.assets
    binary = require_file(assets / "pgn-extract", "pgn-extract")
    movelimit = int(args.max_moves)
    movelimit = 100 if movelimit < 30 else min(movelimit, 250)
    player = (args.player or "").strip()
    output = (args.output or ROOT / "interesting_wins.pgn").resolve()
    very_output = output.with_name(f"{output.stem}-very{output.suffix or '.pgn'}")

    def sort_by_length(source: Path, target: Path, work: Path) -> None:
        buckets = [(None, 19)] + [(low, low + 9) for low in range(20, 120, 10)] + [(120, None)]
        parts: list[Path] = []
        for index, (low, high) in enumerate(buckets):
            part = work / f"len-{index}.pgn"
            cmd = ["--quiet"]
            if high is not None:
                cmd.append(f"-bu{high}")
            if low is not None:
                cmd.append(f"-bl{low}")
            _pgn_extract(binary, [*cmd, source, "--output", part], work)
            parts.append(part)
        combine_text_files(parts, target)

    with tempfile.TemporaryDirectory(prefix="stallion-iws-") as temporary_name:
        work = Path(temporary_name)
        newsource = work / "newsource.pgn"
        if player:
            white = work / "player-w.pgn"
            black = work / "player-b.pgn"
            _pgn_extract(binary, ["--quiet", f"-Tw{player}", f"-bu{movelimit}",
                                  "--fixresulttags", "-Tr1-0", "-C", "-N", "-V",
                                  pgn, "--output", white], work)
            _pgn_extract(binary, ["--quiet", f"-Tb{player}", f"-bu{movelimit}",
                                  "--fixresulttags", "-Tr0-1", "-C", "-N", "-V",
                                  pgn, "--output", black], work)
            combine_text_files((white, black), newsource)
        else:
            _pgn_extract(binary, ["--quiet", f"-bu{movelimit}", "--fixresulttags",
                                  "-Tr1-0", "-Tr0-1", "-C", "-N", "-V",
                                  pgn, "--output", newsource], work)
        white_wins = work / "whitewins.pgn"
        black_wins = work / "blackwins.pgn"
        _pgn_extract(binary, ["--quiet", "-Tr1-0", newsource, "--output", white_wins], work)
        _pgn_extract(binary, ["--quiet", "-Tr0-1", newsource, "--output", black_wins], work)

        results: dict[int, Path] = {}
        for level in (1, 2, 3, 4, 5, 9):
            tag = f"{level}_pawnsac" if level != 9 else "queensac"
            white = work / f"results-w{level}.pgn"
            black = work / f"results-b{level}.pgn"
            merged = work / f"results_opt{level}.pgn"
            _pgn_extract(binary, ["--quiet", "-y" + str(require_file(assets / f"{tag}_white", "IWS pattern")),
                                  white_wins, "--output", white], work)
            _pgn_extract(binary, ["--quiet", "-y" + str(require_file(assets / f"{tag}_black", "IWS pattern")),
                                  black_wins, "--output", black], work)
            combine_text_files((white, black), merged)
            white_wins.write_bytes(white.read_bytes() if white.exists() else b"")
            black_wins.write_bytes(black.read_bytes() if black.exists() else b"")
            results[level] = merged

        unique: dict[int, Path] = {}
        previous: Path | None = None
        for level in (9, 5, 4, 3, 2, 1):
            target = work / f"unique_opt{level}.pgn"
            if previous is None:
                _pgn_extract(binary, ["--quiet", "-D", results[level], "--output", target], work)
            else:
                _pgn_extract(binary, ["--quiet", "-c" + str(previous), "-D",
                                      "-o" + str(target), results[level]], work)
            previous = results[level]
            unique[level] = target

        found: list[Path] = []
        found_top: list[Path] = []
        counts: dict[str, int] = {}
        for level in (9, 5, 4, 3, 2, 1):
            name = f"{level}sac"
            sorted_pgn = work / f"sorted-{level}.pgn"
            if pgn_game_count(unique[level]) > 0:
                sort_by_length(unique[level], sorted_pgn, work)
            else:
                sorted_pgn.write_text("", encoding="latin-1")
            annotated = work / f"anno-{level}.pgn"
            _iws_annotate(sorted_pgn, require_file(assets / f"iws_anno_{name}", "IWS etiket"),
                          annotated)
            found.append(annotated)
            if level != 1:
                found_top.append(annotated)
            counts[name] = pgn_game_count(annotated)

        reached_endgame = work / "reached-endgame.pgn"
        _pgn_extract(binary, ["--quiet", "-z" + str(require_file(assets / "no_endgame", "IWS pattern")),
                              newsource, "--output", reached_endgame], work)
        no_endgame = work / "no_endgame_wins.pgn"
        _pgn_extract(binary, ["--quiet", "-c" + str(reached_endgame), "-D",
                              "-o" + str(no_endgame), newsource], work)
        if pgn_game_count(no_endgame) > 0:
            no_endgame_sorted = work / "no_endgame_sorted.pgn"
            sort_by_length(no_endgame, no_endgame_sorted, work)
        else:
            no_endgame_sorted = no_endgame
        no_endgame_anno = work / "anno-noendgame.pgn"
        _iws_annotate(no_endgame_sorted, require_file(assets / "iws_anno_before_endgame", "IWS etiket"),
                      no_endgame_anno)
        found.append(no_endgame_anno)
        found_top.append(no_endgame_anno)
        counts["before_endgame"] = pgn_game_count(no_endgame_anno)

        imbalance = work / "imbalance.pgn"
        _pgn_extract(binary, ["--quiet", "-z" + str(require_file(assets / "imbalance", "IWS pattern")),
                              newsource, "--output", imbalance], work)
        if pgn_game_count(imbalance) > 0:
            imbalance_sorted = work / "imbalance_sorted.pgn"
            sort_by_length(imbalance, imbalance_sorted, work)
        else:
            imbalance_sorted = imbalance
        imbalance_anno = work / "anno-imbalance.pgn"
        _iws_annotate(imbalance_sorted, require_file(assets / "iws_anno_material_imbalance", "IWS etiket"),
                      imbalance_anno)
        found.append(imbalance_anno)
        counts["imbalance"] = pgn_game_count(imbalance_anno)

        merged_all = work / "foundgames.pgn"
        merged_top = work / "foundtopgames.pgn"
        combine_text_files(found, merged_all)
        combine_text_files(found_top, merged_top)
        final_all = work / "interesting.pgn"
        final_top = work / "very.pgn"
        _pgn_extract(binary, ["--quiet", "-D", merged_all, "--output", final_all], work)
        _pgn_extract(binary, ["--quiet", "-D", merged_top, "--output", final_top], work)
        output.parent.mkdir(parents=True, exist_ok=True)
        atomic_bytes(output, final_all.read_bytes() if final_all.exists() else b"")
        atomic_bytes(very_output, final_top.read_bytes() if final_top.exists() else b"")
    counts["interesting"] = pgn_game_count(output)
    counts["very_interesting"] = pgn_game_count(very_output)
    atomic_json(output.with_suffix(".iwins.json"), {
        "pgn": file_identity(pgn), "movelimit": movelimit, "player": player or None,
        "interesting": str(output), "very_interesting": str(very_output), **counts,
    })
    print(f"[BAŞARILI] IWS: {output} ({counts['interesting']:,} oyun), "
          f"{very_output} ({counts['very_interesting']:,} oyun)", flush=True)
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

    print(f"[TERFİ] {candidate.name} -> {baseline}; yedek={backup.name}. "
          f"Eski netle derlenmiş motor ikilileri bayatladı; yeniden derleyin.")


def maybe_promote(args: argparse.Namespace, phase: str, result: MatchResult,
                  candidate: Path, baseline: Path, eas_report: dict | None = None) -> bool:
    """Promote only after an accepted SPRT (and an EAS gain for aggressive nets)."""
    if not args.promote:
        return False
    stats = (eas_report or {}).get("stats", {})
    cand_eas, base_eas = stats.get("Candidate"), stats.get("Baseline")
    eas_gain = cand_eas - base_eas if cand_eas is not None and base_eas is not None else None
    eas_ok = phase in ("base", "nnue") or (eas_gain is not None and eas_gain >= args.min_eas_gain)
    if result.status != "PASSED" or not result.is_winner or not eas_ok:
        print(f"[TERFİ YOK] SPRT={result.status}; EAS farkı={eas_gain}; aday={candidate}", flush=True)
        return False
    promote_network(candidate, baseline,
                    expected_candidate=result.input_hashes["candidate.nnue"],
                    expected_baseline=result.input_hashes["baseline.nnue"])
    return True


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
        "iws": "iwins",
        "interesting": "iwins",
        "selfplay": "datagen",
        "datagen": "datagen",
        "everything": "all",
    }
    order = ["prepare", "extract", "datagen", "train", "match", "eas", "sacrifices", "iwins", "promote"]
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
    run_dir = args.run_dir or ROOT / "runs" / datetime.now().strftime("%Y%m%d-%H%M%S")
    run_dir.mkdir(parents=True, exist_ok=True)
    for phase in phases:
        baseline = args.baseline if args.baseline else NETS / "stallion.nnue"
        if any(step in steps for step in ("train", "match", "promote")):
            require_network(baseline, f"{phase} taban NNUE")
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

            if args.verbose:
                print(f"[{phase.upper()} {iteration}] adımlar={','.join(steps)}", flush=True)
            for step in steps:
                if step == "prepare":
                    dataset_path = prepare_dataset(_namespace_copy(
                        args, phase=phase, output=dataset_path,
                    ))
                elif step == "datagen":
                    dataset_path = run_datagen(_namespace_copy(
                        args, phase=phase, output=dataset_path,
                    ))
                elif step == "extract":
                    if not dataset:
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
                        swa=args.swa, feature_dropout=args.feature_dropout,
                        verbose=args.verbose,
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
                        games=args.games_base if phase == "base" else args.games_aggressive,
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
                elif step == "iwins":
                    iwins_input = pgnout if pgnout.is_file() else args.pgn
                    if iwins_input is None:
                        raise RuntimeError(
                            "iwins adımı için match PGN'i yok; match çalıştırın veya --pgn verin."
                        )
                    run_iwins(_namespace_copy(
                        args, pgn=iwins_input, output=prefix.with_suffix(".iwins.pgn")
                    ))
                elif step == "promote":
                    if match_result is None:
                        raise RuntimeError("promote adımı match adımından sonra gelmeli.")
                    maybe_promote(args, phase, match_result, candidate, baseline, eas_report)
    print(f"Pipeline tamamlandı: {run_dir}", flush=True)
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
        choices=["pipeline", "extract", "prepare", "datagen", "train", "match", "gauntlet", "eas", "sacrifices", "mine", "label", "iwins"],
    )
    parser.add_argument("--phase", "--mode", "--net-type", dest="phase",
                        choices=["nnue", "all", "base", "aggressive"], default="nnue",
                        help=argparse.SUPPRESS)
    parser.add_argument("--steps", default="all",
                        help="pipeline adımları: extract,train,match,eas,sacrifices,iwins,promote,all")
    parser.add_argument("--run-dir", default=None)
    parser.add_argument("--output", default=None)
    parser.add_argument("--dataset", default=None)
    parser.add_argument("--aggressive-dataset", default=None)

    parser.add_argument("--source", "--sbin", dest="source", default=str(DEFAULT_EVAL),
                        help="Eval SBIN veri kaynağı (varsayılan: training/data/evals.sbin)")
    parser.add_argument("--puzzles", default=str(DEFAULT_PUZZLES),
                        help="Feda bulmaca SBIN kaynağı (varsayılan: training/data/puzzle_sacrifices.sbin)")
    parser.add_argument("--target", type=int, default=None)
    parser.add_argument("--base-target", "--base-positions", dest="base_target",
                        type=int, default=500_000)
    parser.add_argument("--aggressive-target", "--agg-positions", dest="aggressive_target",
                        type=int, default=500_000)
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
    parser.add_argument("--wdl-lambda", type=float, default=0.0,
                        help="WDL hedef karışım oranı (0.0: saf CP dönüşümü, 1.0: kayıttaki WDL; varsayılan: 0.0). Kayıttaki WDL oyun sonucu veya yumuşak eval etiketi olabilir.")
    parser.add_argument("--base-filters", type=_filter_list, default="mate,check,tactical",
                        help="base extract/mine atlanacak konumlar: mate,check,tactical (virgülle; boş: filtre yok)")
    parser.add_argument("--aggressive-filters", type=_filter_list, default="",
                        help="aggressive extract/prepare atlanacak konumlar (varsayılan: yok; bulmacalar taktiktir)")
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

    parser.add_argument("--engine", default=None, help="Self-play/datagen motor binary")
    parser.add_argument("--engine-src", default=None, help="Match için motor kaynak dizini (gömülü derleme)")
    parser.add_argument("--assets", default=str(DEFAULT_ASSETS),
                        help="pgn-extract ve pattern dosyalarının bulunduğu klasör")
    parser.add_argument("--cutechess", default=str(ROOT / "cutechess-cli"))
    parser.add_argument("--book", default=str(DEFAULT_BOOK),
                        help="EPD açılış kitabı; none: kitap yok (datagen rastgele açılış oynar)")
    parser.add_argument("--candidate", default=None)
    parser.add_argument("--baseline", default=None)
    parser.add_argument("--games", type=int, default=100)
    parser.add_argument("--games-base", type=int, default=None)
    parser.add_argument("--games-aggressive", "--games-agg", dest="games_aggressive",
                        type=int, default=None)
    parser.add_argument("--concurrency", type=int, default=4)
    parser.add_argument("--tc", default="5+0.05")
    parser.add_argument("--stockfish", default=str(ROOT / "stockfish"),
                        help="Gauntlet rakibi Stockfish binary")
    parser.add_argument("--anchor-elo", dest="anchor_elo", type=int, default=2400,
                        help="Gauntlet Stockfish UCI_Elo çapası (varsayılan: 2400)")
    parser.add_argument("--min-score", dest="min_score", type=float, default=0.25,
                        help="Gauntlet geçme skoru (varsayılan: 0.25)")
    parser.add_argument("--aggressive-source", choices=["simple", "puzzles"], default="simple",
                        help="aggressive extract kaynağı (puzzles yalnızca puzzle havuzunu kullanır)")
    parser.add_argument("--depth", type=int, default=None,
                        help="datagen/label arama derinliği sınırı")
    parser.add_argument("--nodes", type=int, default=None,
                        help="datagen/label hamle başına düğüm sınırı")
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
    parser.add_argument("--player", default=None,
                        help="iwins için yalnızca bu motor/oyuncunun galibiyetleri")
    parser.add_argument("--sac-type", type=int, choices=[0, 1, 2, 3, 4, 5, 9], default=0)
    parser.add_argument("--max-moves", type=int, default=80)
    parser.add_argument("--max-iters", type=int, default=1)
    parser.add_argument("--verbose", action="store_true",
                        help="Ayrıntılı eğitim ve maç çıktısı")
    parser.add_argument("--version", action="version", version="stallion-training 1.0")
    return parser


def _filter_list(value: str) -> list[str]:
    names = [name.strip() for name in value.split(",") if name.strip()]
    _sbin().filter_mask(names)
    return names


def normalize_args(args: argparse.Namespace) -> argparse.Namespace:
    if args.command != "pipeline" and args.phase == "all":
        raise ValueError("--phase all yalnızca pipeline içindir.")
    if args.games_base is None:
        args.games_base = args.games
    if args.games_aggressive is None:
        args.games_aggressive = args.games
    for name in ("assets", "source", "puzzles", "offset_file", "dataset", "aggressive_dataset", "run_dir",
                 "resume", "candidate", "baseline", "pgn", "pgnout", "json_out", "cutechess", "stockfish",
                 "model", "output"):
        setattr(args, name, resolve_path(getattr(args, name)))
    args.book = None if args.book == "none" else resolve_path(args.book)
    args.engine_src = resolve_path(args.engine_src, ENGINE_ROOT)
    engine_name = {"darwin": "stallion_eas_mac", "win32": "stallion_eas_windows.exe"}.get(sys.platform, "stallion_eas_linux")
    args.engine = resolve_path(args.engine, ENGINE_ROOT / engine_name)
    defaults = {
        "extract": ROOT / "data" / "train.sbin", "prepare": ROOT / "data" / "aggressive-prepared.sbin",
        "datagen": ROOT / "data" / "selfplay.sbin", "eas": ROOT / "statistics_EAS_ratinglist.txt",
        "sacrifices": ROOT / "games_with_sacrifices.pgn", "iwins": ROOT / "interesting_wins.pgn",
        "train": ROOT / "runs" / f"train-{datetime.now():%Y%m%d-%H%M%S-%f}" / "candidate.nnue",
    }
    if args.output is None and args.command in defaults:
        args.output = defaults[args.command]
    if args.command in ("match", "gauntlet"):
        if args.candidate is None or (args.command == "match" and args.baseline is None):
            raise ValueError(f"{args.command} için --candidate{' ve --baseline' if args.command == 'match' else ''} gerekli.")
        if args.pgnout is None:
            args.pgnout = ROOT / "runs" / f"{args.command}-{datetime.now():%Y%m%d-%H%M%S-%f}.pgn"
    if args.command in ("train", "label") and args.dataset is None:
        raise ValueError(f"{args.command} için --dataset gerekli.")
    if args.command in ("eas", "sacrifices", "iwins") and args.pgn is None:
        raise ValueError(f"{args.command} için --pgn gerekli.")
    return args


def main(argv: Sequence[str] | None = None) -> int:
    parser = build_parser()
    try:
        args = normalize_args(parser.parse_args(argv))
        if args.command == "extract":
            extract_dataset(args, args.phase, args.output)
        elif args.command == "prepare":
            prepare_dataset(args)
        elif args.command == "datagen":
            run_datagen(args)
        elif args.command == "mine":
            mine_hard_dataset(args)
        elif args.command == "label":
            run_label(args)
        elif args.command == "train":
            train_nnue(
                args.dataset, args.output, epochs=args.epochs,
                batch_size=args.batch_size, lr=args.lr, resume=args.resume,
                device_name=args.device, seed=args.seed, patience=args.patience,
                validation=args.validation, swa=args.swa,
                feature_dropout=args.feature_dropout, verbose=args.verbose,
            )
        elif args.command == "match":
            result = run_match(args, args.phase)
            eas_report = run_eas(_namespace_copy(
                args, pgn=args.pgnout, output=args.pgnout.with_suffix(".eas.txt"),
                json_out=args.pgnout.with_suffix(".eas.json"),
            ))
            maybe_promote(args, args.phase, result, args.candidate, args.baseline, eas_report)
            return 0 if not args.sprt or result.is_winner else 1
        elif args.command == "gauntlet":
            return 0 if run_gauntlet(args) else 1
        elif args.command == "eas":
            run_eas(args)
        elif args.command == "sacrifices":
            run_sacrifices(args)
        elif args.command == "iwins":
            run_iwins(args)
        else:
            run_pipeline(args)
        return 0
    except (RuntimeError, ValueError, OSError, subprocess.SubprocessError) as exc:
        print(f"[HATA] {exc}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    raise SystemExit(main())

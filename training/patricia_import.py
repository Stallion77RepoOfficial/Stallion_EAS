#!/usr/bin/env python3
"""Convert Patricia datagen text into Stallion SBIN training data."""

from __future__ import annotations

import argparse
import ctypes
import json
import math
import os
from pathlib import Path
import uuid

import sbin_tool


def convert(inputs: list[Path], output: Path, target: int, cp_weight: float,
            cp_scale: float) -> dict:
    """Every line must parse; repeated positions (same first four FEN fields) are skipped."""
    if (target < 1 or not math.isfinite(cp_weight) or
            not 0.0 <= cp_weight <= 1.0 or
            not math.isfinite(cp_scale) or cp_scale <= 0):
        raise ValueError("target pozitif, cp-weight 0..1 ve cp-scale pozitif olmalı")
    lib = sbin_tool.load_native_lib()
    packed = sbin_tool.PackedPosition()
    seen: set[tuple[str, ...]] = set()
    duplicates = 0
    written = 0
    results = {"0": 0, "0.5": 0, "1": 0}
    output.parent.mkdir(parents=True, exist_ok=True)
    temporary = output.with_name(f".{output.name}.{uuid.uuid4().hex}.tmp")
    try:
        with temporary.open("wb") as dst:
            for source in inputs:
                with source.open("r", encoding="utf-8") as src:
                    for number, line in enumerate(src, 1):
                        fields = line.rstrip("\r\n").split(" | ")
                        if len(fields) != 3 or len(fields[0].split()) != 6:
                            raise ValueError(f"{source}:{number}: 'FEN | cp | sonuç' satırı bekleniyordu")
                        fen, cp_text, result_text = fields
                        cp = int(cp_text)
                        result = float(result_text)
                        if result not in (0.0, 0.5, 1.0):
                            raise ValueError(f"{source}:{number}: sonuç 0, 0.5 veya 1 olmalı")
                        key = tuple(fen.split()[:4])
                        if key in seen:
                            duplicates += 1
                            continue
                        clipped_cp = max(-1500, min(1500, cp))
                        cp_wdl = 1.0 / (1.0 + 10.0 ** (-clipped_cp / cp_scale))
                        label = cp_weight * cp_wdl + (1.0 - cp_weight) * result
                        status = lib.sbin_pack_fen(
                            fen.encode("utf-8"), ctypes.c_float(label),
                            ctypes.c_int16(max(-32000, min(32000, cp))), ctypes.byref(packed),
                        )
                        if status:
                            raise ValueError(f"{source}:{number}: paketlenemeyen konum (kod {status})")
                        dst.write(bytes(packed))
                        seen.add(key)
                        results[str(result).removesuffix(".0")] += 1
                        written += 1
                        if written >= target:
                            break
                if written >= target:
                    break
        if written < target:
            raise RuntimeError(f"Yetersiz geçerli konum: {written}/{target}")
        os.replace(temporary, output)
    finally:
        temporary.unlink(missing_ok=True)
    metadata = {
        "sources": [str(path.resolve()) for path in inputs],
        "output": str(output.resolve()), "positions": written, "duplicates": duplicates,
        "cp_weight": cp_weight, "result_weight": 1.0 - cp_weight,
        "cp_scale": cp_scale, "results": results,
    }
    output.with_suffix(".patricia.json").write_text(json.dumps(metadata, indent=2) + "\n")
    return metadata


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("inputs", nargs="+", type=Path)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--target", type=int, default=500_000)
    parser.add_argument("--cp-weight", type=float, default=0.75)
    parser.add_argument("--cp-scale", type=float, default=150.0)
    args = parser.parse_args()
    print(json.dumps(convert(args.inputs, args.output, args.target, args.cp_weight, args.cp_scale)))


if __name__ == "__main__":
    main()

#!/usr/bin/env python3
"""Migrate a legacy 12288-feature NNUE into the current extended format.

Base weights are preserved bit-for-bit; the extra feature rows are
zero-initialized, so the migrated net evaluates identically until retrained.
Usage: python3 migrate_nnue.py <input.nnue> <output.nnue>
"""
from __future__ import annotations

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

import stallion


def main(argv: list[str]) -> int:
    if len(argv) != 3:
        print(f"Kullanım: {argv[0]} <girdi.nnue> <çıktı.nnue>")
        return 2
    src = Path(argv[1])
    dst = Path(argv[2])
    model = stallion._make_nnue_model()
    stallion.load_nnue(model, src)
    stallion.export_nnue(model, dst)
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))

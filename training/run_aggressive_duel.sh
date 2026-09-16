#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

RUN_DIR="$ROOT/training/runs"
mkdir -p "$RUN_DIR" "$RUN_DIR/backups"
LOG_FILE="$RUN_DIR/aggressive_duel.log"

echo "==========================================================" | tee -a "$LOG_FILE"
echo "  STALLION EAS: AGGRESSIVE NNUE DERBİSİ (100 MAÇ)" | tee -a "$LOG_FILE"
echo "  Candidate: base-100m-candidate-swa.nnue" | tee -a "$LOG_FILE"
echo "  Baseline:  engine/nets/aggressive.nnue" | tee -a "$LOG_FILE"
echo "  Fixed Base: engine/nets/base.nnue" | tee -a "$LOG_FILE"
echo "  Started at: $(date)" | tee -a "$LOG_FILE"
echo "==========================================================" | tee -a "$LOG_FILE"

CANDIDATE="$RUN_DIR/base-100m-candidate-swa.nnue"
BASELINE="$ROOT/engine/nets/aggressive.nnue"
FIXED_BASE="$ROOT/engine/nets/base.nnue"
PGN_OUT="$RUN_DIR/aggressive-duel-match.pgn"
JSON_OUT="$RUN_DIR/aggressive-duel-match.json"

rm -f "$PGN_OUT" "$JSON_OUT"

echo -e "\n>>> 1. 100 MAÇLIK AGGRESSIVE MAÇI BAŞLATILIYOR (6 Concurrency, 5+0.1)..." | tee -a "$LOG_FILE"
training/.venv/bin/python training/stallion.py match \
  --phase aggressive \
  --candidate "$CANDIDATE" \
  --baseline "$BASELINE" \
  --fixed-base "$FIXED_BASE" \
  --engine engine/stallion_eas_mac \
  --book training/openings.epd \
  --games 100 \
  --concurrency 6 \
  --tc 5+0.1 \
  --pgnout "$PGN_OUT" \
  --json-out "$JSON_OUT" 2>&1 | tee -a "$LOG_FILE"

echo -e "\n>>> 2. EAS ANALİZİ ÇALIŞTIRILIYOR..." | tee -a "$LOG_FILE"
training/.venv/bin/python training/stallion.py eas \
  --pgn "$PGN_OUT" \
  --output "$RUN_DIR/aggressive-duel-eas.txt" \
  --json-out "$RUN_DIR/aggressive-duel-eas.json" 2>&1 | tee -a "$LOG_FILE"

echo -e "\n>>> 3. EAS KAZANANI BELİRLENİYOR VE TERFİ DEĞERLENDİRİLİYOR..." | tee -a "$LOG_FILE"
training/.venv/bin/python -c "
import json, shutil
from pathlib import Path

eas_file = Path('$RUN_DIR/aggressive-duel-eas.json')
with open(eas_file) as f:
    data = json.load(f)

res = {r['engine']: r for r in data}
cand = res.get('Candidate', {})
base = res.get('Baseline', {})

cand_eas = cand.get('eas', 0)
base_eas = base.get('eas', 0)
print(f'EAS Skorları -> Aday (Candidate): {cand_eas} EAS | Mevcut (Baseline): {base_eas} EAS')

if cand_eas > base_eas:
    print(f'[KAZANAN: ADAY!] Aday {cand_eas} > {base_eas} (+{cand_eas - base_eas} EAS).')
    backup = Path('$RUN_DIR/backups/aggressive.backup-before-100m.nnue')
    shutil.copy2('$BASELINE', backup)
    shutil.copy2('$CANDIDATE', '$BASELINE')
    print(f'[TERFİ TAMAMLANDI] $CANDIDATE -> $BASELINE (Yedek: {backup})')
else:
    print(f'[KAZANAN: MEVCUT!] Mevcut ağ {base_eas} >= {cand_eas} EAS. Mevcut aggressive.nnue korundu.')
" 2>&1 | tee -a "$LOG_FILE"

echo -e "\n==========================================================" | tee -a "$LOG_FILE"
echo "  BİTTİ: $(date)" | tee -a "$LOG_FILE"
echo "==========================================================" | tee -a "$LOG_FILE"

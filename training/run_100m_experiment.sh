#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

RUN_DIR="$ROOT/training/runs"
mkdir -p "$RUN_DIR"
LOG_FILE="$RUN_DIR/experiment_100m.log"

echo "==========================================================" | tee -a "$LOG_FILE"
echo "  STALLION NNUE: 100M MASTER DATASET TRAINING & EVAL" | tee -a "$LOG_FILE"
echo "  Started at: $(date)" | tee -a "$LOG_FILE"
echo "==========================================================" | tee -a "$LOG_FILE"

DATASET="$ROOT/training/data/master_100m.sbin"
CHAMPION="$ROOT/engine/nets/base.nnue"
CANDIDATE_RAW="$RUN_DIR/base-100m-candidate.nnue"
CANDIDATE_SWA="$RUN_DIR/base-100m-candidate-swa.nnue"

if [ ! -f "$DATASET" ]; then
    echo "[HATA] Veri seti bulunamadı: $DATASET" | tee -a "$LOG_FILE"
    exit 1
fi

if [ ! -f "$CHAMPION" ]; then
    echo "[HATA] Mevcut şampiyon ağ bulunamadı: $CHAMPION" | tee -a "$LOG_FILE"
    exit 1
fi

echo -e "\n>>> 1. EĞİTİM BAŞLATILIYOR (100M Pozisyon, 6 Epoch, SWA, MPS)..." | tee -a "$LOG_FILE"
training/.venv/bin/python training/stallion.py train \
  --phase base \
  --dataset "$DATASET" \
  --output "$CANDIDATE_RAW" \
  --resume "$CHAMPION" \
  --epochs 6 \
  --batch-size 2048 \
  --lr 0.00010 \
  --validation 0.01 \
  --patience 6 \
  --device mps 2>&1 | tee -a "$LOG_FILE"

# Determine best candidate to test
if [ -f "$CANDIDATE_SWA" ]; then
    MATCH_CANDIDATE="$CANDIDATE_SWA"
    echo -e "\n[BİLGİ] SWA modeli seçildi: $MATCH_CANDIDATE" | tee -a "$LOG_FILE"
else
    MATCH_CANDIDATE="$CANDIDATE_RAW"
    echo -e "\n[BİLGİ] Tekil model seçildi: $MATCH_CANDIDATE" | tee -a "$LOG_FILE"
fi

echo -e "\n>>> 2. CUTECHESS SPRT MAÇI BAŞLATILIYOR (vs Champion base.nnue)..." | tee -a "$LOG_FILE"
training/.venv/bin/python training/stallion.py match \
  --phase base \
  --candidate "$MATCH_CANDIDATE" \
  --baseline "$CHAMPION" \
  --engine engine/stallion_eas_mac \
  --book training/openings.epd \
  --games 200 \
  --concurrency 6 \
  --tc 5+0.1 \
  --sprt \
  --promote \
  --pgnout "$RUN_DIR/base-100m-match.pgn" \
  --json-out "$RUN_DIR/base-100m-match.json" 2>&1 | tee -a "$LOG_FILE"

echo -e "\n==========================================================" | tee -a "$LOG_FILE"
echo "  TAMAMLANDI: $(date)" | tee -a "$LOG_FILE"
echo "==========================================================" | tee -a "$LOG_FILE"

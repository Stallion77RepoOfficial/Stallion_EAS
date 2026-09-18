# Stallion EAS Eğitim Hattı

## Mimari (motorla birebir aynı olmalı)

- Girdi: `13316 = 12288 baz (16 king-bucket × 768) + 1028 ekstra`, gizli katman `1024`, çıkış `8 bucket × 2048`.
- Ekstra bloklar: materyal `100` + şah-bölgesi taş `234` + şah-bölgesi saldırı `18` + piyon yapısı `384` + kale-hattı `256` + fil-piyon kompleksi `36`. Sadece öğrenilmiş ağırlık — insan değer biçmez (HCE yok).
- King bucket tablosu, `feature_indices` ve `collect_extra_features` formülleri motor header'larından gelir; SBIN native kod aynı header'ları include eder, Python tarafı `parse_fen_fast` ile aynısını üretir (`sbin_tool.py verify` ikisini karşılaştırır).
- Çıkış bucket'ı: `clamp((taş_sayısı - 1) // 4, 0, 7)` — motor ve eğitimde aynı (taş sayısı baz feature'lardan sayılır).
- Kuantizasyon: `QA=255, QB=64, QAB=16320, SCALE=400`; kayıp uzayı `400/ln(10)` ile centipawn'a bağlıdır.
- Net dosyası: payload `27.306.000` bayt, padded `27.306.048` bayt. Motor net'i gömülü taşır (`net_embed.S`); çalışma anında net dosyası okunmaz. Eski 12288-format netler `migrate_nnue.py` ile taşınır (ekstra satırlar sıfır → bit-bit aynı oyun).

## Komutlar (`training/.venv/bin/python stallion.py ...`)

- `extract --phase base --source evals.sbin --output base.sbin` — faz kotasına göre dengeli çıkarım.
- `extract --phase aggressive --source evals.sbin --output aggr.sbin [--puzzles pool.sbin]` — keskinlik/feda ağırlıklı çıkarım (`--sac-ratio`, `--puzzle-ratio`, `--augment-mirror`).
- `prepare` — `extract aggressive` + feda havuzu kaynağı (aynı kod yolu).
- `mine --source evals.sbin --output hard.sbin --model current.nnue --target 5000000` — modelin en çok yanıldığı pozisyonları toplar.
- `train --dataset train.sbin --output candidate.nnue [--resume base.nnue] [--epochs N]` — CPU/CUDA/MPS otomatik; en iyi epoch'u `.nnue` + `.pt` olarak yazar.
- `datagen --engine motor --games 200 --output selfplay.sbin` — WDL-etiketli self-play verisi.
- `match --candidate a.nnue --baseline b.nnue --games 200 [--sprt]` — her net için gömülü motor derler (`runs/engine-cache`, kaynak+net hash'iyle önbellekli), cutechess ile karşılaştırır.
- `pipeline --phase base|aggressive|all --steps extract,train,match --max-iters N` — uçtan uca döngü; taban net yoksa `stallion.nnue` kullanılır.
- `eas` / `sacrifices` — PGN istatistik/rapor araçları (eğitim dışı).

Yardımcılar:

- `convert_brilliant.py --pgn ... --output pool.sbin [--workers N]` — BrilliantPly PGN'den WDL-etiketli feda havuzu üretir.
- `sbin_tool.py verify --sbin ...` — format + native/Python tutarlılık denetimi; `benchmark` okuma hızı ölçer.

## Etiket sistemi (önemli)

SBIN kayıtları `wdl` (u16) ve `eval` (i16 cp) taşır. Bu hattın yazdığı dosyalar (self-play, puzzle, prepared) `eval=0` + WDL-etiketlidir. `labels=cp` istenen her yerde kaynakta gerçek cp aranır; yoksa komut `--sbin-labels wdl` önerisiyle durur (sessiz etiket yıkımına karşı koruma). Lichess dökümlerinden cp'li `evals.sbin` üretimi harici adımdır (plana bakın).

## Örnek akış

```bash
.venv/bin/python stallion.py extract --phase base --source data/evals.sbin --output data/base.sbin --target 2000000
.venv/bin/python stallion.py train --dataset data/base.sbin --output runs/cand.nnue --epochs 40
.venv/bin/python stallion.py match --candidate runs/cand.nnue --baseline ../engine/nets/stallion.nnue --games 200 --sprt
```

Terfi sonrası eski netle derlenmiş motor ikilileri bayatlar; motoru yeniden derleyin. `data/`, `runs/`, `*.pt`, derlenen dylib ve önbellekler git'e girmez.

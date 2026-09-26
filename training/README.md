# Stallion EAS Eğitim Hattı

## Mimari (motorla birebir aynı olmalı)

- Girdi: `13316 = 12288 baz (16 king-bucket × 768 özellik/bucket) + 1028 ekstra`, gizli katman `1024`, çıkış `16 bucket × 2048`. Buradaki 768, `12 taş kodu × 64 kare` stride'ıdır; eski bucket sayısı değildir.
- Ekstra bloklar: materyal `100` + şah-bölgesi taş `234` + şah-bölgesi saldırı `18` + piyon yapısı `384` + kale-hattı `256` + fil-piyon kompleksi `36`. Sadece öğrenilmiş ağırlık — insan değer biçmez (HCE yok).
- King bucket tablosu, `feature_indices`, `collect_extra_features` ve `mirror_extra_features` motor header'larından gelir; SBIN native kodu (`sbin.cpp`) aynı header'ları include eder. `sbin_tool.py verify` native özellikleri bağımsız Python oracle'ı (`oracle_features`) ile karşılaştırır.
- Çıkış bucket'ı: `clamp((taş_sayısı - 1) // 2, 0, 15)` — motor ve eğitimde aynı.
- Kuantizasyon: `QA=255, QB=64, QAB=16320, SCALE=400`; kayıp uzayı `400/ln(10)` ile centipawn'a bağlıdır.
- Net dosyası: payload `27.338.784` bayt, padded `27.338.816` bayt. Motor net'i gömülü taşır (`net_embed.S`); çalışma anında net dosyası okunmaz. Yalnızca 16 king bucket / 16 çıkış bucket biçimi kabul edilir.
- Özellik tamponları tam sınıra göre: taraf başına en fazla `158` özellik (`32` taş + `126` ekstra; motorun kuralı olan taraf başına ≤16 taş, ≤8 piyon altında ulaşılabilecek en yüksek değer). Her pozisyon yalnızca gerçekte aktif olan özellikleri kullanır.

## Eğitim

`train` özellik önbelleği kullanmaz: SBIN kayıtları (32 bayt/pozisyon) RAM'e okunur, her batch arka plan iş parçacığında native olarak CSR biçiminde üretilir (`sbin_build_batch`). GPU tarafında özellik dönüştürücü `embedding_bag`, ağırlık gradyanı özellik-sıralı transpoze CSR üzerinden hesaplanır; çıkış katmanı `torch.compile` ile derlenir, optimizer `AdamW(fused=True)`. Apple M4 (MPS) üzerinde ~85–88k poz/sn (eski önbellekli hat ~37k).

- Doğrulama ayrımı pozisyon anahtarıyladır: ağa aynı görünen konumlar (yan-hamle perspektifinden aynı tahta, renk-çevrik ikizler dahil) hep aynı tarafta kalır.
- Öğrenme oranı tüm koşu boyunca adım başına kosinüsle `lr`'den sıfıra iner. Son batch eksikse atlanır; her epoch tam karıştırılır.
- `--resume model.nnue` ince ayar, `--resume checkpoint.pt` aynı veri/ayar (seed, validation, batch-size) ile kesilen koşuya devamdır.
- Parquet eğitim girdisi değildir; önce `sbin_tool.py convert --parquet x.parquet --sbin x.sbin`.

## Komutlar (`training/.venv/bin/python stallion.py <komut> ...`)

Her komut yalnızca kendi bayraklarını kabul eder; açıklamalı liste için `stallion.py <komut> -h`.

- `extract --phase base|aggressive --target N [--output x.sbin]` — eval SBIN'den (`--source`, varsayılan `data/evals.sbin`) native seçim. Base: faz kotaları (`--phase-dist`), `--base-filters`. Aggressive: feda/keskin konumlar + bulmaca havuzu (`--puzzles`, `--puzzle-ratio`, `--sac-ratio`, `--augment-mirror`), `--aggressive-filters`. Kalınan yer `--offset-file`'a yazılır (`--skip-lines`, `--reset-offset`, `--no-offset`).
- `prepare [--target N]` — yalnızca feda bulmaca havuzundan aggressive veri.
- `mine --target N [--pool-size M] [--model ağ.nnue]` — referans ağın en çok yanıldığı konumlar.
- `train --dataset x.sbin [--resume ağ.nnue] [--epochs N] [--batch-size B] [--lr X]`.
- `datagen --games N (--depth D | --nodes N) [--book none]` — oyun sonucuyla etiketli self-play.
- `label --dataset x.sbin (--depth D | --nodes N)` — WDL-etiketli SBIN'e cp ekler.
- `match --candidate a.nnue [--baseline b.nnue] [--games N] [--sprt] [--promote]` — her net için gömülü motor derler (`runs/engine-cache`), cutechess ile oynatır, ardından EAS raporu yazar.
- `gauntlet --candidate a.nnue [--anchor-elo 2400]` — Stockfish'e karşı hızlı kontrol.
- `pipeline --phase base|aggressive|all --steps extract,train,match,eas [--max-iters N]` — adımları sırayla çalıştırır; eğitim varsayılan olarak taban ağdan (`--baseline`) ince ayardır.
- `eas` / `sacrifices` / `iwins --pgn x.pgn` — PGN rapor araçları.

Arama sınırları bağımsızdır: `--depth` ve `--nodes` birlikte verilirse ilk dolan sınır durdurur; biri diğerini değiştirmez. Her komut tek bir sonuç satırı yazar; ilerleme, maç satırları ve derleme ayrıntısı için `--verbose`. Hatalar sessizce atlanmaz, komut hata ile durur. Otomatik terfi ancak `--sprt` ile H1 kabul edilirse yapılır. Göreli yollar çalışma dizinine göredir.

Yardımcılar:

- `convert_brilliant.py --pgn ... --output pool.sbin [--workers N]` — BrilliantPly PGN'den gerçek oyun sonucuyla etiketli feda havuzu.
- `patricia_import.py data*.txt --output patricia.sbin --target 500000 --cp-weight 0.75 --cp-scale 140` — Patricia datagen metni; CP ölçeğini veri kaynağına göre kalibre edin.
- `sbin_tool.py verify --sbin ... [--all] [--check-eval-labels]`, `sbin_tool.py benchmark --sbin ...` (batch üretim hızı), `sbin_tool.py convert --parquet ... --sbin ...`.

## Etiketler ve filtreler (önemli)

SBIN kayıtları `wdl` (u16, beyaz perspektifi) ve `eval` (i16 cp, beyaz perspektifi) taşır. Bu hattın ürettiği self-play/puzzle dosyaları `eval=0` + WDL etiketlidir; `--sbin-labels cp` cp taşımayan kaynakta hata verir.

`--sbin-labels cp` etiketi cp'den 400 ölçekli lojistikle üretir, `--wdl-lambda` kayıttaki WDL ile karıştırır (varsayılan `0`). `evals.sbin` içindeki WDL alanı oyun sonucu değil, CP'nin ~669 ölçekli dönüşümüdür (`sbin_tool.py verify --sbin data/evals.sbin --check-eval-labels` gösterir); bu yüzden ham `evals.sbin` doğrudan eğitilmemeli, önce `extract` edilmelidir.

`evals.sbin`'de `cp = ±2000` ve WDL `0/65535` olan kayıtlar mat skorudur ve etiketi tam `0/1` olur. Filtresiz faz kotalı çıkarımda bu kayıtlar setin ~%20'sini oluşturur; ayrıca ~%6 konum şahtadır ve ~%19'unda yan-hamlenin malzeme kazandıran bir alma/terfisi vardır (statik değerlendirmenin göremeyeceği taktik). Filtreler:

- `mate` — mat skorlu kayıtlar,
- `check` — hamle sırası olan taraf şahta,
- `tactical` — SEE ≥ 1 olan bir alma/terfi var (motorun `SEE` fonksiyonu).

`--base-filters` varsayılanı `mate,check,tactical`, `--aggressive-filters` varsayılanı boştur: feda bulmacalarının neredeyse tamamı şah/taktik konumdur (filtre ile 100k bulmacadan ~2.6k kalır). Filtreyi kapatmak için `--base-filters ""`.

Ölçüm (R17'den ince ayar, 5M konum, 3 epoch, batch 4096, lr 2e-4; R17'ye karşı 3+0.03, aynı açılışlar):

| Veri | Oyun | Elo (95% aralık) |
| --- | --- | --- |
| filtresiz | 400 | −71.3 (−100..−44) |
| `mate,check` | 212 | −36.2 (−77..+3) |
| `mate,check,tactical` | 216 | +35.5 (−4..+76) |

## Örnek akış

```bash
.venv/bin/python stallion.py extract --phase base --output data/base.sbin --target 5000000
.venv/bin/python stallion.py train --dataset data/base.sbin --output runs/cand.nnue --resume ../engine/nets/stallion.nnue --epochs 3 --batch-size 4096 --lr 2e-4
.venv/bin/python stallion.py match --candidate runs/cand.nnue --baseline ../engine/nets/stallion.nnue --games 400 --sprt
```

Terfi sonrası eski netle derlenmiş motor ikilileri bayatlar; motoru yeniden derleyin. `data/`, `runs/`, `*.pt`, derlenen dylib git'e girmez.

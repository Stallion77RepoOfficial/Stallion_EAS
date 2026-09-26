# Stallion EAS Eğitim Hattı

## Mimari (motorla birebir aynı olmalı)

- Girdi: `13316 = 12288 baz (16 king-bucket × 768 özellik/bucket) + 1028 ekstra`, gizli katman `1024`, çıkış `16 bucket × 2048`. Buradaki 768, `12 taş kodu × 64 kare` stride'ıdır; eski bucket sayısı değildir.
- Ekstra bloklar: materyal `100` + şah-bölgesi taş `234` + şah-bölgesi saldırı `18` + piyon yapısı `384` + kale-hattı `256` + fil-piyon kompleksi `36`. Sadece öğrenilmiş ağırlık — insan değer biçmez (HCE yok).
- King bucket tablosu, `feature_indices` ve `collect_extra_features` formülleri motor header'larından gelir; SBIN native kod aynı header'ları include eder, Python tarafı `parse_fen_fast` ile aynısını üretir (`sbin_tool.py verify` ikisini karşılaştırır).
- Çıkış bucket'ı: `clamp((taş_sayısı - 1) // 2, 0, 15)` — motor ve eğitimde aynı (taş sayısı baz feature'lardan sayılır).
- Kuantizasyon: `QA=255, QB=64, QAB=16320, SCALE=400`; kayıp uzayı `400/ln(10)` ile centipawn'a bağlıdır.
- Net dosyası: payload `27.338.784` bayt, padded `27.338.816` bayt. Motor net'i gömülü taşır (`net_embed.S`); çalışma anında net dosyası okunmaz. Motor ve eğitim yalnızca 16 king bucket / 16 çıkış bucket biçimini kabul eder. Eski ağlar reddedilir; ağırlık kopyalama, sıfır ekleme veya otomatik ağ genişletme yapılmaz.
- Pozisyon başına özellik tamponu `192` slot kullanır; bu sayı ağın `13316` olası girişinden farklıdır. Yasal 32 taşlı konum için üst sınır `172` etkin özellik olduğundan özellik kesilmez. Eski 256 slotlu özellik önbelleği yeniden oluşturulur; eski v9 checkpoint'i ve `.nnue` dosya biçimi uyumludur.

## Komutlar (`training/.venv/bin/python stallion.py ...`)

- `extract --phase base --source evals.sbin --output base.sbin` — faz kotasına göre dengeli çıkarım.
- `extract --phase aggressive --source evals.sbin --output aggr.sbin [--puzzles pool.sbin]` — keskinlik/feda ağırlıklı çıkarım (`--sac-ratio`, `--puzzle-ratio`, `--augment-mirror`).
- `prepare` — `extract aggressive` + feda havuzu kaynağı (aynı kod yolu).
- `mine --source evals.sbin --output hard.sbin --model current.nnue --target 5000000` — modelin en çok yanıldığı pozisyonları toplar.
- `train --dataset train.sbin --output candidate.nnue [--resume base.nnue] [--epochs N]` — CPU/CUDA/MPS otomatik; en iyi epoch'u `.nnue` + `.pt` olarak yazar.
- `datagen --engine motor --games 200 --output selfplay.sbin` — WDL-etiketli self-play verisi.
- `patricia_import.py data*.txt --output patricia.sbin --target 500000 --cp-weight 0.75 --cp-scale 140` — Patricia datagen metnindeki CP ve oyun sonucunu tek eğitim hedefine dönüştürür; CP ayrıca SBIN `eval` alanında saklanır. CP ölçeğini veri kaynağına göre kalibre edin.
- `match --candidate a.nnue --baseline b.nnue --games 200 [--sprt]` — her net için gömülü motor derler (`runs/engine-cache`, kaynak+net hash'iyle önbellekli), cutechess ile karşılaştırır.
- `pipeline --phase base|aggressive|all --steps extract,train,match --max-iters N` — uçtan uca döngü; taban ağ doğrudan `engine/nets/stallion.nnue` veya açıkça verilen `--baseline` dosyasıdır. Seçilen dosya yoksa işlem durur.
- `eas` / `sacrifices` — PGN istatistik/rapor araçları (eğitim dışı).

`train` her epoch sonunda tek özet, `match` ve `gauntlet` sonuç özeti yazar. Ayrıntılı çıkarma, madencilik, etiketleme, eğitim ilerlemesi ve cutechess satırları için `--verbose` kullanın. Başarılı maçlarda gereksiz `.log` dosyası tutulmaz; hata halinde inceleme için saklanır. Otomatik terfi ancak `--sprt` sonucunda H1 kabul edilirse yapılır; tahmini Elo ve yerel EAS raporu tek başına terfi ettirmez.

Yardımcılar:

- `convert_brilliant.py --pgn ... --output pool.sbin [--workers N]` — BrilliantPly PGN'den WDL-etiketli feda havuzu üretir.
- `sbin_tool.py verify --sbin ...` — format + native/Python tutarlılık denetimi; `benchmark` okuma hızı ölçer.

## Etiket sistemi (önemli)

SBIN kayıtları `wdl` (u16) ve `eval` (i16 cp) taşır. Bu hattın yazdığı dosyalar (self-play, puzzle, prepared) `eval=0` + WDL-etiketlidir. `labels=cp` istenen her yerde kaynakta gerçek cp aranır; yoksa komut `--sbin-labels wdl` önerisiyle durur (sessiz etiket yıkımına karşı koruma). Lichess dökümlerinden cp'li `evals.sbin` üretimi harici adımdır (plana bakın).

`--wdl-lambda` CP dönüşümü ile kayıttaki WDL'yi karıştırır; kayıttaki WDL'nin gerçek oyun sonucu olduğunu garanti etmez. Saf CP etiketi için `--sbin-labels cp --wdl-lambda 0`, mevcut etiketleri değiştirmemek için `--sbin-labels wdl` kullanın. Karışım oranı çıkarım manifestine yazılır.

Varsayılan `--wdl-lambda 0`'dır. Mevcut `evals.sbin` kaynağında saklı WDL alanı bağımsız oyun sonucu değil, CP'nin yaklaşık 669 ölçekli lojistik dönüşümü gibi davranıyor. `--wdl-lambda 0.25` bu kaynakta iki farklı değerlendirme ölçeğini karıştırır. Kaynağı hızlı kontrol etmek için `sbin_tool.py verify --sbin data/evals.sbin --samples 10000 --check-eval-labels` kullanın; `--all` verilmedikçe etiket denetimi de örneklemlidir. Başka bir kaynakta WDL gerçek oyun sonucuysa karışım oranını açıkça seçin.

`train` doğrudan SBIN'in WDL alanını okur; `--wdl-lambda` eğitim sırasında uygulanmaz. Ham `evals.sbin` ile doğrudan `train` yapmak, yukarıdaki yaklaşık 669 ölçekli etiketleri kullanır. Saf CP hedefi için önce `extract --sbin-labels cp` ile yeni SBIN üretip onu eğitin.

`convert_brilliant.py` konumları PGN'nin gerçek `Result` değeriyle etiketler; sonucu bilinmeyen oyunları atlar. BrilliantPly tek başına galibiyet etiketi değildir. Eski, fedayı yapan tarafı kazanan sayan havuzlar PGN'den yeniden üretilmeden gerçek sonuç verisi olarak kullanılmamalıdır.

## Örnek akış

```bash
.venv/bin/python stallion.py extract --phase base --source data/evals.sbin --output data/base.sbin --target 2000000
.venv/bin/python stallion.py train --dataset data/base.sbin --output runs/cand.nnue --resume ../engine/nets/stallion.nnue --epochs 40
.venv/bin/python stallion.py match --candidate runs/cand.nnue --baseline ../engine/nets/stallion.nnue --games 200 --sprt
```

Fine-tune için `--resume model.nnue` kullanılır. Aynı veri ve ayarlarla kesilen eğitime devam etmek için `--resume checkpoint.pt` verilir; `--epochs` toplam epoch hedefidir. Checkpoint model, optimizer, scheduler, veri kimliği ve ilgili cihazın RNG durumunu içermelidir; eksik bilgiler varsayılan değerlerle tamamlanmaz. `--resume` verilmezse doğrudan `train` komutu modeli sıfırdan başlatır.

Özellik önbelleği native SBIN decoder ile oluşturulur. Parquet önce SBIN'e dönüştürülür; native derleme veya dönüşüm başarısız olursa Python decoder'a geçilmez. Varsayılan motor yolu `engine/` altındadır; başka ikili için `--engine` kullanılır.

Terfi sonrası eski netle derlenmiş motor ikilileri bayatlar; motoru yeniden derleyin. `data/`, `runs/`, `*.pt`, derlenen dylib ve önbellekler git'e girmez.

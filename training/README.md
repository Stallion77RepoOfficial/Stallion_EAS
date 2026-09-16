# Stallion EAS eğitim araçları

Tüm eğitim akışı `training/stallion.py` içindeki tek CLI'dan çalışır. Komutlar
depo kökünden ve depo içindeki sanal ortamla çalıştırılmalıdır:

```bash
training/.venv/bin/python training/stallion.py KOMUT --help
```

Fazlar `base` ve `aggressive` olarak ayrıdır. `all` yalnızca `pipeline` için
geçerlidir; önce base, sonra aggressive çalışır.

## Veri çıkarma

Lichess eval akışından faz dengeli veri üretme:

```bash
training/.venv/bin/python training/stallion.py extract \
  --phase base --target 500000 --min-depth 16 \
  --zst training/lichess_db_eval.jsonl.zst \
  --output training/data/base.sbin
```

Çıktı uzantısı `.sbin` ise 32-byte native SBIN, `.parquet` ise Parquet yazılır.
Eval kaynağındaki en derin kayıt ve ilk PV kullanılır; `cp` değeri White
perspektifinden WDL hedefine çevrilir, `mate` kayıtları kesin 0/1 olur. FEN,
taş sayısı, şahlar, piyon sıraları, rok ve geçerken alma alanları doğrulanır.
Aynı taş yerleşimi ve sıra tek kayda indirilir.

`--skip-lines N` bu çalışmada ilk N satırı atlayıp N+1'den başlar. Değer
verilmezse `--offset-file` içindeki faz ofseti kullanılır ve başarılı tarama
sonunda güncellenir. Kaynak değiştiyse `--reset-offset` veya açık bir
`--skip-lines` gerekir. Dosya sonuna kadar hedef dolmazsa kısmi çıktı korunur
ve komut, `--allow-short-dataset` verilmedikçe hata kodu 2 ile biter.

Aggressive fazı iki kaynaktan beslenebilir:

```bash
# Eval akışından keskin ve feda pozisyonları
training/.venv/bin/python training/stallion.py extract \
  --phase aggressive --aggressive-source simple --target 500000 \
  --puzzle-ratio 0.20 --sac-ratio 0.50 \
  --zst training/lichess_db_eval.jsonl.zst \
  --output training/data/aggressive.sbin

# Yalnızca Lichess sacrifice bulmacalarının doğrulanmış pozisyonları
training/.venv/bin/python training/stallion.py prepare \
  --phase aggressive --puzzles training/lichess_db_puzzle_sacrifices.csv.zst \
  --target 500000 --output training/data/aggressive-prepared.sbin
```

`prepare` bir Stockfish birleştirme/etiketleme aracı değildir; puzzle CSV/ZST
havuzunu doğrular ve aynı SBIN yazıcısını kullanır. Var olan
`training/data/puzzle_sacrifices.sbin` varsa normal aggressive extraction bunu
örnekleyerek kullanır.

## Self-play veri üretimi

Her oyun için tek kalıcı UCI motor süreci kullanılır. Rastgele açılış ve
`--random-chance` ile çeşitlilik verilebilir. Yalnızca sonuçlanmış oyunların
pozisyonları etiketlenir; `--max-moves` ile kesilen oyunlara sahte beraberlik
etiketi yazılmaz.

```bash
training/.venv/bin/python training/stallion.py datagen \
  --phase base --engine engine/stallion_eas_mac \
  --games 100 --concurrency 4 --depth 8 \
  --random-chance 0.05 --opening-moves 4-8 \
  --output training/data/selfplay.parquet
```

Her çıktı için `.datagen.json` manifesti yazılır. `games`, `depth`,
`max-moves` ve `concurrency` pozitif; `random-chance` 0 ile 1 arasındadır.

## NNUE eğitimi

```bash
training/.venv/bin/python training/stallion.py train \
  --phase base --dataset training/data/base.sbin \
  --output training/runs/base-candidate.nnue \
  --resume engine/nets/base.nnue \
  --epochs 8 --batch-size 2048 --lr 0.00012 \
  --device auto --validation 0.05
```

Motor ve trainer aynı 768 giriş özelliği, 1024 akümülatör ve little-endian
biçimi kullanır. NNUE payload 1,579,010 byte, dosya 64-byte hizalı biçimde
1,579,072 byte'tır. `.nnue` ile devam yalnızca ağı yükler; `.pt` ile devam
optimizer, scheduler ve epoch durumunu da yükler. Checkpoint veri kimliği,
cache sürümü, seed ve validation oranı değişirse reddedilir.

Parquet/SBIN verisi `.feature_cache` altında cache-v7 olarak memory-map edilir.
Train/validation ayrımı iki NNUE accumulator'ının tamamının hash grubuyla
yapılır; aynı gözlenebilir özellik iki bölüme sızmaz. En iyi validation kaybı
`.nnue`, son durum `.pt`, epoch geçmişi `.metrics.json` olarak tutulur.
SBIN içinde geçersiz veya eski biçimli bir kayıt bulunursa eğitim durur.
Checkpoint'ten ek epoch ile devam edildiğinde öğrenme oranı yeni toplam epoch
sayısına göre hesaplanır ve çıktı checkpoint'in en iyi ağıyla eşitlenir.

## Motor maçı ve terfi

```bash
training/.venv/bin/python training/stallion.py match \
  --phase base \
  --candidate training/runs/base-candidate.nnue \
  --baseline engine/nets/base.nnue \
  --engine engine/stallion_eas_mac \
  --book training/openings.epd \
  --games 1000 --concurrency 4 --tc 5+0.1 --sprt \
  --pgnout training/runs/base-match.pgn \
  --json-out training/runs/base-match.json
```

`--games` çift olmalıdır; cutechess her round'da iki renk oynatır. `--book`
motorun kendi opening-book seçeneği değil, eşleştirilmiş cutechess opening
suite'idir; maç sırasında motorun dahili kitabı kapatılır. Candidate ve
baseline dosyaları, motor ve kitap `.inputs` klasörüne kopyalanıp hash'lenir;
maç sürerken kaynak dosya değişirse sonuç güvenilmez sayılır.

`--sprt`, H0=0 ve H1=+15 Elo sınırlarıyla sonuçlanmış bir test ister. Oyun
sayısı dolduğu halde SPRT H1/H0 kararı çıkmayabilir; bu durum
`INCONCLUSIVE`'dir. `--sprt` olmadan sonuç yalnızca tahmindir
(`ESTIMATE_ONLY`). `--promote` için `--sprt` zorunludur ve otomatik terfi
yalnızca SPRT'nin H1 kabulünde gerçekleşir. Aggressive terfisi ayrıca EAS
koşullarını uygular; başarısız veya belirsiz maç adayı korur.

200 oyunluk bir çağrı ön değerlendirme için kullanılabilir; SPRT kararı
çıkması için daha fazla oyun gerekebilir. Daha büyük oyun limitiyle örnek:

```bash
caffeinate -i training/.venv/bin/python training/stallion.py pipeline \
  --phase base --steps extract,train,match \
  --target 5000000 --skip-lines 118406048 \
  --phase-dist 30,35,25,10 --min-depth 16 \
  --lr 0.00012 --batch-size 2048 --epochs 8 \
  --games-base 1000 --tc 5+0.1 --sprt --promote
```

`--skip-lines` yalnızca bilinçli olarak o konumdan yeniden başlamak içindir;
önceki taramanın devamı isteniyorsa kaldırılıp offset dosyası kullanılmalıdır.
Hedefe ulaşılamayan bir taramada pipeline'ın devam etmesi isteniyorsa
`--allow-short-dataset` eklenmelidir.

## EAS, SGS ve SBIN yardımcıları

```bash
training/.venv/bin/python training/stallion.py eas \
  --pgn training/runs/base-match.pgn \
  --output training/runs/base.eas.txt \
  --json-out training/runs/base.eas.json

training/.venv/bin/python training/stallion.py sacrifices \
  --pgn training/runs/base-match.pgn --sac-type 0 --max-moves 80 \
  --output training/runs/base.sacrifices.pgn

training/.venv/bin/python training/sbin_tool.py verify \
  --sbin training/data/base.sbin --all --samples 10000 \
  --report training/data/base.validation.json
```

EAS raporu yerel yaklaşık göstergedir; resmi EAS V6 derecelendirmesi değildir.
SGS tipleri `1,2,3,4,5,9`, `0` hepsi demektir. `sbin_tool.py convert` Parquet
→SBIN dönüşümü, `convert_evals_to_sbin.py` ise tüm eval ZST'sini tek SBIN'e
aktaran geriye dönük uyumluluk aracıdır; faz dengesi ve dedup için normalde
`stallion.py extract` kullanılmalıdır.

`verify --all` bütün kayıtları tarar; `--samples` ayrıca bağımsız
`python-chess` konum ve Python NNUE özellik karşılaştırmasının sayısını
belirler. `--all` kullanılmadığında yalnızca belirtilen örneklem kontrol edilir.
Geçersiz veri varsa CLI çıkış kodu 2 olur. CP değerleri gerçekten etiket
kaynağı olan eval dosyalarında `--check-eval-labels` eklenerek bütün WDL
hedefleri `1 / (1 + 10 ** (-cp / 400))` ölçeğiyle karşılaştırılır.
WDL etiketli self-play veya puzzle dosyalarında bu seçenek kullanılmamalıdır.

SBIN v2 kaydı 32 byte'tır: occupancy 8, taş kodları 16, CP 2, WDL 2,
sıra/rok/EP bayrakları 1, halfmove 1, fullmove/h-dosyası EP bilgisi 2 byte.
Kullanılmayan taş nibble'ları sıfırdır. Fullmove en az 1, en fazla 32767'dir.
Eski v1 dosyalarının sıfır metadata alanı v2 olarak kabul edilmez; eski hamle
sayacı ve kaybedilmiş h-dosyası EP bilgisi kaynak olmadan geri getirilemez.

2026-09-15 tam veri taramasının dosya başına sonuçları ve SHA-256 değerleri
depo kökündeki `SBIN_AUDIT.json` içinde kayıtlıdır. `data/evals.sbin`
3.670.248 standart satranç konum doğrulama hatası ve mevcut eğitim ölçeğinden
farklı WDL etiketleri içerir. Bu dosya yeniden üretilmeden mevcut v2 eğitim
akışında kullanılmamalıdır. Yeni çıktı için:

```bash
training/.venv/bin/python training/convert_evals_to_sbin.py \
  --input training/lichess_db_eval.jsonl.zst \
  --output training/data/evals-validated.sbin --min-depth 14
```

## Derleme ve doğrulama

```bash
make -C engine mac
make -C training
training/.venv/bin/python -m py_compile training/*.py
```

Denetim sonunda 17 regresyon testi ve ASAN/UBSAN kontrolleri başarıyla
çalıştırıldı. Test kaynakları ve test derlemeleri kullanıcının isteğiyle
kaldırıldı. Sonuçlar depo kökündeki `CODE_AUDIT.md` dosyasındadır.

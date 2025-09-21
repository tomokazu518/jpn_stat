# 三重大学の奥村晴彦先生のホームページを参考にしている (感謝)
# https://okumuralab.org/~okumura/stat/shape.html

# ファイルのダウンロード先ディレクトリ作成
dir.create("files", showWarnings = FALSE)

library(tidyverse)
library(sf)

# グラフのテーマ
theme_set(theme_classic(base_family = "IPAexGothic", base_size = 16))


# 国土交通省の国土数値情報ダウンロード＞行政区域データ
# https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N03-v3_0.html
# 　から全国の行政区域データ(shapeファイルを入手する)
# 470MBほどある

download.file(
  "https://nlftp.mlit.go.jp/ksj/gml/data/N03/N03-2021/N03-20210101_GML.zip",
  destfile = "files/N03-20210101_GML.zip",
  method = "curl")

# zipファイルの解凍
  # Rにもunzip()関数が用意されているが，
  # フォルダの区切にバックスラッシュが使われているとうまく展開できないので，
  # ターミナルで展開
system("unzip files/N03-20210101_GML.zip -d files/")

### 以上の作業は，必ずしもRでやらなくても，
### ブラウザでファイルをダウンロードして解凍してもOk

# シェープファイルの読み込み
japan <- read_sf("files/N03-20210101_GML/N03-21_210101.shp")

# 行政区域を都道府県に集約
japan <- japan %>%
  mutate(code = as.numeric(substr(N03_007, 1, 2)))
japan_prefectures <- aggregate(japan, list(japan$code), head, n = 1)

# データの縮小
# km単位に変換　→　1km精度で簡略化　→　緯度経度単位に戻す

japan_prefectures <- japan_prefectures |>
  st_transform("+proj=utm +zone=54 +datum=WGS84 +units=km") |>
  st_simplify(dTolerance = 1) |>
  st_transform("+proj=longlat +ellps=GRS80") |>
  select(code, N03_001, geometry)

# できあがったデータをgeojson形式で保存　400kbくらいまで小さくなった
st_write(japan_prefectures, "japan_prefectures.geojson")

# ggplot2を用いてプロット
graph_japan_prefectures <- japan_prefectures |>
  ggplot() +
  geom_sf()

plot(graph_japan_prefectures)

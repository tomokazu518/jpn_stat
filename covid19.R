library(tidyverse)
library(sf)

# グラフのテーマ
theme_set(theme_classic(base_family = "IPAexGothic", base_size = 16))

# 作成したgeojsonファイルの読込み
# 以下のように奥村先生のホームページからダウンロードしても良い
# japan_prefecture = read_sf("https://okumuralab.org/~okumura/stat/data/japan.geojson")

japan_prefectures <- read_sf("japan_prefectures.geojson")

# Covid-19　都道府県別累積死亡者数データのダウンロード (厚労省のオープンデータ)

covid_deaths <- read.csv("https://covid19.mhlw.go.jp/public/opendata/deaths_cumulative_daily.csv")

# 最新の日付のデータを抽出して地理情報とマージ
# Covid-19のオープンデータの更新は2023/5/9で終了 (5/9は都道府県によって欠損しているので5/8のデータを利用)

covid <- cbind(
  japan_prefectures,
  covid_deaths[nrow(covid_deaths) - 1, ] |>
    select(- ALL) |>
    pivot_longer(cols = - Date)
)

graph_covid <- covid |>
  ggplot(aes(fill = value)) +
  geom_sf() +
  scale_fill_continuous("死亡者数(累積)", low = "white", high = "red")

plot(graph_covid)
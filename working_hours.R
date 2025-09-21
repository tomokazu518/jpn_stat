library(tidyverse)

# グラフのテーマ
theme_set(theme_classic(base_family = "IPAexGothic", base_size = 16))

# ファイルのダウンロード先ディレクトリ作成
dir.create("files", showWarnings = F)

# 毎月勤労統計調査　　長期時系列表	実数・指数累積データ
# 表番号1 実数・指数累積データ　実数

download.file(
  "https://www.e-stat.go.jp/stat-search/file-download?statInfId=000032189776&fileKind=1",
  destfile = "files/maikin.csv",
  method = "curl"
)

maikin <- read.csv(
  "files/maikin.csv",
  fileEncoding = "shift-jis"
) |>
  filter(
    `年` >= 1993 &
      `月` == "CY" &
      substr(`産業分類`, 1, 2) == "TL" &
      `規模` == "T"
  ) |>
  mutate(`総実労働時間` = `総実労働時間` * 12)


w_status <- c("就業形態計", "一般労働者", "パートタイム労働者")

graph_hours <- maikin |>
  mutate(`総実労働時間` = `総実労働時間` * 12) |>
  ggplot(
    aes(
      x = `年`,
      y = `総実労働時間`,
      color = as.factor(`就業形態`)
    )
  ) +
  geom_line() +
  geom_point() +
  scale_color_hue(
    name = "就業形態",
    labels = w_status
  )

plot(graph_hours)
## ---- setup ----

library(tidyverse)
library(readxl)
library(RColorBrewer)

## ---- data ----

# 失業率（月次） 季節調整値
# 労働力調査　基本集計　全都道府県　長期時系列データ　季節調整値　表番号1-a-9
download.file(
  "https://www.e-stat.go.jp/stat-search/file-download?statInfId=000031831365&fileKind=0",
  destfile = "files/unemployment.xlsx",
  method = "curl"
)

# E71:L 列は固定（5〜12 列目）、71 行目から下は行数が増えても読み込む
labor_unemployment <-
  read_excel("files/unemployment.xlsx",
    sheet = "季節調整値",
    range = cell_limits(c(71, 5), c(NA, 12)),
    col_names = F
  )
labor_unemployment <-
  rename(labor_unemployment,
    total = ...1, a15_64 = ...2, a15_24 = ...3, a25_34 = ...4, a35_44 = ...5,
    a45_54 = ...6, a55_64 = ...7, a65over = ...8
  ) |>
  filter(!is.na(total))

# 1975-01 起点の月次（行数は labor_unemployment に合わせる）
month <- seq(as.Date("1975-01-01"), by = "month", length.out = nrow(labor_unemployment))

# 日付と失業率の結合，必要なデータを残してlong形式に

unemployment <- cbind(month, labor_unemployment) |>
  mutate(month = as.Date(month)) |>
  select(month, total, a15_24, a25_34, a35_44, a45_54) |>
  mutate(across(-month, as.numeric)) |>
  pivot_longer(-month)

## ---- plot_recent ----

### 直近のグラフ

graph_unemployment_recent <- unemployment |>
  filter(month >= "2019-01-01") |>
  ggplot(aes(x = month, y = value, color = name)) +
  geom_line() +
  geom_point(size = 1) +
  scale_color_manual(
    values = c(brewer.pal(4, "Pastel1"), "black"),
    name = "年齢階級",
    labels = c(
      a15_24 = "15〜24歳",
      a25_34 = "25〜34歳",
      a35_44 = "35〜44歳",
      a45_54 = "45〜54歳",
      total  = "年齢計"
    )
  ) +
  scale_x_date(
    breaks = \(lims) {
      seq.Date(
        as.Date("2019-01-01"),
        floor_date(lims[2], "year"),
        by = "12 months"
      )
    },
    date_labels = "%Y"
  ) +
  labs(x = "", y = "失業率") +
  theme_classic(base_family = "IPAexGothic", base_size = 16)

plot(graph_unemployment_recent)


## ---- plot_longtime ----

### 長期のグラフ

graph_unemployment_longtime <- unemployment |>
  ggplot(aes(x = month, y = value, color = name)) +
  geom_line() +
  scale_color_manual(
    values = c(brewer.pal(4, "Pastel1"), "black"),
    name = "年齢階級",
    labels = c(
      a15_24 = "15〜24歳",
      a25_34 = "25〜34歳",
      a35_44 = "35〜44歳",
      a45_54 = "45〜54歳",
      total  = "年齢計"
    )
  ) +
  labs(x = "", y = "失業率") +
  theme_classic(base_family = "IPAexGothic", base_size = 16)

plot(graph_unemployment_longtime)

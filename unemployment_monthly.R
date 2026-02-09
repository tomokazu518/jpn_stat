## ---- setup ----

library(tidyverse)
library(readxl)
library(RColorBrewer)

# グラフのテーマ
theme_set(theme_classic(base_family = "IPAexGothic", base_size = 16))

## ---- data ----

# 失業率（月次） 季節調整値
# 労働力調査　基本集計　全都道府県　長期時系列データ　季節調整値　表番号1-a-9
download.file(
  "https://www.e-stat.go.jp/stat-search/file-download?statInfId=000031831365&fileKind=0",
  destfile = "files/unemployment.xlsx",
  method = "curl"
)

labor_unemployment <-
  read_excel("files/unemployment.xlsx",
    sheet = "季節調整値", range = "E71:L677", col_names = F
  )
labor_unemployment <-
  rename(labor_unemployment,
    total = ...1, a15_64 = ...2, a15_24 = ...3, a25_34 = ...4, a35_44 = ...5,
    a45_54 = ...6, a55_64 = ...7, a65over = ...8
  ) |>
  filter(!is.na(total))

# 1975-01 ~ 2025-07までの日付作成

for (i in 1975:2025) {
  if(i == 2025){tmp <- paste(i, "-", seq(1:7), "-01", sep = "")} 
  else{tmp <- paste(i, "-", seq(1:12), "-01", sep = "")}
  if (i == 1975) {
    month <- tmp
  } else {
    month <- c(month, tmp)
  }
}

month <- month[seq_len(nrow(labor_unemployment))]

# 日付と失業率の結合，必要なデータを残してlong形式に

unemployment <- cbind(month, labor_unemployment) %>%
  mutate(month = as.Date(month)) %>%
  select(month, total, a15_24, a25_34, a35_44, a45_54) %>%
  pivot_longer(-month)

## ---- graph ----

### 直近のグラフ

graph_unemployment_recent <- unemployment %>%
  filter(month >= "2019-01-01") %>%
  ggplot(aes(x = month, y = value, color = name)) +
  geom_line() +
  geom_point() +
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
    breaks = seq.Date(
      from = as.Date("2019-01-01"),
      to = as.Date("2025-01-01"),
      by = "12 months"
    ),
    date_labels = "%Y"
  ) +
  labs(x = "", y = "失業率")

plot(graph_unemployment_recent)


## ---- long-term ----

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
  labs(x = "", y = "失業率")

plot(graph_unemployment_longtime)

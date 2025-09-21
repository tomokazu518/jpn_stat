library(tidyverse)
library(estatapi)
library(patchwork)

# e-statのappIDが必要
#   以下のページで利用申請(無料)をすればだれでも入手できる
#   https://www.e-stat.go.jp/api/
# appID = "入手したappIDをここに設定（行頭の#を外す）"

# グラフのテーマ
theme_set(theme_classic(base_family = "IPAexGothic", base_size = 16))

# e-Statからデータ取得
estat_vital <- estat_getStatsData(
  appId = appID,
  statsDataId = "0003411595", # 人口動態調査・人口動態統計・確定数・出生・4−1・上巻
  cdCat01 = c("00100", "00150")
)

vital <- estat_vital |>
  mutate(
    year = as.numeric(time_code) / 1000000,
    name = `出生数・出生率・出生性比`
  ) |>
  select(year, name, value) |>
  filter(year >= 1947) |>
  pivot_wider(names_from = name)

# グラフ作成
birth <- vital |>
  ggplot(
    aes(
      x = year,
      y = `出生数_総数`
    )
  ) +
  geom_bar(
    stat = "identity",
    color = "gray",
    fill = "lightgray"
  ) +
  geom_text(
    aes(
      label = paste(
        format(`出生数_総数`, big.mark = ","),
        "\n (", year, ")",
        sep = ""
      )
    ),
    nudge_y = 50000,
    color = "red",
    size = 4,
    data = subset(vital, year %in% c(1949, 1966, 1989, 2005, 2023))
  ) +
  labs(
    x = "",
    y = "出生数"
  ) +
  scale_y_continuous(
    labels = scales::label_comma(),
    limits = c(0, 3500000)
  ) +
  theme_classic()

tfr <- vital |>
  ggplot(
    aes(
      x = year,
      y = `合計特殊出生率`
    )
  ) +
  geom_line(color = "gray") +
  geom_point(
    size = 1,
    color = "blue"
  ) +
  ylim(0.5, 5) +
  geom_text(
    aes(
      label = paste(`合計特殊出生率`, "\n (", year, ")", sep = "")
    ),
    nudge_y = -0.4,
    color = "red",
    size = 4,
    data = subset(vital, year %in% c(1947, 1966, 1989, 2005, 2023))
  ) +
  labs(
    x = "年",
    y = "合計特殊出生率"
  ) +
  theme_classic()

# patchworkパッケージを使ったプロット
graph <- birth + tfr + plot_layout(ncol = 1)
plot(graph)

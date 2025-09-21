library(tidyverse)
library(estatapi)

# e-statのappIDが必要
#   以下のページで利用申請(無料)をすればだれでも入手できる
#   https://www.e-stat.go.jp/api/
# appID = "入手したappIDをここに設定（行頭の#を外す）"

# グラフのテーマ
theme_set(theme_classic(base_family = "IPAexGothic", base_size = 16))

# e-statからデータ取得 statID 0003445133
# 	国勢調査 令和２年国勢調査 人口等基本集計
#       （主な内容：男女・年齢・配偶関係，世帯の構成，
#           住居の状態，母子・父子世帯，国籍など）
#   表番号2-1-1
#   男女，年齢（各歳），国籍総数か日本人別人口
#       －全国，都道府県，21大都市，特別区，人口50万以上の市

estat_census2020 <- estat_getStatsData(
  appId = appID,
  statsDataId = "0003445133",
  cdCat01 = "0", # 総人口
  cdCat02 = c("1", "2"), # 男・女
  cdArea = "00000" # 全国
)

# 人口ピラミッドを描くためのデータの整理
pop2020 <- estat_census2020 |>
  filter(
    str_detect(cat03_code, "00[1-9]|0[1-9][0-9]|100|R6")
  ) |>
  mutate(
    cat03_code = str_replace(cat03_code, "R6", "101"), # 100歳以上は100歳に
    age = as.numeric(cat03_code) - 1,
    value = case_when(
      `男女` == "男" ~ -value / 1000, # 男性の人口はマイナス(人口ピラミッドを描くため)
      `男女` == "女" ~ value / 1000   # 人口は1000人単位に
    )
  ) |>
  select(age, `男女`, value)

# 人口ピラミッドの描画
graph_pop2020 <- pop2020 |>
  ggplot(
    aes( # x軸に年齢，y軸に人口をとり性別で塗り分け(縦横は入れ替える)
      x = age,
      y = value,
      fill = `男女`
    )
  ) +
  geom_bar(stat = "identity", color = "black") +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(
    breaks = seq(- 1000, 1000, 500),
    labels = abs(seq(- 1000, 1000, 500))
  ) +
  scale_fill_hue(
    name = "",
    labels = c(
      "F" = "女",
      "M" = "男"
    )
  ) +
  labs(
    y = "人口(単位：1,000人)",
    x = "年齢"
  ) +
  coord_flip() + # グラフの回転 (縦軸と横軸の入替)
  theme_classic()

plot(graph_pop2020)
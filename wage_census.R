library(tidyverse)
library(estatapi)

# e-statのappIDが必要
#   以下のページで利用申請(無料)をすればだれでも入手できる
#   https://www.e-stat.go.jp/api/
# appID = "入手したappIDをここに設定（行頭の#を外す）"

# グラフのテーマ
theme_set(theme_classic(base_family = "IPAexGothic", base_size = 16))

standard <- estat_getStatsData(
  appId = appID,
  cdTab = c("42", "44"),
  statsDataId = "0003447178", 
  cdCat01 = c("02", "03"),         # 性別　男・女
  cdCat02 = c("03", "06", "07"),   # 学歴　高校・大学・大学院
  cdCat05 = "01",                  # 産業計
  cdTime = c("2023000000")
) |>
  select(
    表章項目,
    性別_基本,
    cat03_code,
    `学歴_基本８区分（2020年～）`,
    企業規模_基本,
    value
  ) |>
  mutate(
    age = as.numeric(cat03_code) + 14,
    gender = factor(
      性別_基本,
      levels = c("男", "女")
    ),
    education = factor(
      `学歴_基本８区分（2020年～）`,
      levels = c("大学院", "大学", "高校")
    ),
    firm_size = factor(
      企業規模_基本,
      levels = c(
        "企業規模計（10人以上）",
        "1,000人以上",
        "100～999人",
        "10～99人"
      )
    )
  ) |>
  pivot_wider(
    names_from = "表章項目",
    values_from = "value"
  ) |>
  mutate(
    wage = (所定内給与額 * 12 + 年間賞与その他特別給与額) / 10
  )

graph_male <- standard |>
  filter(
    firm_size == "企業規模計（10人以上）",
    age <= 60,
    ! is.na(wage)
  ) |>
  ggplot(aes(x = age, y = wage, color =  education)) +
  geom_line() +
  geom_point() +
  scale_color_hue(name = "学歴") +
  labs(
    title = "標準労働者の学歴別賃金プロファイル",
    x = "年齢",
    y = "年収(万円)"
  ) +
  facet_wrap(~ gender) +
  theme_classic()

graph_female <- standard |>
  filter(
    education == "大学",
    firm_size != "企業規模計（10人以上）",
    age <= 60,
    ! is.na(wage)
  ) |>
  ggplot(aes(x = age, y = wage, color = firm_size)) +
  geom_line() +
  geom_point() +
  scale_color_hue(name = "企業規模") +
  labs(
    title = "大学卒標準労働者の企業規模別賃金プロファイル",
    x = "年齢",
    y = "年収(万円)"
  ) +
  facet_wrap(~ gender) +
  theme_classic()

plot(graph_male + graph_female)
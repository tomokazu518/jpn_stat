library(tidyverse)
library(estatapi)
library(patchwork)

# e-statのappIDが必要
#   以下のページで利用申請(無料)をすればだれでも入手できる
#   https://www.e-stat.go.jp/api/
# appID = "入手したappIDをここに設定（行頭の#を外す）"

# グラフのテーマ
theme_set(theme_classic(base_family = "IPAexGothic", base_size = 16))

# 名目・実質・GDPデフレータ

nominal_gdp <- estat_getStatsData(
  appId = appID,
  statsDataId = "0004028475",
  cdTab = "11",
  cdCat01 = "47"
) |>
  mutate(
    year = as.numeric(time_code) / 1000000,
    `名目GDP` = value
  ) |>
  select(year, `名目GDP`)

real_gdp <- estat_getStatsData(
  appId = appID,
  statsDataId = "0004028480",
  cdTab = "11",
  cdCat01 = "67"
) |>
  mutate(
    year = as.numeric(time_code) / 1000000,
    `実質GDP` = value
  ) |>
  select(year, `実質GDP`)

gdp <- merge(nominal_gdp, real_gdp, by = "year")

gdp <- gdp |>
  mutate(`GDPデフレータ` = `名目GDP` / `実質GDP` * 100) |>
  pivot_longer(cols = - year)

g1 <- gdp |>
  filter(name != "GDPデフレータ") |>
  ggplot(aes(x = year, y = value, color = name)) +
  geom_point() +
  geom_line() +
  labs(y = "GDP(10億円)") +
  scale_color_hue(name = "") +
  theme_bw(base_family = "IPAexGothic") +
  theme(legend.position = "bottom")

g2 <- gdp |>
  filter(name == "GDPデフレータ") %>%
  ggplot(aes(x = year, y = value, color = name)) +
  geom_point() +
  geom_line() +
  labs(y = "GDPデフレータ") +
  scale_color_hue(name = "") +
  theme_bw(base_family = "IPAexGothic") +
  theme(legend.position = "bottom")

plot(g1 + g2)

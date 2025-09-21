library(tidyverse)
library(estatapi)
library(RColorBrewer)
library(stringi)

# e-statのappIDが必要
#   以下のページで利用申請(無料)をすればだれでも入手できる
#   https://www.e-stat.go.jp/api/
# appID = "入手したappIDをここに設定（行頭の#を外す）"

# グラフのテーマ
theme_set(theme_classic(base_family = "IPAexGothic", base_size = 16))

# 国勢調査・時系列データ・人口の労働力状態，就業者の産業・職業　表番号7
# 職業（大分類），男女別15歳以上就業者数－全国（平成7年～令和2年）
occupation_latest <-
  estat_getStatsData(
    appId = appID,
    statsDataId = "0003410408",
    cdTab = "2020_45",      # 表章項目 == "人口構成比 [職業別]"
    cdCat01From = 110,      # 職業分類
    cdCat01To   = 220,      # 
    cdCat02     = 100       # 男女_時系列 == "総数")
  ) |>
  filter(substring(time_code, 9, 10) == "00") |>
  mutate(
    year = as.numeric(time_code) / 1000000,
    occupation = `職業大分類2015`
  ) |>
  select(year, occupation, value)

# 【参考】職業（旧大分類），男女別15歳以上就業者数及び産業別割合
# －全国（昭和25年～平成17年）※平成21年12月改訂前
occupation_old <-
  estat_getStatsData(
    appId = appID, 
    statsDataId = "0003410409",
    cdTab = "2020_45",      # 表章項目 == "割合"
    cdCat01From = 110,      # 職業分類
    cdCat01To   = 200,      # 
    cdCat02     = 100       # 男女_時系列 == "総数")
  ) |>
  mutate(
    year = as.numeric(time_code) / 1000000,
    occupation = `職業大分類（旧大分類H21改定前）`
  ) |>
  select(year, occupation, value) |>
  filter(year <= 1990)

# 新旧統合分類を作成

occupations <-
  c(
    "専門的・技術的職業",
    "管理的職業",
    "事務職",
    "販売職",
    "サービス職",
    "保安職",
    "農林漁業作業",
    "運輸・通信(1990年以前)",
    "輸送・機械運転",
    "生産工程(1990年以前は労務作業を含む)",
    "建設・採掘",
    "運搬・清掃・包装等",
    "分類不能"
  )

## 1995年以降

occupation_since1995 <- occupation_latest |>
  pivot_wider(names_from = occupation) |>
  mutate(
    `専門的・技術的職業` = `Ｂ専門的・技術的職業従事者`,
    `管理的職業` = `Ａ管理的職業従事者`,
    `事務職` = `Ｃ事務従事者`,
    `販売職` = `Ｄ販売従事者`,
    `サービス職` = `Ｅサービス職業従事者`,
    `保安職` = `Ｆ保安職業従事者`,
    `農林漁業作業` = `Ｇ農林漁業従事者`,
    `輸送・機械運転` = `Ｉ輸送・機械運転従事者`,
    `建設・採掘` = `Ｊ建設・採掘従事者`,
    `運搬・清掃・包装等` = `Ｋ運搬・清掃・包装等従事者`,
    `生産工程(1990年以前は労務作業を含む)` = `Ｈ生産工程従事者`,
    `分類不能` = `Ｌ分類不能の職業`
  ) |>
  select(year, `専門的・技術的職業`:`分類不能`) |>
  pivot_longer(- year, names_to = "occupation") |>
  mutate(occupation = factor(occupation, levels = occupations))


## 1990年以前

occupation_before1995 <- occupation_old |>
  pivot_wider(names_from = occupation) |>
  mutate(
    `専門的・技術的職業` = `A専門的・技術的職業従事者`,
    `管理的職業` = `B管理的職業従事者`,
    `事務職` = `C事務従事者`,
    `販売職` = `D販売従事者`,
    `サービス職` = `Eサービス職業従事者`,
    `保安職` = `F保安職業従事者`,
    `農林漁業作業` = `G農林漁業作業者`,
    `運輸・通信(1990年以前)` = `H運輸・通信従事者`,
    `生産工程(1990年以前は労務作業を含む)` = `I生産工程・労務作業者`,
    `分類不能` = `J分類不能の職業`
  ) |>
  select(year, `専門的・技術的職業`:`分類不能`) |>
  pivot_longer(-year, names_to = "occupation") |>
  mutate(occupation = factor(occupation, levels = occupations))

# Plot

occupation <- rbind(occupation_before1995, occupation_since1995)

clr <- c(brewer.pal(7, "Accent"), brewer.pal(5, "YlOrRd"), "#FFFFFF")

graph_occupation <- occupation |>
  mutate(year = as.factor(year)) |>
  ggplot(aes(x = year, y = value, fill = occupation)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(name = "職業", values = clr) +
  labs(x = "年", y = "") +
  theme_classic() +
  theme(text = element_text(size = 11))

plot(graph_occupation)
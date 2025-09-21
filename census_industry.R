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

# 産業（大分類），男女別15歳以上就業者数
# －全国（平成7年～令和2年）※平成19年11月改訂後
industry_latest <-
  estat_getStatsData(
    appId = appID,
    statsDataId = "0003410395",
    cdTab = "2020_44",     # 表章項目 == "人口構成比 [産業別]"
    cdCat01From = 120,     # 産業分類
    cdCat01To   = 330,     # 　総数，小計，再掲などを除く
    cdCat02     = 100      # 男女_時系列 == "総数"
  ) |>
  filter(substring(time_code, 9, 10) == "00") |>
  mutate(
    year = as.numeric(time_code) / 1000000,
    industry = `産業大分類2015`
  ) |>
  select(year, industry, value)

#【参考】産業（旧大分類），男女別15歳以上就業者数及び産業別割合
# －全国（大正9年～平成12年）※平成14年3月改訂前
industry_old <-
  estat_getStatsData(
    appId = appID,
    statsDataId = "0003410396",
    cdTab = "2020_44",      # 表章項目 == "人口構成比 [産業別]"
    cdCat01From = 120,      # 産業分類
    cdCat01Tob  = 330,      # 　総数を除く
    cdCat02     = 100,      # 男女_時系列 == "総数"
  ) |>
  filter(cat01_code != 150 & cat01_code != 190) %>%  # 小計(第n次産業)を除く
  mutate(
    year = as.numeric(time_code) / 1000000,
    industry = `産業大分類（平成14年3月改訂前）`
  ) |>
  select(year, industry, value) |>
  filter(year <= 1990)

# 新旧分類を統合して，産業を以下のように分類

industries <-
  c(
    "農林漁業",
    "鉱業",
    "建設業",
    "製造業",
    "電気・ガス・熱供給・水道業",
    "情報通信業",
    "運輸業(1990年以前は通信業含む)",
    "卸売・小売(1990年以前は飲食店含む)",
    "金融・保険業",
    "不動産業(1995年以降は物品賃貸業含む)",
    "サービス業(1995年以前)",
    "学術研究，専門・技術サービス業",
    "宿泊業，飲食サービス業",
    "生活関連サービス業，娯楽業",
    "教育，学習支援業",
    "医療，福祉",
    "複合サービス事業",
    "その他のサービス業",
    "公務",
    "分類不能"
  )


## 1995年以降（新産業分類）

industry_since1995  <- industry_latest  |>
  pivot_wider(names_from = industry) |>
  mutate(
    `農林漁業` = `Ａ農業，林業` + `Ｂ漁業`,
    `鉱業` = `Ｃ鉱業，採石業，砂利採取業`,
    `建設業` = `Ｄ建設業`,
    `製造業` = `Ｅ製造業`,
    `電気・ガス・熱供給・水道業` = `Ｆ電気・ガス・熱供給・水道業`,
    `情報通信業` = `Ｇ情報通信業`,
    `運輸業(1990年以前は通信業含む)` = `Ｈ運輸業，郵便業`,
    `卸売・小売(1990年以前は飲食店含む)` = `Ｉ卸売業，小売業`,
    `金融・保険業` = `Ｊ金融業，保険業`,
    `不動産業(1995年以降は物品賃貸業含む)` = `Ｋ不動産業，物品賃貸業`,
    `学術研究，専門・技術サービス業` = `Ｌ学術研究，専門・技術サービス業`,
    `宿泊業，飲食サービス業` = `Ｍ宿泊業，飲食サービス業`,
    `生活関連サービス業，娯楽業` = `Ｎ生活関連サービス業，娯楽業`,
    `教育，学習支援業` = `Ｏ教育，学習支援業`,
    `医療，福祉` = `Ｐ医療，福祉`,
    `複合サービス事業` = `Ｑ複合サービス事業`,
    `その他のサービス業` = `Ｒサービス業（他に分類されないもの）`,
    `公務` = `Ｓ公務（他に分類されるものを除く）`,
    `分類不能` = `Ｔ分類不能の産業`
  ) |>
  select(year, `農林漁業`:`分類不能`) |>
  pivot_longer(-year, names_to = "industry") |>
  mutate(industry = factor(industry, levels = industries))

## 1990年以前（旧産業分類）

industry_before1995 <- industry_old |>
  pivot_wider(names_from = industry) |>
  mutate(
    `農林漁業` = `A農業` + `B林業` + `C漁業`,
    `鉱業` = `D鉱業`,
    `建設業` = `E建設業`,
    `製造業` = `F製造業`,
    `電気・ガス・熱供給・水道業` = `G電気・ガス・熱供給・水道業`,
    `運輸業(1990年以前は通信業含む)` = `H運輸・通信業`,
    `卸売・小売(1990年以前は飲食店含む)` = `I卸売・小売業，飲食店`,
    `金融・保険業` = `J金融・保険業`,
    `不動産業(1995年以降は物品賃貸業含む)` = `K不動産業`,
    `サービス業(1995年以前)` = `Lサービス業`,
    `公務` = `M公務(他に分類されないもの)`,
    `分類不能` = `N分類不能の産業`
  ) |>
  select(year, `農林漁業`:`分類不能`) |>
  pivot_longer(-year, names_to = "industry") |>
  mutate(industry = factor(industry, levels = industries))

#### Plot

industry <- rbind(industry_since1995, industry_before1995)

clr <- c(brewer.pal(11, "Paired"), brewer.pal(8, "YlOrBr"), "#FFFFFF")

graph_industry <- industry %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(x = year, y = value, fill = industry)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(name = "産業", values = clr) +
  labs(x = "年", y = "") +
  theme_classic() +
  theme(text = element_text(size = 11))

plot(graph_industry)
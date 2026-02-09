## ---- setup ----

library(tidyverse)
library(estatapi)
library(patchwork)
library(RColorBrewer)

# e-statのappIDが必要
#   以下のページで利用申請(無料)をすればだれでも入手できる
#   https://www.e-stat.go.jp/api/
# appID = "入手したappIDをここに設定（行頭の#を外す）"

# グラフのテーマ
theme_set(theme_classic(base_family = "IPAexGothic", base_size = 16))

## ---- data ----

# データ取得
# 四半期GDP速報のstatsDataIdは"0003109750"
qgdp <- estat_getStatsData(
  appId = appID,
  statsDataId = "0003109750"
)

# データの整理

# 必要なものだけ残す
list <- c(
  "国内総生産(支出側)",
  "民間最終消費支出", 
  "民間企業設備",
  "民間住宅",
  "民間在庫変動",
  "政府最終消費支出",
  "公的固定資本形成", 
  "公的在庫変動",
  "財貨・サービス_純輸出", "開差"
)

qgdp <- qgdp |>
  filter(国内総生産_実質季節調整系列 %in% list) |>
  select(国内総生産_実質季節調整系列, time_code, value)

colnames(qgdp) <- c("variable", "time_code", "value")

# データ作成

qgdp <- qgdp |>
  pivot_wider(names_from = variable) |> # wide型に変換
  rename(      # 扱いやすいように変数名を英数字に
    gdp = `国内総生産(支出側)`,
    consumption = `民間最終消費支出`,
    equip = `民間企業設備`,
    housing = `民間住宅`,
    stock = `民間在庫変動`,
    government = `政府最終消費支出`,
    pub_capital = `公的固定資本形成`,
    pub_stock = `公的在庫変動`,
    net_export = `財貨・サービス_純輸出`,
    error = `開差`
  ) |>
  mutate(
    number = row_number(), # 四半期に通し番号を振る
    year = str_sub(time_code, 1, 4), # time_codeから年の部分だけを抽出(グラフ作成時にラベルとして利用)
    # 投資の項目をまとめる
    private_investment = equip + housing + stock,
    public_investment = pub_capital + pub_stock,
    # 経済成長率，寄与度の計算
    growth = gdp / lag(gdp, 1) - 1,
    contF_consumption =
      (consumption - lag(consumption, 1)) / lag(gdp, 1),
    contE_private_investment =
      (private_investment - lag(private_investment, 1)) / lag(gdp, 1),
    contD_government =
      (government - lag(government, 1)) / lag(gdp, 1),
    contC_public_investment =
      (public_investment - lag(public_investment, 1)) / lag(gdp, 1),
    contB_net_export =
      (net_export - lag(net_export, 1)) / lag(gdp, 1),
    contA_error =
      (error - lag(error, 1)) / lag(gdp, 1)
    # 寄与度は積み上げ棒グラフの順でA~Fの記号を振る
  )

# グラフ作成に必要な経済成長率と寄与度だけ残してlong型に変換
growth <- select(
  qgdp,
  number,
  year,
  time_code,
  growth,
  contF_consumption,
  contE_private_investment,
  contD_government,
  contC_public_investment,
  contB_net_export,
  contA_error
) |>
  pivot_longer(! c(time_code, number, year))

# 配色
palette <- c(
  "#999999", 
  "#E69F00",
  "#56B4E9",
  "#009E73",
  "#F0E442",
  "#0072B2",
  "#D55E00",
  "#CC79A7"
)

## ---- plot_gdp ----

# グラフの凡例
legends <- c(
  contA_error = "開差",
  contB_net_export = "純輸出",
  contC_public_investment = "公的投資",
  contD_government = "政府支出",
  contE_private_investment = "民間投資",
  contF_consumption = "民間消費"
)

# グラフを描く期間の指定
length <- nrow(qgdp) # データの長さ
start <- c(5, 53, 101) # 期間の始めと終わりを通し番号で指定
end <- c(52, 100, length) # startは第一四半期(4の倍数+1)になるように

# グラフの作成
for (i in seq_along(start)) {

  # 期間の限定
  graphdata <- filter(growth, number >= start[i] & number <= end[i])

  # x軸の目盛りラベル（2年おきにラベル）
  years <- unique(graphdata$year)
  xlabels <- years[seq(1, length(years), by = 2)]  # 2年おき

  # ggplot

  g <- ggplot() +
    geom_bar(
      data = filter(graphdata, name != "growth"),
      stat = "identity",
      color = "black", 
      width = 0.7, # 積み上げ棒グラフ
      aes(
        x = number,
        y = value, 
        fill = name
      )
    ) +
    scale_fill_manual( # 色と凡例の設定
      name = "寄与度",
      values = palette,
      labels = legends
    ) +
    geom_line(# 折れ線グラフ
      data = filter(graphdata, name == "growth"),
      aes(
        x = number,
        y = value,
        color = name
      ),
      linewidth = 0.8
    ) +
    geom_point(# マーカー
      data = filter(graphdata, name == "growth"),
      aes(
        x = number,
        y = value,
        color = name
      )
    ) +
    scale_color_manual( # 色と凡例の設定
      name = "",
      values = "red",
      labels = "経済成長率"
    ) +
    scale_x_continuous(
      name = "", # x軸のラベルの設定
      breaks = seq(start[i], end[i], by = 8),  # 2年おき（8四半期おき）
      labels = xlabels
    ) +
    scale_y_continuous(name = "") +
    ggtitle("四半期GDP速報")

  plot(g)
}
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
theme_set(theme_classic(base_family = "IPAexGothic", base_size = 14))

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
  "#D9D9D9",
  "#F6D6A8",
  "#CFE7F6",
  "#BFE9D8",
  "#FAF0B3",
  "#C7D9F6",
  "#F6C3B1",
  "#E9C7E9"
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

# グラフは4分割 (期間の始めと終わりを通し番号で指定)
# 1995年の第一四半期 (number=5)から始める
# startは第一四半期(4の倍数+1)になるように、区間幅を4の倍数に丸める
length <- nrow(qgdp) # データの長さ
total_quarters <- length - 4  # number=5 から length までの四半期数
chunk <- ceiling((total_quarters / 4) / 4) * 4 # 1枚のグラフに表示する四半期数 (必ず4の倍数に)
start <- 5 + (0:3) * chunk
end <- pmin(start + chunk - 1, length)
end[4] <- length

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
      linewidth = 0.6
    ) +
    geom_point(# マーカー
      data = filter(graphdata, name == "growth"),
      aes(
        x = number,
        y = value,
        color = name
      ),
      size = 0.5
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
    theme(
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    )
  plot(g)
}
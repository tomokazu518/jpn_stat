library(tidyverse)
library(readxl)
library(RColorBrewer)
library(patchwork)

plot_rousai <- function(df, ymax) {
    ind <- df %>%
        filter(year == max(year)) %>%
        pull(industry) %>%
        as.character()

    denied <- df %>%
        group_by(year) %>%
        summarize(
            `支給決定件数` = sum(`決定件数`) - sum(`支給決定件数`),
            .groups = "drop"
        ) %>%
        mutate(industry = "不支給", `請求件数` = 0)

    ind_lab <- c("不支給", ind)
    df <- bind_rows(df, denied) %>%
        mutate(industry = factor(industry, levels = ind_lab))
    ind_lab[2] <- "農業，林業，漁業， \n 鉱業，採石業，砂利採取業"

    clr <- c("white", colorRampPalette(brewer.pal(11, "Spectral"))(length(ind)))

    g1 <- df %>%
        ggplot(aes(x = year, y = `請求件数`, fill = industry)) +
        geom_bar(stat = "identity", color = "black") +
        ylim(0, ymax) +
        labs(x = "") +
        scale_fill_manual(name = "産業", values = clr) +
        theme_classic() +
        theme(legend.position = "none")

    g2 <- df %>%
        ggplot(aes(x = year, y = `支給決定件数`, fill = industry)) +
        geom_bar(stat = "identity", color = "black") +
        ylim(0, ymax) +
        labs(x = "") +
        scale_fill_manual(name = "産業", values = clr, labels = ind_lab) +
        theme_classic()

    g1 + g2
}


data <- read_csv(
    "https://raw.githubusercontent.com/tomokazu518/jpn_stat/main/labor_compensation.csv"
)

brain_heart <- filter(data, type == "脳・心臓疾患")
mental <- filter(data, type == "精神疾患")

plot(plot_rousai(brain_heart, 1000))
plot(plot_rousai(mental, 4000))

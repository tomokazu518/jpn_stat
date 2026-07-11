library(tidyverse)
library(readxl)

dir.create("files", showWarnings = FALSE)
file_name <- "files/d26_T6-01.xlsx"

if (!file.exists(file_name)) {
    download.file(
        "https://www.jil.go.jp/kokunai/statistics/databook/2026/06/d26_T6-01.xlsx",
        destfile = file_name,
        method = "curl"
    )
}

years_1 <-
    read_excel(
        file_name,
        sheet = "6-1 (s1)",
        range = "D5:K5",
        col_names = F
    )

years_2 <-
    read_excel(
        file_name,
        sheet = "6-1 (s2)",
        range = "D5:K5",
        col_names = F
    )

years <- gsub("年", "", cbind("country", years_1, years_2))


data_1 <-
    read_excel(
        file_name,
        sheet = "6-1 (s1)",
        range = "D10:K26",
        col_names = F
    )

data_2 <-
    read_excel(
        file_name,
        sheet = "6-1 (s2)",
        range = "D10:K26",
        col_names = F
    )

countries <-
    read_excel(
        file_name,
        sheet = "6-1 (s1)",
        range = "M10:M26",
        col_names = F
    )

data <- cbind(countries, data_1, data_2)
colnames(data) <- years

data <- mutate_all(data, ~ gsub(., pattern = "－", replacement = NA))

selected_countries <- c("JPN", "USA", "UK", "DEU", "SWE", "KOR")

data <- data %>%
    filter(country %in% selected_countries) %>%
    pivot_longer(cols = -country, names_to = "year") %>%
    mutate(
        year = as.integer(year),
        value = as.integer(value),
        country = factor(country, levels = selected_countries)
    )

data %>%
    ggplot(aes(
        x = year,
        y = value,
        color = country,
        shape = country,
        group = country
    )) +
    geom_line() +
    geom_point(size = 3) +
    labs(x = "", y = "年間総実労働時間") +
    scale_color_discrete(name = "") +
    scale_shape_discrete(name = "") +
    theme_classic(base_size = 16)

library(tidyverse)
library(readxl)

dir.create("files", showWarnings = FALSE)
file_name <- "files/d26_T6-03.xlsx"

if (!file.exists(file_name)) {
    download.file(
        "https://www.jil.go.jp/kokunai/statistics/databook/2026/06/d26_T6-03.xlsx",
        destfile = file_name,
        method = "curl"
    )
}

years <-
    gsub(
        "年",
        "",
        c(
            "country",
            read_excel(
                file_name,
                sheet = "6-3（s1）",
                range = "D5:K5",
                col_names = F
            )
        )
    )

countries <-
    read_excel(
        file_name,
        sheet = "6-3（s1）",
        range = "M10:M36",
        col_names = F
    )

sheets <- c("6-3（s1）", "6-3（s2）", "6-3（s3）")
gender <- c("男女計", "男", "女")

for (i in 1:3) {
    temp <- read_xlsx(
        file_name,
        sheet = sheets[i],
        range = "D10:K36",
        col_names = F
    )

    temp <- mutate_all(temp, as.numeric)
    temp <- mutate(temp, gender = gender[i])
    temp <- cbind(countries, temp)

    if (i == 1) {
        data <- temp
    } else {
        data <- rbind(data, temp)
    }
}

colnames(data) <- c(years, "gender")

selected_countries <- c("JPN", "USA", "UK", "DEU", "SWE", "KOR")

data <- data %>%
    filter(country %in% selected_countries) %>%
    pivot_longer(cols = c(-country, -gender), names_to = "year") %>%
    mutate(
        country = factor(country, levels = selected_countries),
        gender = factor(gender, levels = c("男女計", "男", "女")),
        year = as.integer(year)
    )

g1 <- data %>%
    filter(gender == "男女計") %>%
    ggplot(aes(
        x = year,
        y = value,
        color = country,
        shape = country,
        group = country
    )) +
    geom_line() +
    geom_point(size = 3) +
    labs(x = "", y = "長時間労働の割合(就業者)") +
    scale_color_discrete(name = "") +
    scale_shape_discrete(name = "") +
    scale_x_continuous(breaks = c(2010, 2015, 2020)) +
    theme_classic(base_size = 16)

g2 <- data %>%
    filter(gender != "男女計") %>%
    ggplot(aes(
        x = year,
        y = value,
        color = country,
        shape = country,
        group = country
    )) +
    geom_line() +
    geom_point(size = 3) +
    labs(x = "", y = "長時間労働の割合(就業者)") +
    scale_color_discrete(name = "") +
    scale_shape_discrete(name = "") +
    scale_x_continuous(breaks = c(2010, 2015, 2020)) +
    facet_grid(. ~ gender) +
    theme_classic(base_size = 16)

plot(g1)
plot(g2)

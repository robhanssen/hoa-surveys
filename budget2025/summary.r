library(tidyverse)
theme_set(
    theme_light() +
        theme(
            # axis.title = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "none",
            plot.title.position = "plot",
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0)
        )
)

glcolors <-
    list(
        green = "#295043",
        tan = "#D3BDA8",
        dark = "gray30",
        light = "gray70",
        middle = "gray50"
    )

create_summary <- function() {
    dfiles <- list.files(path = "./budget2025/source/", pattern = "^ques.*csv$", full.names = TRUE)[1:3]
    map_df(dfiles, read_csv, show_col_types = FALSE) %>%
        distinct(Answer) %>%
        write_csv("budget2025/summary.csv")
}



datafiles <- "budget2025/summary.csv"

data <- read_csv(datafiles)

data %>%
    count(Category) %>%
    mutate(
        Category = str_to_sentence(Category),
        Category = fct_reorder(Category, n),
        pct = n / sum(n)
    ) %>%
    ggplot(aes(y = Category, x = pct)) +
    geom_col(fill = glcolors$green, alpha = .8) +
    labs(
        x = "", y = "",
        caption = paste("As of", format(today(), format = "%b %d, %Y"))
        # title = "Topics in the 2025 Budget survey"
    ) +
    scale_x_continuous(
        labels = scales::label_percent(),
        expand = c(0, 0),
        breaks = seq(0, 1, 0.05),
        limits = c(0, .21)
    ) +
    geom_text(
        aes(y = Category, x = .005, label = Category),
        angle = 0, hjust = 0, color = glcolors$tan, size = 6
    ) +
    theme(
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank()
    )

ggsave("budget2025/budget_survey_topic.png", width = 8, height = 6)

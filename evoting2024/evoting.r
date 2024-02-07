library(tidyverse)
library(patchwork)

theme_set(
    theme_light() + 
    theme(
        axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.title.position = "plot"
    )
)


display_results <- function(dat, question) {
    dat %>%
    mutate(vote = Count / sum(Count)) %>%
    ggplot(aes(x = vote, y = Answer)) + 
    geom_col() + 
    scale_x_continuous(
        limits = c(0, 1),
        labels = scales::label_percent()
    ) +
    labs(x = "", y = "",
        title = question) + 
    theme(
        axis.title = element_blank()
    )

}

q1 <- "Did you vote by paper ballot this year (2024)?"
q3 <- "If there was an option to vote electronically, would you be more likely to vote?"
q4 <- "How much should the HOA spend on an electronic voting option? (Current cost approx. $1200)"

q1res <- read_csv("evoting2024/question1.csv", col_types = "fi")
q3res <- read_csv("evoting2024/question3.csv", col_types = "fi")
q4res <- read_csv("evoting2024/question4.csv", col_types = "fi")

total <- sum(q1res$Count)

title <- glue::glue(
    "2024 Survey on electronic voting"
)

subtitle <- glue::glue(
    "Total response: {total}"
)


p <-
    display_results(q1res, question = q1) +
    display_results(q3res, question = q3) +
    display_results(q4res, question = q4) +
    plot_layout(ncol = 1) + 
    plot_annotation(
        title = title,
        subtitle = subtitle
    )

ggsave("evoting2024/evoting_results.png",
    # width = 6,
    # height = 10,
    plot = p)
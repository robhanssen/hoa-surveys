library(tidyverse)
library(patchwork)
library(tidytext)
library(wordcloud2)
data(stop_words)

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

datafiles <- list.files(path = "./budget2025/source", pattern = "^quest.*csv$", full.names = TRUE)

default_fee <- 800

lvls <- c(
    "$100 lower than FY2024",
    "$50 lower than FY2024",
    "Keep it the same as FY2024",
    "$50 higher than FY2024",
    "$100 higher than FY2024"
)

fee_question <-
    map_df(datafiles[4], read_csv) %>%
    mutate(
        Answer = factor(Answer, levels = lvls, ordered = TRUE)
    ) %>%
    arrange(Answer) %>%
    mutate( # correcting for my own test answer
        Count = case_when(
            Answer == lvls[5] ~ Count - 1,
            TRUE ~ Count
        ),
        diff = default_fee + c(-100, -50, 0, 50, 100)
    )

d <- rep(fee_question$diff, fee_question$Count)

mean_fee_change <- weighted.mean(fee_question$diff, fee_question$Count)
errbars <- mean(d) + qt(c(0.05, 0.95), df = length(d) - 1) * sd(d) / sqrt(length(d))

label_height <- max(fee_question$Count)

fee_question %>%
    ggplot(
        aes(x = diff, y = Count)
    ) +
    geom_point(size = 3, shape = 19, color = glcolors$green) +
    geom_segment(
        aes(x = diff, xend = diff, y = Count, yend = 0),
        linewidth = 2, alpha = .5, color = glcolors$green
    ) +
    annotate("text",
        x = mean_fee_change - 5, y = label_height, hjust = 1,
        label = glue::glue("Average (weighed):\n{scales::dollar(mean_fee_change)}")
    ) +
    geom_segment(
        inherit.aes = FALSE,
        aes(x = errbars[1], xend = errbars[2], y = 0, yend = 0),
        color = glcolors$tan, linewidth = 3, alpha = .25
    ) +
    geom_vline(xintercept = mean_fee_change, linewidth = 3, alpha = .25, color = glcolors$tan) +
    labs(
        x = "Desired fee (in US$)",
        y = ""
    ) +
    scale_y_continuous(
        breaks = scales::breaks_pretty()
    ) +
    scale_x_continuous(
        breaks = scales::breaks_pretty()
    )

ggsave("budget2025/fee_question.png", width = 6, height = 4)

open_questions <-
    map_df(datafiles, read_csv, .id = "file") %>%
    filter(!str_detect(Answer, "testing")) %>%
    mutate(file = as.numeric(file))


words1_3 <-
    open_questions %>%
    filter(file %in% c(1:3)) %>%
    slice(rep(seq_len(n()), times = (3 - file + 1))) %>%
    select(Answer) %>%
    unnest_tokens(word, Answer) %>%
    count(word, sort = TRUE) %>%
    anti_join(stop_words, by = "word")

set.seed(12354) # for reproducibility

wordcloud2(
    data = words1_3, size = 1.3
)


words5_6 <-
    open_questions %>%
    filter(file %in% c(5:6)) %>%
    slice(rep(seq_len(n()), times = (6 - file + 1))) %>%
    select(Answer) %>%
    unnest_tokens(word, Answer) %>%
    count(word, sort = TRUE) %>%
    anti_join(stop_words, by = "word")

set.seed(12354) # for reproducibility

wordcloud2(
    data = words5_6, size = 1.3
)


# log tracking

log <- read_csv("budget2025/source/activities.csv", show_col_types = FALSE) %>%
    filter(str_detect(Action, "Survey response given"), Date > "2024-08-01") %>%
    arrange(Date) %>%
    # remove the first entry: test user
    slice(-1) %>%
    mutate(x = 1, count = cumsum(x), in_data = TRUE) %>%
    bind_rows(tibble(Date = now(), count = nrow(.), in_data = FALSE))

log %>%
    ggplot(
        aes(x = Date, y = count, shape = in_data, linetype = in_data)
    ) +
    scale_x_datetime(
        date_breaks = "1 day",
        date_labels = "%b %d"
    ) +
    scale_y_continuous(
        limits = c(0, NA)
    ) +
    geom_point() +
    scale_shape_manual(values = c("TRUE" = 19, "FALSE" = 1)) +
    geom_line(color = "gray50", alpha = .5) +
    scale_linetype_manual(values = c("TRUE" = 1, "FALSE" = 1)) +
    labs(
        x = "",
        y = "Cumulative survey responses",
        title = "Budget 2025 survey response count"
    )

ggsave("budget2025/survey_count.png",
    width = 6, height = 4
)

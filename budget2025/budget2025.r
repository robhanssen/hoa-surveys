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

datafiles <- list.files(path = "./budget2025/source", pattern = "*.csv$", full.names = TRUE)

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

mean_fee_change <- weighted.mean(fee_question$diff, fee_question$Count)

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
        x = mean_fee_change + 5, y = label_height, hjust = 0,
        label = glue::glue("Average (weighed):\n{scales::dollar(mean_fee_change)}")
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

open_questions <-
    map_df(datafiles, read_csv, .id = "file") %>%
    filter(!str_detect(Answer, "testing"))


words1_3 <-
    open_questions %>%
    filter(file %in% c(1:3)) %>%
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
    select(Answer) %>%
    unnest_tokens(word, Answer) %>%
    count(word, sort = TRUE) %>%
    anti_join(stop_words, by = "word")

set.seed(12354) # for reproducibility

wordcloud2(
    data = words5_6, size = 1.3
)

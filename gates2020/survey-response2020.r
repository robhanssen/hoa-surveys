library(tidyverse)
library(patchwork)
library(ggpmisc)
library(tidytext)
library(wordcloud2)
data(stop_words)


theme_set(theme_light() +
    theme(
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0),
        axis.ticks = element_blank(),
        panel.grid.minor.y = element_blank()
    ))

# resp <-
#     read_csv("gates2020/activities.csv") %>%
#     janitor::clean_names() %>%
#     filter(str_detect(action, "Survey response")) %>%
#     arrange(date) %>%
#     mutate(
#         date = with_tz(date, "US/Eastern"),
#         count = seq_len(nrow(.)),
# )

answer_1 <- "Closed 24/7"
answer_2 <- "Open during daytime"

results <-
    c(
        rep(answer_1, 105),
        rep(answer_2, 50)
    )

# last_date <-
#     last(resp$date) %>%
#     format(., format = "%H:%M on %b %d")

# time_plot <-
#     ggplot(resp, aes(x = date, y = count)) +
#     geom_point(shape = 1) +
#     geom_line(
#         # stat = "smooth", method = "loess",
#         color = "gray70",
#         alpha = .5
#     ) +
#     scale_y_continuous(
#         limits = c(0, NA),
#         labels = scales::label_number()
#     ) +
#     labs(
#         x = "", y = "Response count (cumulative)",
#         title = glue::glue("Total response to the survey: ", length(results)),
#         caption = glue::glue("Data collected up to {last_date}")
#     )

#
# analysis of results
#

chi <- chisq.test(
    table(results),
)

params <- broom::tidy(chi)

note_table <- as_tibble(table(results)) %>% bind_rows(tibble(results = "", n = sum(.$n)))

sign <- ifelse(params$p.value < .05, "", "not ")
pval <- scales::pvalue(params$p.value)

note <-
    case_when(
        params$p.value > 0.05 ~
            glue::glue(
                "No option is preferred over the other significantly ",
                "(p", ifelse(str_detect(pval, "<"), "", "="), "{pval}) "
            ),
        sum(results == answer_1) > sum(results == answer_2) ~
            glue::glue(
                "There is a preference for {answer_1} ",
                "(p", ifelse(str_detect(pval, "<"), "", "="), "{pval}) "
            ),
        sum(results == answer_1) < sum(results == answer_2) ~
            glue::glue(
                "There is a preference for {answer_2} ",
                "(p", ifelse(str_detect(pval, "<"), "", "="), "{pval})"
            ),
        TRUE ~
            ""
    )

set.seed(20231001)

mns <-
    map_dbl(
        seq_len(10000),
        ~ mean(sample(results,
            length(results),
            replace = TRUE
        ) == "Closed 24/7")
    )

bounds <-
    broom::tidy(
        binom.test(sum(results == "Closed 24/7"), length(results))
    )


analysis_g <-
    ggplot(data = tibble(m = mns), aes(x = m)) +
    geom_density(fill = "gray50", alpha = .5) +
    geom_vline(
        xintercept = .5,
        color = "gray20",
        linewidth = 2,
        alpha = .8
    ) +
    scale_x_continuous(
        labels = scales::percent_format(),
        limits = c(0, 1)
    ) +
    geom_errorbar(
        data = bounds,
        aes(xmin = conf.low, xmax = conf.high, y = .75), inherit.aes = FALSE,
        width = .3, linewidth = 2, alpha = .5, color = "gray20"
    ) +
    annotate("table",
        x = 0, y = 6,
        label = note_table, hjust = 0
    ) +
    annotate("label",
        x = c(0, 1), y = .75, vjust = 1, hjust = c(0, 1),
        label = c(answer_2, answer_1)
    ) +
    labs(
        x = "Vote proportion", y = "",
        title = note
    ) +
    theme(
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank()
    )

ggsave("gates2020/vote_analysis.png",
    width = 6, height = 6,
    plot = analysis_g +
        plot_annotation(
            title = "Glen Lake Gate Survey (July 2020)",
            theme = theme(plot.title = element_text(hjust = .5))
        )
)

totalset <-
    c(
        rep(answer_1, 482 / 2),
        rep(answer_2, 482 / 2)
    )

set.seed(20231001)

base_results <-
    map_dbl(
        seq_len(10000),
        ~ mean(
            sample(
                totalset,
                length(results),
                replace = FALSE
            ) == answer_1
        )
    )

ttest <- broom::tidy(
    t.test(results == answer_1,
        base_results,
        mu = 0
    )
)

pval <- scales::pvalue(ttest$p.value)

errbars <-
    tibble(xs = quantile(base_results, c(0.05, 0.95)))

note <-
    case_when(
        params$p.value > 0.05 ~
            glue::glue(
                "No option is preferred over the other significantly ",
                "(p", ifelse(str_detect(pval, "<"), "", "="), "{pval}) "
            ),
        sum(results == answer_1) > sum(results == answer_2) ~
            glue::glue(
                "There is a preference for {answer_1} ",
                "(p", ifelse(str_detect(pval, "<"), "", "="), "{pval}) "
            ),
        sum(results == answer_1) < sum(results == answer_2) ~
            glue::glue(
                "There is a preference for {answer_2} ",
                "(p", ifelse(str_detect(pval, "<"), "", "="), "{pval})"
            ),
        TRUE ~
            ""
    )



binom_g <- ggplot(tibble(x = base_results), aes(x)) +
    geom_density(fill = "gray70", alpha = .5) +
    geom_vline(
        xintercept = mean(results == answer_1),
        color = "gray60",
        # linewidth = 2,
        alpha = .8
    ) +
    scale_x_continuous(
        labels = scales::percent_format(),
        limits = c(0, 1),
        breaks = 0:4 / 4
    ) +
    geom_errorbar(
        data = errbars, inherit.aes = FALSE,
        aes(xmin = min(xs), xmax = max(xs), y = 1),
        width = .3, linewidth = 2, alpha = .5, color = "gray60"
    ) +
    annotate("table",
        x = 0, y = 6,
        label = note_table, hjust = 0
    ) +
    annotate("label",
        x = c(0, 1), y = .75, vjust = 1, hjust = c(0, 1),
        label = c(answer_2, answer_1)
    ) +
    labs(
        x = "Vote proportion", y = "",
        title = note
    ) +
    theme(
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank()
    )

ggsave("gates2020/vote_analysis-binom.png",
    width = 6, height = 6,
    plot =  binom_g +
        plot_annotation(
            title = "Glen Lake Gate Survey (July 2020)",
            theme = theme(plot.title = element_text(hjust = .5))
        )
)

#
# using exact binomial analysis
#

binom_exact <-
    tibble(
        p = c(0, seq_along(results)),
        y = dbinom(p, length(results), prob = .5)
    ) %>%
    mutate(across(p:y, ~ .x / max(.x)))

errbars <-
    tibble(x = qbinom(c(.05, .95), length(results), prob = .50)) %>%
    mutate(x = x / length(results))

mean <- .5 * length(results)
sd <- sqrt(.5 * (1 - .5) * length(results))

# p.value <- pnorm((sum(results == answer_1) - mean)/sd, lower.tail = FALSE)
# pval <- scales::pvalue(p.value)

# note <-
#     case_when(
#         pval > 0.05 ~
#             glue::glue(
#                 "No option is preferred over the other significantly ",
#                 "(p", ifelse(str_detect(pval, "<"), "", "="), "{pval}) "
#             ),
#         sum(results == answer_1) > sum(results == answer_2) ~
#             glue::glue(
#                 "There is a preference for {answer_1} ",
#                 "(p", ifelse(str_detect(pval, "<"), "", "="), "{pval}) "
#             ),
#         sum(results == answer_1) < sum(results == answer_2) ~
#             glue::glue(
#                 "There is a preference for {answer_2} ",
#                 "(p", ifelse(str_detect(pval, "<"), "", "="), "{pval})"
#             ),
#         TRUE ~
#             ""
#     )


binom_exact_g <-
    ggplot(
        binom_exact,
        aes(x = p, y = y)
    ) +
    geom_line() +
    geom_vline(
        xintercept = mean(results == answer_1),
        color = "gray30",
        alpha = .5,
        linewidth = 1
    ) +
    geom_errorbar(
        inherit.aes = FALSE,
        data = errbars,
        aes(xmin = min(x), xmax = max(x), y = .1),
        width = .05,
        linewidth = 2,
        color = "gray70",
        alpha = .5
    ) +
    scale_x_continuous(
        breaks = 0:4 / 4,
        labels = scales::percent_format()
    ) +
    annotate("label",
        x = c(0, 1), y = .10, vjust = .5, hjust = c(0, 1),
        label = c(answer_2, answer_1)
    ) +
    labs(
        x = "Vote proportion", y = "",
        title = note
    ) +
    annotate("table",
        x = 0, y = .8,
        label = note_table, hjust = 0
    ) +
    theme(
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank()
    )

ggsave("gates2020/vote_analysis-binomexact.png",
    width = 6, height = 6,
    plot = binom_exact_g +
        plot_annotation(
            title = "Glen Lake Gate Survey (July 2020)",
            theme = theme(plot.title = element_text(hjust = .5))
        )
)

#
# word cloud
#
#

words <-
    read_csv("gates2020/question2.csv") %>%
    select(Answer) %>%
    unnest_tokens(word, Answer) %>%
    count(word, sort = TRUE) %>%
    anti_join(stop_words, by = "word") %>%
    filter(!str_detect(word, "gates"))

set.seed(12354) # for reproducibility

wordcloud2(
    data = words, size = 1.3
)

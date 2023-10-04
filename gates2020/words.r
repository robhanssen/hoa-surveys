library(tidyverse)
library(tidytext)
library(wordcloud2)
data(stop_words)

words <-
    read_csv("hoa-survey/question2.csv") %>%
    select(Answer) %>%
    unnest_tokens(word, Answer) %>%
    count(word, sort = TRUE) %>%
    anti_join(stop_words, by= "word") %>%
    filter(!str_detect(word, "gates"))

set.seed(12354) # for reproducibility

wordcloud2(
    data = words, size = 1.3
)

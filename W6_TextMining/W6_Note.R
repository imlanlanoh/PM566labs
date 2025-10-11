library(tidytext)
library(tidyverse)
library(dplyr)
library(ggplot2)

getwd()

alice <- readRDS("./W6_TextMining/alice.rds")
alice

alice |>
  unnest_tokens(token, text) |>
  count(token, sort = TRUE)


alice |>
  unnest_tokens(token, text) |>
  group_by(chapter) |>
  count(token) |>
  top_n(10, n)


alice |>
  unnest_tokens(token, text) |>
  count(token) |>
  top_n(10, n) |>
  ggplot(aes(n, token)) +
  geom_col()


library(forcats)
alice |>
  unnest_tokens(token, text) |>
  count(token) |>
  top_n(10, n) |>
  ggplot(aes(n, fct_reorder(token, n))) +
  geom_col()

stop_words

table(stop_words$lexicon)

stop_words |>
  filter(lexicon == "snowball") |>
  pull(word)

alice |>
  unnest_tokens(token, text) |>
  anti_join(stop_words, by = c("token" = "word")) |>
  count(token, sort = TRUE)

alice %>%
  unnest_tokens(token, text) %>%
  filter(str_detect(token, "^alice$")) %>%
  count()


alice %>%
  unnest_tokens(token, text) %>%
  filter(str_detect(token, "^_?alice'?s?_?$|^_?alice_?$")) %>%
  count()

alice |>
  unnest_tokens(word, text) |>
  anti_join(stop_words, by = c("word")) |>
  count(word, sort = TRUE) |>
  top_n(10, n) |>
  ggplot(aes(n, fct_reorder(word, n))) +
  geom_col()

# I want to know the format of _xx and xx_ and xxx
pattern <- regex("^_*alice(?:['’]s)?_*$", ignore_case = TRUE)

the_pattern <- regex("^_*the_*$", ignore_case = TRUE)

library(stringr)
alice %>%
  unnest_tokens(token, text) %>%
  filter(str_detect(token, the_pattern)) %>%
  count()

alice |>
  unnest_ngrams(ngram, text, n = 2)

# Count two words together, and count the number of set starting with Alice
# biagram
# when using biagram, we ignore the italic format
alice |>
  unnest_ngrams(ngram, text, n = 2) |>
  separate(ngram, into = c("word1", "word2"), sep = " ") |>
  select(word1, word2) |>
  filter(word1 == "alice") |>
  count(word2, sort = T)

alice |>
  unnest_ngrams(ngram, text, n = 2) |>
  separate(ngram, into = c("word1", "word2"), sep = " ") |>
  select(word1, word2) |>
  filter(word2 == "alice") |>
  count(word1, sort = TRUE)

alice |>
  unnest_tokens(text, text) |>
  count(text, chapter) |>
  bind_tf_idf(text, chapter, n)

alice |>
  unnest_tokens(text, text) |>
  count(text, chapter) |>
  bind_tf_idf(text, chapter, n) |>
  arrange(desc(tf_idf))

library(textdata)
get_sentiments("bing")

diff_by_chap <- alice |>
  unnest_tokens(word, text) |>
  inner_join(get_sentiments("bing")) |> 
  group_by(chapter) |> 
  summarise(sentiment = sum(sentiment == "positive") - sum(sentiment == "negative"))

get_sentiments('afinn')

# this is just a count. it has not been normalized by the length of chapter
barplot(diff_by_chap$sentiment, names.arg = diff_by_chap$chapter)

avg_by_chap <- alice |>
  unnest_tokens(word, text) |>
  inner_join(get_sentiments("afinn")) |> 
  group_by(chapter) |> 
  summarise(sentiment = mean(value))

barplot(avg_by_chap$sentiment, names.arg = avg_by_chap$chapter)



alice |>
  unnest_tokens(word, text) |>
  inner_join(get_sentiments("nrc")) |> 
  group_by(chapter) |> 
  summarise(sentiment = names(which.max(table(sentiment))))

nrc_fun <- get_sentiments("nrc")
nrc_fun <- nrc_fun[!nrc_fun$sentiment %in% c("positive","negative"), ]

alice |>
  unnest_tokens(word, text) |>
  inner_join(nrc_fun) |> 
  group_by(chapter) |> 
  summarise(sentiment = names(which.max(table(sentiment))))

#================================================================#

#un nested-version
ds <- unnest_tokens(alice, word, text)

## -----------------------------------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE, warning = FALSE,
  eval = requireNamespace("tm", quietly = TRUE) && requireNamespace("quanteda", quietly = TRUE) && requireNamespace("topicmodels", quietly = TRUE) && requireNamespace("ggplot2", quietly = TRUE)
)

## -----------------------------------------------------------------------------
library(ggplot2)
theme_set(theme_bw())

## -----------------------------------------------------------------------------
library(tm)
data("AssociatedPress", package = "topicmodels")
AssociatedPress

## -----------------------------------------------------------------------------
library(dplyr)
library(tidytext)

ap_td <- tidy(AssociatedPress)

## -----------------------------------------------------------------------------
ap_sentiments <- ap_td %>%
  inner_join(get_sentiments("bing"), join_by(term == word))

ap_sentiments

## -----------------------------------------------------------------------------
library(tidyr)

ap_sentiments %>%
  count(document, sentiment, wt = count) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  arrange(sentiment)

## -----------------------------------------------------------------------------
library(ggplot2)

ap_sentiments %>%
  count(sentiment, term, wt = count) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(n, term, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(sentiment), scales = "free_y") +
  labs(x = "Contribution to sentiment", y = NULL)

## -----------------------------------------------------------------------------
library(methods)

data("data_corpus_inaugural", package = "quanteda")
d <- quanteda::tokens(data_corpus_inaugural) %>%
  quanteda::dfm()

d

tidy(d)

## -----------------------------------------------------------------------------
ap_td

# cast into a Document-Term Matrix
ap_td %>%
  cast_dtm(document, term, count)

# cast into a Term-Document Matrix
ap_td %>%
  cast_tdm(term, document, count)

# cast into quanteda's dfm
ap_td %>%
  cast_dfm(term, document, count)


# cast into a Matrix object
m <- ap_td %>%
  cast_sparse(document, term, count)
class(m)
dim(m)

## -----------------------------------------------------------------------------
reut21578 <- system.file("texts", "crude", package = "tm")
reuters <- VCorpus(DirSource(reut21578),
                   readerControl = list(reader = readReut21578XMLasPlain))

reuters

## -----------------------------------------------------------------------------
reuters_td <- tidy(reuters)
reuters_td

## -----------------------------------------------------------------------------
library(quanteda)

data("data_corpus_inaugural")

data_corpus_inaugural

inaug_td <- tidy(data_corpus_inaugural)
inaug_td

## -----------------------------------------------------------------------------
inaug_words <- inaug_td %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

inaug_words

## -----------------------------------------------------------------------------
inaug_freq <- inaug_words %>%
  count(Year, word) %>%
  complete(Year, word, fill = list(n = 0)) %>%
  group_by(Year) %>%
  mutate(year_total = sum(n), percent = n / year_total) %>%
  ungroup()

inaug_freq

## -----------------------------------------------------------------------------
library(broom)
models <- inaug_freq %>%
  group_by(word) %>%
  filter(sum(n) > 50) %>%
  group_modify(
    ~ tidy(glm(cbind(n, year_total - n) ~ Year, ., family = "binomial"))
  ) %>%
  ungroup() %>%
  filter(term == "Year")

models

models %>%
  filter(term == "Year") %>%
  arrange(desc(abs(estimate)))

## -----------------------------------------------------------------------------
library(ggplot2)

models %>%
  mutate(adjusted.p.value = p.adjust(p.value)) %>%
  ggplot(aes(estimate, adjusted.p.value)) +
  geom_point() +
  scale_y_log10() +
  geom_text(aes(label = word), vjust = 1, hjust = 1, check_overlap = TRUE) +
  labs(x = "Estimated change over time", y = "Adjusted p-value")

## -----------------------------------------------------------------------------
library(scales)

models %>%
  slice_max(abs(estimate), n = 6) %>%
  inner_join(inaug_freq) %>%
  ggplot(aes(Year, percent)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(vars(word)) +
  scale_y_continuous(labels = percent_format()) +
  labs(y = "Frequency of word in speech")


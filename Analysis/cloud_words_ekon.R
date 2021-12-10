# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#                analiza tytulow ofert pracy dla obszaru ekonomicznego
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------


# Load the packages
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("tidytext")

set.seed(1234) # for reproducibility 
# Modify encoding
Sys.setlocale(category = "LC_ALL", locale = "Polish")
# preprocessing
require(dplyr)
jobs.ekon <- bind_rows(page.p, page.ekon.ip)
job.ekon.cleaned <- jobs.ekon %>%
  unnest_tokens(word, title) %>%
  anti_join(stop.words) %>%
  anti_join(stop_words) %>%
  anti_join(stop.words.ekonomia) %>%
  count(word, sort = TRUE)

par(mar=c(1,1,1,1))
wordcloud(words = job.ekon.cleaned$word, freq = job.ekon.cleaned$n, min.freq = 10, max.words=200, 
          random.order=FALSE, colors=brewer.pal(8, "Dark2"))

wordcloud(words = offers_cleaned$word, freq = offers_cleaned$n, min.freq = 5, max.words=200, 
          random.order=FALSE, colors=brewer.pal(8, "Dark2"))



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ngram analysis (bigram) for job titles----------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------


# dzielimy ogłoszenia na "bigramy" - złączenia po dwa wyrazy
job.ekon.bigrams <- jobs.ekon %>% 
  unnest_tokens(bigram, title, token = "ngrams", n = 2)

job.ekon.bigrams.count <- job.ekon.bigrams %>%
  count(bigram, sort = TRUE)

# dzielimy wyrazy na kolumny
library(tidyr)
bigrams.job.ekon.separated <- job.ekon.bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# filtrujemy pod kątem słów niechcianych; jak się okazuje, te angielskie też są przydatne
data(stop_words)
bigrams.job.ekon.filtered <- bigrams.job.ekon.separated %>%
  filter(!word1 %in% stop.words$word) %>%
  filter(!word2 %in% stop.words$word) %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% stop.words.ekonomia$word) %>%
  filter(!word2 %in% stop.words.ekonomia$word)

# zliczamy częstość ngramów w odfiltrowanej bazie
bigram.job.ekon.counts <- bigrams.job.ekon.filtered %>%
  count(word1, word2, sort = TRUE)

# łączymy wyrazy
bigrams.job.ekon.united <- bigrams.job.ekon.filtered %>%
  unite(bigram, word1, word2, sep = " ")

# wykres najczęściej pojawiających się bigramów
require("ggplot2")
bigrams.job.ekon.united %>%
  count(bigram, sort = TRUE) %>%
  filter(n > 10) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

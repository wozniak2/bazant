# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#                analiza nazw stanowisk dla portalu linkedin
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
jobs.linked <- linked[c(1,2)]
job.linked.cleaned <- jobs.linked %>%
  unnest_tokens(word, job) %>%
  anti_join(stop.words) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

dev.off()
wordcloud(words = job.linked.cleaned$word, freq = job.linked.cleaned$n, min.freq = 5, max.words=200, 
          random.order=FALSE, colors=brewer.pal(8, "Dark2"))

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ngram analysis (bigram) for job titles----------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------


# dzielimy ogłoszenia na "bigramy" - złączenia po dwa wyrazy
jobs.linked.bigrams <- jobs.linked %>% 
  unnest_tokens(bigram, job, token = "ngrams", n = 2)

jobs.linked.bigrams.count <- jobs.linked.bigrams %>%
  count(bigram, sort = TRUE)

# dzielimy wyrazy na kolumny
library(tidyr)
bigrams.job.linked.separated <- jobs.linked.bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# filtrujemy pod kątem słów niechcianych; jak się okazuje, te angielskie też są przydatne
data(stop_words)
bigrams.job.linked.filtered <- bigrams.job.linked.separated %>%
  filter(!word1 %in% stop.words$word) %>%
  filter(!word2 %in% stop.words$word) %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# zliczamy częstość ngramów w odfiltrowanej bazie
bigram.job.linked.counts <- bigrams.job.linked.filtered %>%
  count(word1, word2, sort = TRUE)

# łączymy wyrazy
bigrams.job.linked.united <- bigrams.job.linked.filtered %>%
  unite(bigram, word1, word2, sep = " ")

# wykres najczęściej pojawiających się złączeń dwuwyrazowych w nazwie ofert
require(ggplot2)
bigrams.job.linked.united %>%
  count(bigram, sort = TRUE) %>%
  filter(n > 4) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

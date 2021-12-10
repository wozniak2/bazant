# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#                analiza tytulow ofert pracy dla obszaru informatycznego
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
jobs.info <- bind_rows(page.p.info, page.info.ip)
job.info.cleaned <- jobs.info %>%
  unnest_tokens(word, title) %>%
  anti_join(stop.words) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

dev.off()
wordcloud(words = job.info.cleaned$word, freq = job.info.cleaned$n, min.freq = 15, max.words=200, 
          random.order=FALSE, colors=brewer.pal(8, "Dark2"))

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ngram analysis (bigram) for job titles----------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------


# dzielimy ogłoszenia na "bigramy" - złączenia po dwa wyrazy
job.info.bigrams <- jobs.info %>% 
  unnest_tokens(bigram, title, token = "ngrams", n = 2)

job.info.bigrams.count <- job.info.bigrams %>%
  count(bigram, sort = TRUE)

# dzielimy wyrazy na kolumny
library(tidyr)
bigrams.job.info.separated <- job.info.bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# filtrujemy pod kątem słów niechcianych; jak się okazuje, te angielskie też są przydatne
data(stop_words)
bigrams.job.info.filtered <- bigrams.job.info.separated %>%
  filter(!word1 %in% stop.words$word) %>%
  filter(!word2 %in% stop.words$word) %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# zliczamy częstość ngramów w odfiltrowanej bazie
bigram.job.info.counts <- bigrams.job.info.filtered %>%
  count(word1, word2, sort = TRUE)

# łączymy wyrazy
bigrams.job.info.united <- bigrams.job.info.filtered %>%
  unite(bigram, word1, word2, sep = " ")

# wykres najczęściej pojawiających się złączeń dwuwyrazowych w nazwie ofert
bigrams.job.info.united %>%
  count(bigram, sort = TRUE) %>%
  filter(n > 70) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
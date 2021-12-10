# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#                analiza tytulow ofert pracy dla obszaru zarządzania i jakości
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

page.p.zarz <- pagep_zarz
page.zarz.ip <- page_zarz_ip

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
jobs.zarz <- bind_rows(page.p.zarz, page.zarz.ip)
job.zarz.cleaned <- jobs.zarz %>%
  unnest_tokens(word, title) %>%
  anti_join(stop.words) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

dev.off()
wordcloud(words = job.zarz.cleaned$word, freq = job.zarz.cleaned$n, min.freq = 13, max.words=200, 
          random.order=FALSE, colors=brewer.pal(8, "Dark2"))

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ngram analysis (bigram) for job titles----------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------


# dzielimy ogłoszenia na "bigramy" - złączenia po dwa wyrazy
job.zarz.bigrams <- jobs.zarz %>% 
  unnest_tokens(bigram, title, token = "ngrams", n = 2)

job.zarz.bigrams.count <- job.zarz.bigrams %>%
  count(bigram, sort = TRUE)

# dzielimy wyrazy na kolumny
library(tidyr)
bigrams.job.zarz.separated <- job.zarz.bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# filtrujemy pod kątem słów niechcianych; jak się okazuje, te angielskie też są przydatne
data(stop_words)
bigrams.job.zarz.filtered <- bigrams.job.zarz.separated %>%
  filter(!word1 %in% stop.words$word) %>%
  filter(!word2 %in% stop.words$word) %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# zliczamy częstość ngramów w odfiltrowanej bazie
bigram.job.zarz.counts <- bigrams.job.zarz.filtered %>%
  count(word1, word2, sort = TRUE)

# łączymy wyrazy
bigrams.job.zarz.united <- bigrams.job.zarz.filtered %>%
  unite(bigram, word1, word2, sep = " ")

# wykres najczęściej pojawiających się złączeń dwuwyrazowych w nazwie ofert
bigrams.job.zarz.united %>%
  count(bigram, sort = TRUE) %>%
  filter(n > 15) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
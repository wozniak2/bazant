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

set.seed(1234) # for reproducibility 
# Modify encoding
Sys.setlocale(category = "LC_ALL", locale = "Polish")
# preprocessing
require(dplyr)
jobs.finan <- bind_rows(page.p.finan, page.finan.ip)
job.finan.cleaned <- jobs.finan %>%
  unnest_tokens(word, title) %>%
  anti_join(stop.words) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

par(mar=c(1,1,1,1))
wordcloud(words = job.finan.cleaned$word, freq = job.finan.cleaned$n, min.freq = 10, max.words=200, 
          random.order=FALSE, colors=brewer.pal(8, "Dark2"))

wordcloud(words = offers_cleaned$word, freq = offers_cleaned$n, min.freq = 5, max.words=200, 
          random.order=FALSE, colors=brewer.pal(8, "Dark2"))



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ngram analysis (bigram) for job titles----------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------


# dzielimy ogłoszenia na "biggramy" - złączenia po dwa wyrazy
job.finan.bigrams <- jobs.finan %>% 
  unnest_tokens(bigram, title, token = "ngrams", n = 2)

job.finan.bigrams.count <- job.finan.bigrams %>%
  count(bigram, sort = TRUE)

# dzielimy wyrazy na kolumny
library(tidyr)
bigrams.job.finan.separated <- job.finan.bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# filtrujemy pod kątem słów niechcianych; jak się okazuje, te angielskie też są przydatne
data(stop_words)
bigrams.job.finan.filtered <- bigrams.job.finan.separated %>%
  filter(!word1 %in% stop.words$word) %>%
  filter(!word2 %in% stop.words$word) %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# zliczamy częstość ngramów w odfiltrowanej bazie
bigram.job.finan.counts <- bigrams.job.finan.filtered %>%
  count(word1, word2, sort = TRUE)

# łączymy wyrazy
bigrams.job.finan.united <- bigrams.job.finan.filtered %>%
  unite(bigram, word1, word2, sep = " ")

# wykres najczęściej pojawiających się bigramów
bigrams.job.finan.united %>%
  count(bigram, sort = TRUE) %>%
  filter(n > 12) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
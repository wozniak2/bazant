# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#                analiza treści ofert pracy dla obszaru zarządzania i jakości
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------


# unifikujemy bazy z obu portali
offers.p.zarz <- aggregate(word ~ offer, data = offers.p.zarz, FUN = paste, collapse = " ")
offers.zarz.ip <- aggregate(word ~ offer, data = offers.zarz.ip, FUN = paste, collapse = " ")

# preprocessing + łączenie baz w jedną całość
require(dplyr)
offers <- bind_rows(offers.p.zarz, offers.zarz.ip)
offers$offer <- as.character(offers$offer)
offers$word <- as.character(offers$word)

# covert to tidy text format
dfo <- tibble(offers)

# remove dots
dfo$word <- gsub('\\.', ' ', dfo$word)
dfo$word <- gsub('\\:', ' ', dfo$word)

# count words frequencies
# Rozbijamy słowa na tokeny
require(tidytext)
offers.zarz.word <- dfo %>%
  unnest_tokens(word, word)

# Czyścimy ze słów niechcianych
Sys.setlocale(category = "LC_ALL", locale = "Polish")
offers.zarz.word <- offers.zarz.word %>%
  anti_join(stop.words) %>%
  anti_join(stop_words)

# tworzymy prosty wykres słupkowy dla częstości wystąpienia poszczególnych słów w treści ogłoszeń
require(ggplot2)
offers.zarz.word %>%
  count(word, sort = TRUE) %>%
  filter(n > 500) %>%     ## dopasuj częstość
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# chmura słów dla treści ogłoszeń
offers.count <- offers.zarz.word %>%
  count(word, sort = TRUE)

dev.off()
require(wordcloud)
wordcloud(words = offers.count$word, freq = offers.count$n, min.freq = 5, max.words = 350, 
          random.order=FALSE, colors=brewer.pal(8, "Dark2"))


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ngram analysis (bigram) for job offers----------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# dzielimy ogłoszenia na "biggramy" - złączenia po dwa wyrazy
offers.zarz.bigrams <- dfo %>% 
  unnest_tokens(bigram, word, token = "ngrams", n = 2)

zarz.bigrams.count <- offers.zarz.bigrams %>%
  count(bigram, sort = TRUE)

# dzielimy wyrazy na kolumny
library(tidyr)
bigrams.zarz.separated <- offers.zarz.bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# filtrujemy pod kątem słów niechcianych; jak się okazuje, te angielskie też są przydatne
data(stop_words)
bigrams.zarz.filtered <- bigrams.zarz.separated %>%
  filter(!word1 %in% stop.words$word) %>%
  filter(!word2 %in% stop.words$word) %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)


# zliczamy częstość ngramów w odfiltrowanej bazie
bigram.zarz.counts <- bigrams.zarz.filtered %>%
  count(word1, word2, offer, sort = TRUE)

# łączymy wyrazy
bigrams.zarz.united <- bigrams.zarz.filtered %>%
  unite(bigram, word1, word2, sep = " ")

# wykres najczęściej pojawiających się bigramów
bigrams.zarz.united %>%
  count(bigram, sort = TRUE) %>%
  filter(n > 90) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# graph visualization of n-grams -----------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
library(igraph)

bigram.zarz.graph <- bigram.zarz.counts %>%
  filter(n > 9) %>%
  graph_from_data_frame()

library(ggraph)
#set.seed(2017)
#dev.off()
a <- grid::arrow(type = "closed", length = unit(.1, "inches"))


ggraph(bigram.zarz.graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = -0.5, hjust = 0.5, size=3.5)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# tf idf statistics for bigrams
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
total.zarz.bigrams <- bigram.zarz.counts %>%
  group_by(offer) %>%
  summarize(total = sum(n))
  

bigram.zarz.counts <- left_join(bigram.zarz.counts, total.zarz.bigrams)
bigram.zarz.counts$bigram <- paste(bigram.zarz.counts$word1, bigram.zarz.counts$word2)

zarz_tf_idf <- bigram.zarz.counts %>%
  bind_tf_idf(bigram, offer, n)

library(forcats)
zarz_tf_idf %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf))) +
  geom_col(show.legend = FALSE)

write.csv(zarz_tf_idf, "zarz_tf_idf.csv", fileEncoding = "UTF-8")


#--------------------------------------------------------------------
#--------------------------------------------------------------------
#Cloud words for job offers words------------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------

# Load the packages
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

offers.zarz.word.counts <- offers.zarz.word %>%
  count(word, sort = TRUE)

set.seed(1234) # for reproducibility 

par(mar=c(1,1,1,1))
wordcloud(words = offers.zarz.word.counts$word, freq = offers.zarz.word.counts$n, min.freq = 170, max.words=200, 
          random.order=FALSE, colors=brewer.pal(8, "Dark2"))

wordcloud(words = offers_cleaned$word, freq = offers_cleaned$n, min.freq = 5, max.words=200, 
          random.order=FALSE, colors=brewer.pal(8, "Dark2"))

write.csv(dfos, "dfos.csv", fileEncoding="WINDOWS-1250")


job.words.cleaned <- df %>%
  unnest_tokens(word, title) %>%
  anti_join(stop.words) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

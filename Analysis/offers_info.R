# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#                analiza treści ofert pracy dla obszaru informatycznego
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------


# unifikujemy bazy z obu portali
offers.p.info <- aggregate(word ~ offer, data = offers.p.info, FUN = paste, collapse = " ")
offers.info.ip <- aggregate(word ~ offer, data = offers.info.ip, FUN = paste, collapse = " ")

# preprocessing + łączenie baz w jedną całość
require(dplyr)
offers <- bind_rows(offers.p.info, offers.info.ip)
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
offers.info.word <- dfo %>%
  unnest_tokens(word, word)

# Czyścimy ze słów niechcianych
Sys.setlocale(category = "LC_ALL", locale = "Polish")
offers.info.word <- offers.info.word %>%
  anti_join(stop.words) %>%
  anti_join(stop_words)

# tworzymy prosty wykres słupkowy dla częstości wystąpienia poszczególnych słów w treści ogłoszeń
require(ggplot2)
offers.info.word %>%
  count(word, sort = TRUE) %>%
  filter(n > 500) %>%     ## dopasuj częstość
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# chmura słów dla treści ogłoszeń
offers.count <- offers.info.word %>%
  count(word, sort = TRUE)

dev.off()
require(wordcloud)
wordcloud(words = offers.count$word, freq = offers.count$n, min.freq = 40, max.words = 250, 
          random.order=FALSE, colors=brewer.pal(8, "Dark2"))


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ngram analysis (bigram) for job offers----------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# dzielimy ogłoszenia na "biggramy" - złączenia po dwa wyrazy
offers.info.bigrams <- dfo %>% 
  unnest_tokens(bigram, word, token = "ngrams", n = 2)

info.bigrams.count <- offers.info.bigrams %>%
  count(bigram, sort = TRUE)

# dzielimy wyrazy na kolumny
library(tidyr)
bigrams.info.separated <- offers.info.bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# filtrujemy pod kątem słów niechcianych; jak się okazuje, te angielskie też są przydatne
data(stop_words)
bigrams.info.filtered <- bigrams.info.separated %>%
  filter(!word1 %in% stop.words$word) %>%
  filter(!word2 %in% stop.words$word) %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)


# zliczamy częstość ngramów w odfiltrowanej bazie
bigram.info.counts <- bigrams.info.filtered %>%
  count(word1, word2, offer, sort = TRUE)

# łączymy wyrazy
bigrams.info.united <- bigrams.info.filtered %>%
  unite(bigram, word1, word2, sep = " ")

# wykres najczęściej pojawiających się bigramów
bigrams.info.united %>%
  count(bigram, sort = TRUE) %>%
  filter(n > 457) %>%
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

bigram.info.graph <- bigram.info.counts %>%
  filter(n > 63) %>%
  graph_from_data_frame()

library(ggraph)
#set.seed(2017)
#dev.off()
a <- grid::arrow(type = "closed", length = unit(.1, "inches"))


ggraph(bigram.info.graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = -0.5, hjust = 0.7, size=4)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# tf idf statistics for bigrams
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
total.info.bigrams <- bigram.info.counts %>%
  group_by(offer) %>%
  summarize(total = sum(n))
  

bigram.info.counts <- left_join(bigram.info.counts, total.info.bigrams)
bigram.info.counts$bigram <- paste(bigram.info.counts$word1, bigram.info.counts$word2)

info_tf_idf <- bigram.info.counts %>%
  bind_tf_idf(bigram, offer, n)

library(forcats)
info_tf_idf %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf))) +
  geom_col(show.legend = FALSE)

write.csv(info_tf_idf, "info_tf_idf.csv", fileEncoding = "UTF-8")


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

offers.info.word.counts <- offers.info.word %>%
  count(word, sort = TRUE)

set.seed(1234) # for reproducibility 

par(mar=c(1,1,1,1))
wordcloud(words = offers.info.word.counts$word, freq = offers.info.word.counts$n, min.freq = 170, max.words=200, 
          random.order=FALSE, colors=brewer.pal(8, "Dark2"))

wordcloud(words = offers_cleaned$word, freq = offers_cleaned$n, min.freq = 5, max.words=200, 
          random.order=FALSE, colors=brewer.pal(8, "Dark2"))

write.csv(dfos, "dfos.csv", fileEncoding="WINDOWS-1250")


job.words.cleaned <- df %>%
  unnest_tokens(word, title) %>%
  anti_join(stop.words) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

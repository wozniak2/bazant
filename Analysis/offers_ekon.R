# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#                analiza treści ofert pracy dla obszaru ekonomicznego
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

offers.e.ip <- offers_e_ip
offers.e.p <- offers_p_ekon

offers.e.p <- aggregate(word ~ offer, data = offers_p_ekon, FUN = paste, collapse = " ")
offers.e.ip <- aggregate(word ~ offer, data = offers.e.ip, FUN = paste, collapse = " ")

# preprocessing
require(dplyr)
offers <- bind_rows(offers.e.p, offers.e.ip)
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
offers.ekon.word <- dfo %>%
  unnest_tokens(word, word)

# Czyścimy ze słów niechcianych
Sys.setlocale(category = "LC_ALL", locale = "Polish")
offers.ekon.word <- offers.ekon.word %>%
  anti_join(stop.words) %>%
  anti_join(stop_words)

# tworzymy prosty wykres słupkowy dla częstości wystąpienia poszczególnych słów w treści ogłoszeń
require(ggplot2)
offers.ekon.word %>%
  count(word, sort = TRUE) %>%
  filter(n > 500) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ngram analysis (bigram) for job offers----------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# dzielimy ogłoszenia na "bigramy" - złączenia po dwa wyrazy
offers.ekon.bigrams <- dfo %>% 
  unnest_tokens(bigram, word, token = "ngrams", n = 2)

ekon.bigrams.count <- offers.ekon.bigrams %>%
  count(bigram, sort = TRUE)

# dzielimy wyrazy na kolumny
library(tidyr)
bigrams.ekon.separated <- offers.ekon.bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# filtrujemy pod kątem słów niechcianych; jak się okazuje, te angielskie też są przydatne
data(stop_words)
bigrams.ekon.filtered <- bigrams.ekon.separated %>%
  filter(!word1 %in% stop.words$word) %>%
  filter(!word2 %in% stop.words$word) %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% stop.words.ekonomia$word) %>%
  filter(!word2 %in% stop.words.ekonomia$word)

# zliczamy częstość ngramów w odfiltrowanej bazie
bigram.ekon.counts <- bigrams.ekon.filtered %>%
  count(word1, word2, offer, sort = TRUE)

# łączymy wyrazy
bigrams.ekon.united <- bigrams.ekon.filtered %>%
  unite(bigram, word1, word2, sep = " ")

# wykres najczęściej pojawiających się bigramów
bigrams.ekon.united %>%
  count(bigram, sort = TRUE) %>%
  filter(n > 80) %>%
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

bigram.ekon.graph <- bigram.ekon.counts %>%
  filter(n > 13) %>%
  graph_from_data_frame()

library(ggraph)
set.seed(2017)
a <- grid::arrow(type = "closed", length = unit(.1, "inches"))
par(mar=c(1,3,1,1))
ggraph(bigram.ekon.graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = -0.7, hjust = 0.5, size=3.7)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# tf idf statistics for bigrams
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
total.ekon.bigrams <- bigram.ekon.counts %>%
  group_by(offer) %>%
  summarize(total = sum(n))
  

bigram.ekon.counts <- left_join(bigram.ekon.counts, total.ekon.bigrams)
bigram.ekon.counts$bigram <- paste(bigram.ekon.counts$word1, bigram.ekon.counts$word2)

ekon_tf_idf <- bigram.ekon.counts %>%
  bind_tf_idf(bigram, offer, n)

library(forcats)
ekon_tf_idf %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf))) +
  geom_col(show.legend = FALSE)

write.csv(ekon_tf_idf, "ekon_df_idf.csv")


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

offers.ekon.word.counts <- offers.ekon.word %>%
  count(word, sort = TRUE)

set.seed(1234) # for reproducibility 

par(mar=c(1,1,1,1))
wordcloud(words = offers.ekon.word.counts$word, freq = offers.ekon.word.counts$n, min.freq = 170, max.words=200, 
          random.order=FALSE, colors=brewer.pal(8, "Dark2"))

wordcloud(words = offers_cleaned$word, freq = offers_cleaned$n, min.freq = 5, max.words=200, 
          random.order=FALSE, colors=brewer.pal(8, "Dark2"))

write.csv(dfos, "dfos.csv", fileEncoding="WINDOWS-1250")


job.words.cleaned <- df %>%
  unnest_tokens(word, title) %>%
  anti_join(stop.words) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

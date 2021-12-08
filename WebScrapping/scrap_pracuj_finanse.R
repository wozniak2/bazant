require(rvest)
require(purrr)

# Modify encoding
Sys.setlocale(category = "LC_ALL", locale = "Polish")

# wklejamy pobrany ze strony pracuj.pl link
url <- "https://www.pracuj.pl/praca?rd=30&cc=5003%2c5008%2c5027%2c5032&pn="


# funkcja mapująca; z każdej podstrony wyodrębniane są nazwy ogłoszeń oraz linki do szczegółwej treści
# każej oferty pracy. Procedura jest dwustopniowa. Po wyodrębnieniu linków, są one po kolei otwierane
# i w ramach każdego z nich mapowane jest pole z opisem oferty. Pole to zostało wskazane z wykorzystaniem
# przeglądarki internetowej Google Chrome i narzędzia Selector Gadget https://selectorgadget.com/ (offer-details_title-link)

# wersja alternatywna z opcją ciasteczek
# install docker, splash, disable private mode in splash
# docs: https://splash.readthedocs.io/en/stable/faq.html#how-do-i-disable-private-mode
# install splash (once docker is running): docker pull scrapinghub/splash
# from command line: docker run -it -p 8050:8050 scrapinghub/splash --disable-private-mode
# btw introduction to splash: https://cran.r-project.org/web/packages/splashr/vignettes/intro_to_splashr.html
# wpisujemy liczbę podstron


require(splashr)
require(magick)
require(tcltk)

last.page <- 135
page.p.finan <- NULL
pb <- tkProgressBar(title = "progress bar", min = 0,
                    max = last.page, width = 300)
map_df(1:last.page, function(i){
  
  print(i)
  setTkProgressBar(pb, i, label=paste( round(i/last.page*100, 0),
                                       "% done"))
  page <- read_html(paste0(url,i))

  data.frame(title = html_text(html_nodes(page,".offer-details__title-link")),
             link = html_attr(html_nodes(page,".offer-details__title-link"), "href"),
               job = html_text(html_nodes(page,".offer-labels__item--location")))
  
  
  }) -> page.p.finan

close(pb)
setwd("C:/Users/wozni/Google Drive/UAM/PowerBażant/scripts/")
write.csv(page.p.finan, "pagep_finanse.csv")


#  clear the missing data
page.p.finan <- page.p.finan[complete.cases(page.p.finan), ]



# wczytujemy biblioteke 'dplyr' i konwerujemy dane do tabeli tibble; konwertujemy dane na tekst
library(dplyr)
title.p_df <- as_tibble(page.p.finan$title)
title.p_df <- mutate(title.p_df, text = as.character(page.p.finan$title))

# liczy występowanie słów w poszczególnych ogłoszeniach; bez sensu?, potrzebne do statystyk idf
library(tidytext)
title.p.word <- title.p_df %>%
  unnest_tokens(word, text) %>%
  count(value, word, sort = TRUE)

# wczytujemy słowniki słów niechcianych
data(stop_words)
source("C:/.../stop_words.R")

title.p.word <- title.p.word %>%
  anti_join(stop.words)

# tworzymy prosty wykres słupkowy dla częstości wystąpienia poszczególnych słów w nazwie oferty
require(ggplot2)
plot1.p <- title.p.word %>%
  count(word, sort = TRUE) %>%
  filter(n > 30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# tworzymy chmurę słów 
require(wordcloud)
word.n <- count(title.word, word)
wordcloud(words=word.n$word,freq=word.n$n)

# ngrams for job title 
p_bigrams <- title.p_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

library(tidyr)
p_bigrams_separated <- p_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")


p_bigrams_filtered <- p_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% stop.words$word) %>%
  filter(!word2 %in% stop.words$word)

# new bigram counts:
p_bigram_counts <- p_bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

p_bigrams_united <- p_bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

p_bigram_united_counts <- p_bigrams_united %>%
  count(bigram, sort=TRUE)

plot2.p <- p_bigram_united_counts %>%
  filter(n > 8) %>%
  ggplot(aes(x= reorder(bigram,n), y=n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

library(gridExtra)
grid.arrange(plot1.p, plot2.p, ncol=2)



# Funkcja mapująca poszczególne ogłoszenia o pracę
# Każdy z linków (ogłoszeń) zgromadzonych w bazie 'domain' jest otwierany, następnie
# ekstraktujemy pole zawierające opis oferty '#description', z zawężeniem do wymagań 'nth-child(2)'
# Tutaj też wykorzystno SelectorGadget do identyfikacji selektorów CSS
# .t


require(tcltk)
pb <- tkProgressBar(title = "progress bar", min = 0,
                    max = length(page.p.finan$link), width = 300)

map_df(1:length(page.p.finan$link), function(i){
  
    if((i %% 30) == 0){
      message("taking a break")
      Sys.sleep(15 + sample(5:15, 1))
    }
  
  tryCatch({(print(page.p.finan$link[i]))}, 
           error=function(e) print("Catch Error"))
  print(i)
  setTkProgressBar(pb, i, label=paste( round(i/length(page.p.finan$link)*100, 0),
                                       "% done"))
  
  pp <- tryCatch({
          read_html(paste0(as.character(page.p.finan$link[i])))}, 
            error=function(e) read_html(paste0(as.character(page.p.finan$link[1]))))
  
  tryCatch({
    data.frame(word = html_text(html_nodes(pp,".OfferView2JLZCu")),
               offer = page.p.finan$title[i])}, error=function(e) data.frame())
          
}) -> offers.p.finan
close(pb)


require(tcltk)
pb <- tkProgressBar(title = "progress bar", min = 0,
                    max = length(page.p.finan$link), width = 300)

# mapowanie pola '.desc' dla części ofert
map_df(1:length(page.p.finan$link), function(i){
  
  if((i %% 30) == 0){
    message("taking a break")
    Sys.sleep(15 + sample(6:16, 1))
  }
  
  tryCatch({(print(page.p.finan$link[i]))}, 
           error=function(e) print("Catch Error"))
  print(i)
  setTkProgressBar(pb, i, label=paste( round(i/length(page.p.finan$link)*100, 0),
                                       "% done"))
  
  pp <- tryCatch({
    read_html(paste0(as.character(page.p.finan$link[i])))}, 
    error=function(e) read_html(paste0(as.character(page.p.finan$link[1]))))
  
  tryCatch({
    data.frame(word = html_text(html_nodes(pp,".desc")),
               offer = page.p.finan$title[i])}, error=function(e) data.frame())
  
}) -> offers.p1.finan
close(pb)



offers.p.finan <- rbind(offers.p.finan, offers.p1.finan)

setwd("C:/Users/wozni/Google Drive/UAM/PowerBażant/scripts/")
write.csv(offers.p.finan, "offers_p_finan.csv")


# clear duplicates
offers <- data.frame(offers$word[!duplicated(offers$word)])
colnames(offers) <- "word"


# Przekształcamy dane do formatu 'tidytext'
offer_df <- as_tibble(offers$word)
offer_df <- mutate(offer_df, text = as.character(offers$word))

# Rozbijamy słowa na tokeny
offer.word <- offer_df %>%
  unnest_tokens(word, text)

# Czyścimy ze słów niechcianych
offer.word <- offer.word %>%
  anti_join(stop.words) %>%
  anti_join(stop_words)



# tworzymy prosty wykres słupkowy dla częstości wystąpienia poszczególnych słów w treści ogłoszeń
offer.word %>%
  count(word, sort = TRUE) %>%
  filter(n > 50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


# ngram analysis
# dzielimy ogłoszenia na "ngramy" - złączenia po dwa wyrazy
offers.bigrams <- offer_df %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

offers.bigrams %>%
  count(bigram, sort = TRUE)

# dzielimy wyrazy na kolumny
library(tidyr)
bigrams_separated <- offers.bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
  
# filtrujemy pod kątem słów niechcianych; jak się okazuje, te angielskie też są przydatne
data(stop_words)
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop.words$word) %>%
  filter(!word2 %in% stop.words$word) %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# zliczamy częstość ngramów w odfiltrowanej bazie
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

# łączymy 
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

# wchodzi w analizę trigramów, czyli złączeń 3 wyrazów, postępujemy analogicznie jak w poprzednim przypadku
# niezapominamy o odfiltrowaniu słów niechcianych
offer_df %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(
    !word1 %in% stop_words$word,
    !word2 %in% stop_words$word,
    !word3 %in% stop_words$word,
    !word1 %in% stop.words$word,
    !word2 %in% stop.words$word,
    !word3 %in% stop.words$word
    
  ) %>%
  
  count(word1, word2, word3, sort = TRUE)


# visualization of n-grams
library(igraph)

bigram_graph <- bigram_counts %>%
  filter(n > 10) %>%
  graph_from_data_frame()

library(ggraph)
set.seed(2017)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# idf = ln(n docs / n docs with term)
 
idf.offers <- ln(lenght())






require(rvest)
require(purrr)

# Modify encoding
Sys.setlocale(category = "LC_ALL", locale = "Polish")

# wklejamy pobrany ze strony pracuj.pl link
url <- "https://www.infopraca.pl/praca?d=20&dy=60&rw=40&pg="

url1 <- "&ct=audyt-ksiegowosc-kontrola-skarbowa&ct=bankowosc&ct=doradztwo-konsulting&ct=finanse&ct=ubezpieczenia"


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

last.page <- 35
pb <- tkProgressBar(title = "progress bar", min = 0,
                    max = last.page, width = 300)
map_df(1:last.page, function(i){
  
  print(i)
  setTkProgressBar(pb, i, label=paste( round(i/last.page*100, 0),
                                       "% done"))
  page <- read_html(paste0(url,i,url1))
  
  
  data.frame(title = html_text(html_nodes(page,".job-offer")),
             link = html_attr(html_nodes(page,".job-offer"), "href"))
    
              
    
}) -> page.finan.ip
close(pb)

setwd("C:/Users/wozni/Google Drive/UAM/PowerBażant/scripts/")
write.csv(page.finan.ip, "page_finan_ip.csv")

# retrieve data on localization of the job-offer
last.page <- 35
pb <- tkProgressBar(title = "progress bar", min = 0,
                    max = last.page, width = 300)
map_df(1:last.page, function(i){
  
  print(i)
  setTkProgressBar(pb, i, label=paste( round(i/last.page*100, 0),
                                       "% done"))
  page <- read_html(paste0(url,i,url1))

tryCatch({data.frame(job = html_text(html_nodes(page,".p-locality"))) }, error=function(e) return(NA) ) 

}) -> page.ip.loc
close(pb)
  
#  clear the missing data
page.ip.loc <- page.ip.loc[complete.cases(page.ip.loc), ]

page.ip.loc <- as.data.frame(page.ip.loc)

# wczytujemy biblioteke 'dplyr' i konwerujemy dane do tabeli tibble; konwertujemy dane na tekst
library(dplyr)
title.ip_df <- as_tibble(page.ip$title)
title.ip_df <- mutate(title.ip_df, text = as.character(page.ip$title))

# liczy występowanie słów w poszczególnych ogłoszeniach; bez sensu?, potrzebne do statystyk idf
library(tidytext)
title.ip.word <- title.ip_df %>%
  unnest_tokens(word, text) %>%
  count(value, word, sort = TRUE)

# wczytujemy słowniki słów niechcianych
data(stop_words)
source("C:/.../stop_words.R")

title.ip.word <- title.ip.word %>%
  anti_join(stop.words)


# tworzymy prosty wykres słupkowy dla częstości wystąpienia poszczególnych słów w nazwie oferty
require(ggplot2)
plot1.ip <- title.ip.word %>%
  count(word, sort = TRUE) %>%
  filter(n > 130) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()



# ngrams for job title 
ip_bigrams <- title.ip_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

library(tidyr)
ip_bigrams_separated <- ip_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

ip_bigrams_separated <- ip_bigrams_separated[complete.cases(ip_bigrams_separated), ]

ip_bigrams_filtered <- ip_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% stop.words$word) %>%
  filter(!word2 %in% stop.words$word)

# new bigram counts:
ip_bigram_counts <- ip_bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

ip_bigrams_united <- ip_bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

ip_bigram_united_counts <- ip_bigrams_united %>%
  count(bigram, sort=TRUE)

plot2.ip <- ip_bigram_united_counts %>%
  filter(n > 80) %>%
  ggplot(aes(x= reorder(bigram,n), y=n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

library(gridExtra)
grid.arrange(plot1.ip, plot2.ip, ncol=2)

# Funkcja mapująca poszczególne ogłoszenia o pracę
# Każdy z linków (ogłoszeń) zgromadzonych w bazie 'domain' jest otwierany, następnie
# ekstraktujemy pole zawierające opis oferty '#description', z zawężeniem do wymagań 'nth-child(2)'
# Tutaj też wykorzystno SelectorGadget do identyfikacji selektorów CSS
# .t
require(tcltk)
pb <- tkProgressBar(title = "progress bar", min = 0,
                    max = length(page.finan.ip$link), width = 300)
url2 <- "https://infopraca.pl"
map_df(1:length(page.finan.ip$link), function(i){
  
  if((i %% 500) == 0){
    message("taking a break")
    Sys.sleep(10)
  }
  
  try(print(page.finan.ip$link[i]))
  print(i)
  setTkProgressBar(pb, i, label=paste( round(i/length(page.finan.ip$link)*100, 0),
                                       "% done"))
  
  pp <- try(read_html(paste0(url2, page.finan.ip$link[i])))
  if (inherits(pp, 'try-error')) {read_html(paste0(url2, page.finan.ip$link[i-1]))} 
  
  tryCatch(data.frame(word = html_text(html_nodes(pp,".description")), offer=page.finan.ip$title[i]), error=function(e) data.frame())
  
}) -> offers.finan.ip
close(pb)

# clear duplicates
offers <- data.frame(offers$word[!duplicated(offers$word)])
colnames(offers) <- "word"


setwd("C:/Users/wozni/Google Drive/UAM/PowerBażant/scripts/")
write.csv(offers.ekon.ip, "offers_e_ip.csv")
























         
           
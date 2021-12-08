
require(rvest)
require(purrr)

# Modify encoding
Sys.setlocale(category = "LC_ALL", locale = "Polish")

# wklejamy pobrany ze strony pracuj.pl link
url <- "https://www.infopraca.pl/praca?d=20&dy=60&rw=40&pg="

url1 <- "&ct=it-bazy-danych&ct=it-erp&ct=it-konsulting&ct=it-programowanie-analizy&ct=it-project-management"


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

last.page <- 60
pb <- tkProgressBar(title = "progress bar", min = 0,
                    max = last.page, width = 300)
map_df(1:last.page, function(i){
  
  print(i)
  setTkProgressBar(pb, i, label=paste( round(i/last.page*100, 0),
                                       "% done"))
  page <- read_html(paste0(url,i,url1))
  Sys.sleep(1)
  
  
  data.frame(title = html_text(html_nodes(page,".job-offer")),
             link = html_attr(html_nodes(page,".job-offer"), "href"))
    
              
    
}) -> page.info.ip
close(pb)

setwd("C:/Users/wozni/Google Drive/UAM/PowerBażant/scripts/")
write.csv(page.info.ip, "page_info_ip.csv")

# retrieve data on localization of the job-offer to plot the map
last.page <- 61
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


# Funkcja mapująca poszczególne ogłoszenia o pracę
# Każdy z linków (ogłoszeń) zgromadzonych w bazie 'domain' jest otwierany, następnie
# ekstraktujemy pole zawierające opis oferty '#description'
# Tutaj też wykorzystno SelectorGadget do identyfikacji selektorów CSS
# .t
require(tcltk)
pb <- tkProgressBar(title = "progress bar", min = 0,
                    max = length(page.info.ip$link), width = 300)
url2 <- "https://infopraca.pl"
map_df(1:length(page.info.ip$link), function(i){
  
  if((i %% 500) == 0){
    message("taking a break")
    Sys.sleep(10)
  }
  
  try(print(page.info.ip$link[i]))
  print(i)
  setTkProgressBar(pb, i, label=paste( round(i/length(page.info.ip$link)*100, 0),
                                       "% done"))
  
  pp <- try(read_html(paste0(url2, page.info.ip$link[i])))
  if (inherits(pp, 'try-error')) {read_html(paste0(url2, page.info.ip$link[i-1]))} 
  
  tryCatch(data.frame(word = html_text(html_nodes(pp,".description")), offer=page.info.ip$title[i]), error=function(e) data.frame())
  
}) -> offers.info.ip
close(pb)

# save file fo further computations
setwd("C:/Users/wozni/Google Drive/UAM/PowerBażant/scripts/")
write.csv(offers.info.ip, "offers_info_ip.csv")


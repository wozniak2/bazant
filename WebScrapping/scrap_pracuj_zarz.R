require(rvest)
require(purrr)

# Modify encoding
Sys.setlocale(category = "LC_ALL", locale = "Polish")

# wklejamy pobrany ze strony pracuj.pl link
url <- "https://www.pracuj.pl/praca?rd=30&cc=5011%2c5034%2c5017%2c5018%2c5024%2c5025&pn="


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
page.p.zarz <- NULL
last.page <- 228
pb <- tkProgressBar(title = "progress bar", min = 0,
                    max = last.page, width = 300)
map_df(1:last.page, function(i){
  
  print(i)
  setTkProgressBar(pb, i, label=paste( round(i/last.page*100, 0),
                                       "% done"))
  
  page <- render_html(url = paste0(url,i))
  
  data.frame(title = html_text(html_nodes(page,".offer-details__title-link")),
             link = html_attr(html_nodes(page,".offer-details__title-link"), "href"),
              job = html_text(html_nodes(page,".offer-labels__item--location")))
  
  
  }) -> page.p.zarz
close(pb)

setwd("C:/Users/wozni/Google Drive/UAM/PowerBażant/scripts/")
write.csv(page.p.zarz, "pagep_zarz.csv")



# Funkcja mapująca poszczególne ogłoszenia o pracę
# Każdy z linków (ogłoszeń) zgromadzonych w bazie 'domain' jest otwierany, następnie
# ekstraktujemy pole zawierające opis oferty '#description', z zawężeniem do wymagań 'nth-child(2)'
# Tutaj też wykorzystno SelectorGadget do identyfikacji selektorów CSS
# .t

require(tcltk)
pb <- tkProgressBar(title = "progress bar", min = 0,
                    max = length(page.p.zarz$link), width = 300)

map_df(1:length(page.p.zarz$link), function(i){
  
    if((i %% 30) == 0){
      message("taking a break")
      Sys.sleep(15 + sample(5:15, 1))
    }
  
  tryCatch({(print(page.p.zarz$link[i]))}, 
           error=function(e) print("Catch Error"))
  print(i)
  setTkProgressBar(pb, i, label=paste( round(i/length(page.p.zarz$link)*100, 0),
                                       "% done"))
  
  pp <- tryCatch({
          read_html(paste0(as.character(page.p.zarz$link[i])))}, 
            error=function(e) read_html(paste0(as.character(page.p.zarz$link[1]))))
  
  tryCatch({
    data.frame(word = html_text(html_nodes(pp,".OfferView2JLZCu")),
               offer = page.p.zarz$title[i])}, error=function(e) data.frame())
          
}) -> offers.p.zarz
close(pb)

# dodatkowe pole do mapowania (po zmianach na portalu w okolicach lipca 2021)
require(tcltk)
pb <- tkProgressBar(title = "progress bar", min = 0,
                    max = length(page.p.zarz$link), width = 300)

# mapowanie kolejnego pola '.OfferView1PIsMp' dla części ofert
map_df(1:length(page.p.zarz$link), function(i){
  
  if((i %% 30) == 0){
    message("taking a break")
    Sys.sleep(15 + sample(5:15, 1))
  }
  
  tryCatch({(print(page.p.zarz$link[i]))}, 
           error=function(e) print("Catch Error"))
  print(i)
  setTkProgressBar(pb, i, label=paste( round(i/length(page.p.zarz$link)*100, 0),
                                       "% done"))
  
  pp <- tryCatch({
    read_html(paste0(as.character(page.p.zarz$link[i])))}, 
    error=function(e) read_html(paste0(as.character(page.p.zarz$link[1]))))
  
  tryCatch({
    data.frame(word = html_text(html_nodes(pp,".OfferView1PIsMp")),
               offer = page.p.zarz$title[i])}, error=function(e) data.frame())
  
}) -> offers.p1.zarz
close(pb)


# łączymy oferty po ekstrkacji pól
offers.p.zarz <- rbind(offers.p.zarz, offers.p1.zarz)

setwd("C:/Users/wozni/Google Drive/UAM/PowerBażant/scripts/")
write.csv(offers.p.zarz, "offers_p_zarz.csv")


# clear duplicates
# offers <- data.frame(offers$word[!duplicated(offers$word)])
# colnames(offers) <- "word"


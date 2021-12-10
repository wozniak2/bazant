# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# -------------TWORZENIE MAP - konieczne dane z portali-------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

require(ggmap)

# register in GoogleMaps with API
# please provide your Google API key

register_google(key = "xxxxxxxxxxxxxxxx")

tab <- page.ip.loc
tab1 <- as.data.frame(page.p.zarz$job)
colnames(tab1) <- "job"
colnames(tab) <- "job"

tab <- rbind(tab, tab1)

tab <- as.data.frame(table(tab$job))

# to extract only those localizations that are more frequent than 1
# tab <- subset(tab, tab$Freq > 1)

library(qdap)
tab$Var1 <-  genX(tab$Var1, " (", ")")


# geocode the extracted localizations
tab$latlon <- geocode(tab$Var1, output = c("latlon"))
tab$lat <- tab$latlon$lat
tab$lon <- tab$latlon$lon

map <- get_map(location = "Poland", zoom=6, scale=1)

ggmap(map, extend="device", maptype="terrain", color="color") + 
  geom_point(data=tab, aes(x=lon, y=lat, size=Freq), alpha=0.9, shape=19)

ggmap(map, extend="device", maptype="terrain", color="color") + 
stat_density2d(data=tab, aes(x = lon, y = lat, fill = ..level..), show.legend = FALSE, size = 0.3, bins = 6, alpha=0.5, n=300, contour=T, geom = "polygon" ) + 
  scale_fill_gradient(low = "grey", high = "red") + 
  geom_point(data=tab, aes(x=lon, y=lat, size=Freq), col="blue", alpha=0.4, shape=19)



# Wykres częstości dla najczęściej pojawiających się lokalizacji
# Modify encoding

Sys.setlocale(category = "LC_ALL", locale = "Polish")
colnames(tab)[1] <- "job"
tab$job[tab$job == "Warsaw"] <- "Warszawa"
tab$job[tab$job == "Krakow"] <- "Kraków"
tab$job[tab$job == "Wroclaw"] <- "Wrocław"
tab$job[tab$job == "Poznan"] <- "Poznań"
require(stringr)
tab$job <- str_remove_all(tab$job, " ")


bar <- tab[, c(1,2)]
bar <- bar[complete.cases(bar), ]
bar <- subset(bar, bar$Freq>90)

# z uwagi na rozbieżności w kodowaniu polskich znaków, konieczność ręcznej edycji
library("openxlsx")
# zapisujemy plik w formacie Excel
write.xlsx(bar, "barzarz.xlsx")
# po edycji wczytujemy plik ponownie 
bar <- read.xlsx("barzarz.xlsx")

# tworzymy wykres słupkowy
bar <- as.data.frame(bar)
bar <- bar[order(bar$Freq,decreasing = FALSE),]
par(mar=c(2,8,0,1))
barplot(bar$Freq, names.arg=bar$job, horiz=TRUE, las=1, xlim=c(0,10000), cex.names=0.9)





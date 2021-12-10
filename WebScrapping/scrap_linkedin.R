# URL of the PUEB LinkedIn page 
user_url <- "https://www.linkedin.com/school/uniwersytet-ekonomiczny-w-poznaniu/people/?keywords=uniwersytet%20ekonomiczny%20w%20poznaniu"

# since the information isn't available without being logged in, the web
# scraper needs to log in. Provide your LinkedIn user/pw here (this isn't stored
# anywhere, it's just used to log in during the scrape session)

############################

library(RSelenium)
library(rvest)
library(stringr)
library(xml2)
library(tidyverse)

userID = "wozniac@gmail.com"
passID = "3eWIZjamw"

# takes a couple seconds and might throw a warning, but ignore the warning
try(rsDriver(port = 4444L, browser = 'firefox'))
remDr <- remoteDriver()
remDr$open()
remDr$navigate("https://www.linkedin.com/login")    # go to login page

user <- remDr$findElement(using = 'id',"username")
user$sendKeysToElement(list(userID,key="tab"))

pass <- remDr$findElement(using = 'id',"password")
pass$sendKeysToElement(list(passID,key="enter"))    # logs in the user

Sys.sleep(3) # give the page time to fully load

remDr$navigate(user_url)

Sys.sleep(3) # give the page time to fully load


scroll.num <- 390
webElem <- remDr$findElement("css", "body")
for (i in 1:scroll.num) {
  webElem$sendKeysToElement(list(key = "end"))
  Sys.sleep(1)
  print(i)
}

# Modify encoding
Sys.setlocale(category = "LC_ALL", locale = "Polish")

# extract three elemenents from web page - job, name and profile link
#parent.jobs = remDr$findElements("css", value=".lt-line-clamp.lt-line-clamp--multi-line.ember-view")
#parent.name = remDr$findElements("css", value=".ember-view.link-without-visited-state")
#parent.fake.name = remDr$findElements("xpath", value="//*[contains(text(),'Członek LinkedIn')]")


parent.links = remDr$findElements(using = "css", "[href]")


# convert data to lists
#lst.jobs <- sapply(parent.jobs, function(x){x$getElementText()})
#lst.name <- sapply(parent.name, function(x){x$getElementText()})

lst.links <- sapply(parent.links, function(x){x$getElementAttribute("href")})

# clean links and double entries
library(tidyverse)
lst.links <- map(lst.links, keep, str_detect, '^https://www.linkedin.com/in/')
links.df <- as.data.frame(unlist(lst.links))

links.df <- (unique(links.df))
require(dplyr)  
  links.df <- links.df %>% 
    filter(row_number() <= n()-2)


# append to data frame
#names.df <- as.data.frame(unlist(lst.name))
#names.df <- cbind(names.df, links.df)
#colnames(names.df) <- c("name", "link")
#jobs.df <- as.data.frame(unlist(lst.jobs))

# prepare empty cells for extracting skills
namevector <- c("job", "school1", "school2", "school3", "time1", "time2",  "time3", "skill1", "skill2", "skill3", "skill4", "skill5", "skill6", "skill7", "skill8", "skill9", "skill10",
                "skill11", "skill12", "skill13", "skill14", "skill15", "skill16", "skill17", "skill18", "skill19", "skill20",
                "skill21", "skill22", "skill23", "skill24", "skill25", "skill26", "skill27", "skill28", "skill29", "skill30",
                "skill31", "skill32", "skill33", "skill34", "skill35", "skill36", "skill37", "skill38", "skill39", "skill40")
links.df[ , namevector] <- NA
colnames(links.df)[1] <- "links"
# function extracting skills from users profiles
# NA if skills are not included into profile

for (i in 907:927) {
  
  
    remDr$navigate(links.df$links[i])
  
    Sys.sleep(1+ runif(1, 0, 1))                          
                                                        
    parent.job <- remDr$findElements("css", value = ("h2.mt1"))
    lst.job <- sapply(parent.job, function(x){x$getElementText()})
    webElem <- remDr$findElement("css", "body")
    webElem$sendKeysToElement(list(key = "end"))
    
    Sys.sleep(1+ runif(1, 0, 1))
    
    tryCatch({
      
      parent.school1 <- remDr$findElements("xpath", value =("//*[@class='pv-entity__school-name t-16 t-black t-bold']"))
      
      lst.school1 <- sapply(parent.school1, function(x){x$getElementText()})
      
      parent.school1.time <- remDr$findElements("css", value =(".pv-entity__dates > span:nth-child(2)"))
      lst.school1.time <- sapply(parent.school1.time, function(x){x$getElementText()})
      
            }, error=function(e){print("No school element found")})
      
    Sys.sleep(1 + runif(1, 0, 1))
      
      tryCatch({
        parent.skills <- NA
        lst.skills <- NA
        
         more.skills.button <- remDr$findElement("css", value="button.pv-profile-section__card-action-bar")
          more.skills.button$clickElement()
          
 
         Sys.sleep(0.86 + runif(1, 0, 1))
         
             parent.skills <- remDr$findElements("xpath", value = ("//span[@class='pv-skill-category-entity__name-text t-16 t-black t-bold']"))
             lst.skills <- sapply(parent.skills, function(x){x$getElementText()})
             
             }, error=function(e) {print("no skills section found ")} )
         
      tryCatch({    
             links.df[i, 2] <- t(unlist(lst.job))
             links.df[i, 3:(length(unlist(lst.school1))+2)] <- t(unlist(lst.school1))
             links.df[i, 6:(length(unlist(lst.school1.time))+5)] <- t(unlist(lst.school1.time))
             links.df[i, 9:(length(unlist(lst.skills))+8)] <- t(unlist(lst.skills)) 
              
      }, error=function(e){ })
  }
library("WriteXLS")
write_xlsx(links.df, "linked.xlsx")










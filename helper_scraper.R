
library(rvest)
library(xml2)
library(dplyr)
library(tidyr)
library(httr)
library(magrittr)
library (data.table)
library(jsonlite)
library(readr)
library(forcats)

library(stringi)

library(splitstackshape)
library(purrr)

library(extrafont)
library(extrafontdb)
library(ggplot2)
library(xaringan)

# Необхідні дані завантажити ####


# Download the current acts - the signed and active bills who 
bills_acts_skl9 <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_acts-skl9.csv")

# The executives of Rada 9
bills_executives_skl9 <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_executives-skl9.csv", 
                                  fileEncoding = "UTF-8")%>%
  filter(type=="mainExecutive")%>% # We need the main 
  select(-convocation, -person_id, -organization, -post)%>%
  mutate(bill_id=as.character(bill_id))

# The main file with bills' activities
bills_main_skl9 <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_main-skl9.csv", fileEncoding = "UTF-8")%>%
  #select(bill_id, number,type, rubric, subject,currentPhase_title)%>%
  mutate(number=as.character(number))

# Скачати приналежність до комітетів перед стартом з цього файлу **helper_komitety.R**

# Get MPs ####

mps09 <- read.csv("https://data.rada.gov.ua/ogd/mps/skl9/mps09-data.csv", fileEncoding = "UTF-8")%>%
  #select(rada_id, full_name, date_end)%>%
  filter(date_end=="")%>%
  select(id, full_name, region_name)

mps09$id <- as.list(mps09$id)

# Amends. Part-1 ####
get_amends <- function(){
  
  for(i in mps09$id) {
    
    url <- paste0("http://w1.c1.rada.gov.ua/pls/pt2/reports.dep2?PERSON=", i, "&SKL=10")
    
    url_table <- read_html(url, 
                           encoding = "Windows-1251")%>%
      xml_find_all("//table") %>% 
      html_table()
    
    # Amendments are located in a second table
    # Some tables would be empty because MPs' didn't amend yet
    
    results_scraping <- url_table[[2]]%>% 
      data.frame()%>% 
      replace(!nzchar(.), NA)
    
    if (nrow(results_scraping)>0) {
      
      name <- read_html(url, encoding = "Windows-1251")%>%
        html_node("span b")%>%
        html_text(trim = TRUE)
      
      results_scraping$name <- name
      
      
      # Write data 
      write.table(results_scraping, fileEncoding = "UTF-8", 
                  paste0("data/Amends_scraping_", Sys.Date(),   
                         ".csv"), 
                  append = TRUE, col.names = FALSE, 
                  row.names = FALSE, sep = ";")  
      
      message(paste(i, '_downloaded'))
    }
    Sys.sleep(0.5)
    
  }
}

amends <- get_amends()

# Amends. Part-2 ####

factions_09$id <- as.list(factions_09$id)

get_offers_table <- function(){
  
  for(i in factions_09$id) {
    
    url <- paste0("https://itd.rada.gov.ua/billInfo/LawmakingActivity/OffersTables/", i, "/10")
    
    url_table <- read_html(url, 
                           encoding = "UTF-8")%>%
      xml_find_all("//table") %>% 
      html_table()
    
    # Amendments are located in a second table
    # Some tables would be empty because MPs' didn't amend yet
    
    results_scraping <- url_table[[1]]%>% 
      data.frame()%>% 
      replace(!nzchar(.), NA)
    
    if (nrow(results_scraping)>0) {
      
      name <- read_html(url, encoding = "UTF-8")%>%
        html_node(".h2")%>%
        html_text(trim = TRUE)
      
      results_scraping$name <- name
      
      
      # Write data 
      write.table(results_scraping, fileEncoding = "UTF-8", 
                  paste0("data/Amends_OffersTable_", Sys.Date(),   
                         ".csv"), 
                  append = TRUE, col.names = FALSE, 
                  row.names = FALSE, sep = ";")  
      
      message(paste(i, '_downloaded'))
    }
    Sys.sleep(0.2)
    
  }
}

offers_table <- get_offers_table()

# Unite two tables of amends ####

# Перша таблиця
amends_scraping <- read_delim(paste0("data/Amends_scraping_2021-07-22", ".csv" ), # Change date
                              ";", escape_double = FALSE, col_names = FALSE, 
                              col_types = cols(X1 = col_character()), 
                              trim_ws = TRUE) %>% 
  mutate(X1=stringr::str_pad(X1, 4, pad = "0")) 

AMENDS_f <- amends_scraping%>%
  separate(X11, c("surname", "name", "parent"), sep = " ")%>%
  unite(fullname, c("surname", "name", "parent"), sep = " ")%>%
  rename(
    number = X1,
    name_bill = X2,
    reading_date = X3,
    totally = X4,
    accepted = X5,
    rejected = X6,
    partly_accepted = X7,
    redakciyno_accepted = X8,
    others = X9,
    no_conclusion = X10)

# Друга таблиця 
offers_table_dl <- read_delim(paste0("data/Amends_OffersTable_2021-07-22", ".csv" ), # Change date
                              ";", escape_double = FALSE, col_names = FALSE, 
                              col_types = cols(X1 = col_character()), 
                              trim_ws = TRUE) %>%
  mutate(X1=stringr::str_pad(X1, 4, pad = "0")) %>%
  separate(X12, c("fullname", "trash_1", "trash_2"), sep = ",", extra="drop")%>%
  select(-trash_1, -trash_2, -X2)%>%
  rename(
    number = X1,
    name_bill = X3,
    reading_date = X4,
    totally = X5,
    accepted = X6,
    rejected = X7,
    partly_accepted = X8,
    redakciyno_accepted = X9,
    others = X10,
    no_conclusion = X11)

# Bind together ####
binded_amends <- rbind(AMENDS_f, offers_table_dl) %>% 
  left_join(factions_09, by=c("fullname"="fullname"))%>%
  mutate(number=as.character(number))%>%
  left_join(bills_main_skl9, by=c("number"="number"))%>%
  mutate(bill_id=as.character(bill_id))%>%
  left_join(bills_executives_skl9, by=c("bill_id"="bill_id"))

# Wrangling data ####

mps09 <- read.csv("https://data.rada.gov.ua/ogd/mps/skl9/mps09-data.csv", fileEncoding = "UTF-8")%>%
  filter(date_end=="")%>%
  select(id, full_name, region_name) %>% 
  mutate(id=as.character(id))



# Grouping files ####


# Grouping by MPs' SUM #### 
amends_by_mps <- binded_amends%>%
  group_by(factions, fullname, number, region_name)%>%
  summarise(totally=sum(totally),
            accepted=sum(accepted),
            partly_accepted=sum(partly_accepted),
            rejected=sum(rejected),
            redakciyno_accepted=sum(redakciyno_accepted),
            others=sum(others),
            no_conclusion=sum(no_conclusion))%>% 
  separate(fullname, c("surname","name","fathername"), sep = " ")%>%
  unite(short_name, c("name","surname"), sep = " ") 

save(amends_by_mps, file=paste0("amends_by_mps", ".Rda"))


# Grouping by MPs' SUM without numbers #### 

amends_by_mps_WO <- binded_amends%>%
  group_by(factions, fullname, region_name)%>%
  summarise(totally=sum(totally),
            accepted=sum(accepted),
            partly_accepted=sum(partly_accepted),
            rejected=sum(rejected),
            redakciyno_accepted=sum(redakciyno_accepted),
            others=sum(others),
            no_conclusion=sum(no_conclusion))%>% 
  separate(fullname, c("surname","name","fathername"), sep = " ")%>%
  unite(short_name, c("name","surname"), sep = " ") 

head(amends_by_mps_WO,1)

save(amends_by_mps_WO, file =  "amends_by_mps_WO.Rda")



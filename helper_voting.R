

# Комітети ####

# Членство в комітетах 
# Scraping of the komit_members is in Rada-9 file

View(membership_k)

#  Factions-1 ####

get_factions_open <- function(){
  posts <- read_csv("https://data.rada.gov.ua/ogd/mps/skl9/mp-posts_ids.csv")
  posts_ids <- read_tsv("https://data.rada.gov.ua/ogd/mps/skl9/mp-posts_unit.txt", 
                        locale(encoding = "windows-1251"), col_names = F, col_types = NULL) %>% 
    rename(unit_id = X1, unit = X2)
  mps <- read_csv("https://data.rada.gov.ua/ogd/mps/skl9/mps09-data.csv")
  
  factions_full <- posts %>% 
    left_join(mps[, c("rada_id" ,"id", "full_name", "date_end", "region_name", "gender")], by = c("mp_id" = "id")) %>% 
    left_join(posts_ids) %>% 
    filter(unit_type %in% c("grp", "fra")) %>% 
    select(mp_id, full_name, unit)
  
  factions_df <-  mps %>% 
    #filter(is.na(resignation_text)) %>% 
    select(rada_id, id, full_name, region_name, date_end, gender) %>% 
    left_join(factions_full, by = c("id" = "mp_id", "full_name")) %>% 
    mutate(unit = ifelse(is.na(unit), "Позафракційні", unit)) %>% 
    rename(factions = unit, fullname = full_name) %>% 
    mutate(factions = recode(factions, 
                             `Фракція ПОЛІТИЧНОЇ ПАРТІЇ "СЛУГА НАРОДУ"` = "Слуга Народу",
                             `Фракція Політичної Партії "ГОЛОС" у Верховній Раді України дев'ятого скликання` = "ГОЛОС",
                             `Фракція ПОЛІТИЧНОЇ ПАРТІЇ "ЄВРОПЕЙСЬКА СОЛІДАРНІСТЬ"` = "ЄС",
                             `Фракція Політичної партії "ОПОЗИЦІЙНА ПЛАТФОРМА - ЗА ЖИТТЯ" у Верховній Раді України` = "ОПЗЖ",
                             `Фракція політичної партії Всеукраїнське об'єднання "Батьківщина" у Верховній Раді України дев'ятого скликання` = "Батьківщина",
                             `Група "Партія "За майбутнє"` = "За майбутнє",
                             `Група "ДОВІРА"`= "ДОВІРА"))%>%
    mutate(rada_id=as.character(rada_id), id=as.integer(id)) %>% 
    mutate(rada_id=recode(rada_id, 
                          "208|438"="438"))%>% # Можливо тимчасово, бо не встигли змінити айдішник Радіної
    mutate(date_end = ifelse(is.na(date_end), "", date_end))%>% # Replace NA with a blank
    mutate(region_name = ifelse(is.na(region_name), "", region_name)) # Replace NA with a blank
  
  return(factions_df)
}

factions_09 <- get_factions_open()%>%
  select( -gender, ) %>% 
  # Поєднуємо членство в комітетах і фракціх/групи ВРУ-9 
  #left_join(membership_k, by=c("fullname"="full_name_k")) %>% 
  filter(date_end=="")


#  Factions-2 by voting ####

factions <- function() {
  
  factions <- fromJSON(readLines(file("http://w1.c1.rada.gov.ua/pls/radan_gs09/od_data_frac?type_data=j")))
  
  factions <- factions$faction%>%
    mutate(name = recode(name,
                         `Фракція політичної партії "СЛУГА НАРОДУ"` = "Слуга Народу",
                         `Фракція політичної партії "ГОЛОС"` = "ГОЛОС",
                         `Фракція політичної партії "Європейська солідарність"` = "ЄС",
                         `Фракція політичної партії "ОПОЗИЦІЙНА ПЛАТФОРМА-ЗА ЖИТТЯ"` = "ОПЗЖ",
                         `Фракція політичної партії Всеукраїнське об’єднання "Батьківщина"` = "Батьківщина",
                         `Депутатська група "За майбутнє"` = "За майбутнє",
                         `Депутатська група "ДОВІРА"`= "ДОВІРА",
                         `Депутатська група "Партія "За майбутнє"`= "За майбутнє") # 8, June 2020
    )%>%
    select(-type)
  
  return(factions)
}

factions <- factions()



# mps09 ####

mps09 <- function() {
  
  mps09 <- fromJSON(readLines(file("http://w1.c1.rada.gov.ua/pls/radan_gs09/od_data_dep?type_data=j")))
  pers_vote <- mps09$mp
  
  # Fix names according to names of MPs in main mps_09 DF and factions_09 DF
  
  pers_vote$name[pers_vote$name == "Ар’єв Володимир Ігорович"] <- "Ар'єв Володимир Ігорович"
  pers_vote$name[pers_vote$name == "Безугла Мар’яна Володимирівна"] <- "Безугла Мар'яна Володимирівна"
  pers_vote$name[pers_vote$name ==  "Володіна Дар’я Артемівна"] <- "Володіна Дар'я Артемівна"
  pers_vote$name[pers_vote$name ==  "Джемілєв Мустафа  "] <- "Джемілєв Мустафа" 
  pers_vote$name[pers_vote$name ==  "Заблоцький Мар’ян Богданович"] <- "Заблоцький Мар'ян Богданович" 
  pers_vote$name[pers_vote$name ==  "Кожем’якін Андрій Анатолійович"] <- "Кожем'якін Андрій Анатолійович"   
  pers_vote$name[pers_vote$name ==  "М’ялик Віктор Ничипорович"] <- "М'ялик Віктор Ничипорович" 
  pers_vote$name[pers_vote$name ==  "Медяник В’ячеслав Анатолійович"] <- "Медяник В'ячеслав Анатолійович"
  pers_vote$name[pers_vote$name ==  "Салійчук Олександр В’ячеславович"] <- "Салійчук Олександр В'ячеславович"   
  pers_vote$name[pers_vote$name ==  "Циба Тетьяна Вікторівна"] <- "Циба Тетяна Вікторівна" 
  pers_vote$name[pers_vote$name ==  "Красносільська Анастасія Олегівна"] <- "Радіна Анастасія Олегівна" # Changed surname in March 2020
  
  pers_vote$id_mp[pers_vote$id_mp ==  "208"] <- "438" # Красносільська Changed surname in March 2020
  
  return(pers_vote)
}
mps09 <- mps09()  


# MPs' ####
mps_09 <- read.csv("https://data.rada.gov.ua/ogd/mps/skl9/mps09-data.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)%>%
  select(rada_id, id, full_name, region_name, date_end)%>%
  filter(date_end=="") 


# Data prep for voting ####

agenda <- function(){ 
  
  agenda <- fromJSON(readLines(file("https://data.rada.gov.ua/ogd/zal/ppz/skl9/dict/agendas_9_skl.json")))
  questions <- agenda$agenda$question
  quest <- reduce(questions, bind_rows) # More efficient way
  quest <- quest%>%
    select(-reporter_question)
  return(quest)
  
}

agenda <- agenda()

# Save the agenda
save(agenda, file =  paste0("agenda",".Rda"))
# Load the agenda
#load("agenda.Rda")

# Події у ВРУ з таймінгом ####
event_question <- read.csv("https://data.rada.gov.ua/ogd/zal/ppz/skl9/plenary_event_question-skl9.csv", 
                           fileEncoding = "UTF-8")%>%
  select(-date_agenda, -id_question, -date_question)

#  Результати поіменного голосування депутатів 9 скл ####
personal_vote <- read.delim("https://data.rada.gov.ua/ogd/zal/ppz/skl9/plenary_vote_results-skl9.tsv")%>%
  mutate(id_question=as.character(id_question))%>%
  left_join(event_question, by=c("id_event"="id_event")) 

save(personal_vote, file =  paste0("personal_vote",".Rda"))
load("personal_vote.Rda")


# Приєднуємо усі законодавчі активності ####   

zp_names <- personal_vote%>%
  left_join(agenda, by=c("id_question"="id_question"))%>%
  left_join(bills_main_skl9, by=c("number_question"="number"))

save(zp_names, file =  paste0("zp_names",".Rda"))
#load("zp_names.Rda")


# Voting ####

amendments_voting <- function (){
  
  personal_vote <- read.delim("https://data.rada.gov.ua/ogd/zal/ppz/skl9/plenary_vote_results-skl9.tsv")%>%
    mutate(id_question=as.character(id_question))
  
  bills_main_skl9 <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_main-skl9.csv", 
                              fileEncoding = "UTF-8")%>%
    select(bill_id, number,type, rubric, subject, currentPhase_title)
  
  bills_executives <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_executives-skl9.csv",
                               fileEncoding = "UTF-8")%>%
    filter(type=="mainExecutive")%>%
    select(-convocation, -person_id, -organization, -post)
  
  amendments_voting <- read.csv("https://data.rada.gov.ua/ogd/zal/ppz/skl9/plenary_event_question-skl9.csv", 
                                fileEncoding = "UTF-8")%>%
    select( -date_event, -type_event,  -id_question, -date_agenda) %>%
    left_join(personal_vote, by=c("id_event"="id_event"))%>%
    filter(grepl("Поіменне голосування про поправку №|Поіменне голосування про підтримку поправки №", name_event)) %>% 
    left_join(agenda, by=c("id_question"="id_question"))%>%
    # Download bills in bills_09.R
    left_join(bills_main_skl9, by=c("number_question"="number"))%>%
    left_join(bills_executives, by=c("bill_id"="bill_id"))
  
  return(amendments_voting) 
  
}

amendments_voting <- amendments_voting()

save(amendments_voting, file =  paste0("amendments_voting", ".Rda"))

# Amends 226 
act_amends <- amendments_voting %>% 
  filter(for.>225)

save(act_amends, file =  paste0("act_amends",".Rda"))

# Roll call voting - Amends ####

out_amends <- function(){
  
  out_amends <- cSplit(amendments_voting, "results", sep="|", "long")%>%
    separate(results, c("mps_id", "faction", "vote_status"), ":")%>%
    mutate(faction = recode(faction,
                            `0` = "Позафракційні",
                            `1` = "Слуга Народу",
                            `2` = "ОПЗЖ",
                            `3` = "Батьківщина",
                            `4` = "ЄС",
                            `5` = "ГОЛОС",
                            `6` = "За майбутнє",
                            `7`=  "ДОВІРА",
                            `8` = "За майбутнє"
    ))%>%
    mutate(vote_status = recode(vote_status,
                                `0` = "Відсутній",
                                `1` = "За",
                                `2` = "Проти",
                                `3` = "Утримався",
                                `4` = "Не голосував",
                                `5` = "Присутній"))
  
  
  out_amends$mps_id[out_amends$mps_id ==  "208"] <- "438" # Changed surname in March 2020
  
  out_amends <- out_amends%>%
    left_join(factions_09, by=c("mps_id"="rada_id"))%>%
    filter(date_end=="")%>%
    mutate(id_question=as.character(id_question))%>%
    mutate(mps_id=as.integer(mps_id))
}

out_amends <- out_amends()

save(out_amends, file =  paste0("out_amends", ".Rda"))


# all ####
out_amends_all <- out_amends %>% 
  #filter(for.>225) %>% 
  group_by(factions)%>%
  summarise(
    # У відсотках
    vote_for_perc = round(mean(vote_status == "За")*100, 1), 
    vote_abstain_perc = round(mean(vote_status == "Утримався")*100, 1),
    vote_against_perc = round(mean(vote_status == "Проти")*100, 1), 
    vote_present_perc = round(mean(vote_status == "Присутній")*100, 1),
    vote_not_voting_perc = round(mean(vote_status == "Не голосував")*100, 1),
    vote_absent_perc = round(mean(vote_status == "Відсутній")*100, 1)) %>% 
  mutate(factions=as.factor(factions))%>%
  arrange(vote_for_perc, factions) %>% 
  mutate(factions = fct_reorder(factions, levels(factions))) %>% 
  gather(status, n_vote, vote_for_perc:vote_absent_perc, factor_key = TRUE)%>%
  filter(!status=="vote_present_perc") %>% 
  mutate(status=recode(status,
                       "vote_for_perc"="За",
                       "vote_abstain_perc"="Утрималися",
                       "vote_against_perc"="Проти",
                       "vote_not_voting_perc"="Не голосували",
                       "vote_absent_perc"="Відсутні"))

save(out_amends_all, file =  paste0("out_amends_all", ".Rda"))

# 226 ####
out_amends_acts <- out_amends %>%
  filter(for.>225) %>% 
  group_by(factions)%>%
  summarise(
    # У відсотках
    vote_for_perc = round(mean(vote_status == "За")*100, 1), 
    vote_abstain_perc = round(mean(vote_status == "Утримався")*100, 1),
    vote_against_perc = round(mean(vote_status == "Проти")*100, 1), 
    vote_present_perc = round(mean(vote_status == "Присутній")*100, 1),
    vote_not_voting_perc = round(mean(vote_status == "Не голосував")*100, 1),
    vote_absent_perc = round(mean(vote_status == "Відсутній")*100, 1)) %>% 
  mutate(factions=as.factor(factions))%>%
  arrange(vote_for_perc, factions) %>% 
  mutate(factions = fct_reorder(factions, levels(factions))) %>% 
  gather(status, n_vote, vote_for_perc:vote_absent_perc, factor_key = TRUE)%>%
  filter(!status=="vote_present_perc") %>% 
  mutate(status=recode(status,
                       "vote_for_perc"="За",
                       "vote_abstain_perc"="Утрималися",
                       "vote_against_perc"="Проти",
                       "vote_not_voting_perc"="Не голосували",
                       "vote_absent_perc"="Відсутні"))

# Save file
save(out_amends_acts, file =  paste0("out_amends_acts", ".Rda"))


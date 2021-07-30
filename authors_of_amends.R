
# Актуальних членів комітетів -- membership_k_short -- можна завантажити у файлі membership_komitety.R

komit_members <- membership_k_short %>% 
  mutate(full_name_k=str_squish(full_name_k) ) %>% 
  left_join(factions_09, by=c("full_name_k"="fullname"))

# The goal is to separate surnames from the table
# https://docs.google.com/spreadsheets/d/10yx74N7qO3PcaCmOZieZip3rWCkeFmOglggW1Un2FRI/edit#gid=1081447673

# Автори ####

# Завантажуємо файл з вже проставленими авторами поправок
authors_raw <- read.csv("~/R_JOB/Rada_09-master/authors_of_amends_29_07_2021.csv", sep=";", stringsAsFactors = FALSE) %>% 
  
  cSplit("full_name", sep="\n", "long")%>% 
  left_join(komit_members, by=c("full_name"="full_name_k"))%>% 
  # zp_names is located in the file roll_call_voting.R
  left_join(zp_names, by=c("id_event"="id_event")) %>%  
  filter(!is.na(date_end)) # Виключення не чинних нардепів і групових поправок


# list_initiators ####  без комітетських 

list_initiators <- authors_raw %>%
  group_by(full_name, factions, region_name, department_k) %>% 
  summarise(weight_name=n()) %>% 
  ungroup()

save(list_initiators, file =  "list_initiators.Rda")

# Скорочені ПІБи ініціаторів-нардепів
shortened_list_initiators <- list_initiators %>% 
  separate(full_name, c("name", "surname"), sep=" ")%>%
  unite("full_name", name, surname, sep=" ")

save(shortened_list_initiators, file =  "shortened_list_initiators.Rda")

# x y ####

x_authors <- authors_raw %>%
  group_by(full_name, id_event)%>%
  summarise(n_x=n()) %>% 
  rename(names_mps_x=full_name,
         id_event_x=id_event)

y_authors <- authors_raw %>%
  
  group_by(full_name, id_event)%>%
  summarise(n_y=n()) %>% 
  rename(names_mps_y=full_name,
         id_event_y=id_event)

# Type 1 #### names_mps
authors_x_y <- x_authors%>%
  inner_join(y_authors, by=c("id_event_x"="id_event_y"))%>% #Retain only rows in both sets
  filter(names_mps_x != names_mps_y)%>%  # Відкидаємо повторювання
  group_by(names_mps_x, names_mps_y) %>% # Групуємо без законопроектів
  count() %>% 
  mutate(pair = (
    sort(c(names_mps_x, names_mps_y)) %>% paste0(collapse = "|")
  )) %>% 
  group_by(pair) %>% 
  mutate(order = seq_along(pair))%>%
  filter(order == 1) %>% # 
  ungroup()%>% # Є якась проблема у декількох ЗП з подвоєнням ініціаторів, тому треба відсіяти все, що більше 1
  left_join(list_initiators, 
            by=c("names_mps_x"="full_name"))

# Крок 2 ####
authors_x_y_step2 <- authors_x_y%>%
  left_join(list_initiators, by=c("names_mps_y"="full_name"))

# Для побудови мережі
IA_x_y3 <- authors_x_y_step2 %>% 
  select(1:3)

save(IA_x_y3, file =  "IA_x_y3.Rda")

# Short edges #####
short_edges <- IA_x_y3 %>% 
  separate(names_mps_x, c("name_x", "surname_x"), sep=" ")%>%
  unite("full_name_x", name_x, surname_x, sep=" ") %>% 
  
  separate(names_mps_y, c("name_y", "surname_y"), sep=" ")%>%
  unite("full_name_y", name_y, surname_y, sep=" ") %>% 
  arrange(desc(n))

# Gephi data ####

library(igraph)

g <- graph_from_data_frame(short_edges, directed=FALSE, vertices=shortened_list_initiators)
network <- igraph_to_networkD3(g, group = vertex_attr(g, "factions", index = V(g)))

network$nodes$department <- vertex_attr(g, "department_k")
network$nodes$weight_name <- shortened_list_initiators$weight_name

write.csv(network$links, file = paste0("data_network/gephi_edges_amends_", Sys.Date(), ".csv"))
write.csv(network$nodes, file = paste0("data_network/gephi_nodes_amends_", Sys.Date(), ".csv"))


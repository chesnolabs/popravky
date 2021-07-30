#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#install.packages(c("rvest", "xml2"))

#library(rvest)
#library(xml2)

library(shinyWidgets)
library(shinydashboard)
library(shiny)
library(shinythemes)

library(tidyverse)

library(plotly)
library(data.table)
library(DT)
library(splitstackshape)
library(purrr)
library(jsonlite)

library(extrafont)
library(extrafontdb)

library(igraph)
library(networkD3)

# Load functions and helpers
# source("helper.R", encoding = "UTF-8")
source("functions.R") # Functions for network
source("chesno_themes.R") # Functions for network


# Load datasets that we need

# From Rada Scraping
load("amends_by_mps_WO.Rda")
load("amends_by_mps.Rda")

# Voting open data
# helper_voting.R
load("out_amends.Rda")

load("amendments_voting.Rda")
load("act_amends.Rda")

load("out_amends_acts.Rda")
load("out_amends_all.Rda")


# authors_of_amends.R

load("IA_x_y3.Rda")
load("list_initiators.Rda")


# Define UI for application that draws a histogram
# 1. UI ####


ui <- navbarPage( 
  
  "Як нардепи Ради-9 подають поправки і як їх приймають",
                 header = "",
                 
                 position = "fixed-top", 

                 # tabPanel-1 ####
                 tabPanel(title = "Графік",
                          
                          br(),
                          br(),
                          br(),
                          br(),

                          sidebarLayout(
                              sidebarPanel(
                                  selectInput('faction', 'Вибрати фракцію чи групу',
                                              choices = unique(amends_by_mps_WO$factions),
                                              #multiple = TRUE,
                                              selected = "Батьківщина"),
                                  # CSV UI 
                                  downloadButton("downloadData", "Завантажити дані таблиці і графіка"),
                                  br(),
                                  br(),
                                  h4("Оновлено 23 липня 2021 року"),
                                  a(href = "https://www.rada.gov.ua/", 
                                    "Дані персональних сторінок законодавчої активності з сайту Верховної Ради", target = "_blank")),
                              
                              mainPanel(
                                  tabsetPanel(type = "tabs",
                                              tabPanel("Топ-20 подавачів поправок, пофракційно",

                                                       br(),
                                                       plotly::plotlyOutput("plot", height = "800px")

                                              ),
                                              tabPanel("Топ-20 нардепів, чиї правки найбільше приймали",

                                                       br(),
                                                       plotly::plotlyOutput("positive_amends_plot", height = "800px")
                                              ))))),
                 # tabPanel-2 ####
                 tabPanel(title = "Табличні дані",
                          
                          br(),
                          br(),
                          br(),
                          br(),

                          sidebarLayout(
                              sidebarPanel(
                                  selectInput('faction_tab2',
                                              'Вибрати фракцію чи групу',
                                              choices = unique(amends_by_mps_WO$factions),
                                              selected = "Батьківщина"),
                                  # CSV UI
                                  downloadButton("downloadData_table", "Завантажити дані таблиці і графіка"),
                                  br(),
                                  br(),
                                  h4("Оновлено 23 липня 2021 року"),
                                  a(href = "https://www.rada.gov.ua/",
                                    "Дані персональних сторінок законодавчої активності з сайту Верховної Ради",
                                    target = "_blank")),
                              mainPanel(
                                  tabsetPanel(type = "tabs",
                                              tabPanel("Хто як подає поправки",
                                                       br(),
                                                       DT::DTOutput("table")),
                                              tabPanel("Розширена таблиця з № законопроектів, до яких подали поправки",
                                                       br(),
                                                       DT::DTOutput("table_zakon")
                                              ))))),
                 # tabPanel-3 ####
                 tabPanel(title = "Голосування",
                          
                          br(),
                          br(),
                          br(),
                          br(),

                          sidebarLayout(
                              sidebarPanel(
                                  # CSV UI 
                                  downloadButton("downloadData_table_zakon226", "Дані: 226 голосів"),
                                  downloadButton("downloadData_plot_amends", "Дані: усі голосування"), #
                                  br(),
                                  br(),
                                  h4("Оновлено 23 липня 2021 року"),
                                  a(href = "https://www.rada.gov.ua/",
                                    "Дані персональних сторінок законодавчої активності з сайту Верховної Ради",
                                    target = "_blank")),
                              mainPanel(
                                  tabsetPanel(type = "tabs",
                                              # 3.1 < 226 голосів ####
                                              tabPanel("Поправки 226 голосів",
                                                       h4(strong(paste("Усього відбулося", length(act_amends$id_event), 
                                                                       "голосувань за поправки, які набрали 226 голосів"))),
                                                       br(),
                                                       plotOutput('plot_amends', width = "950px", height = "450px"),
                                                       # br(),   br(),
                                                       DT::DTOutput("table_voting_226")),
                                              
                                              # 3.2 All votings ####
                                              tabPanel("Усі голосування",
                                                       h4(strong(paste("Усього відбулося", length(amendments_voting$id_event),
                                                                       "голосувань за поправки"))),
                                                       br(),
                                                       plotOutput("plot_amends_all", width = "950px", height = "450px"),
                                                       DT::DTOutput("table_voting")
                                              ),
                                              # 3.3 Top bills by votes ####
                                              tabPanel("Законопроекти-лідери",
                                                       h4(strong(paste("Поправки, які набрали 226 голосів, відповідно до № законопроекту"))),
                                                       br(),
                                                       plotly::plotlyOutput("top_per_bill_226", width = "950px", height = "550px"),
                                                       br(),
                                                       br(),
                                                       h4(strong(paste("Голосування за всі поправки відповідно до № законопроекту"))),
                                                       plotly::plotlyOutput("top_per_bill", width = "950px", height = "1000px")
                                              ),
                                              # 3.4 ####
                                              tabPanel("Таблиця: топ-законопроекти",
                                                       h4(strong(paste("Поправки, які набрали 226 голосів"))),
                                                       br(),
                                                       DT::DTOutput("number_amends")
                                              )
                                              
                                              )))),
                 # tabPanel-4 Network ####
                
                tabPanel(title = "Мережа",
                         
                         br(),
                         br(),
                         br(),
                         br(),

                         sidebarLayout(
                             sidebarPanel(
                                 sliderInput("amends_connection",
                                             "Кількість поданих спільно поправок",
                                             min = 1, 
                                             max = 16,
                                             value = 5),
                                 
                                 checkboxGroupInput("factions",
                                                    label = "Обрати фракцію",
                                                    # 
                                                    choices = unique(list_initiators$factions), 
                                                    selected = unique(list_initiators$factions)), #,
                                 # CSV UI 
                                 br(),
                                 br(),
                                 h4("Оновлено 29 липня року 2021 року")),
                             mainPanel(
                                 forceNetworkOutput("network_amends")
                             ))),
                
                 
                 # tabPanel-5 ####
                 tabPanel(title = "Про інструмент",
                          
                          br(),
                          br(),
                          br(),
                          br(),

                          sidebarLayout(
                              sidebarPanel(
                                  h4("Оновлено 23 липня 2021 року"),
                                  a(href = "https://www.rada.gov.ua/",
                                    "Дані персональних сторінок законодавчої активності з сайту Верховної Ради",
                                    target = "_blank")),
                              mainPanel(h4(strong("Інструмент моніторингу поправок народних депутатів Ради 9 скликання")),
                                        h5("Ви знаходитеся на сторінці бета-версії інструменту Руху ЧЕСНО по відстеженню поправок нардепів"),
                                        h5('З початку 2020 року Рух ЧЕСНО займається моніторингом поправок нардепів та роботою в комітетах'),
                                        h5("Для зручності ми створили цей інструмент, де кожен аналітик і журналіст"), 
                                        h5("може побачити у зручному форматі, як приймаються поправки нардепів, фракцій та груп"),
                                        h5("Якщо у Вас є побажання з приводу поліпшення інструменту, напишіть на o.stavniichuk@gmail.com"))
                          )),
    
    tags$head(tags$style('body {font-family: OpenSans;}')),
    tags$head(includeHTML(("google-analytics.html")))
    
)


# 2 Server ####
server <- function(input, output, session) {

    reactive_plot <- reactive({ 
        
        amends_by_mps_WO %>% 
            dplyr::group_by(factions) %>% 
            dplyr::top_n(20, totally) %>%
            ungroup()%>% 
            #gather(type, number, accepted:no_conclusion, factor_key = TRUE) %>% 
            dplyr::mutate(factions=as.factor(factions)) %>% 
            tidyr::pivot_longer(-factions:-totally, names_to = "type", values_to = "number") %>%
            ungroup()%>% 
            dplyr::mutate(type=as.factor(type)) %>% 
            arrange(desc(totally, factions))%>%
            dplyr::filter(factions==input$faction) %>% 
            dplyr::mutate(type=recode(type,
                                "accepted"="Враховані",
                                "partly_accepted"="Частково враховані",
                                "rejected"="Відхилені",
                                "redakciyno_accepted"="Редакційно враховані",
                                "others"="Інше",
                                "no_conclusion"="Немає висновку"))
        
        
    })
    # Tab 1.1 Plot ####
    
    output$plot <-  plotly::renderPlotly ({
        
        # Plot ####
        reactive_plot() %>% 
            ggplot(aes(x=reorder(short_name, number),y=number, fill=type))+
            geom_col(position = position_stack(reverse = TRUE))+
            coord_flip()+
            scale_fill_manual(labels = c("Враховані", 
                                         "Частково враховані", 
                                         "Відхилені", 
                                         "Редакційно враховані", 
                                         "Інше", 
                                         "Немає висновку"),
                              values = c("Враховані" = "#452571", # #583479 #452571
                                         "Частково враховані" = "#e5e500", # #f39200  #e5e500 #E5E5E5
                                         "Відхилені" = "#FC3F1B", #firebrick 
                                         "Редакційно враховані" = "#E5E5E5", #
                                         "Інше" = "#999999",
                                         "Немає висновку"="#333333"))+ # #333333
            chesno_theme()


    })
    
    #  # Tab 1.2 Reactive ####
    # Чиї поправки найбільше приймалися (не у відсотках)
    rea_positive <- reactive({
        
        amends_by_mps_WO %>%
            select(-rejected,-others,-no_conclusion) %>% 
            group_by(factions) %>% 
            top_n(20, accepted) %>%
            ungroup()%>% 
            #gather(type, number, accepted:no_conclusion, factor_key = TRUE) %>% 
            mutate(factions=as.factor(factions)) %>% 
            filter(factions==input$faction) %>% 
            arrange(desc(accepted, factions))%>%
            pivot_longer(-factions:-totally, names_to = "type", values_to = "number") %>%
            ungroup()%>% 
            mutate(type=as.factor(type)) %>% 
            mutate(type=recode(type,
                               "accepted"="Враховані",
                               "partly_accepted"="Частково враховані",
                               "redakciyno_accepted"="Редакційно враховані"))
    })
    
    #  # Tab 1.2 Plot ####
    output$positive_amends_plot <-  plotly::renderPlotly ({
        
        rea_positive() %>% 
            ggplot(aes(x=reorder(short_name, number),y=number, fill=type))+
            geom_col(position = position_stack(reverse = TRUE))+
            coord_flip()+
            scale_fill_manual(
                values = c("Враховані" = "#452571", # #583479 #452571
                           "Частково враховані" = "#e5e500", # #f39200  #e5e500 #E5E5E5
                           "Редакційно враховані" = "#E5E5E5"))+ 
            chesno_theme()

    })
    
    
    #  Tab 2.1 Reactive ####
    reactive_table <- reactive({ 
        
      amends_by_mps_WO %>%
        filter(factions==input$faction_tab2) %>%
        rename(
          

          "Ім'я"="short_name",
          "По-батькові"="fathername",
          "Фракція"="factions", 
          "Регіон"="region_name",
          "Усього"="totally",
          "Прийняті"="accepted",
          "Частково прийняті"="partly_accepted",
          "Відхилені"="rejected",
          "Редакційно прийняті"="redakciyno_accepted",
          "Інше"="others",
          "Без висновку"="no_conclusion"
          )
        
    })
    
    # Tab 2.1 Table ####
    output$table <- DT::renderDT({
        
        reactive_table() %>% 
            # Change names for buttons
            DT::datatable(filter = 'top',
                          extensions = "Buttons", 
                          options = list( language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Ukrainian.json')))
        
    })
    
    # Tab 2.2 Reactive  ####
    reactive_table_zakon <- reactive({
        
        amends_by_mps %>% 
            filter(factions==input$faction_tab2) %>%
        rename(
          
          
          "Ім'я"="short_name",
          "По-батькові"="fathername",
          "Фракція"="factions", 
          "Регіон"="region_name",
          "Усього"="totally",
          "Прийняті"="accepted",
          "Частково прийняті"="partly_accepted",
          "Відхилені"="rejected",
          "Редакційно прийняті"="redakciyno_accepted",
          "Інше"="others",
          "Без висновку"="no_conclusion",
          "№ ЗП"="number"
        )

    })
    
    # Tab 2.2 Table ####
    output$table_zakon <- DT::renderDT({
        
        reactive_table_zakon() %>% 
            DT::datatable(filter = 'top',
                          style = 'bootstrap',
                          options = list( 
                              language = list(fixedHeader = TRUE, url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Ukrainian.json')))
    })
    
    
    # Tab 3-1 reactive ####
    # Reactive graph for amends personal voting 
    reactive_plot_amends_voting <- reactive({ 
        
        out_amends_acts 
        
    })
    
    # Tab 3-1 Plot ####
    
    output$plot_amends <-  renderPlot ({
        
        reactive_plot_amends_voting() %>% 
            ggplot(aes(x=factions, y=n_vote, fill=status))+
            geom_col(position = position_stack(reverse = TRUE))+
            coord_flip()+
            ggtitle("Голосування, які набрали 226 голосів, % ")+
            scale_fill_manual(labels = c("За", "Утрималися", "Проти", "Не голосували", "Відсутні"),
                              values = c("За" = "#452571", 
                                         "Утрималися" = "#e5e500",
                                         "Проти" = "#FC3F1B", 
                                         "Не голосували" = "#e8e8e8",
                                         "Відсутні" = "#333333"))+
            chesno_theme_2()
           
    })
    
    
    # Tab 3-2 reactive ####
    reactive_plot_amends_all <- reactive({    
        
      out_amends_all
        
    })
    
    # Tab 3-2 plot all amends ####
    
    output$plot_amends_all <-  renderPlot ({
        
        reactive_plot_amends_all() %>% 
            ggplot(aes(x=factions, y=n_vote, fill=status))+
            geom_col(position = position_stack(reverse = TRUE))+
            coord_flip()+
            ggtitle("Усі голосування за поправки у %")+
            scale_fill_manual(labels = c("За", "Утрималися", "Проти", "Не голосували", "Відсутні"),
                              values = c("За" = "#452571", 
                                         "Утрималися" = "#e5e500",
                                         "Проти" = "#FC3F1B", 
                                         "Не голосували" = "#e8e8e8",
                                         "Відсутні" = "#333333"))+
            chesno_theme_2()

        
        
    })
    
    # Tab 3-3 reactive ####
    
    table_amends226_mps <- reactive({
      
        out_amends %>%
            filter(for.>225) %>% 
            group_by(fullname, factions)%>%
            summarise(vote_for = sum(vote_status == "За"), 
                      vote_abstain = sum(vote_status == "Утримався"),
                      vote_against_ = sum(vote_status == "Проти"), 
                      vote_present = sum(vote_status == "Присутній"),
                      vote_not_voting = sum(vote_status == "Не голосував"),
                      vote_absent = sum(vote_status == "Відсутній")) %>% 
            arrange(vote_for, fullname) %>% 
            #dplyr::ungroup() %>% 
            select(-vote_present) %>% 
            filter(!is.na(fullname)) %>% 
            separate(fullname, c("surname","name","fathername"), sep = " ")%>%
            unite(short_name, c("name","surname"), sep = " ")  %>%  
            #filter(!vote_status=="vote_present") %>%  
            #filter(factions==input$faction) %>% 
            rename(
                "За"="vote_for",
                "Утрималися"="vote_abstain",
                "Проти"="vote_against_",
                #"Не голосували"="vote_present",
                "Не голосували"="vote_not_voting",
                "Відсутні"="vote_absent",
                "Ім'я"="short_name",
                "По-батькові"="fathername",
                "Фракція"="factions")
        
    })
    
    
    # Tab 2 TABLE 226 ####
    output$table_voting_226 <- DT::renderDT({
        
        table_amends226_mps() %>% 
            # Change names for buttons
            # https://rstudio.github.io/DT/004-i18n.html
            DT::datatable(filter = 'top',
                          style = 'bootstrap',
                          #extensions = "Buttons", 
                          options = list( 
                              language = list(fixedHeader = TRUE, url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Ukrainian.json')))
        
        
    })
    
    # Tab 4   Reactive data prep all amends TABLE ####
    
    table_amends_mps <- reactive({
        
        out_amends %>% 
            group_by(fullname, factions)%>%
            summarise(vote_for = sum(vote_status == "За"), 
                      vote_abstain = sum(vote_status == "Утримався"),
                      vote_against_ = sum(vote_status == "Проти"), 
                      vote_present = sum(vote_status == "Присутній"),
                      vote_not_voting = sum(vote_status == "Не голосував"),
                      vote_absent = sum(vote_status == "Відсутній")) %>% 
            arrange(vote_for, fullname) %>% 
            #dplyr::ungroup() %>% 
            select(-vote_present) %>% 
            filter(!is.na(fullname)) %>% 
            separate(fullname, c("surname","name","fathername"), sep = " ")%>%
            unite(short_name, c("name","surname"), sep = " ")  %>%  
            #filter(!vote_status=="vote_present") %>%  
            #filter(factions==input$faction) %>% 
            rename(
                "За"="vote_for",
                "Утрималися"="vote_abstain",
                "Проти"="vote_against_",
                #"Не голосували"="vote_present",
                "Не голосували"="vote_not_voting",
                "Відсутні"="vote_absent",
                "Ім'я"="short_name",
                "По-батькові"="fathername",
                "Фракція"="factions")
        
        
    })
    
    
    # Reactive table for plot plot_amends_all ####
    reactive_plot_amends_226 <- reactive({    
        
        out_amends %>%
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
        
        
    })
    
    
    # Render plot all amends #### vote_status
    
    output$plot_amends_all <-  renderPlot ({
        
        reactive_plot_amends_226()%>% 
            ggplot(aes(x=factions, y=n_vote, fill=status))+
            geom_col(position = position_stack(reverse = TRUE))+
            coord_flip()+
            ggtitle("Усі голосування за поправки у %")+
            scale_fill_manual(labels = c("За", "Утрималися", "Проти", "Не голосували", "Відсутні"),
                              values = c("За" = "#452571", 
                                         "Утрималися" = "#e5e500",
                                         "Проти" = "#FC3F1B", 
                                         "Не голосували" = "#e8e8e8",
                                         "Відсутні" = "#333333"))+
            chesno_theme_2()

        
    })
    
    
    
    # Tab 4.2.b Render table amends ####
    
    output$table_voting <- DT::renderDT({
        
        table_amends_mps() %>% 
            DT::datatable(filter = 'top',
                          style = 'bootstrap',
                          options = list( 
                              language = list(fixedHeader = TRUE, 
                                              url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Ukrainian.json')))
        
    })
    
    # tab 5 ####
    # Лідери за к-стю поправок
    # Reactive prep for топ-голосування за закони
    top_bill_reac <- reactive({
        #n_amends
        # Фільтруємо більше 25 поправок ####
        amendments_voting %>% 
            group_by(number_question, name_question) %>% 
            summarise(n=n()) %>% 
            arrange(desc(n))%>% 
            filter(n>25)  # Прибрати фільтр, аби побачити повну картинку 
        
        
    })
    
    
    # Plot based on reactive prepareation top_bill_reac
    
    output$top_per_bill <- plotly::renderPlotly({
        
        top_bill_reac() %>% 
            ggplot(aes(x=reorder(number_question,n), n))+
            geom_col(fill="#452571", width = 0.7)+
            coord_flip()+
            chesno_theme_3()
  

    })
    
    
    
    # tab 6 ####
    # Лідери за к-стю поправок
    # Reactive prep for топ-голосування за закони
    top_bill_226_reac <- reactive({
        
        amendments_voting %>%
            filter(for.>225)%>% 
            group_by(number_question, name_question) %>% 
            summarise(n=n())%>% 
            filter(n>=10) %>% ungroup() # Змінювати відповідно до ситуації
        
        
    })
    
    
    # Plot based on reactive prepareation top_bill_reac
    
    output$top_per_bill_226 <- plotly::renderPlotly({
        
        top_bill_226_reac() %>% 
            ggplot(aes(x=reorder(number_question, n), n))+
            geom_col(fill="#452571", width = 0.7)+
            coord_flip()+
            #ggtitle("Кількість голосувань за поправки за всю каденцію Ради-9
            # Відповідно до законопроекту")+
            chesno_theme_3()

        
    })
    
    # ####
    top_bills_events <- reactive({
        
        amendments_voting %>%
            filter(for.>225)%>% 
            select(number_question, name_question, name_event, id_event, for.) %>% 
            rename("№ ЗП"="number_question",
                   "Назва ЗП"="name_question",
                   "Голосування"="name_event",
                   "№ Голосування"="id_event",
                   "За"="for.")
        
        
    })
    
    
    # Tab  Render table amends ####
    
    output$number_amends <- DT::renderDT({
        
        top_bills_events() %>% 
            DT::datatable(filter = 'top',
                          style = 'bootstrap',
                          options = list( 
                              language = list(fixedHeader = TRUE, 
                                              url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Ukrainian.json')))
        
    })
    # # Network Prep ####
    
    # output-1 ####
    output$network_amends <- renderForceNetwork({
        
        mps_force_network(
            filter(IA_x_y3, n >= input$amends_connection),
            # Зв'язки
            
            filter(
                list_initiators,
                list_initiators$factions %in% input$factions 
                
            )
        ) # Nodes
    })
    

    # Down button 1 ####
    
    output$downloadData <- downloadHandler(
        
        filename = function() {
            paste("amends_by_mps_WO-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(amends_by_mps_WO, file)
        }
    )
    
    # Down button 2 ####
    
    output$downloadData_table <- downloadHandler(
        
        filename = function() {
            paste("amends_by_mps-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(amends_by_mps, file)
        }
    )
    
    # Down button 3 ####
    
    output$downloadData_table_zakon <- downloadHandler(
        
        filename = function() {
            paste("amends_by_mps-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(amends_by_mps, file)
        }
    )
    
    # Down button 4 ####
    
    output$downloadData_plot_amends <- downloadHandler(
        
        filename = function() {
            paste("amends_all_", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(table_amends_mps(), file)   ###
        }
    )
    
    # Down button 5 ####
    
    output$downloadData_table_zakon226 <- downloadHandler(
        
        filename = function() {
            paste("amends_226_", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(table_amends226_mps(), file) ####
        }
    )
    
    

}

# Run the application ####
shinyApp(ui = ui, server = server)

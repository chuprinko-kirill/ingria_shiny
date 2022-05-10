library(shiny)
library(shinyWidgets)
library(colourpicker)
library(tidyverse)
library(leaflet)
library(lingtypology)

#Sys.setlocale("LC_ALL","Russian_Russia.1251")

ink <- read.csv("ink_processed.csv", sep = ";", encoding = "UTF-8")
ink$Широта <- as.numeric(str_replace_all(ink$Широта, ",", "\\."))
ink$Долгота <- as.numeric(str_replace_all(ink$Долгота, ",", "\\."))
ink <- ink[-c(1),]


ui <- fluidPage(
  titlePanel("Tentative inkeri interactive atlas"),
  navbarPage("",
             tabPanel("main window",
                      column(1, wellPanel(
                        submitButton("redraw"),
                        shinyWidgets::pickerInput(
                          inputId = "GLOSS",
                          label = "select a gloss", 
                          choices = c(names(ink)), 
                          selected = "картофель", #картофель в качестве выбора при запуске
                          options = list(
                            `actions-box` = TRUE), 
                          multiple = F
                        )
                      )),
                      column(6, leafletOutput("MYMAP", height = 600, width = 800))
             )))

server <- function(input, output) {
  output$MYMAP <- renderLeaflet({
    ink %>% 
      select(ФИ, Год_рождения, Приход, Широта, Долгота, Деревня_рождения, input$GLOSS) -> ink   
    
    ### Продублируем информантов, у которых несколько вариантов ответа, оставив в каждой копии по одному ответу
    
    s <- strsplit(ink[,7], split = "/")
    ink <- data.frame(ФИ = rep(ink$ФИ, sapply(s, length)),Приход = rep(ink$Приход, sapply(s, length)), Широта = rep(ink$Широта, sapply(s, length)), Долгота = rep(ink$Долгота, sapply(s, length)), Деревня_рождения = rep(ink$Деревня_рождения, sapply(s, length)), word = unlist(s))
    
    # Если сделали несколько строк на предыдущем этапе, чуть-чуть подвинем вторую новую точку, чтобы точки не накладывались друг на друга на карте
    #https://stackoverflow.com/questions/47478826/r-changing-duplicate-values-within-subjects
    ink <- ink %>%
      group_by(Деревня_рождения, Широта) %>%
      mutate(Count = row_number()) %>%
      ungroup() %>%
      mutate(Широта = ifelse(Count > 1, Широта + 0.0135, Широта)) %>% #альтернативный вариант для неналожения точек: сделать их более прозрачными, если пакет позволяет
      select(-Count)
    
    
    map.feature(language="Finnish",
                features=ink$word,
                popup = paste("Name: ", ink$ФИ, "Birthplace: ",ink$Деревня_рождения,"Gloss: ", ink$word, sep=" ", collapse=NULL),
                latitude=ink$Широта,
                longitude=ink$Долгота,
                width=8)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)







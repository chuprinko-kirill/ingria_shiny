
library(shiny)
library(shinyWidgets)
library(colourpicker)
library(tidyverse)
library(leaflet)
library(lingtypology)
library(readxl)
library(janitor)

ink <- read_excel("C:/Users/Kirill/Desktop/Ингерманландия/shiny/inkeri.xlsx")
ink <- as.data.frame(t(ink))
# нужно вставить строчку, чтобы убирать совершенно пустые строки, которые могут быть в .xlsx, иначе будут дупликаты при создании имен из рядов
ink <- row_to_names(ink, row_number = 1)
ink <- ink[-c(1),]

ink$Широта <- as.character(ink$Широта)
ink$Широта <- as.numeric(ink$Широта)
ink$Долгота <- as.character(ink$Долгота)
ink$Долгота <- as.numeric(ink$Долгота)
ink <-ink %>% filter(Широта != "NA")


# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Tentative inkeri interactive atlas"),
    navbarPage("",
    tabPanel("main window",
                        column(1, wellPanel(
                            submitButton("redraw"),
                            pickerInput(
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
            select(Год_рождения, Приход, Широта, Долгота, Деревня_рождения, input$GLOSS) -> ink   
    
        ### Продублируем информантов, у которых несколько вариантов ответа, оставив в каждой копии по одному ответу
        
        s <- strsplit(ink[,6], split = "/")
        ink <- data.frame(Приход = rep(ink$Приход, sapply(s, length)), Широта = rep(ink$Широта, sapply(s, length)), Долгота = rep(ink$Долгота, sapply(s, length)), Деревня_рождения = rep(ink$Деревня_рождения, sapply(s, length)), word = unlist(s))
        
        # Если сделали несколько строк на предыдущем этапе, чуть-чуть подвинем вторую новую точку, чтобы точки не накладывались друг на друга на карте
        #https://stackoverflow.com/questions/47478826/r-changing-duplicate-values-within-subjects
        ink <- ink %>%
            group_by(Деревня_рождения, Широта) %>%
            mutate(Count = row_number()) %>%
            ungroup() %>%
            mutate(Широта = ifelse(Count > 1, Широта + 0.0135, Широта)) %>%
            select(-Count)
        
            
    map.feature(language="Finnish",
                features=ink$word, #6 потому что input после select стал 6-й колонкой
                popup = ink$Деревня_рождения,
                latitude=ink$Широта,
                longitude=ink$Долгота,
                width=8)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

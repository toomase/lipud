library(shiny)
library(tidyverse)
library(DT)
library(shinyjs)

riigid_lippudega <- read_csv("~/Dropbox/DataScience/R/lipud/lipud/responses/riigid_lippudega.csv")

save_data <- function(data) {
  write_csv(x = data, 
            path = file.path("~/Dropbox/DataScience/R/lipud/lipud/responses", 
                             str_c(as.integer(Sys.time()), ".csv")))
}

load_data <- function() {
  files <- list.files("~/Dropbox/DataScience/R/lipud/lipud/responses", full.names = TRUE)
  data <- map_df(files, read_csv)
  data
}


ui <- fluidPage(
  useShinyjs(),
  title = "Lippude andmebaas",
  sidebarLayout(
    sidebarPanel(
      
      wellPanel(
        h3("Laenutus"),
      textInput(inputId = "laenutaja", 
                label = "Kes laenutab"
      ),
      
      selectInput(inputId = "riik", 
                  label = "Vali riik:", 
                  multiple = TRUE,
                  choices = riigid_lippudega$riik
      ),

      actionButton(inputId = "lisa", 
                   label = "Lisa")
    ),
    
    wellPanel(
      h3("Tagastud"),
      
      uiOutput("tagastus_riik"),
      
      actionButton(inputId = "tagasta", 
                   label = "Tagasta")
    )
  ),
    
    mainPanel(
      DT::dataTableOutput("table"),
      
      textOutput("ridu")
    )
  )
)



server <- function(input, output) {
  
  laenutatud <- reactive({
    validate(
      need(input$laenutaja != "", "Please select a data set")
    )
    
    data <- riigid_lippudega %>%
    filter(riik %in% input$riik) %>% 
    mutate(laenutamise_aeg = as.character(format(Sys.Date(), "%d.%m.%Y")),
           laenutaja = ifelse(input$riik == riik, input$laenutaja, NA),
           id = str_c(id, as.integer(Sys.time())))
    
    data
    })
  
  observeEvent(input$lisa, {
    if (input$laenutaja == "") {
      alert("Sisesta laenutaja nimi")
    }
  })
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$lisa, {
    save_data(laenutatud())
  })
  



  observeEvent(input$lisa, {
    reset("riik")
})
  
  observeEvent(input$lisa, {
    reset("laenutaja")
  })
  
  observeEvent(input$tagasta, {
    reset("tagastus_riik")
  })
  
  df <- eventReactive(input$lisa | input$tagasta, {
    load_data()
  })
  
    tagastatud <- reactive({
    data <- df() %>%
      filter(str_c(riik, " - ", laenutaja) %in% input$tagastus_riik) %>% 
      mutate(tagastamise_aeg = as.character(format(Sys.Date(), "%d.%m.%Y")))
    
    data
  })
  
    
  observeEvent(input$tagasta, {
    save_data(tagastatud())
  })
  
  output$ridu <- renderText({nrow(df())})
  
  df_2 <- eventReactive(input$lisa | input$tagasta, {
    load_data()
  })
  
  output$tagastus_riik <- renderUI({
    selectInput(inputId = "tagastus_riik", 
                label = "Vali riik:", 
                multiple = TRUE,
                choices = df_2() %>% 
                  arrange(riik, tagastamise_aeg, laenutamise_aeg) %>%
                  distinct(riik, .keep_all = TRUE) %>% 
                  filter(!is.na(laenutamise_aeg), is.na(tagastamise_aeg)) %>% 
                  mutate(riik_2 = str_c(riik, " - ", laenutaja)) %>% 
                  pull(riik_2)
    )
  })
  
  output$table <- renderDataTable({
  datatable({
    df_2() %>% 
      arrange(riik, tagastamise_aeg, laenutamise_aeg) %>%
      distinct(riik, .keep_all = TRUE) %>%
      # select(-id, -tagastamise_aeg) %>%
      arrange(laenutamise_aeg, riik)
      }, escape = FALSE)})
}

# Run the application 
shinyApp(ui = ui, server = server)

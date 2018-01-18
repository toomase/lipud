library(shiny)
library(tidyverse)
library(DT)
library(shinyjs)

# Lae algandmed kõigi lippude kohta
riigid_lippudega <- read_csv("~/Dropbox/DataScience/R/lipud/lipud/responses/riigid_lippudega.csv")

# Funktsioon salvestab tabeli csv faili ja nimetab selle kuupäeva-kellaaja järgi unikaalselt
# Kasutan seda laenutamiste ja tagastamiste tabeli salvestamiseks
# Iga andmete lisamine salvestub eraldi csv failina
save_data <- function(data) {
  write_csv(x = data, 
            path = file.path("~/Dropbox/DataScience/R/lipud/lipud/responses", 
                             str_c(as.integer(Sys.time()), ".csv")))
}

# Funktsioon laeb kõik failid "responses" kaustast ja kirjutab need kokku ühte tabelisse
# Kasutan seda, et kõik lippude laenutamiste ja tagastamiste csv failid äppi laadida
load_data <- function() {
  files <- list.files("~/Dropbox/DataScience/R/lipud/lipud/responses", full.names = TRUE)
  data <- map_df(files, read_csv)
  data
}


ui <- fluidPage(
  useShinyjs(),  # vajalik lisafunktsioonide (alert ja reset) jaoks 
  
  title = "Lippude andmebaas",
  
  sidebarLayout(
    sidebarPanel(
      
      # Laenutamise valikute blokk
      wellPanel(
        h3("Laenutus"),
        
        # Sisesta laenutaja nimi - vabateksti väli
        textInput(inputId = "laenutaja",
                  label = "Kes laenutab"
        ),
        
        # Vali riigilipud, mida soovid laenutada
        selectInput(inputId = "riik", 
                    label = "Vali riik:", 
                    multiple = TRUE,
                    choices = riigid_lippudega$riik  # valikus on kõik algbaasis olevad lipud
        ),
        
        # Nupp, mis lisab laenutuse andmed baasi
        actionButton(inputId = "lisa", 
                     label = "Lisa")
      ),
      
      
      # Tagastamise valikute blokk
      wellPanel(
        h3("Tagastus"),
        
        # Vali riigilipu ja laenutaja kombinatsioonid, mida soovid tagastada
        # Sisendiks on kõik hetkel väljalaenutatud lipud
        uiOutput("tagastus_riik"),
      
        # Nupp, mis lisab tagastuse andmed baasi
        actionButton(inputId = "tagasta", 
                     label = "Tagasta")
      )
    ),
    
    mainPanel(
      # Kuva välja tabel kõigi lippude kohta koos laenutamise staatusega
      dataTableOutput("table"),
      
      # Kuva kogu ridade arv "responses" kaustas
      textOutput("ridu")
    )
  )
)



server <- function(input, output) {
  
  laenutatud <- reactive({
    validate(
      need(input$laenutaja != "", ""),
      need(input$riik, "")
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
      arrange(riik, tagastamise_aeg, laenutamise_aeg) %>%
      distinct(riik, .keep_all = TRUE) %>%
      # select(-id, -tagastamise_aeg) %>%
      arrange(laenutamise_aeg, riik) %>% 
      filter((str_c(riik, " - ", laenutaja) %in% input$tagastus_riik | 
               row_number() %in% input$table_rows_selected),
             !is.na(laenutamise_aeg)) %>% 
      mutate(tagastamise_aeg = as.character(format(Sys.Date(), "%d.%m.%Y")))
    
    data
  })
  
    observeEvent(input$tagasta, {
      if (nrow(tagastatud()) == 0) {
        alert("Vali tagastatav lipp")
      }
    })
    
  observeEvent(input$tagasta, {
    if (nrow(tagastatud()) > 0) {
      save_data(tagastatud()) 
    }
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
      }, escape = FALSE, filter = "top")})
}

# Käivita äpp
shinyApp(ui = ui, server = server)

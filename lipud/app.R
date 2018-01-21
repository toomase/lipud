library(shiny)
library(tidyverse)
library(DT)
library(shinyjs)
library(lubridate)

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
        
        # Vali laenutamise algus (vaikimisi täna) ja lõpp (vaikimisi täna + 2 nädalat) kp
        # Vaikimisi väärtus tänane kp
        dateRangeInput(inputId = "laenutamise_kp",
                       label = "Laenutamise ajavahemik:",
                       start = Sys.Date(),
                       end = Sys.Date() + 14,
                       format = "dd.mm.yyyy",
                       separator = " kuni ",
                       language = "et",
                       weekstart = 1
        ),
        
        # Vali riigilipud, mida soovid laenutada
        uiOutput("riik"),
        
        # Sisesta laenutaja nimi - vabateksti väli
        textInput(inputId = "laenutaja",
                  label = "Kes laenutab"
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
        
        # Vali kuupäev, millal lipp tegelikult tagastati
        # Vaikimisi väärtus tänane kp
        dateInput(inputId = "tagastamise_kp", 
                  label = "Tagastamise kp:", 
                  value = Sys.Date(),
                  format = "dd.mm.yyyy",
                  language = "et",
                  weekstart = 1
        ),
      
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
  
  # Leia laenutatud lippude tabel
  # Võta aluseks lippude algtabel
  laenutatud <- reactive({
    # Leia tabel ainult siis kui riik ja laenutaja on valitud
    validate(
      need(input$laenutaja != "", ""),
      need(input$riik, "")
      )
    
    # Koosta tabel kõigist riikidest, kes on valitud
    data <- riigid_lippudega %>%
      filter(riik %in% input$riik) %>% 
      mutate(algus_kp = as.character(format(input$laenutamise_kp[1], "%d.%m.%Y")),
             lopp_kp = as.character(format(input$laenutamise_kp[2], "%d.%m.%Y")),
             laenutaja = ifelse(input$riik %in% riik, input$laenutaja, NA),
             id = str_c(id, as.integer(Sys.time()), sep = "-"))
      
      data
  })
  
  # Anna veateade, kui laenutaja nimi on sisestamata ja klikitakse "Lisa"
  observeEvent(input$lisa, {
    if (input$laenutaja == "") {
      alert("Sisesta laenutaja nimi")
    }
  })
  
  # Kui klikitakse "Lisa", siis salvesta csv
  observeEvent(input$lisa, {
    save_data(laenutatud())
  })
  
  
  # Pärast "Lisa" klikkimist tühjenda input andmeväljad
  observeEvent(input$lisa, {
    reset("riik")
    reset("laenutaja")
    reset("laenutamise_kp")
  })
  
  # Pärast "Tagasta" klikkimist tühjenda input andmeväljad
  observeEvent(input$tagasta, {
    reset("tagastus_riik")
  })
  
  # Pärast "Lisa" või "Tagasta" klikkimist lae "responses" kaustast kõik andmed
  # Kirjuta need reactive tabelisse
  df <- eventReactive(input$lisa | input$tagasta, {
    load_data()
  })
  
  # Koosta tagastatud lippude tabel
  # Võta aluseks kõik andmed "responses" kaustast
  tagastatud <- reactive({
    
    # Kui rida ei ole tabelist valitud või lippu drop down menüüst,
    # siis ära tekita tagastamise kohta tabelit
    if (is.null(input$tagastus_riik) & is.null(input$table_rows_selected)) {
      validate(
        need(input$tagastus_riik, "")
      )
    }
    
    # Sorteeri andmed samale kujule nagu äpis on välja kuvatud
    # See on vajalik, et tabelis klikkimise rea numbrite põhjal tagstamisi teha
    data <- df() %>%
      group_by(id) %>%
      filter(n_distinct(tagastamise_kp, na.rm = TRUE) == 0) %>%
      ungroup() %>%
      group_by(riik) %>% 
      mutate(arv = n_distinct(algus_kp)) %>% 
      ungroup() %>% 
      filter((!is.na(algus_kp) & arv > 1) | 
               (is.na(algus_kp) & arv == 1)) %>%
      arrange(lopp_kp, riik) %>% 
      filter((str_c(riik, " - ", laenutaja) %in% input$tagastus_riik |  # riigilipu ja laenutaja kombinatsioon
               row_number() %in% input$table_rows_selected),   # valik tabelist
             !is.na(algus_kp)) %>%  # ainult väjalaenatud lipud
      mutate(tagastamise_kp = as.character(format(input$tagastamise_kp, "%d.%m.%Y")))
    
    data
  })
  
  # Anna veateade, kui tagastatav lipp on drop-down menüüst valimata või tabelist valimata
  # ja klikitakse "Tagasta"
  observeEvent(input$tagasta, {
    if (is.null(input$tagastus_riik) & is.null(input$table_rows_selected)) {
      alert("Sisesta tagastaja")
    }
  })
  
  # Salvesta tagastamiste fail "responses" kausta ainult siis kui tabelis on mõni rida  
  observeEvent(input$tagasta, {
    if (nrow(tagastatud()) > 0) {
      save_data(tagastatud()) 
    }
  })
  
  # Alati kui klikitakse "Lisa" või "Tagasta", lae kõik andmed "responses" kaustast
  df_2 <- eventReactive(input$lisa | input$tagasta, {
    load_data()
  })
  
  # Testimiseks kuva kogu ridade arv "responses" kaustas
  output$ridu <- renderText({nrow(df_2())})
  
  # Koosta dünaamiline tagastatavate lippude nimekiri, et seda drop-down menüüs kuvada
  # Sisaldab piirangut, et kuvatakse ainult välja laenutatud riigilipud
  output$tagastus_riik <- renderUI({
    selectInput(inputId = "tagastus_riik", 
                label = "Vali riik:", 
                multiple = TRUE,
                choices = df_2() %>% 
                  group_by(id) %>%
                  filter(n_distinct(tagastamise_kp, na.rm = TRUE) == 0) %>%
                  ungroup() %>%
                  group_by(riik) %>% 
                  mutate(arv = n_distinct(algus_kp)) %>% 
                  ungroup() %>% 
                  filter((!is.na(algus_kp) & arv > 1) | 
                           (is.na(algus_kp) & arv == 1)) %>%
                  arrange(riik) %>% 
                  # ainult need, mis on välja laenutatud
                  filter(!is.na(algus_kp), is.na(tagastamise_kp)) %>% 
                  mutate(riik_2 = str_c(riik, " - ", laenutaja)) %>% 
                  pull(riik_2)
    )
  })
    
  # Koosta dünaamiline laenutatavate lippude nimekiri, et seda drop-down menüüs kuvada
  # Kuvatud on ainult need lipud, mis ei ole valitud ajavahemikus välja laenutatud
  output$riik <- renderUI({
    selectInput(inputId = "riik", 
                label = "Vali riigilipp:", 
                multiple = TRUE,
                choices = df_2() %>% 
                  group_by(id) %>%
                  filter(n_distinct(tagastamise_kp, na.rm = TRUE) == 0) %>%
                  ungroup() %>%
                  group_by(riik) %>% 
                  mutate(arv = n_distinct(algus_kp)) %>% 
                  ungroup() %>% 
                  filter((!is.na(algus_kp) & arv > 1) | 
                           (is.na(algus_kp) & arv == 1)) %>%
                  arrange(riik) %>% 
                  # ainult need, mis ei ole välja laenutatud
                  mutate(vahel = ifelse((input$laenutamise_kp[1] >= as.Date(algus_kp, "%d.%m.%Y") &
                                           input$laenutamise_kp[1] <= as.Date(lopp_kp, "%d.%m.%Y")) |
                                        (as.Date(algus_kp, "%d.%m.%Y") >= input$laenutamise_kp[1] &
                                             as.Date(algus_kp, "%d.%m.%Y") <= input$laenutamise_kp[2]), 1, 0)) %>%
                  filter(vahel == 0 | is.na(algus_kp)) %>%
                  pull(riik)
    )
  })
  
  # Kuva tabelis kõik lipud koos nende laenutus/broneerimis kuupäevadega
  # Kuvatud on iga lipp nii mitme kordselt kui mitu kehtivat laenutust või tuleviku broneeringut tal peal on
  output$table <- renderDataTable({
  datatable({
    df_2() %>% 
      group_by(id) %>%
      filter(n_distinct(tagastamise_kp, na.rm = TRUE) == 0) %>%
      ungroup() %>%
      group_by(riik) %>% 
      mutate(arv = n_distinct(algus_kp)) %>% 
      ungroup() %>% 
      filter((!is.na(algus_kp) & arv > 1) | 
               (is.na(algus_kp) & arv == 1)) %>%
      select(-arv, -id, -tagastamise_kp) %>% 
      arrange(lopp_kp, riik)
      }, 
    escape = FALSE, rownames = FALSE,
    colnames = c(" " = "lipp"),
    # lipu järgi ei saa filtreerida
    options = list(
      columnDefs = list(list(searchable = FALSE, targets = 0))),
    ) %>% 
      formatStyle('lopp_kp',  color = 'red')})
}

# Käivita äpp
shinyApp(ui = ui, server = server)
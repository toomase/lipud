library(shiny)
library(tidyverse)
library(DT)
library(shinyjs)
library(lubridate)
library(shinythemes)
library(hrbrthemes)
library(shinyalert)
library(writexl)

# Selleks, et uus äpi versioon Shiny serverisse tõsta käivita Terminalis käsk:
# cp ~/Dropbox/DataScience/R/lipud/server.R ~/ShinyApps/lipud/

# Lae algandmed kõigi lippude kohta
riigid_lippudega <- read_csv("~/Dropbox/DataScience/R/lipud/responses/riigid_lippudega.csv")

# Funktsioon salvestab tabeli csv faili ja nimetab selle kuupäeva-kellaaja järgi unikaalselt
# Kasutan seda laenutamiste ja tagastamiste tabeli salvestamiseks
# Iga andmete lisamine salvestub eraldi csv failina
save_data <- function(data) {
  write_csv(x = data, 
            path = file.path("~/Dropbox/DataScience/R/lipud/responses", 
                             str_c(as.integer(Sys.time()), ".csv")))
}

# Funktsioon laeb kõik failid "responses" kaustast ja kirjutab need kokku ühte tabelisse
# Kasutan seda, et kõik lippude laenutamiste ja tagastamiste csv failid äppi laadida
load_data <- function() {
  files <- list.files("~/Dropbox/DataScience/R/lipud/responses", full.names = TRUE)
  data <- map_df(files, read_csv)
  data 
}


# eraldi UI element, mis küsib partooli
shinyServer(function(input, output, session) {
#### UI code --------------------------------------------------------------
  output$ui <- renderUI({
    if (user_input$authenticated == FALSE) {
      ##### UI code for login page
      fluidPage(
        titlePanel("", windowTitle = "Lipud"),
        fluidRow(
          column(width = 3, offset = 5,
            br(), br(), br(), br(),
            uiOutput("uiLogin"),
            uiOutput("pass")
          )
        )
      )
    } else {
      #### Your app's UI code goes here!
      
      fluidPage(
        theme = shinytheme("flatly"),  # muuda üldine kujundus
        useShinyjs(),  # vajalik lisafunktsioonide (alert ja reset) jaoks 
        useShinyalert(),  # vajalik popup alertide kuvamiseks
        
        titlePanel("EOK lippude laenutamise andmebaas",
                   windowTitle = "Lipud"),
        
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
                        label = "Kes laenutab:"
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
            ),
            
            wellPanel(
              h3("Download"),
              h5("Lae alla Excel kõigi lippude või laenutamiste kohta"),
              
              downloadButton("download_koik", "Kõik lipud"),
              
              downloadButton("download_laenutatud", "Hetkel laenutatud lipud")
            )
          ),
          
          # Kuva eraldi tabidel laenutamiste tabel ja statistika
          mainPanel(
            tabsetPanel(type = "tabs",
                        # Kuva välja tabel kõigi lippude kohta koos laenutamise staatusega
                        tabPanel("Laenutus",  
                                 dataTableOutput("table")),
                        tabPanel("Statistika", plotOutput("top15_lippu"),
                                 plotOutput("top15_laenutajat")))
          )
        )
      )
    }
  })
  
  
  
#### YOUR APP'S SERVER CODE GOES HERE ----------------------------------------
    
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
      if (input$laenutaja == "" & !is.null(input$riik)) {
        shinyalert(title = "Sisesta laenutaja",
                   type = "error",
                   closeOnClickOutside = TRUE,
                   confirmButtonText = "OK",
                   confirmButtonCol = "#95A5A5")
      } else if (input$laenutaja != "" & is.null(input$riik)) {
        shinyalert(title = "Sisesta lipp",
                   type = "error",
                   closeOnClickOutside = TRUE,
                   confirmButtonText = "OK",
                   confirmButtonCol = "#95A5A5")
      } else if (input$laenutaja == "" & is.null(input$riik)) {
        shinyalert(title = "Sisesta lipp ja laenutaja",
                   type = "error",
                   closeOnClickOutside = TRUE,
                   confirmButtonText = "OK",
                   confirmButtonCol = "#95A5A5")
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
        arrange(as.Date(lopp_kp, "%d.%m.%Y"), riik) %>% 
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
        shinyalert(title = "Vali tagastatav lipp",
                   type = "error",
                   closeOnClickOutside = TRUE,
                   confirmButtonText = "OK",
                   confirmButtonCol = "#95A5A5")
      }
    })
    
    # Salvesta tagastamiste fail "responses" kausta ainult siis kui tabelis on mõni rida  
    observeEvent(input$tagasta, {
      if (nrow(tagastatud()) > 0) {
        save_data(tagastatud()) 
      }
    })
    
    # Alati kui klikitakse "Lisa" või "Tagasta", lae kõik andmed "responses" kaustast
    # Töötle tabelit nii, et kuvatud ei ole tagastatud lippude kohta laenutamise aega
    df_2 <- eventReactive(input$lisa | input$tagasta, {
      load_data() %>% 
        group_by(id) %>%
        filter(n_distinct(tagastamise_kp, na.rm = TRUE) == 0) %>%
        ungroup() %>%
        group_by(riik) %>% 
        mutate(arv = n_distinct(algus_kp)) %>% 
        ungroup() %>% 
        filter((!is.na(algus_kp) & arv > 1) | 
                 (is.na(algus_kp) & arv == 1)) %>% 
        mutate(hilinenud = if_else(as.Date(lopp_kp, "%d.%m.%Y") < Sys.Date(), 1, 0))
    })
    
    # Koosta dünaamiline tagastatavate lippude nimekiri, et seda drop-down menüüs kuvada
    # Sisaldab piirangut, et kuvatakse ainult välja laenutatud riigilipud
    output$tagastus_riik <- renderUI({
      selectInput(inputId = "tagastus_riik", 
                  label = "Vali riik:", 
                  multiple = TRUE,
                  choices = df_2() %>% 
                    arrange(riik) %>% 
                    # ainult need, mis on välja laenutatud
                    filter(!is.na(algus_kp), is.na(tagastamise_kp)) %>% 
                    # kuva välja riiginimi ja laenutaja nimi
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
                    arrange(riik) %>% 
                    # ainult need, mis ei ole valitud kuupäevadel välja laenutatud
                    # ja mille tagastamine ei ole hilinenud
                    mutate(vahel = ifelse((input$laenutamise_kp[1] >= as.Date(algus_kp, "%d.%m.%Y") &
                                             input$laenutamise_kp[1] <= as.Date(lopp_kp, "%d.%m.%Y")) |
                                            (as.Date(algus_kp, "%d.%m.%Y") >= input$laenutamise_kp[1] &
                                               as.Date(algus_kp, "%d.%m.%Y") <= input$laenutamise_kp[2]) |
                                            as.Date(lopp_kp, "%d.%m.%Y") < Sys.Date(), 1, 0)) %>%
                    filter(vahel == 0 | is.na(algus_kp)) %>%
                    pull(riik)
      )
    })
    
    # Kuva tabelis kõik lipud koos nende laenutus/broneerimis kuupäevadega
    # Kuvatud on iga lipp nii mitme kordselt kui mitu kehtivat laenutust või tuleviku broneeringut tal peal on
    output$table <- renderDataTable({
      datatable({
        df_2() %>% 
          select(-arv, -id, -tagastamise_kp) %>% 
          arrange(as.Date(lopp_kp, "%d.%m.%Y"), riik)
      }, 
      escape = FALSE, 
      rownames = FALSE,
      colnames = c(" " = "lipp", "Riik" = "riik", "Laenutaja" = "laenutaja",
                   "Laenutatud alates" = "algus_kp", "Laenutatud kuni" = "lopp_kp"),
      # lipu järgi ei saa filtreerida
      options = list(
        pageLength = 15,  # kuva vaikimisi 20 rida
        columnDefs = list(list(targets = 5, visible = FALSE),  # peida veerg "hilinenud" - kasutan taustavärviks
                          list(width = "20px", targets = 0))  # lipu veerg kindla laiusega  
        )
      ) %>% 
        # kõik laenutused, mis ei ole tähtajaks tagastatud kuva punase taustavärviga
        formatStyle("hilinenud", target = "row",  backgroundColor = styleEqual(1, '#fee0d2'))
      })
    
    # Salvesta Exceli failina kogu lippude tabel (samal kujul nagu äpis välja kuvatud)
    output$download_koik <- downloadHandler(
      filename = "koik_lipud.xlsx",
      
      content = function(file){
        data <- df_2() %>% 
          select(-arv, -id, -tagastamise_kp, -lipp) %>% 
          arrange(as.Date(lopp_kp, "%d.%m.%Y"), riik) %>% 
          write_xlsx(file)
      }
    )
    
    # Salvesta Exceli failina hetkel laenutatud lippude tabel
    output$download_laenutatud <- downloadHandler(
      filename = "laenutatud_lipud.xlsx",
      
      content = function(file){
        data <- df_2() %>% 
          select(-arv, -id, -tagastamise_kp, -lipp) %>% 
          arrange(as.Date(lopp_kp, "%d.%m.%Y"), riik) %>% 
          filter(!is.na(lopp_kp)) %>% 
          write_xlsx(file)
      }
    )
        
    
    df_graafikuks <- eventReactive(input$lisa | input$tagasta, {
      load_data() %>% 
        filter(!is.na(algus_kp)) %>% 
        arrange(id, algus_kp, tagastamise_kp) %>% 
        distinct(id, .keep_all = TRUE) %>% 
        mutate(riik_puhastatud = str_replace(riik, "-[0-9]$", ""),
               laenutaja_puhastatud = str_to_lower(laenutaja))
    })

    output$top15_lippu <- renderPlot({
      df_graafikuks() %>%
        count(riik_puhastatud, sort = TRUE) %>%
        head(15) %>%
        ggplot(aes(fct_reorder(riik_puhastatud, n), n)) +
        geom_col(alpha = 0.8, fill = "#2b8cbe") +
        geom_text(aes(label = n), hjust = 1.7, colour = "white", fontface = "bold") +
        scale_y_continuous(expand = c(0, 0)) +
        coord_flip() +
        theme_ipsum_rc() +
        guides(fill = FALSE) +
        labs(title = "TOP15 lippu",
             y = "laenutuste arv",
             x = NULL)
    })
    
    output$top15_laenutajat <- renderPlot({
      df_graafikuks() %>% 
        count(laenutaja_puhastatud, sort = TRUE) %>% 
        head(15) %>% 
        ggplot(aes(fct_reorder(laenutaja_puhastatud, n), n)) +
        geom_col(alpha = 0.8, fill = "#2b8cbe") + 
        geom_text(aes(label = n), hjust = 1.7, colour = "white", fontface = "bold") +
        scale_y_continuous(expand = c(0, 0)) +
        coord_flip() +
        theme_ipsum_rc() +
        guides(fill = FALSE) +
        labs(title = "TOP15 laenutajat",
             y = "laenutatud lippude koguarv",
             x = NULL)
    })
    
    
#### PASSWORD server code ---------------------------------------------------- 
  # reactive value containing user's authentication status
  user_input <- reactiveValues(authenticated = FALSE, valid_credentials = FALSE, 
                               user_locked_out = FALSE, status = "")

  # authenticate user by:
  #   1. checking whether their user name and password are in the credentials 
  #       data frame and on the same row (credentials are valid)
  #   2. if credentials are valid, retrieve their lockout status from the data frame
  #   3. if user has failed login too many times and is not currently locked out, 
  #       change locked out status to TRUE in credentials DF and save DF to file
  #   4. if user is not authenticated, determine whether the user name or the password 
  #       is bad (username precedent over pw) or he is locked out. set status value for
  #       error message code below
  observeEvent(input$login_button, {
    credentials <- readRDS("credentials/credentials.rds")
    
    row_username <- which(credentials$user == input$user_name)
    row_password <- which(credentials$pw == digest(input$password)) # digest() makes md5 hash of password

    # if user name row and password name row are same, credentials are valid
    #   and retrieve locked out status
    if (length(row_username) == 1 && 
        length(row_password) >= 1 &&  # more than one user may have same pw
        (row_username %in% row_password)) {
      user_input$valid_credentials <- TRUE
      user_input$user_locked_out <- credentials$locked_out[row_username]
    }

    # if user is not currently locked out but has now failed login too many times:
    #   1. set current lockout status to TRUE
    #   2. if username is present in credentials DF, set locked out status in 
    #     credentials DF to TRUE and save DF
    if (input$login_button == num_fails_to_lockout & 
        user_input$user_locked_out == FALSE) {

      user_input$user_locked_out <- TRUE
            
      if (length(row_username) == 1) {
        credentials$locked_out[row_username] <- TRUE
        
        saveRDS(credentials, "credentials/credentials.rds")
      }
    }
      
    # if a user has valid credentials and is not locked out, he is authenticated      
    if (user_input$valid_credentials == TRUE & user_input$user_locked_out == FALSE) {
      user_input$authenticated <- TRUE
    } else {
      user_input$authenticated <- FALSE
    }

    # if user is not authenticated, set login status variable for error messages below
    if (user_input$authenticated == FALSE) {
      if (user_input$user_locked_out == TRUE) {
        user_input$status <- "locked_out"  
      } else if (length(row_username) > 1) {
        user_input$status <- "credentials_data_error"  
      } else if (input$user_name == "" || length(row_username) == 0) {
        user_input$status <- "bad_user"
      } else if (input$password == "" || length(row_password) == 0) {
        user_input$status <- "bad_password"
      }
    }
  })   

  # password entry UI componenets:
  #   username and password text fields, login button
  output$uiLogin <- renderUI({
    wellPanel(
      textInput("user_name", "Kasutajanimi:"),
      
      passwordInput("password", "Parool:"),

      actionButton("login_button", "Sisene")
    )
  })

  # red error message if bad credentials
  output$pass <- renderUI({
    if (user_input$status == "locked_out") {
      h5(strong(paste0("Your account is locked because of too many\n",
                       "failed login attempts. Contact administrator."), style = "color:red"), align = "center")
    } else if (user_input$status == "credentials_data_error") {    
      h5(strong("Credentials data error - contact administrator!", style = "color:red"), align = "center")
    } else if (user_input$status == "bad_user") {
      h5(strong("User name not found!", style = "color:red"), align = "center")
    } else if (user_input$status == "bad_password") {
      h5(strong("Incorrect password!", style = "color:red"), align = "center")
    } else {
      ""
    }
  })  
})

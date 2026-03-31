

#library(webr)
#library(stringi)
#library(stringr)
#library(ggthemes)
#library(plyr)
#library(readxl)
#library(openxlsx)
#library(data.table)
library(shiny)
library(shinydashboard)
library(DT)
library(openxlsx)
library(readxl)
#library(leaflet)
#library(sf)
library(shinycssloaders)

library(data.table)

library(stringi)
library(stringr)


library(viridis)


library(shinyscreenshot)
library(scales)

library(ggplot2)
library(stringr)
library(shiny)
library(ggthemes)
library(dplyr)
library(tidyr)
#library(igraph)
#library(ggraph)

#library(tidytext)

#library(wordcloud)

#library(widyr)
library(ggthemes)

#library(tidygraph)




#library(curl)

#library(shinyAce)
#source("chooser.R")
#library(shinyscreenshot)
library(scales)
#library(lavaan)

#library(mnormt)
#library(curl)
#library(plspm)
#library(ggplot2)

########################################
########UI (User Interface)#############
########################################

modul_dashboard_ui <- function(id) {
  
  
  
  ns <- NS(id)
  

  fluidPage(
    
    
    
    
    dashboardPage(
      
      
      
      dashboardHeader(title = "",
                      titleWidth = "100%"),
      
      
      dashboardSidebar(disable = TRUE),
      
     
      
      dashboardBody(skin = "purple",
        
        
        
                    
                    fluidRow(
                      # A static valueBox
                      
                      box(
                        title = "", status = "warning", solidHeader = TRUE,
                        collapsible = TRUE, width = "50px",
                        withSpinner(valueBoxOutput(ns("jumlah_data_review_2023"), width = 3)), 
                        
                        
                        
                        withSpinner(valueBoxOutput(ns("jumlah_data_review_2024"), width = 3)),
                        withSpinner(valueBoxOutput(ns("jumlah_data_review_2025"), width = 3)),
                        withSpinner(valueBoxOutput(ns("jumlah_data_review_2026"), width = 3)),
                        
                        br()
                        
                        
                        
                      ), #akhir box
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    ), #Akhir fluidrow
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
        
                    ##################################
                    
                    
                    fluidRow(
                      
                      
                      
                      
                      box(
                        title = "Dataset of Shopee Reviews", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, width = "100%",
                        
                        
                        
                        
                        uiOutput(ns("pilih_tahun_dataset_full")),
                        
                        
                        
                        
                        
                        withSpinner(DT::DTOutput(ns("dataset_full"))),
                        
                      ) #akhir box
                      
                      
                    ), #Akhir fluid row
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    ##################################
                    
                    
                    fluidRow(
                      
                      
                      
                      
                      box(
                        title = "Random Sampling & Visualization", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, width = "100%",
                        
                        
                        
                        shinycssloaders::withSpinner(plotOutput(ns
                          ("grafik_batang_1"), width = "900px", height = "500px" )),
                        
                        
                        
                        
                      ) #akhir box
                      
                      
                    ), #Akhir fluid row
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
        
        
      ) #akhir dashboardBody
      
      
      
      
      
      
      
    ), #Akhir dashboardpage
    
    
    
    
    br()
    
  ) #Akhir dari fluidpage
    
    
    
    
    
    
    
    
    

  
  
} #Akhir dari modul_dashboard_ui

#Akhir dari modul_dashboard_ui
#Akhir dari modul_dashboard_ui
#Akhir dari modul_dashboard_ui
#Akhir dari modul_dashboard_ui











































































########################################
################Server##################
########################################



modul_dashboard_server <- function(input, output, session) {
  
  
  
  
  
  ############Jumlah Data 2023
  
  
  
  
  
  
  output$jumlah_data_review_2023 <- renderValueBox({
    
    data_2023 <- read.xlsx("data_2023.xlsx")
     jumlah_data_2023 <- length(data_2023[,1])
    
    
    
    valueBox(
      
      
      
      jumlah_data_2023, "Number of Reviews in 2023", icon = icon("list"),
      color = "purple", width = "50px"
      
      
      
    )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ############Jumlah Data 2024
  
  
  
  
  
  
  output$jumlah_data_review_2024 <- renderValueBox({
    
    data_2024 <- read.xlsx("data_2024.xlsx")
    jumlah_data_2024 <- length(data_2024[,1])
    
    
    
    valueBox(
      
      
      
      jumlah_data_2024, "Number of Reviews in 2024", icon = icon("list"),
      color = "red", width = "50px"
      
      
      
    )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ############Jumlah Data 2025 
  
  
  
  
  
  
  output$jumlah_data_review_2025  <- renderValueBox({
    
    data_2025  <- read.xlsx("data_2025.xlsx")
    jumlah_data_2025  <- length(data_2025 [,1])
    
    
    
    valueBox(
      
      
      
      jumlah_data_2025 , "Number of Reviews in 2025", icon = icon("list"),
      color = "green", width = "50px"
      
      
      
    )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  ############Jumlah Data 2026  
  
  
  
  
  
  
  output$jumlah_data_review_2026   <- renderValueBox({
    
    data_2026   <- read.xlsx("data_2026.xlsx")
    jumlah_data_2026   <- length(data_2026  [,1])
    
    
    
    valueBox(
      
      
      
      jumlah_data_2026  , "Number of Reviews in 2026", icon = icon("list"),
      color = "orange", width = "50px"
      
      
      
    )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###########Fungsi Tahun
  
  
  fungsi_nama_tahun <- function()
  {
    
    
    
    #data_2023 <- read.xlsx("data_2023.xlsx")
    #dataset_2024 <- read.xlsx("data_2024.xlsx")
    #dataset_2025 <- read.xlsx("data_2025.xlsx")
    #dataset_2026 <- read.xlsx("data_2026.xlsx")
    
    #dataset_full <- rbind(data_2023, dataset_2024, dataset_2025, dataset_2026)
    
    
    #nama_tahun <- dataset_full[,"Year"]
    #nama_tahun <- as.factor(nama_tahun)
    #nama_tahun <- levels(nama_tahun)
    #nama_tahun <- as.numeric(nama_tahun)
    
    nama_tahun <- c(2023, 2024, 2025, 2026)
    
    return(nama_tahun)
    
    
    
  }
  
  
  
  
  
  
  
  
  
  
  
  ################
  
  
  output$pilih_tahun_dataset_full <- renderUI({
    
    
    
    
    checkboxGroupInput(session$ns("terpilih_pilih_tahun_dataset_full"), 
                       label="Year:", choices = c(   fungsi_nama_tahun()    ), 
                       selected=c( fungsi_nama_tahun()  ), inline = TRUE )
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ####################
  
  
  
  
  output$dataset_full <- DT::renderDT({
    
    
    
    
    data_2023 <- read.xlsx("data_2023.xlsx")
    dataset_2024 <- read.xlsx("data_2024.xlsx")
    dataset_2025 <- read.xlsx("data_2025.xlsx")
    dataset_2026 <- read.xlsx("data_2026.xlsx")
    
    dataset_full <- rbind(data_2023, dataset_2024, dataset_2025, dataset_2026)
    
    
    
    tahun_full <- dataset_full[,"Year"]
    
    
    terpilih_pilih_tahun_dataset_full <- input$terpilih_pilih_tahun_dataset_full
    terpilih_pilih_tahun_dataset_full <- as.numeric(terpilih_pilih_tahun_dataset_full)
    
    indeks_terpilih <- tahun_full %in% terpilih_pilih_tahun_dataset_full
    indeks_terpilih <- which(indeks_terpilih == TRUE)
    
    
    
    data_terpilih <- dataset_full[c(indeks_terpilih),]
    
    
    print(data_terpilih)
    
    
    
    
  }) 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #############Visualisasi
  
  
  
  
  
  
  output$grafik_batang_1 <- renderPlot({
    
    dataku <- readxl::read_xlsx("data_sampling_2026.xlsx")
    dataku <- as.data.frame(dataku)
    
    
    
    pilih_grup <- c(1,2,3)
    
    grup_lengkap <- dataku[,"group"]
    
    indeks <- grup_lengkap %in% pilih_grup
    indeks <- which(indeks == TRUE)
    
    data_dengan_grup_terpilih <- dataku[indeks,]
    
    
    
    
    
    
    jumlah_positif = 0
    jumlah_negatif = 0
    jumlah_netral = 0
    
    simpan_positif = 0
    simpan_negatif = 0
    simpan_netral = 0
    simpan_grup = 0
    
    
    for(i in 1 :  length(pilih_grup) )
    {
      
      grup_lengkap <- data_dengan_grup_terpilih[,"group"]
      
      indeks <- grup_lengkap %in% pilih_grup[i]
      indeks <- which(indeks == TRUE)
      
      data_sesuai_indeks <- data_dengan_grup_terpilih[c(indeks),]
      
      x <- data_sesuai_indeks[,"Sentiment"]
      
      
      indeks_positif <-  x %in% "+" 
      indeks_positif <- which(indeks_positif == TRUE)
      jumlah_positif <- length(indeks_positif)
      print(jumlah_positif)
      
      
      indeks_negatif <- x %in% "-"
      indeks_negatif <- which(indeks_negatif == TRUE)
      jumlah_negatif <- length(indeks_negatif) 
      
      indeks_netral <- x %in% "0"
      indeks_netral <- which(indeks_netral == TRUE)
      jumlah_netral <- length(indeks_netral) 
      
      simpan_positif[i] <- jumlah_positif
      simpan_negatif[i] <- jumlah_negatif
      simpan_netral[i] <- jumlah_netral
      simpan_grup[i] <- pilih_grup[i]
      
      
    }
    
    
    dframe <- data.frame(simpan_grup, simpan_positif, simpan_negatif, simpan_netral)
    colnames(dframe) = c("Group", "Positive (+)", "Negative (-)", "Neutral (0)")
 
    
    
    
    
    
    #############
    
    
    jumlah_grup <- length(dframe[,1])
    sentiment <- c("+","-","0")
    jumlah_sentimen <- length(sentiment)
    
    simpan_grup <- vector(mode = "numeric")
    simpan_sentimen <- vector(mode = "character")
    jumlah <- vector(mode = "numeric")
    persentase <- vector(mode = "numeric")
    
    for(i in 1 : jumlah_grup   )
    {
      
      if(i == 1)
      {
        
        simpan_grup <- rep(x = paste0("Sampling Group ", dframe[i,1]) ,  jumlah_sentimen   )
        
        simpan_sentimen <- sentiment
        
        jumlah <- c(dframe[i,2], dframe[i,3], dframe[i,4])
        
        jumlah_baris <- sum(dframe[i,2], dframe[i,3], dframe[i,4])
        persentase <- c(dframe[i,2] / jumlah_baris * 100, dframe[i,3] / jumlah_baris * 100, dframe[i,4] / jumlah_baris * 100)
        persentase <- round(persentase, digits = 2)
        
      }
      
      
      if(i > 1)
      {
        
        A <- rep(x = paste0("Sampling Group ", dframe[i,1]),  jumlah_sentimen   )
        simpan_grup <- c(simpan_grup, A)
        
        B <- sentiment
        simpan_sentimen <- c(simpan_sentimen, B)
        
        C <- c(dframe[i,2], dframe[i,3], dframe[i,4])
        jumlah <- c(jumlah, C)
        
        
        D <- c(dframe[i,2] / jumlah_baris * 100, dframe[i,3] / jumlah_baris * 100, dframe[i,4] / jumlah_baris * 100)
        D <- round(D, digits = 2)
        
        
        persentase <- c(persentase, D)
        
        
        
      }
      
      
      
      
      
      
    }
    
    
    data_sudah_disusun_untuk_grafik <- data.frame(simpan_grup, simpan_sentimen, jumlah, persentase)
    colnames(data_sudah_disusun_untuk_grafik) <- c("Group", "Sentiment", "Frequency", "Percentage")
    
    
    
    
    
    
    
    
    
    gambar <- ggplot(data_sudah_disusun_untuk_grafik,
           aes(fill = Sentiment, 
               y = Frequency, x = Sentiment)) + 
      geom_bar(position = "dodge", 
               stat="identity") +
      scale_fill_viridis(discrete = T, option = "E") +
      ggtitle("Frequency Distribution of Sentiment Based on Sampling Group (Sampling based on Shopee Review Data from 2026)") +
      facet_wrap( ~ Group) +
      theme(legend.position="none") +
      xlab("") +
       theme(axis.text.x=element_text( color="black", size = 15),
              axis.text.y=element_text( color="black", size = 15),
              axis.title=element_text( color="black" , size = 15),
              legend.text=element_text(size = 15, colour="blue") )+
      theme(plot.title = element_text(hjust=0.5,  size=15, color = "black"     )) +
      theme(strip.background = element_rect( fill = "grey"  ), 
            strip.text.x = element_text(colour = "black"  , 
                                        size  = 15),
            strip.text.y = element_text(colour = "black"  , 
                                        size  = 15)) 
    
   
    
    
    print(gambar)
    
    
    
    
    
    
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
} #akhir dari modul_dashboard_server

#akhir dari modul_dashboard_server
#akhir dari modul_dashboard_server
#akhir dari modul_dashboard_server

















































































ui <- fluidPage(
  
  
 includeHTML("intro_home.html"),
  
 
 
 
  
  uiOutput("modul_dashboard"),
  
  
  br()
  
) #Akhir dari UI











server <- function(input, output) {
  
  
  
  
  
  output$modul_dashboard <- renderUI({
    
    
    
    #source("module//modul_dashboard.R")
    callModule(module = modul_dashboard_server, id = "modul_dashboard")
    modul_dashboard_ui(id = "modul_dashboard")
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
} #Akhir dari server










shinyApp(ui, server)















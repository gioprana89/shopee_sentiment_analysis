

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
                        collapsible = TRUE, collapsed = TRUE, width = "100%",
                        
                        
                        
                        
                        uiOutput(ns("pilih_tahun_dataset_full")),
                        
                        
                        
                        
                        
                        withSpinner(DT::DTOutput(ns("dataset_full"))),
                        
                      ) #akhir box
                      
                      
                    ), #Akhir fluid row
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    ##################################
                    
                    
                    fluidRow(
                      
                      
                      
                      
                      
                      
                      
                      box(
                        title = "Visualization of Sentiment Distribution Based on 10 Random Sampling (Bar Chart)", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, width = "100%",
                        
                        
                        
                        
                        
                        
                        
                        
                        fluidRow(
                          column(4,
                                 
                                 
                                 
                                 
                                 sliderInput(ns("grafik1_axis.text.x"), "axis.text.x:",
                                             min = 1, max = 30,
                                             value = 15),
                                 
                                 
                                 sliderInput(ns("grafik1_axis.text.y"), "axis.text.y:",
                                             min = 1, max = 30,
                                             value = 15),
                                 
                                 
                                 
                                 
                                 sliderInput(ns("grafik1_axis.title"), "axis.title:",
                                             min = 1, max = 30,
                                             value = 15),
                                 
                                 sliderInput(ns("grafik1_legend.text"), "legend.text:",
                                             min = 1, max = 30,
                                             value = 15),
                                 
                                 
                                 
                                 sliderInput(ns("grafik1_element_text"), "element_text:",
                                             min = 1, max = 30,
                                             value = 15),
                                 
                                 
                                 
                                 sliderInput(ns("grafik1_strip.text.x"), "strip.text.x:",
                                             min = 1, max = 30,
                                             value = 15),
                                 
                                 
                                 sliderInput(ns("grafik1_strip.text.y"), "strip.text.y:",
                                             min = 1, max = 30,
                                             value = 15),
                                 
                                 
                                 
                                 
                                 
                                 
                                 br()
                                 
                                 
                          ), #akhir column
                          
                          
                          column(4,
                                 
                                 
                                 radioButtons(ns("grafik1_theme"),
                                              
                                              "Theme:", 
                                              c("theme_bw" = "theme_bw", 
                                                "theme_linedraw"="theme_linedraw",
                                                "theme_light"="theme_light",
                                                "theme_dark"="theme_dark",
                                                "theme_minimal"="theme_minimal", 
                                                "theme_classic"="theme_classic",
                                                "theme_void" = "theme_void",
                                                
                                                'theme_tufte'='theme_tufte', 
                                                'theme_economist'='theme_economist', 
                                                'theme_solarized'='theme_solarized', 
                                                'theme_solarized_2'='theme_solarized_2',
                                                'theme_stata'='theme_stata',
                                                'theme_excel'='theme_excel',
                                                'theme_igray'='theme_igray'
                                                
                                              ), inline=TRUE, selected = "theme_classic"   ),
                                 
                                 
                                 
                                 
                                 
                                 
                                 sliderInput(ns("grafik1_vjust_text"), "vjust:",
                                             min = -5, max = 5,
                                             value = 0, step = 0.1),
                                 
                                 
                                 
                                 
                                 
                                 
                                 sliderInput(ns("grafik1_size_text"), "size:",
                                             min = 0, max = 30,
                                             value = 5),
                                 
                                 
                                 textInput(ns("grafik1_color_text"),
                                           "Text Color", 
                                           "orange"),
                                 
                                 
                                 
                                 
                                 textInput(ns("grafik1_warnabar1"),
                                           "First Color for Bar", 
                                           "#ee82ee"),
                                 
                                 
                                 textInput(ns("grafik1_warnabar2"),
                                           "Second Color for Bar", 
                                           "#a7fc00"),
                                 
                                 
                                 textInput(ns("grafik1_warnabar3"),
                                           "Third Color for Bar", 
                                           "#ffcc33"),
                                 
                                 
                                 
                                 
                                 
                                 
                                 
                                 
                                 
                                 br()
                                 
                                 
                          ), #akhir column
                          
                          
                          
                          column(4,
                                 
                                 
                                 br()
                                 
                                 
                          ) #akhir column
                          
                          
                        ), #akhir fluidrow
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        shinycssloaders::withSpinner(plotOutput(ns
                          ("grafik_batang_1"), width = "1200px", height = "700px" )),
                        
                        
                        
                        
                      ) #akhir box
                      
                      
                    ), #Akhir fluid row
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    ##################################
                    
                    
                    fluidRow(
                      
                      
                      
                      
                      
                      
                      
                      box(
                        title = "Visualization of Sentiment Distribution Based on 10 Random Sampling (Lollipop Chart)", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, collapsed = FALSE, width = "100%",
                        
                        
                        
                        
                        
                        
                        
                        
                        fluidRow(
                          column(4,
                                 
                                 
                                 
                                 sliderInput(ns("grafik2_axis.text.x"), "axis.text.x:",
                                             min = 1, max = 30,
                                             value = 15),
                                 
                                 
                                 sliderInput(ns("grafik2_axis.text.y"), "axis.text.y:",
                                             min = 1, max = 30,
                                             value = 15),
                                 
                                 
                                 
                                 
                                 sliderInput(ns("grafik2_axis.title"), "axis.title:",
                                             min = 1, max = 30,
                                             value = 15),
                                 
                                 sliderInput(ns("grafik2_legend.text"), "legend.text:",
                                             min = 1, max = 30,
                                             value = 15),
                                 
                                 
                                 
                                 sliderInput(ns("grafik2_element_text"), "element_text:",
                                             min = 1, max = 30,
                                             value = 15),
                                 
                                 
                                 
                                 sliderInput(ns("grafik2_strip.text.x"), "strip.text.x:",
                                             min = 1, max = 30,
                                             value = 15),
                                 
                                 
                                 sliderInput(ns("grafik2_strip.text.y"), "strip.text.y:",
                                             min = 1, max = 30,
                                             value = 15),
                                 
                                 
                                 
                                 
                                 
                                 
                                 
                                 br()
                                 
                                 
                          ), #akhir column
                          
                          
                          column(4,
                                 
                                 
                                 
                                 radioButtons(ns("grafik2_theme"),
                                              
                                              "Theme:", 
                                              c("theme_bw" = "theme_bw", 
                                                "theme_linedraw"="theme_linedraw",
                                                "theme_light"="theme_light",
                                                "theme_dark"="theme_dark",
                                                "theme_minimal"="theme_minimal", 
                                                "theme_classic"="theme_classic",
                                                "theme_void" = "theme_void",
                                                
                                                'theme_tufte'='theme_tufte', 
                                                'theme_economist'='theme_economist', 
                                                'theme_solarized'='theme_solarized', 
                                                'theme_solarized_2'='theme_solarized_2',
                                                'theme_stata'='theme_stata',
                                                'theme_excel'='theme_excel',
                                                'theme_igray'='theme_igray'
                                                
                                              ), inline=TRUE, selected = "theme_classic"   ),
                                 
                                 
                                 
                                 
                                 
                                 sliderInput(ns("grafik2_geom_point"), "size:",
                                             min = 0, max = 20,
                                             value = 7),
                                 
                                 
                                 
                                 sliderInput(ns("grafik2_geom_segment"), "size:",
                                             min = 0, max = 10,
                                             value = 1),
                                 
                                 
                                 
                                 
                                 sliderInput(ns("grafik2_vjust_text"), "vjust:",
                                             min = -5, max = 5,
                                             value = 0, step = 0.1),
                                 
                                 
                                 
                                 
                                 
                                 
                                 sliderInput(ns("grafik2_size_text"), "size:",
                                             min = 0, max = 30,
                                             value = 5),
                                 
                                 
                                 textInput(ns("grafik2_color_text"),
                                           "Text Color", 
                                           "orange"),
                                 
                                 
                                 
                                 textInput(ns("grafik2_color_point"),
                                           "Point Color", 
                                           "green"),
                                 
                                 
                                 textInput(ns("grafik2_color_segment"),
                                           "Segment Color", 
                                           "blue"),
                                 
                                 
                                 
                                 
                                 br()
                                 
                                 
                          ), #akhir column
                          
                          
                          
                          column(4,
                                 
                                 
                                 br()
                                 
                                 
                          ) #akhir column
                          
                          
                        ), #akhir fluidrow
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        fluidRow(
                          column(4,
                                 
                                 
                        
                        shinycssloaders::withSpinner(plotOutput(ns
                                                                ("grafik_lolipop_1"), width = "500px", height = "500px" )),
                        
                        
                          ),
                        
                        
                        column(4,
                               
                               
                               
                               shinycssloaders::withSpinner(plotOutput(ns
                                                                       ("grafik_lolipop_2"), width = "500px", height = "500px" )),
                               
                               
                        ),
                        
                        
                        column(4,
                               
                               
                               
                               shinycssloaders::withSpinner(plotOutput(ns
                                                                       ("grafik_lolipop_3"), width = "500px", height = "500px" )),
                               
                               
                        )
                        
                        
                        ), #Akhir fluidrow
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                      ) #akhir box
                      
                      
                    ), #Akhir fluid row
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                  #######################
                  #######################
                  
                  
                  
                  
                  
                  
                  
                  ##################################
                  
                  
                  fluidRow(
                    
                    
                    
                    
                    
                    
                    
                    box(
                      title = "Visualization of Wordcloud Based on Negative Sentiment: 10 Random Sampling", status = "primary", solidHeader = TRUE,
                      collapsible = TRUE, collapsed = FALSE, width = "100%",
                      
                      
                      
                  
                      
                      
                      fluidRow(
                        column(4,
                               
                               radioButtons(ns("warna_wordcloud_sampling_1_sentiment_negatif"),
                                            
                                            "Theme of Words:", 
                                            c("Blues" = "Blues", "BuGn"="BuGn",
                                              "BuPu"="BuPu", "GnBu"="GnBu", "Greens"="Greens", "YlOrRd"="YlOrRd", "YlOrBr" = "YlOrBr", "YlGnBu" = "YlGnBu",
                                              "Spectral" = "Spectral", "RdYlGn" = "RdYlGn", "YlGn" = "YlGn",
                                              "RdBu" = "RdBu", "RdGy" = "RdGy", "RdYlBu" = "RdYlBu",
                                              "PiYG" = "PiYG", "PRGn" = "PRGn", "PuOr" = "PuOr",
                                              "Purples" = "Purples", "RdPu" = "RdPu", "BrBG" = "BrBG",
                                              "Dark2" = "Dark2"), inline=TRUE, selected = "Dark2"   ),
                               
                               
                               
                               
                               br()
                               
                               
                        ),
                        
                        
                        column(4,
                               
                               
                               
                               
                               sliderInput(ns("max_words_sampling_1_sentiment_negatif"), "max.words:",
                                           min = 1, max = 100,
                                           value = 20),
                               
                               
                               
                               
                               sliderInput(ns("n.brewer.pal_sampling_1_sentiment_negatif"), "n.brewer.pal:",
                                           min = 1, max = 100,
                                           value = 10),
                               
                               #n.brewer.pal
                               
                               
                               br()
                               
                               
                        ),
                        
                        
                        
                        column(4,
                               
                               
                               
                               sliderInput(ns("min_freq_sampling_1_sentiment_negatif"), "min.freq:",
                                           min = 1, max = 100,
                                           value = 4),
                               
                               
                               textAreaInput(ns("rot.per_sampling_1_sentiment_negatif"), 
                                             "rot.per", value = "0.35", height = 70, width = 100),
                               
                               
                               #rot.per=0.35
                               
                               
                               #min.freq = 4
                               
                               br()
                               
                               
                        )
                        
                        
                        
                      ), #Akhir fluidrow
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      fluidRow(
                        
                        
                        column(4,
                               
                               
                               box(
                                 title = "Sampling 1", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = "100%",
                                 
                                 
                                
                                 
                                 
                                 
                                 
                                 
                                 
                                 
                                 
                                 
                                 
                                 
                                 
                                 
                                 
                                 
                                 
                                 
                                 
                                 
                                 
                                 
                                 
                                 
                                 
                                 
                                 
                                 
                                 
                                 
                                 
                                 
                                 shinycssloaders::withSpinner(plotOutput(ns("grafik_wordcloud_sentiment_negatif_sampling_1"),
                                                                         width = "80%" ) ),
                                 
                                 
                                 
                                 br()
                                 
                               ) #akhir box
                               
                        ),
                        
                        
                        
                        column(4,
                               
                               
                               box(
                                 title = "Sampling 2", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = "100%",
                                 
                                 
                                 shinycssloaders::withSpinner(plotOutput(ns("grafik_wordcloud_sentiment_negatif_sampling_2"),
                                                                         width = "80%" ) ),
                                 
                                 
                                 
                                 br()
                                 
                               )#akhir box
                               
                        ),
                        
                        
                        
                        
                        column(4,
                               
                               
                               box(
                                 title = "Sampling 3", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = "100%",
                                 
                                 
                                 shinycssloaders::withSpinner(plotOutput(ns("grafik_wordcloud_sentiment_negatif_sampling_3"),
                                                                         width = "80%" ) ),
                                 
                                 
                                 br()
                                 
                               )#akhir box
                               
                        ),
                        
                        
                        
                      ), #fluidrow
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      ###############
                      
                      
                      
                      
                      
                      
                      fluidRow(
                        
                        
                        column(4,
                               
                               
                               box(
                                 title = "Sampling 4", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = "100%",
                                 
                                 
                                 
                                 
                                 shinycssloaders::withSpinner(plotOutput(ns("grafik_wordcloud_sentiment_negatif_sampling_4"),
                                                                         width = "80%" ) ),
                                 
                                 
                                 
                                 br()
                                 
                               ) #akhir box
                               
                        ),
                        
                        
                        
                        column(4,
                               
                               
                               box(
                                 title = "Sampling 5", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = "100%",
                                 
                                 
                                 shinycssloaders::withSpinner(plotOutput(ns("grafik_wordcloud_sentiment_negatif_sampling_5"),
                                                                         width = "80%" ) ),
                                 
                                 
                                 
                                 br()
                                 
                               )#akhir box
                               
                        ),
                        
                        
                        
                        
                        column(4,
                               
                               
                               box(
                                 title = "Sampling 6", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = "100%",
                                 
                                 
                                 shinycssloaders::withSpinner(plotOutput(ns("grafik_wordcloud_sentiment_negatif_sampling_6"),
                                                                         width = "80%" ) ),
                                 
                                 
                                 br()
                                 
                               )#akhir box
                               
                        ),
                        
                        
                        
                      ), #fluidrow
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      fluidRow(
                        
                        
                        column(4,
                               
                               
                               box(
                                 title = "Sampling 7", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = "100%",
                                 
                                 
                                 
                                 
                                 shinycssloaders::withSpinner(plotOutput(ns("grafik_wordcloud_sentiment_negatif_sampling_7"),
                                                                         width = "80%" ) ),
                                 
                                 
                                 
                                 br()
                                 
                               ) #akhir box
                               
                        ),
                        
                        
                        
                        column(4,
                               
                               
                               box(
                                 title = "Sampling 8", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = "100%",
                                 
                                 
                                 shinycssloaders::withSpinner(plotOutput(ns("grafik_wordcloud_sentiment_negatif_sampling_8"),
                                                                         width = "80%" ) ),
                                 
                                 
                                 
                                 br()
                                 
                               )#akhir box
                               
                        ),
                        
                        
                        
                        
                        column(4,
                               
                               
                               box(
                                 title = "Sampling 9", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = "100%",
                                 
                                 
                                 shinycssloaders::withSpinner(plotOutput(ns("grafik_wordcloud_sentiment_negatif_sampling_9"),
                                                                         width = "80%" ) ),
                                 
                                 
                                 br()
                                 
                               )#akhir box
                               
                        ),
                        
                        
                        
                      ), #fluidrow
                      
                      
                      
                      
                      
                      
                      
                      ##################
                      
                      
                      
                      
                      fluidRow(
                        
                        
                        column(4,
                               
                               
                               box(
                                 title = "Sampling 10", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = "100%",
                                 
                                 
                                 
                                 
                                 shinycssloaders::withSpinner(plotOutput(ns("grafik_wordcloud_sentiment_negatif_sampling_10"),
                                                                         width = "80%" ) ),
                                 
                                 
                                 
                                 br()
                                 
                               ) #akhir box
                               
                        ),
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                      ), #fluidrow
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
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
    
    
    
    pilih_grup <- c(1,2,3,4,5,6,7,8,9,10)
    
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
    
    
    
    #############
    
    
    
    
    urutan_narasi_sampling <- c("Sampling Group 1", "Sampling Group 2", "Sampling Group 3", "Sampling Group 4", "Sampling Group 5", "Sampling Group 6", "Sampling Group 7", "Sampling Group 8", "Sampling Group 9", "Sampling Group 10")
    
    data_sudah_disusun_untuk_grafik[,"Group"] <- as.factor(data_sudah_disusun_untuk_grafik[,"Group"])
    
    a = data_sudah_disusun_untuk_grafik[,"Group"]
    
    data_sudah_disusun_untuk_grafik[,"Group"] <- factor(a, levels = urutan_narasi_sampling)
    

    
    
    
    gambar <- ggplot(data_sudah_disusun_untuk_grafik,
           aes(fill = Sentiment, 
               y = Frequency, x = Sentiment)) + 
      geom_bar(position = "dodge", 
               stat="identity") +
      scale_fill_viridis(discrete = T, option = "E") +
      ggtitle("Frequency Distribution of Sentiment Based on Sampling Group (Sampling based on Shopee Review Data from 2026)") +
      facet_wrap( ~ Group) +
      theme(legend.position="none") +
      xlab("") 
    
    
    
    
    get_theme = input$grafik1_theme
    
    if(get_theme == 'theme_bw')
    {
      gambar <- gambar + theme_bw()
    }
    
    if(get_theme == 'theme_linedraw')
    {
      gambar <- gambar + theme_linedraw()
    }
    if(get_theme == 'theme_light')
    {
      gambar <- gambar + theme_light()
    }
    if(get_theme == 'theme_dark')
    {
      gambar <- gambar + theme_dark()
    }
    if(get_theme == 'theme_minimal')
    {
      gambar <- gambar + theme_minimal()
    }
    if(get_theme == 'theme_classic')
    {
      gambar <- gambar + theme_classic()
    }
    if(get_theme == 'theme_void')
    {
      gambar <- gambar + theme_void()
    }
    if(get_theme == 'theme_tufte')
    {
      gambar <- gambar + theme_tufte()
    }
    if(get_theme == 'theme_economist')
    {
      gambar <- gambar + theme_economist()
    }
    if(get_theme == 'theme_solarized')
    {
      gambar <- gambar + theme_solarized()
    }
    if(get_theme == 'theme_solarized_2')
    {
      gambar <- gambar + theme_solarized(light = FALSE)
    }
    if(get_theme == 'theme_stata')
    {
      gambar <- gambar + theme_stata()
    }
    if(get_theme == 'theme_excel')
    {
      gambar <- gambar + theme_excel()
    }
    if(get_theme == 'theme_igray')
    {
      gambar <- gambar + theme_igray()
    }
    
    
    
    
    grafik1_color_text <- input$grafik1_color_text
    grafik1_color_text <- as.character(grafik1_color_text)
    
    
    warna1 <- input$grafik1_warnabar1
    
    
    warna2 <- input$grafik1_warnabar2
    warna3 <- input$grafik1_warnabar3
    
    
    gambar <- gambar +   theme(axis.text.x=element_text( color="black", size = input$grafik1_axis.text.x),
              axis.text.y=element_text( color="black", size = input$grafik1_axis.text.y),
              axis.title=element_text( color="black" , size = input$grafik1_axis.title),
              legend.text=element_text(size = input$grafik1_legend.text, colour="blue") )+
      theme(plot.title = element_text(hjust=0.5,  size = input$grafik1_element_text, color = "black"     )) +
      theme(strip.background = element_rect( fill = "grey"  ), 
            strip.text.x = element_text(colour = "black"  , 
                                        size  = input$grafik1_strip.text.x),
            strip.text.y = element_text(colour = "black"  , 
                                        size  = input$grafik1_strip.text.y)) +
       geom_text(aes(label=paste0(Frequency) ), 
                  color= grafik1_color_text,
                 position = position_dodge(0), 
                  vjust = input$grafik1_vjust_text, 
                 size = input$grafik1_size_text ) +
      scale_fill_manual(values = c(warna1, warna2, warna3))
    
    
   
    
    
    print(gambar)
    
    
    
    
    
    
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ############
  ############
  
  
  
  
  
  
  output$grafik_lolipop_1 <- renderPlot({
    
    dataku <- readxl::read_xlsx("data_sampling_2026.xlsx")
    dataku <- as.data.frame(dataku)
    
    
    
    pilih_grup <- c(1,2,3,4,5,6,7,8,9,10)
    
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
    
    
    
    #############
    
    
    
    
    urutan_narasi_sampling <- c("Sampling Group 1", "Sampling Group 2", "Sampling Group 3", "Sampling Group 4", "Sampling Group 5", "Sampling Group 6", "Sampling Group 7", "Sampling Group 8", "Sampling Group 9", "Sampling Group 10")
    
    data_sudah_disusun_untuk_grafik[,"Group"] <- as.factor(data_sudah_disusun_untuk_grafik[,"Group"])
    
    a = data_sudah_disusun_untuk_grafik[,"Group"]
    
    data_sudah_disusun_untuk_grafik[,"Group"] <- factor(a, levels = urutan_narasi_sampling)
    
    
    
    
    pilih_sentimen <- "+"
    
    seluruh_sentimen <- data_sudah_disusun_untuk_grafik[,"Sentiment"]
    
    indeks <- seluruh_sentimen %in% pilih_sentimen
    indeks <- which(indeks == TRUE)
    
    data_sudah_disusun_untuk_grafik2 <- data_sudah_disusun_untuk_grafik[c(indeks),]
    
    # Plot
  gambar <-  ggplot(data_sudah_disusun_untuk_grafik2,
           aes(x = Group, 
               y = Frequency)) +
      geom_segment( 
        aes(x = Group, xend = Group, 
            y = 0, yend = Frequency) , 
        size = input$grafik2_geom_segment, 
        color = input$grafik2_color_segment, 
        linetype="solid" ) +
      geom_point( color = input$grafik2_color_point,
                  size = input$grafik2_geom_point) +
      theme_light()  + coord_flip() 
  
  
  
  
  
  
  
  
  get_theme = input$grafik2_theme
  
  if(get_theme == 'theme_bw')
  {
    gambar <- gambar + theme_bw()
  }
  
  if(get_theme == 'theme_linedraw')
  {
    gambar <- gambar + theme_linedraw()
  }
  if(get_theme == 'theme_light')
  {
    gambar <- gambar + theme_light()
  }
  if(get_theme == 'theme_dark')
  {
    gambar <- gambar + theme_dark()
  }
  if(get_theme == 'theme_minimal')
  {
    gambar <- gambar + theme_minimal()
  }
  if(get_theme == 'theme_classic')
  {
    gambar <- gambar + theme_classic()
  }
  if(get_theme == 'theme_void')
  {
    gambar <- gambar + theme_void()
  }
  if(get_theme == 'theme_tufte')
  {
    gambar <- gambar + theme_tufte()
  }
  if(get_theme == 'theme_economist')
  {
    gambar <- gambar + theme_economist()
  }
  if(get_theme == 'theme_solarized')
  {
    gambar <- gambar + theme_solarized()
  }
  if(get_theme == 'theme_solarized_2')
  {
    gambar <- gambar + theme_solarized(light = FALSE)
  }
  if(get_theme == 'theme_stata')
  {
    gambar <- gambar + theme_stata()
  }
  if(get_theme == 'theme_excel')
  {
    gambar <- gambar + theme_excel()
  }
  if(get_theme == 'theme_igray')
  {
    gambar <- gambar + theme_igray()
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    gambar <- gambar +  geom_text(aes(label=paste0(Frequency) ), 
                color= input$grafik2_color_text,
                position = position_dodge(0), 
                vjust = input$grafik2_vjust_text, 
                size = input$grafik2_size_text ) +
    theme(axis.text.x=element_text( color="black", size = input$grafik2_axis.text.x),
          axis.text.y=element_text( color="black", size = input$grafik2_axis.text.y),
          axis.title=element_text( color="black" , size = input$grafik2_axis.title),
          legend.text=element_text(size = input$grafik2_legend.text, colour="blue") )+
    theme(plot.title = element_text(hjust=0.5,  size = input$grafik2_element_text, color = "black"     )) +
    theme(strip.background = element_rect( fill = "grey"  ), 
          strip.text.x = element_text(colour = "black"  , 
                                      size  = input$grafik2_strip.text.x),
          strip.text.y = element_text(colour = "black"  , 
                                      size  = input$grafik2_strip.text.y)) +
      ggtitle("Positive Sentiment")
    
    
    
    
    
    
#    grafik1_color_text <- input$grafik1_color_text
 #   grafik1_color_text <- as.character(grafik1_color_text)
    
    
  #  warna1 <- input$grafik1_warnabar1
    
    
   # warna2 <- input$grafik1_warnabar2
    #warna3 <- input$grafik1_warnabar3
    

    
    
    print(gambar)
    
    
    
    
    
    
    
    
    
    
  })
  
  
  #############
  #############
  
  
  
  
  
  output$grafik_lolipop_2 <- renderPlot({
    
    dataku <- readxl::read_xlsx("data_sampling_2026.xlsx")
    dataku <- as.data.frame(dataku)
    
    
    
    pilih_grup <- c(1,2,3,4,5,6,7,8,9,10)
    
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
    
    
    
    #############
    
    
    
    
    urutan_narasi_sampling <- c("Sampling Group 1", "Sampling Group 2", "Sampling Group 3", "Sampling Group 4", "Sampling Group 5", "Sampling Group 6", "Sampling Group 7", "Sampling Group 8", "Sampling Group 9", "Sampling Group 10")
    
    data_sudah_disusun_untuk_grafik[,"Group"] <- as.factor(data_sudah_disusun_untuk_grafik[,"Group"])
    
    a = data_sudah_disusun_untuk_grafik[,"Group"]
    
    data_sudah_disusun_untuk_grafik[,"Group"] <- factor(a, levels = urutan_narasi_sampling)
    
    
    
    
    pilih_sentimen <- "-"
    
    seluruh_sentimen <- data_sudah_disusun_untuk_grafik[,"Sentiment"]
    
    indeks <- seluruh_sentimen %in% pilih_sentimen
    indeks <- which(indeks == TRUE)
    
    data_sudah_disusun_untuk_grafik2 <- data_sudah_disusun_untuk_grafik[c(indeks),]
    
    # Plot
    gambar <-  ggplot(data_sudah_disusun_untuk_grafik2,
                      aes(x = Group, 
                          y = Frequency)) +
      geom_segment( 
        aes(x = Group, xend = Group, 
            y = 0, yend = Frequency) , 
        size = input$grafik2_geom_segment, 
        color = input$grafik2_color_segment, 
        linetype="solid" ) +
      geom_point( color = input$grafik2_color_point,
                  size = input$grafik2_geom_point) +
      theme_light()  + coord_flip() 
    
    
    
    
    
    
    
    
    get_theme = input$grafik2_theme
    
    if(get_theme == 'theme_bw')
    {
      gambar <- gambar + theme_bw()
    }
    
    if(get_theme == 'theme_linedraw')
    {
      gambar <- gambar + theme_linedraw()
    }
    if(get_theme == 'theme_light')
    {
      gambar <- gambar + theme_light()
    }
    if(get_theme == 'theme_dark')
    {
      gambar <- gambar + theme_dark()
    }
    if(get_theme == 'theme_minimal')
    {
      gambar <- gambar + theme_minimal()
    }
    if(get_theme == 'theme_classic')
    {
      gambar <- gambar + theme_classic()
    }
    if(get_theme == 'theme_void')
    {
      gambar <- gambar + theme_void()
    }
    if(get_theme == 'theme_tufte')
    {
      gambar <- gambar + theme_tufte()
    }
    if(get_theme == 'theme_economist')
    {
      gambar <- gambar + theme_economist()
    }
    if(get_theme == 'theme_solarized')
    {
      gambar <- gambar + theme_solarized()
    }
    if(get_theme == 'theme_solarized_2')
    {
      gambar <- gambar + theme_solarized(light = FALSE)
    }
    if(get_theme == 'theme_stata')
    {
      gambar <- gambar + theme_stata()
    }
    if(get_theme == 'theme_excel')
    {
      gambar <- gambar + theme_excel()
    }
    if(get_theme == 'theme_igray')
    {
      gambar <- gambar + theme_igray()
    }
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    gambar <- gambar +  geom_text(aes(label=paste0(Frequency) ), 
                                  color= input$grafik2_color_text,
                                  position = position_dodge(0), 
                                  vjust = input$grafik2_vjust_text, 
                                  size = input$grafik2_size_text ) +
      theme(axis.text.x=element_text( color="black", size = input$grafik2_axis.text.x),
            axis.text.y=element_text( color="black", size = input$grafik2_axis.text.y),
            axis.title=element_text( color="black" , size = input$grafik2_axis.title),
            legend.text=element_text(size = input$grafik2_legend.text, colour="blue") )+
      theme(plot.title = element_text(hjust=0.5,  size = input$grafik2_element_text, color = "black"     )) +
      theme(strip.background = element_rect( fill = "grey"  ), 
            strip.text.x = element_text(colour = "black"  , 
                                        size  = input$grafik2_strip.text.x),
            strip.text.y = element_text(colour = "black"  , 
                                        size  = input$grafik2_strip.text.y)) +
      ggtitle("Negative Sentiment")
    
    
    
    
    
    
    #    grafik1_color_text <- input$grafik1_color_text
    #   grafik1_color_text <- as.character(grafik1_color_text)
    
    
    #  warna1 <- input$grafik1_warnabar1
    
    
    # warna2 <- input$grafik1_warnabar2
    #warna3 <- input$grafik1_warnabar3
    
    
    
    
    print(gambar)
    
    
    
    
    
    
    
    
    
    
  })
  
  
  
  #############
  #############
  
  
  
  
  
  output$grafik_lolipop_3 <- renderPlot({
    
    dataku <- readxl::read_xlsx("data_sampling_2026.xlsx")
    dataku <- as.data.frame(dataku)
    
    
    
    pilih_grup <- c(1,2,3,4,5,6,7,8,9,10)
    
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
    
    
    
    #############
    
    
    
    
    urutan_narasi_sampling <- c("Sampling Group 1", "Sampling Group 2", "Sampling Group 3", "Sampling Group 4", "Sampling Group 5", "Sampling Group 6", "Sampling Group 7", "Sampling Group 8", "Sampling Group 9", "Sampling Group 10")
    
    data_sudah_disusun_untuk_grafik[,"Group"] <- as.factor(data_sudah_disusun_untuk_grafik[,"Group"])
    
    a = data_sudah_disusun_untuk_grafik[,"Group"]
    
    data_sudah_disusun_untuk_grafik[,"Group"] <- factor(a, levels = urutan_narasi_sampling)
    
    
    
    
    pilih_sentimen <- "0"
    
    seluruh_sentimen <- data_sudah_disusun_untuk_grafik[,"Sentiment"]
    
    indeks <- seluruh_sentimen %in% pilih_sentimen
    indeks <- which(indeks == TRUE)
    
    data_sudah_disusun_untuk_grafik2 <- data_sudah_disusun_untuk_grafik[c(indeks),]
    
    # Plot
    gambar <-  ggplot(data_sudah_disusun_untuk_grafik2,
                      aes(x = Group, 
                          y = Frequency)) +
      geom_segment( 
        aes(x = Group, xend = Group, 
            y = 0, yend = Frequency) , 
        size = input$grafik2_geom_segment, 
        color = input$grafik2_color_segment, 
        linetype="solid" ) +
      geom_point( color = input$grafik2_color_point,
                  size = input$grafik2_geom_point) +
      theme_light()  + coord_flip() 
    
    
    
    
    
    
    
    
    get_theme = input$grafik2_theme
    
    if(get_theme == 'theme_bw')
    {
      gambar <- gambar + theme_bw()
    }
    
    if(get_theme == 'theme_linedraw')
    {
      gambar <- gambar + theme_linedraw()
    }
    if(get_theme == 'theme_light')
    {
      gambar <- gambar + theme_light()
    }
    if(get_theme == 'theme_dark')
    {
      gambar <- gambar + theme_dark()
    }
    if(get_theme == 'theme_minimal')
    {
      gambar <- gambar + theme_minimal()
    }
    if(get_theme == 'theme_classic')
    {
      gambar <- gambar + theme_classic()
    }
    if(get_theme == 'theme_void')
    {
      gambar <- gambar + theme_void()
    }
    if(get_theme == 'theme_tufte')
    {
      gambar <- gambar + theme_tufte()
    }
    if(get_theme == 'theme_economist')
    {
      gambar <- gambar + theme_economist()
    }
    if(get_theme == 'theme_solarized')
    {
      gambar <- gambar + theme_solarized()
    }
    if(get_theme == 'theme_solarized_2')
    {
      gambar <- gambar + theme_solarized(light = FALSE)
    }
    if(get_theme == 'theme_stata')
    {
      gambar <- gambar + theme_stata()
    }
    if(get_theme == 'theme_excel')
    {
      gambar <- gambar + theme_excel()
    }
    if(get_theme == 'theme_igray')
    {
      gambar <- gambar + theme_igray()
    }
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    gambar <- gambar +  geom_text(aes(label=paste0(Frequency) ), 
                                  color= input$grafik2_color_text,
                                  position = position_dodge(0), 
                                  vjust = input$grafik2_vjust_text, 
                                  size = input$grafik2_size_text ) +
      theme(axis.text.x=element_text( color="black", size = input$grafik2_axis.text.x),
            axis.text.y=element_text( color="black", size = input$grafik2_axis.text.y),
            axis.title=element_text( color="black" , size = input$grafik2_axis.title),
            legend.text=element_text(size = input$grafik2_legend.text, colour="blue") )+
      theme(plot.title = element_text(hjust=0.5,  size = input$grafik2_element_text, color = "black"     )) +
      theme(strip.background = element_rect( fill = "grey"  ), 
            strip.text.x = element_text(colour = "black"  , 
                                        size  = input$grafik2_strip.text.x),
            strip.text.y = element_text(colour = "black"  , 
                                        size  = input$grafik2_strip.text.y)) +
      ggtitle("Neutral Sentiment")
    
    
    
    
    
    
    #    grafik1_color_text <- input$grafik1_color_text
    #   grafik1_color_text <- as.character(grafik1_color_text)
    
    
    #  warna1 <- input$grafik1_warnabar1
    
    
    # warna2 <- input$grafik1_warnabar2
    #warna3 <- input$grafik1_warnabar3
    
    
    
    
    print(gambar)
    
    
    
    
    
    
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##############Wordcloud
  ##############Wordcloud
  ##############Wordcloud
  ##############Wordcloud
  
  
  
  output$grafik_wordcloud_sentiment_negatif_sampling_1 <- renderPlot({
    
    
    dataku <- readxl::read_xlsx("data_sampling_2026.xlsx")
    dataku <- as.data.frame(dataku)
    
    
    pilih_grup <- c(1)
    
    grup_lengkap <- dataku[,"group"]
    
    indeks <- grup_lengkap %in% pilih_grup
    indeks <- which(indeks == TRUE)
    
    data_dengan_grup_terpilih <- dataku[indeks,]
    
    
    ##############
    
    sentimen_negatif <- data_dengan_grup_terpilih[,c("Sentiment")]
    
    indeks <- which(sentimen_negatif == c("-"))
    
    data_sentimen_negatif <- data_dengan_grup_terpilih[c(indeks),]
    
  
    
    ###########
    
    library(dplyr)
    text_df <- data_frame(line = 1 : length(indeks) , text = data_sentimen_negatif[,c("content")])

    
    
    #########
    
    
    
    library(tidytext)
    tidy_books <- text_df %>%
      unnest_tokens(word, text)
   
    
    #########
    
    data(stop_words)
    
    tidy_books <- tidy_books %>% anti_join(stop_words)
    
    
    
    #########
    
    hapus_kata <- c("di", "tidak", "shopee", "yang", "saya", "bisa", "nya", "ada", "tapi", "sekarang",
                    "dan", "ke", "makin", "mau", "dari", "kalau", "kenapa", "sangat", "udah", "banget",
                    "gak", "padahal", "sama", "sampai", "yang", "yg", "ini", "gk","dengan", "tetapi", "5",
                    "sering", "untuk", "aja", "banyak", "dulu", "gak", "atau", "lagi", "sekali", "khusus", "jadi",
                    "ga", "baru", "klo", "2", "buat", "juga", "gratis", "sudah", "sdh", "bagus", "aku", "jangan", "hari",
                    "1", "itu", "pakai", "selalu", "3", "ya", "karena", "pas", "lebih", "pihak", "suka", "pake", "sesuai",
                    "jelas", "mana", "sih", "buka",
                    "nggak", "lain", "cuma", "sy", "bikin", "dalam", "belum", "tuh", "hati", "kalo", "kali", "kadang",
                    "kalian", "terlalu", "apa", "lah", "langsung", "shoppe")
    
    kata <- tidy_books[,"word"]
    
    kata <- unlist(kata)
    
    simpan_indeks <- vector(mode = "numeric")
    
    
    k = 0
    
    
    for(i in 1 : length(kata))
    {
      
      if(kata[i] %in% hapus_kata == FALSE)
      {
        
        k = k + 1
        simpan_indeks[k] = i
        
      }
      
      
    }
    
    
    
    ###########
    
    
    tidy_books2 <- tidy_books[c(simpan_indeks),]
    

    ############
    
    
    library(wordcloud)
  # p <- tidy_books2 %>%
   #   anti_join(stop_words) %>%
    #  count(word) %>%
     # with(wordcloud(word, n, max.words = input$max_words_sampling_1_sentiment_negatif, colors=brewer.pal(30, "Dark2")))
    
    
    
    rot.per <- read.csv(text=input$rot.per_sampling_1_sentiment_negatif, header = FALSE, sep="", na.strings=c("","NA","."))
    rot.per = unlist(rot.per)
    rot.per = as.numeric(rot.per)
    angka_rot.per <- rot.per
    
    
    
    p <- tidy_books2 %>%
      anti_join(stop_words) %>%
      count(word) %>%
      with(wordcloud(word, 
                     n, 
                     max.words = input$max_words_sampling_1_sentiment_negatif,
                     min.freq = input$min_freq_sampling_1_sentiment_negatif,
                     random.order=FALSE, rot.per = angka_rot.per,
                     
                     
                     colors=brewer.pal(input$n.brewer.pal_sampling_1_sentiment_negatif, 
                                       input$warna_wordcloud_sampling_1_sentiment_negatif  )
                     
                   #  with(wordcloud(word, n, max.words = 20, colors=brewer.pal(30, "Dark2")))
                     
                     ))
    
    
    
    
    

   
  # with(wordcloud(word, n, 
   #               max.words = input$max_words_sampling_1_sentiment_negatif,
    #              min.freq = input$min_freq_sampling_1_sentiment_negatif,           
     #             random.order=FALSE, rot.per = angka_rot.per,            
      #            colors=brewer.pal(input$n.brewer.pal_sampling_1_sentiment_negatif, 
       #                             input$warna_wordcloud_sampling_1_sentiment_negatif  )))
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
    print(p)
    
    
  })
  
  
  
  
  
  
  ###############
  ###############
  
  
  
  
  output$grafik_wordcloud_sentiment_negatif_sampling_2 <- renderPlot({
    
    
    dataku <- readxl::read_xlsx("data_sampling_2026.xlsx")
    dataku <- as.data.frame(dataku)
    
    
    pilih_grup <- c(2)
    
    grup_lengkap <- dataku[,"group"]
    
    indeks <- grup_lengkap %in% pilih_grup
    indeks <- which(indeks == TRUE)
    
    data_dengan_grup_terpilih <- dataku[indeks,]
    
    
    ##############
    
    sentimen_negatif <- data_dengan_grup_terpilih[,c("Sentiment")]
    
    indeks <- which(sentimen_negatif == c("-"))
    
    data_sentimen_negatif <- data_dengan_grup_terpilih[c(indeks),]
    
    
    
    ###########
    
    library(dplyr)
    text_df <- data_frame(line = 1 : length(indeks) , text = data_sentimen_negatif[,c("content")])
    
    
    
    #########
    
    
    
    library(tidytext)
    tidy_books <- text_df %>%
      unnest_tokens(word, text)
    
    
    #########
    
    data(stop_words)
    
    tidy_books <- tidy_books %>% anti_join(stop_words)
    
    
    
    #########
    
    hapus_kata <- c("di", "tidak", "shopee", "yang", "saya", "bisa", "nya", "ada", "tapi", "sekarang",
                    "dan", "ke", "makin", "mau", "dari", "kalau", "kenapa", "sangat", "udah", "banget",
                    "gak", "padahal", "sama", "sampai", "yang", "yg", "ini", "gk","dengan", "tetapi", "5",
                    "sering", "untuk", "aja", "banyak", "dulu", "gak", "atau", "lagi", "sekali", "khusus", "jadi",
                    "ga", "baru", "klo", "2", "buat", "juga", "gratis", "sudah", "sdh", "bagus", "aku", "jangan", "hari",
                    "1", "itu", "pakai", "selalu", "3", "ya", "karena", "pas", "lebih", "pihak", "suka", "pake", "sesuai",
                    "jelas", "mana", "sih", "buka",
                    "nggak", "lain", "cuma", "sy", "bikin", "dalam", "belum", "tuh", "hati", "kalo", "kali", "kadang",
                    "kalian", "terlalu", "apa", "lah", "langsung", "shoppe")
    
    kata <- tidy_books[,"word"]
    
    kata <- unlist(kata)
    
    simpan_indeks <- vector(mode = "numeric")
    
    
    k = 0
    
    
    for(i in 1 : length(kata))
    {
      
      if(kata[i] %in% hapus_kata == FALSE)
      {
        
        k = k + 1
        simpan_indeks[k] = i
        
      }
      
      
    }
    
    
    
    ###########
    
    
    tidy_books2 <- tidy_books[c(simpan_indeks),]
    
    
    ############
    
    
    
    rot.per <- read.csv(text=input$rot.per_sampling_1_sentiment_negatif, header = FALSE, sep="", na.strings=c("","NA","."))
    rot.per = unlist(rot.per)
    rot.per = as.numeric(rot.per)
    angka_rot.per <- rot.per
    
    
    
    p <- tidy_books2 %>%
      anti_join(stop_words) %>%
      count(word) %>%
      with(wordcloud(word, 
                     n, 
                     max.words = input$max_words_sampling_1_sentiment_negatif,
                     min.freq = input$min_freq_sampling_1_sentiment_negatif,
                     random.order=FALSE, rot.per = angka_rot.per,
                     
                     
                     colors=brewer.pal(input$n.brewer.pal_sampling_1_sentiment_negatif, 
                                       input$warna_wordcloud_sampling_1_sentiment_negatif  )
                     
                     #  with(wordcloud(word, n, max.words = 20, colors=brewer.pal(30, "Dark2")))
                     
      ))
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    print(p)
    
    
  })
  
  
  
  
  
  ###########
  
  
  
  
  
  
  output$grafik_wordcloud_sentiment_negatif_sampling_3 <- renderPlot({
    
    
    dataku <- readxl::read_xlsx("data_sampling_2026.xlsx")
    dataku <- as.data.frame(dataku)
    
    
    pilih_grup <- c(3)
    
    grup_lengkap <- dataku[,"group"]
    
    indeks <- grup_lengkap %in% pilih_grup
    indeks <- which(indeks == TRUE)
    
    data_dengan_grup_terpilih <- dataku[indeks,]
    
    
    ##############
    
    sentimen_negatif <- data_dengan_grup_terpilih[,c("Sentiment")]
    
    indeks <- which(sentimen_negatif == c("-"))
    
    data_sentimen_negatif <- data_dengan_grup_terpilih[c(indeks),]
    
    
    
    ###########
    
    library(dplyr)
    text_df <- data_frame(line = 1 : length(indeks) , text = data_sentimen_negatif[,c("content")])
    
    
    
    #########
    
    
    
    library(tidytext)
    tidy_books <- text_df %>%
      unnest_tokens(word, text)
    
    
    #########
    
    data(stop_words)
    
    tidy_books <- tidy_books %>% anti_join(stop_words)
    
    
    
    #########
    
    hapus_kata <- c("di", "tidak", "shopee", "yang", "saya", "bisa", "nya", "ada", "tapi", "sekarang",
                    "dan", "ke", "makin", "mau", "dari", "kalau", "kenapa", "sangat", "udah", "banget",
                    "gak", "padahal", "sama", "sampai", "yang", "yg", "ini", "gk","dengan", "tetapi", "5",
                    "sering", "untuk", "aja", "banyak", "dulu", "gak", "atau", "lagi", "sekali", "khusus", "jadi",
                    "ga", "baru", "klo", "2", "buat", "juga", "gratis", "sudah", "sdh", "bagus", "aku", "jangan", "hari",
                    "1", "itu", "pakai", "selalu", "3", "ya", "karena", "pas", "lebih", "pihak", "suka", "pake", "sesuai",
                    "jelas", "mana", "sih", "buka",
                    "nggak", "lain", "cuma", "sy", "bikin", "dalam", "belum", "tuh", "hati", "kalo", "kali", "kadang",
                    "kalian", "terlalu", "apa", "lah", "langsung", "shoppe")
    
    kata <- tidy_books[,"word"]
    
    kata <- unlist(kata)
    
    simpan_indeks <- vector(mode = "numeric")
    
    
    k = 0
    
    
    for(i in 1 : length(kata))
    {
      
      if(kata[i] %in% hapus_kata == FALSE)
      {
        
        k = k + 1
        simpan_indeks[k] = i
        
      }
      
      
    }
    
    
    
    ###########
    
    
    tidy_books2 <- tidy_books[c(simpan_indeks),]
    
    
    ############
    
    
    
    rot.per <- read.csv(text=input$rot.per_sampling_1_sentiment_negatif, header = FALSE, sep="", na.strings=c("","NA","."))
    rot.per = unlist(rot.per)
    rot.per = as.numeric(rot.per)
    angka_rot.per <- rot.per
    
    
    
    p <- tidy_books2 %>%
      anti_join(stop_words) %>%
      count(word) %>%
      with(wordcloud(word, 
                     n, 
                     max.words = input$max_words_sampling_1_sentiment_negatif,
                     min.freq = input$min_freq_sampling_1_sentiment_negatif,
                     random.order=FALSE, rot.per = angka_rot.per,
                     
                     
                     colors=brewer.pal(input$n.brewer.pal_sampling_1_sentiment_negatif, 
                                       input$warna_wordcloud_sampling_1_sentiment_negatif  )
                     
                     #  with(wordcloud(word, n, max.words = 20, colors=brewer.pal(30, "Dark2")))
                     
      ))
    
    
    
    
    print(p)
    
    
  })
  
  
  
  
  
  ###############
  
  
  
  
  
  
  
  
  
  
  output$grafik_wordcloud_sentiment_negatif_sampling_4 <- renderPlot({
    
    
    dataku <- readxl::read_xlsx("data_sampling_2026.xlsx")
    dataku <- as.data.frame(dataku)
    
    
    pilih_grup <- c(4)
    
    grup_lengkap <- dataku[,"group"]
    
    indeks <- grup_lengkap %in% pilih_grup
    indeks <- which(indeks == TRUE)
    
    data_dengan_grup_terpilih <- dataku[indeks,]
    
    
    ##############
    
    sentimen_negatif <- data_dengan_grup_terpilih[,c("Sentiment")]
    
    indeks <- which(sentimen_negatif == c("-"))
    
    data_sentimen_negatif <- data_dengan_grup_terpilih[c(indeks),]
    
    
    
    ###########
    
    library(dplyr)
    text_df <- data_frame(line = 1 : length(indeks) , text = data_sentimen_negatif[,c("content")])
    
    
    
    #########
    
    
    
    library(tidytext)
    tidy_books <- text_df %>%
      unnest_tokens(word, text)
    
    
    #########
    
    data(stop_words)
    
    tidy_books <- tidy_books %>% anti_join(stop_words)
    
    
    
    #########
    
    hapus_kata <- c("di", "tidak", "shopee", "yang", "saya", "bisa", "nya", "ada", "tapi", "sekarang",
                    "dan", "ke", "makin", "mau", "dari", "kalau", "kenapa", "sangat", "udah", "banget",
                    "gak", "padahal", "sama", "sampai", "yang", "yg", "ini", "gk","dengan", "tetapi", "5",
                    "sering", "untuk", "aja", "banyak", "dulu", "gak", "atau", "lagi", "sekali", "khusus", "jadi",
                    "ga", "baru", "klo", "2", "buat", "juga", "gratis", "sudah", "sdh", "bagus", "aku", "jangan", "hari",
                    "1", "itu", "pakai", "selalu", "3", "ya", "karena", "pas", "lebih", "pihak", "suka", "pake", "sesuai",
                    "jelas", "mana", "sih", "buka",
                    "nggak", "lain", "cuma", "sy", "bikin", "dalam", "belum", "tuh", "hati", "kalo", "kali", "kadang",
                    "kalian", "terlalu", "apa", "lah", "langsung", "shoppe")
    
    kata <- tidy_books[,"word"]
    
    kata <- unlist(kata)
    
    simpan_indeks <- vector(mode = "numeric")
    
    
    k = 0
    
    
    for(i in 1 : length(kata))
    {
      
      if(kata[i] %in% hapus_kata == FALSE)
      {
        
        k = k + 1
        simpan_indeks[k] = i
        
      }
      
      
    }
    
    
    
    ###########
    
    
    tidy_books2 <- tidy_books[c(simpan_indeks),]
    
    
    ############
    
    
    
    rot.per <- read.csv(text=input$rot.per_sampling_1_sentiment_negatif, header = FALSE, sep="", na.strings=c("","NA","."))
    rot.per = unlist(rot.per)
    rot.per = as.numeric(rot.per)
    angka_rot.per <- rot.per
    
    
    
    p <- tidy_books2 %>%
      anti_join(stop_words) %>%
      count(word) %>%
      with(wordcloud(word, 
                     n, 
                     max.words = input$max_words_sampling_1_sentiment_negatif,
                     min.freq = input$min_freq_sampling_1_sentiment_negatif,
                     random.order=FALSE, rot.per = angka_rot.per,
                     
                     
                     colors=brewer.pal(input$n.brewer.pal_sampling_1_sentiment_negatif, 
                                       input$warna_wordcloud_sampling_1_sentiment_negatif  )
                     
                     #  with(wordcloud(word, n, max.words = 20, colors=brewer.pal(30, "Dark2")))
                     
      ))
    
    
    
    
    print(p)
    
    
  })
  
  
  
  
  
  ###############
  
  
  
  
  
  
  output$grafik_wordcloud_sentiment_negatif_sampling_5 <- renderPlot({
    
    
    dataku <- readxl::read_xlsx("data_sampling_2026.xlsx")
    dataku <- as.data.frame(dataku)
    
    
    pilih_grup <- c(5)
    
    grup_lengkap <- dataku[,"group"]
    
    indeks <- grup_lengkap %in% pilih_grup
    indeks <- which(indeks == TRUE)
    
    data_dengan_grup_terpilih <- dataku[indeks,]
    
    
    ##############
    
    sentimen_negatif <- data_dengan_grup_terpilih[,c("Sentiment")]
    
    indeks <- which(sentimen_negatif == c("-"))
    
    data_sentimen_negatif <- data_dengan_grup_terpilih[c(indeks),]
    
    
    
    ###########
    
    library(dplyr)
    text_df <- data_frame(line = 1 : length(indeks) , text = data_sentimen_negatif[,c("content")])
    
    
    
    #########
    
    
    
    library(tidytext)
    tidy_books <- text_df %>%
      unnest_tokens(word, text)
    
    
    #########
    
    data(stop_words)
    
    tidy_books <- tidy_books %>% anti_join(stop_words)
    
    
    
    #########
    
    hapus_kata <- c("di", "tidak", "shopee", "yang", "saya", "bisa", "nya", "ada", "tapi", "sekarang",
                    "dan", "ke", "makin", "mau", "dari", "kalau", "kenapa", "sangat", "udah", "banget",
                    "gak", "padahal", "sama", "sampai", "yang", "yg", "ini", "gk","dengan", "tetapi", "5",
                    "sering", "untuk", "aja", "banyak", "dulu", "gak", "atau", "lagi", "sekali", "khusus", "jadi",
                    "ga", "baru", "klo", "2", "buat", "juga", "gratis", "sudah", "sdh", "bagus", "aku", "jangan", "hari",
                    "1", "itu", "pakai", "selalu", "3", "ya", "karena", "pas", "lebih", "pihak", "suka", "pake", "sesuai",
                    "jelas", "mana", "sih", "buka",
                    "nggak", "lain", "cuma", "sy", "bikin", "dalam", "belum", "tuh", "hati", "kalo", "kali", "kadang",
                    "kalian", "terlalu", "apa", "lah", "langsung", "shoppe")
    
    kata <- tidy_books[,"word"]
    
    kata <- unlist(kata)
    
    simpan_indeks <- vector(mode = "numeric")
    
    
    k = 0
    
    
    for(i in 1 : length(kata))
    {
      
      if(kata[i] %in% hapus_kata == FALSE)
      {
        
        k = k + 1
        simpan_indeks[k] = i
        
      }
      
      
    }
    
    
    
    ###########
    
    
    tidy_books2 <- tidy_books[c(simpan_indeks),]
    
    
    ############
    
    
    rot.per <- read.csv(text=input$rot.per_sampling_1_sentiment_negatif, header = FALSE, sep="", na.strings=c("","NA","."))
    rot.per = unlist(rot.per)
    rot.per = as.numeric(rot.per)
    angka_rot.per <- rot.per
    
    
    
    p <- tidy_books2 %>%
      anti_join(stop_words) %>%
      count(word) %>%
      with(wordcloud(word, 
                     n, 
                     max.words = input$max_words_sampling_1_sentiment_negatif,
                     min.freq = input$min_freq_sampling_1_sentiment_negatif,
                     random.order=FALSE, rot.per = angka_rot.per,
                     
                     
                     colors=brewer.pal(input$n.brewer.pal_sampling_1_sentiment_negatif, 
                                       input$warna_wordcloud_sampling_1_sentiment_negatif  )
                     
                     #  with(wordcloud(word, n, max.words = 20, colors=brewer.pal(30, "Dark2")))
                     
      ))
    
    
    
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  #############
  
  
  
  
  
  
  output$grafik_wordcloud_sentiment_negatif_sampling_6 <- renderPlot({
    
    
    dataku <- readxl::read_xlsx("data_sampling_2026.xlsx")
    dataku <- as.data.frame(dataku)
    
    
    pilih_grup <- c(6)
    
    grup_lengkap <- dataku[,"group"]
    
    indeks <- grup_lengkap %in% pilih_grup
    indeks <- which(indeks == TRUE)
    
    data_dengan_grup_terpilih <- dataku[indeks,]
    
    
    ##############
    
    sentimen_negatif <- data_dengan_grup_terpilih[,c("Sentiment")]
    
    indeks <- which(sentimen_negatif == c("-"))
    
    data_sentimen_negatif <- data_dengan_grup_terpilih[c(indeks),]
    
    
    
    ###########
    
    library(dplyr)
    text_df <- data_frame(line = 1 : length(indeks) , text = data_sentimen_negatif[,c("content")])
    
    
    
    #########
    
    
    
    library(tidytext)
    tidy_books <- text_df %>%
      unnest_tokens(word, text)
    
    
    #########
    
    data(stop_words)
    
    tidy_books <- tidy_books %>% anti_join(stop_words)
    
    
    
    #########
    
    hapus_kata <- c("di", "tidak", "shopee", "yang", "saya", "bisa", "nya", "ada", "tapi", "sekarang",
                    "dan", "ke", "makin", "mau", "dari", "kalau", "kenapa", "sangat", "udah", "banget",
                    "gak", "padahal", "sama", "sampai", "yang", "yg", "ini", "gk","dengan", "tetapi", "5",
                    "sering", "untuk", "aja", "banyak", "dulu", "gak", "atau", "lagi", "sekali", "khusus", "jadi",
                    "ga", "baru", "klo", "2", "buat", "juga", "gratis", "sudah", "sdh", "bagus", "aku", "jangan", "hari",
                    "1", "itu", "pakai", "selalu", "3", "ya", "karena", "pas", "lebih", "pihak", "suka", "pake", "sesuai",
                    "jelas", "mana", "sih", "buka",
                    "nggak", "lain", "cuma", "sy", "bikin", "dalam", "belum", "tuh", "hati", "kalo", "kali", "kadang",
                    "kalian", "terlalu", "apa", "lah", "langsung", "shoppe")
    
    kata <- tidy_books[,"word"]
    
    kata <- unlist(kata)
    
    simpan_indeks <- vector(mode = "numeric")
    
    
    k = 0
    
    
    for(i in 1 : length(kata))
    {
      
      if(kata[i] %in% hapus_kata == FALSE)
      {
        
        k = k + 1
        simpan_indeks[k] = i
        
      }
      
      
    }
    
    
    
    ###########
    
    
    tidy_books2 <- tidy_books[c(simpan_indeks),]
    
    
    ############
    
    
    
    rot.per <- read.csv(text=input$rot.per_sampling_1_sentiment_negatif, header = FALSE, sep="", na.strings=c("","NA","."))
    rot.per = unlist(rot.per)
    rot.per = as.numeric(rot.per)
    angka_rot.per <- rot.per
    
    
    
    p <- tidy_books2 %>%
      anti_join(stop_words) %>%
      count(word) %>%
      with(wordcloud(word, 
                     n, 
                     max.words = input$max_words_sampling_1_sentiment_negatif,
                     min.freq = input$min_freq_sampling_1_sentiment_negatif,
                     random.order=FALSE, rot.per = angka_rot.per,
                     
                     
                     colors=brewer.pal(input$n.brewer.pal_sampling_1_sentiment_negatif, 
                                       input$warna_wordcloud_sampling_1_sentiment_negatif  )
                     
                     #  with(wordcloud(word, n, max.words = 20, colors=brewer.pal(30, "Dark2")))
                     
      ))
    
    
    
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  #####################
  
  
  
  
  
  
  output$grafik_wordcloud_sentiment_negatif_sampling_7 <- renderPlot({
    
    
    dataku <- readxl::read_xlsx("data_sampling_2026.xlsx")
    dataku <- as.data.frame(dataku)
    
    
    pilih_grup <- c(7)
    
    grup_lengkap <- dataku[,"group"]
    
    indeks <- grup_lengkap %in% pilih_grup
    indeks <- which(indeks == TRUE)
    
    data_dengan_grup_terpilih <- dataku[indeks,]
    
    
    ##############
    
    sentimen_negatif <- data_dengan_grup_terpilih[,c("Sentiment")]
    
    indeks <- which(sentimen_negatif == c("-"))
    
    data_sentimen_negatif <- data_dengan_grup_terpilih[c(indeks),]
    
    
    
    ###########
    
    library(dplyr)
    text_df <- data_frame(line = 1 : length(indeks) , text = data_sentimen_negatif[,c("content")])
    
    
    
    #########
    
    
    
    library(tidytext)
    tidy_books <- text_df %>%
      unnest_tokens(word, text)
    
    
    #########
    
    data(stop_words)
    
    tidy_books <- tidy_books %>% anti_join(stop_words)
    
    
    
    #########
    
    hapus_kata <- c("di", "tidak", "shopee", "yang", "saya", "bisa", "nya", "ada", "tapi", "sekarang",
                    "dan", "ke", "makin", "mau", "dari", "kalau", "kenapa", "sangat", "udah", "banget",
                    "gak", "padahal", "sama", "sampai", "yang", "yg", "ini", "gk","dengan", "tetapi", "5",
                    "sering", "untuk", "aja", "banyak", "dulu", "gak", "atau", "lagi", "sekali", "khusus", "jadi",
                    "ga", "baru", "klo", "2", "buat", "juga", "gratis", "sudah", "sdh", "bagus", "aku", "jangan", "hari",
                    "1", "itu", "pakai", "selalu", "3", "ya", "karena", "pas", "lebih", "pihak", "suka", "pake", "sesuai",
                    "jelas", "mana", "sih", "buka",
                    "nggak", "lain", "cuma", "sy", "bikin", "dalam", "belum", "tuh", "hati", "kalo", "kali", "kadang",
                    "kalian", "terlalu", "apa", "lah", "langsung", "shoppe")
    
    kata <- tidy_books[,"word"]
    
    kata <- unlist(kata)
    
    simpan_indeks <- vector(mode = "numeric")
    
    
    k = 0
    
    
    for(i in 1 : length(kata))
    {
      
      if(kata[i] %in% hapus_kata == FALSE)
      {
        
        k = k + 1
        simpan_indeks[k] = i
        
      }
      
      
    }
    
    
    
    ###########
    
    
    tidy_books2 <- tidy_books[c(simpan_indeks),]
    
    
    ############
    
    
    
    rot.per <- read.csv(text=input$rot.per_sampling_1_sentiment_negatif, header = FALSE, sep="", na.strings=c("","NA","."))
    rot.per = unlist(rot.per)
    rot.per = as.numeric(rot.per)
    angka_rot.per <- rot.per
    
    
    
    p <- tidy_books2 %>%
      anti_join(stop_words) %>%
      count(word) %>%
      with(wordcloud(word, 
                     n, 
                     max.words = input$max_words_sampling_1_sentiment_negatif,
                     min.freq = input$min_freq_sampling_1_sentiment_negatif,
                     random.order=FALSE, rot.per = angka_rot.per,
                     
                     
                     colors=brewer.pal(input$n.brewer.pal_sampling_1_sentiment_negatif, 
                                       input$warna_wordcloud_sampling_1_sentiment_negatif  )
                     
                     #  with(wordcloud(word, n, max.words = 20, colors=brewer.pal(30, "Dark2")))
                     
      ))
    
    
    
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  ###############
  
  
  
  
  
  output$grafik_wordcloud_sentiment_negatif_sampling_8 <- renderPlot({
    
    
    dataku <- readxl::read_xlsx("data_sampling_2026.xlsx")
    dataku <- as.data.frame(dataku)
    
    
    pilih_grup <- c(8)
    
    grup_lengkap <- dataku[,"group"]
    
    indeks <- grup_lengkap %in% pilih_grup
    indeks <- which(indeks == TRUE)
    
    data_dengan_grup_terpilih <- dataku[indeks,]
    
    
    ##############
    
    sentimen_negatif <- data_dengan_grup_terpilih[,c("Sentiment")]
    
    indeks <- which(sentimen_negatif == c("-"))
    
    data_sentimen_negatif <- data_dengan_grup_terpilih[c(indeks),]
    
    
    
    ###########
    
    library(dplyr)
    text_df <- data_frame(line = 1 : length(indeks) , text = data_sentimen_negatif[,c("content")])
    
    
    
    #########
    
    
    
    library(tidytext)
    tidy_books <- text_df %>%
      unnest_tokens(word, text)
    
    
    #########
    
    data(stop_words)
    
    tidy_books <- tidy_books %>% anti_join(stop_words)
    
    
    
    #########
    
    hapus_kata <- c("di", "tidak", "shopee", "yang", "saya", "bisa", "nya", "ada", "tapi", "sekarang",
                    "dan", "ke", "makin", "mau", "dari", "kalau", "kenapa", "sangat", "udah", "banget",
                    "gak", "padahal", "sama", "sampai", "yang", "yg", "ini", "gk","dengan", "tetapi", "5",
                    "sering", "untuk", "aja", "banyak", "dulu", "gak", "atau", "lagi", "sekali", "khusus", "jadi",
                    "ga", "baru", "klo", "2", "buat", "juga", "gratis", "sudah", "sdh", "bagus", "aku", "jangan", "hari",
                    "1", "itu", "pakai", "selalu", "3", "ya", "karena", "pas", "lebih", "pihak", "suka", "pake", "sesuai",
                    "jelas", "mana", "sih", "buka",
                    "nggak", "lain", "cuma", "sy", "bikin", "dalam", "belum", "tuh", "hati", "kalo", "kali", "kadang",
                    "kalian", "terlalu", "apa", "lah", "langsung", "shoppe")
    
    kata <- tidy_books[,"word"]
    
    kata <- unlist(kata)
    
    simpan_indeks <- vector(mode = "numeric")
    
    
    k = 0
    
    
    for(i in 1 : length(kata))
    {
      
      if(kata[i] %in% hapus_kata == FALSE)
      {
        
        k = k + 1
        simpan_indeks[k] = i
        
      }
      
      
    }
    
    
    
    ###########
    
    
    tidy_books2 <- tidy_books[c(simpan_indeks),]
    
    
    ############
    
    
    
    rot.per <- read.csv(text=input$rot.per_sampling_1_sentiment_negatif, header = FALSE, sep="", na.strings=c("","NA","."))
    rot.per = unlist(rot.per)
    rot.per = as.numeric(rot.per)
    angka_rot.per <- rot.per
    
    
    
    p <- tidy_books2 %>%
      anti_join(stop_words) %>%
      count(word) %>%
      with(wordcloud(word, 
                     n, 
                     max.words = input$max_words_sampling_1_sentiment_negatif,
                     min.freq = input$min_freq_sampling_1_sentiment_negatif,
                     random.order=FALSE, rot.per = angka_rot.per,
                     
                     
                     colors=brewer.pal(input$n.brewer.pal_sampling_1_sentiment_negatif, 
                                       input$warna_wordcloud_sampling_1_sentiment_negatif  )
                     
                     #  with(wordcloud(word, n, max.words = 20, colors=brewer.pal(30, "Dark2")))
                     
      ))
    
    
    
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  ##############
  
  
  
  
  
  
  output$grafik_wordcloud_sentiment_negatif_sampling_9 <- renderPlot({
    
    
    dataku <- readxl::read_xlsx("data_sampling_2026.xlsx")
    dataku <- as.data.frame(dataku)
    
    
    pilih_grup <- c(9)
    
    grup_lengkap <- dataku[,"group"]
    
    indeks <- grup_lengkap %in% pilih_grup
    indeks <- which(indeks == TRUE)
    
    data_dengan_grup_terpilih <- dataku[indeks,]
    
    
    ##############
    
    sentimen_negatif <- data_dengan_grup_terpilih[,c("Sentiment")]
    
    indeks <- which(sentimen_negatif == c("-"))
    
    data_sentimen_negatif <- data_dengan_grup_terpilih[c(indeks),]
    
    
    
    ###########
    
    library(dplyr)
    text_df <- data_frame(line = 1 : length(indeks) , text = data_sentimen_negatif[,c("content")])
    
    
    
    #########
    
    
    
    library(tidytext)
    tidy_books <- text_df %>%
      unnest_tokens(word, text)
    
    
    #########
    
    data(stop_words)
    
    tidy_books <- tidy_books %>% anti_join(stop_words)
    
    
    
    #########
    
    hapus_kata <- c("di", "tidak", "shopee", "yang", "saya", "bisa", "nya", "ada", "tapi", "sekarang",
                    "dan", "ke", "makin", "mau", "dari", "kalau", "kenapa", "sangat", "udah", "banget",
                    "gak", "padahal", "sama", "sampai", "yang", "yg", "ini", "gk","dengan", "tetapi", "5",
                    "sering", "untuk", "aja", "banyak", "dulu", "gak", "atau", "lagi", "sekali", "khusus", "jadi",
                    "ga", "baru", "klo", "2", "buat", "juga", "gratis", "sudah", "sdh", "bagus", "aku", "jangan", "hari",
                    "1", "itu", "pakai", "selalu", "3", "ya", "karena", "pas", "lebih", "pihak", "suka", "pake", "sesuai",
                    "jelas", "mana", "sih", "buka",
                    "nggak", "lain", "cuma", "sy", "bikin", "dalam", "belum", "tuh", "hati", "kalo", "kali", "kadang",
                    "kalian", "terlalu", "apa", "lah", "langsung", "shoppe")
    
    kata <- tidy_books[,"word"]
    
    kata <- unlist(kata)
    
    simpan_indeks <- vector(mode = "numeric")
    
    
    k = 0
    
    
    for(i in 1 : length(kata))
    {
      
      if(kata[i] %in% hapus_kata == FALSE)
      {
        
        k = k + 1
        simpan_indeks[k] = i
        
      }
      
      
    }
    
    
    
    ###########
    
    
    tidy_books2 <- tidy_books[c(simpan_indeks),]
    
    
    ############
    
    
    
    rot.per <- read.csv(text=input$rot.per_sampling_1_sentiment_negatif, header = FALSE, sep="", na.strings=c("","NA","."))
    rot.per = unlist(rot.per)
    rot.per = as.numeric(rot.per)
    angka_rot.per <- rot.per
    
    
    
    p <- tidy_books2 %>%
      anti_join(stop_words) %>%
      count(word) %>%
      with(wordcloud(word, 
                     n, 
                     max.words = input$max_words_sampling_1_sentiment_negatif,
                     min.freq = input$min_freq_sampling_1_sentiment_negatif,
                     random.order=FALSE, rot.per = angka_rot.per,
                     
                     
                     colors=brewer.pal(input$n.brewer.pal_sampling_1_sentiment_negatif, 
                                       input$warna_wordcloud_sampling_1_sentiment_negatif  )
                     
                     #  with(wordcloud(word, n, max.words = 20, colors=brewer.pal(30, "Dark2")))
                     
      ))
    
    
    
    
    print(p)
    
    
  })
  
  
  
  
  
  ####################
  
  
  
  
  
  output$grafik_wordcloud_sentiment_negatif_sampling_10 <- renderPlot({
    
    
    dataku <- readxl::read_xlsx("data_sampling_2026.xlsx")
    dataku <- as.data.frame(dataku)
    
    
    pilih_grup <- c(10)
    
    grup_lengkap <- dataku[,"group"]
    
    indeks <- grup_lengkap %in% pilih_grup
    indeks <- which(indeks == TRUE)
    
    data_dengan_grup_terpilih <- dataku[indeks,]
    
    
    ##############
    
    sentimen_negatif <- data_dengan_grup_terpilih[,c("Sentiment")]
    
    indeks <- which(sentimen_negatif == c("-"))
    
    data_sentimen_negatif <- data_dengan_grup_terpilih[c(indeks),]
    
    
    
    ###########
    
    library(dplyr)
    text_df <- data_frame(line = 1 : length(indeks) , text = data_sentimen_negatif[,c("content")])
    
    
    
    #########
    
    
    
    library(tidytext)
    tidy_books <- text_df %>%
      unnest_tokens(word, text)
    
    
    #########
    
    data(stop_words)
    
    tidy_books <- tidy_books %>% anti_join(stop_words)
    
    
    
    #########
    
    hapus_kata <- c("di", "tidak", "shopee", "yang", "saya", "bisa", "nya", "ada", "tapi", "sekarang",
                    "dan", "ke", "makin", "mau", "dari", "kalau", "kenapa", "sangat", "udah", "banget",
                    "gak", "padahal", "sama", "sampai", "yang", "yg", "ini", "gk","dengan", "tetapi", "5",
                    "sering", "untuk", "aja", "banyak", "dulu", "gak", "atau", "lagi", "sekali", "khusus", "jadi",
                    "ga", "baru", "klo", "2", "buat", "juga", "gratis", "sudah", "sdh", "bagus", "aku", "jangan", "hari",
                    "1", "itu", "pakai", "selalu", "3", "ya", "karena", "pas", "lebih", "pihak", "suka", "pake", "sesuai",
                    "jelas", "mana", "sih", "buka",
                    "nggak", "lain", "cuma", "sy", "bikin", "dalam", "belum", "tuh", "hati", "kalo", "kali", "kadang",
                    "kalian", "terlalu", "apa", "lah", "langsung", "shoppe")
    
    kata <- tidy_books[,"word"]
    
    kata <- unlist(kata)
    
    simpan_indeks <- vector(mode = "numeric")
    
    
    k = 0
    
    
    for(i in 1 : length(kata))
    {
      
      if(kata[i] %in% hapus_kata == FALSE)
      {
        
        k = k + 1
        simpan_indeks[k] = i
        
      }
      
      
    }
    
    
    
    ###########
    
    
    tidy_books2 <- tidy_books[c(simpan_indeks),]
    
    
    ############
    
    
    
    rot.per <- read.csv(text=input$rot.per_sampling_1_sentiment_negatif, header = FALSE, sep="", na.strings=c("","NA","."))
    rot.per = unlist(rot.per)
    rot.per = as.numeric(rot.per)
    angka_rot.per <- rot.per
    
    
    
    p <- tidy_books2 %>%
      anti_join(stop_words) %>%
      count(word) %>%
      with(wordcloud(word, 
                     n, 
                     max.words = input$max_words_sampling_1_sentiment_negatif,
                     min.freq = input$min_freq_sampling_1_sentiment_negatif,
                     random.order=FALSE, rot.per = angka_rot.per,
                     
                     
                     colors=brewer.pal(input$n.brewer.pal_sampling_1_sentiment_negatif, 
                                       input$warna_wordcloud_sampling_1_sentiment_negatif  )
                     
                     #  with(wordcloud(word, n, max.words = 20, colors=brewer.pal(30, "Dark2")))
                     
      ))
    
    
    
    
    print(p)
    
    
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















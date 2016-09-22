library("ggplot2")
#library("stringr")
#library("repmis")
library("ggthemes")
#library("quantmod")
#library("foreach")
library("scales")
library("shiny")
library("leaflet")
library("plotly")


shinyUI(
  navbarPage("OIS Analysis",
             tabPanel("Ton Time-Series",
               sidebarLayout(
                 sidebarPanel(width = 3,
                   dateInput("startDate", label = h5("Start Date:"), value = "2016-01-01"),
                   dateInput("endDate", label = h5("End Date:"), value = "2017-01-01"),
                   selectizeInput('graphs', label = h5("Graph(s):"), choices = list(
                     Sales = c(`Joist Sold` = 'Joist Sold', `Deck Sold` = 'Deck Sold',`Joist Quoted` = 'Joist Quoted',`Deck Quoted` = 'Deck Quoted'),
                     Engineering = c(`Joist Released` = 'Joist Released', `Deck Released` = 'Deck Released'),
                     Shipping = c(`Joist Shipped` = 'Joist Shipped')
                   ), multiple = TRUE),
                   uiOutput("estimators1"),
                   uiOutput("designers1"),
                   selectInput("interval", label = h5("Interval:"), choices=list(
                     Interval = c(`DAILY` = 'daily', `WEEKLY` = 'weekly',`MONTHLY` = 'monthly', `QUARTERLY` = 'quarterly', `YEARLY` = 'yearly')))
                 ),
                 
                 mainPanel(width = 8,
                   plotlyOutput("myPlot"),
                   tags$br(),
                   tags$div(HTML("<HR>")),
                   tags$div(HTML("<HR>")),
                   tags$br(),
                   plotlyOutput("myBarPlot")
                 )
               )
             ),
             tabPanel("Map",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                          selectInput("whatToMap", label = h5("Map:"), choices=list(
                            whatToMap = c(`Joist Sold` = 'joistSold', `Joist Quoted` = 'joistQuoted'))),
                          uiOutput("estimators2"),
                          dateInput("startDate_Map", label = h5("Start Date:"), value = "2016-01-01"),
                          dateInput("endDate_Map", label = h5("End Date:"), value = "2017-01-01")
                        ),
                      
                      mainPanel(width = 9,
                        leafletOutput("map", height = "600pt")
                      )
                      )
             ),
             tabPanel("Data",
                      navbarPage("Data",
                                tabPanel("Joist Sold",
                                         dataTableOutput('joistSoldTable'),tags$head(tags$style("tfoot {display: table-header-group;}"))
                                ),
                                tabPanel("Joist Quoted",
                                         dataTableOutput('joistQuotedTable'),tags$head(tags$style("tfoot {display: table-header-group;}"))
                                ),
                                tabPanel("Deck Sold",
                                         dataTableOutput('deckSoldTable'),tags$head(tags$style("tfoot {display: table-header-group;}"))
                                ),
                                tabPanel("Deck Quoted",
                                         dataTableOutput('deckQuotedTable'),tags$head(tags$style("tfoot {display: table-header-group;}"))
                                ),
                                tabPanel("Joist Released",
                                         dataTableOutput('joistReleasedTable'),tags$head(tags$style("tfoot {display: table-header-group;}"))
                                ),
                                tabPanel("Joist Shipped",
                                         dataTableOutput('joistShippedTable'),tags$head(tags$style("tfoot {display: table-header-group;}"))
                                )
                                
                      )
             )
  )
)
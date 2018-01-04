
library(shiny)
library(shinydashboard)
library(xts)
library(ggplot2)
library(stringr)
library(lubridate)
library(dygraphs)
library(plotly)
library(reshape2)
library(DT)
library(shinythemes)

#load("henry data AGK added.RData")
load("Comparable firm data March 2016.RData")



shinyUI(bootstrapPage(theme = shinytheme("cosmo"),
                      
tags$head(includeScript("google-analytics2.js")),

dashboardPage(
  skin = "purple",
  
  dashboardHeader(title = "Asset beta master 2.0",
                  titleWidth = 300),
  
  dashboardSidebar(
  #   tags$head(tags$style(HTML('
  #                             .sidebar {
  #                             background-color:black;
  #                             }
  #                             '))),
    
    width = 250,
    
    a(style="color:#99ff33; padding-left:5px; font-size:15px", 
      href="https://au.linkedin.com/in/yanjun-liu-a5646057",
      onclick="ga('send', 'event', 'click', 'link', 'linkedin', 1)",
      "Yanjun Liu (linkedin)"),
  
    br(),
    # menuItem("Individual beta", icon = icon("search")),
    # menuItem("Cross firm beta", icon = icon("bar-chart")),
    # menuItem("Categorical analysis", icon = icon("pie-chart")),
    # menuItem("Dynamic analysis", icon = icon("line-chart")),
    # menuItem("Portfolio analysis", icon = icon("area-chart")), 
    
    selectInput("mkt", "Market", names(index), selected="AU"),
    uiOutput("var2"),
    dateInput("sd", "Start date", value=as.Date("1990-01-01")), #value=head(time(index),1)
    dateInput("ed", "End date", value=as.Date("2016-03-01")),  #value=tail(time(index),1)
    selectInput("freq", "Frequency", c("days", "weeks", "months",
                                       "quarters","years"), selected="weeks"),
    numericInput("cb", "Count forward days", value = 0),
    uiOutput("var"),
    checkboxInput("fs", "Minimal 10 obs", TRUE)
    
    # withTags({
    #   div(align="center",
    #       a(href='http://www.ceg-global.com/', 
    #         onclick="ga('send', 'event', 'click', 'link', 'CEG', 1)",
    #         img(src='logo.png',height='80',width='180'))
    #   )
    # })
  ),
    
  dashboardBody(
    tags$head(tags$style(HTML('
      .main-header .logo {
                              font-family: "Lobster", cursive;
                              font-weight: bold;
                              font-size: 26px;
                              front-color: #ad1d28;
                              }
                              '))),
      fluidRow(
        box(tabsetPanel(
          tabPanel("Individual Firm",icon = icon("search"),
            box(title = "2D plot", plotlyOutput("scat", height = 500), width = 6),
            #box(title = "3D plot (click and drag to see)", webGLOutput("myWebGL", height = 550), width = 6),
            box(title = "3D plot",plotlyOutput("plotly3d", height = 500), width = 6),
            box(title = "Bar plot", plotlyOutput("den"), collapsible = T, width = 12),
            box(downloadButton('download.re', 'Download Return series'), width = 12),
            box(title = "Return data", dataTableOutput("t.return"), width = 12),
            box(title = "Mkt cap", dataTableOutput("t.mcap"), width = 12)
            ),
          
          tabPanel("Market Return", icon = icon("pie-chart"), 
                   box(tabsetPanel(
                     tabPanel(title = "Price ($)",
                              box(dygraphOutput("p.series", height = 500), width = 12)
                              ),
                     tabPanel(title = "Return (%)",
                              box(dygraphOutput("p.return", height = 500), width = 12)
                              ),
                     tabPanel(title = "Gearing (%)",
                              box(dygraphOutput("p.gearing", height = 500), width = 12)
                     )
                   ), width = 12),
                   box(title = "Return distribution by stock (%)", plotlyOutput("p.rbs", height=500), width = 12),
                   box(title = "Series data", dataTableOutput("t.series"), width = 12)
          ),
          
          tabPanel("Cross Firm analysis", icon = icon("bar-chart"),
            box(title = "Equity beta", plotlyOutput("bvg2", height = 500), width = 6),
            box(title = "Asset beta", plotlyOutput("avg2", height = 500), width = 6),
            box(title = "Result table", 
                downloadButton('download.Result', 'Download Result'),
                br(),
                br(),
                dataTableOutput("t.result"), 
                width = 12)
          ),
          
          tabPanel("Dynamic analysis", icon = icon("line-chart"), 
            box(numericInput("rp","Rolling period (year)?",10), width = 3),
            box(numericInput("rg","Rolling gap (month)?",12), width = 3),
            box(selectInput("fbi","Filter by index?",c("All",names(index))), width = 3),
            box(selectInput("fbs","Filter by stock name?",c("All",names(stock1))), width = 3),
#            box(selectInput("fbt","Filter by type?",c("All",c("stock","portfolio"))), width = 2),
            
            box(title = "Rolling Equity beta, Asset beta and Gearing", 
                plotlyOutput("p.bar", height = 500), width = 12),
            
            box(downloadButton('download.rollingMatrix', 'Download Rolling Matrix'),
                br(),
                br(),
              tabsetPanel(
              tabPanel(title="Rolling beta", box(dataTableOutput("r.beta"), width = 12)),
              tabPanel(title="Rolling gearing", box(dataTableOutput("r.gearing"), width = 12)),
              tabPanel(title="Rolling abeta", box(dataTableOutput("r.abeta"), width = 12)),
              tabPanel(title="Rolling sample size", box(dataTableOutput("r.sample.size"), width = 12))
              ), width = 12)
          ),

          tabPanel("Portfolio analysis", icon = icon("area-chart"), 
               box(numericInput("wd","Wbeta window size?", 260), width = 3),
               tabBox(side='left', title = "Rolling Portfolio Chart", selected='Asset beta',
                      tabPanel("Equity beta",dygraphOutput("p.wbeta", height = 500)),
                      tabPanel("Gearing",dygraphOutput("p.wgearing", height = 500)),
                      tabPanel("Asset beta",dygraphOutput("p.wabeta", height = 500))
               , width = 12),
               box(downloadButton('download.rw', 'Download Rolling Result'), width = 3)
        )
      ), width = 12)
      
  )
  
))

))





library(shiny)
library(shiny)
library(dplyr)
library(tidyverse)
library(DT)
library(ggplot2)
library(lubridate)
library(plotly)
library(hrbrthemes)
library(highcharter)
library(RColorBrewer)
if(!require(fontawesome)) devtools::install_github("rstudio/fontawesome")
library(geojsonio)
library(readr)
library(leaflet)
library(shinythemes)
library(shinyWidgets)

backgroundpic <- "https://images.unsplash.com/photo-1609945648638-cefddce6e6d8?ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&ixlib=rb-1.2.1&auto=format&fit=crop&w=2532&q=80" 


shinyUI(
  fluidPage(
    navbarPage(theme = shinytheme("sandstone"), collapsible = TRUE,
               title= "",
               id="nav",
               windowTitle = "The NYC Department of Homeless Services during the COVID-19 Pandemic",
               header = tagList(
                 useShinydashboard()
               ),

               
               tabPanel('Restaurants by Covid Area', icon = icon("viruses"),
                        titlePanel("Restaurant Location"),
                        
                        leafletOutput("map", width="100%", height=700),
                        
                        absolutePanel(id = "choices", class = "panel panel-default",
                                      top = 160, left = 40, width = 240, fixed=FALSE,
                                      draggable = TRUE, height = 470,
                                      tags$h1("Where Do You Want To Eat?",
                                              align = "left", style = "font-size:20px"),
                                      numericInput(
                                        "zip",
                                        "Zip Code",
                                        value = 10027,
                                        min = 10001,
                                        max = 10229)
                                      )
    )
  )
)
)

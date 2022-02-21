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
library(purrr)

backgroundpic <- "https://images.unsplash.com/photo-1609945648638-cefddce6e6d8?ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&ixlib=rb-1.2.1&auto=format&fit=crop&w=2532&q=80" 


shinyUI(
  fluidPage(
    navbarPage(theme = shinytheme("sandstone"), collapsible = TRUE,
               title= "",
               id="nav",
               windowTitle = "NYC Restaurants",
               header = tagList(
                 useShinydashboard()
               ),

               
               tabPanel('Restaurants by Covid Area', icon = icon("viruses"),
                        titlePanel("Restaurants"),
                        
                         leafletOutput("map", width="100%", height=700)
    )
  )
)
)

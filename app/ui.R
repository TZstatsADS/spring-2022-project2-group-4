packages.used=c("shiny","dplyr","tidyverse","DT","ggplot2",
                "lubridate","plotly","hrbrthemes","highcharter",
                "RColorBrewer","geojsonio","readr","leaflet",
                "shinythemes","shinyWidgets","purrr")
# Check for packages that need to be installed.
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))
# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}

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
               header = tagList(useShinydashboard()),

               tabPanel('Restaurants and Covid Risk', icon = icon("viruses"),
                        titlePanel("Restaurants"),
                        p("Each circle on the map is a restaurant, colored by the COVID risk of the neighborhood it's in. The COVID risk is calculated on the basis of COVID Test Positivity Rate."),
                        p("Zoom in for finer detail. Click on a restaurant for more information."),
                        leafletOutput("map", width="100%", height=700)
                        )
              )
            )
)

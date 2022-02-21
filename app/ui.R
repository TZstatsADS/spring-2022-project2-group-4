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


shinyUI(
  fluidPage(
    navbarPage(theme = shinytheme("sandstone"), collapsible = TRUE,
               title= "",
               id="nav",
               windowTitle = "NYC Restaurants",
               header = tagList(useShinydashboard()),

               tabPanel('Restaurants and Covid Risk',
                        titlePanel("Restaurants"),
                        p("Each circle on the map is a restaurant, colored by the COVID risk of the neighborhood it's in. The COVID risk is calculated on the basis of COVID Test Positivity Rate over the past 7 days (updated weekly)."),
                        p("Zoom in for finer detail. Click on a restaurant for more information."),
                        leafletOutput("map", width="100%", height=700)
                        )
              )
            )
)


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
library("RSocrata")


#========================================================================================================================================================================================

restaurant <-read.csv(url("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/latest/last7days-by-modzcta.csv"), 
                      as.is = TRUE)

df <- read.socrata(
  "https://data.cityofnewyork.us/resource/43nn-pn8j.csv",
  app_token = "g19Oad6Fq1r0KCmwHc0B14riH",
  email     = "vvv2108@columbia.edu",
  password  = "dymheh-1dozdo-jinGin"
)
df <- df[,c("dba","boro","latitude","longitude","zipcode","cuisine_description")]
df <- unique(df)

#========================================================================================================================================================================================

shinyServer(function(input, output) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addProviderTiles("CartoDB.Voyager") %>%  
      setView(lng = -73.935242, lat = 40.730610, zoom = 10) %>%
      addMarkers(lng = -73.9618416, lat = 40.8081563,popup = "Here's Columbia!") 
  })
  reactive(leafletProxy("map", data = df %>% filter(zipcode == input$zip))%>%
    clearShapes() %>%
    addProviderTiles("CartoDB.Voyager") %>%    
    addCircleMarkers(~longitude, ~latitude, radius=6,
                     label = paste(df$dba, "-", df$zipcode))
)
})

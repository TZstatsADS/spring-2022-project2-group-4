
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
library(purrr)


#========================================================================================================================================================================================

restaurant <- read.csv("../data/restaurant.csv")
restaurant$risk <- factor(restaurant$risk, levels = c("Very Low", "Low", "Moderate", "High"), ordered = T)
# The restaurants dataset it big, so I have saved the cleaned version locally. To get the latest dataset, uncomment the chunk of code below

# restaurant <- read.socrata(
#   "https://data.cityofnewyork.us/resource/43nn-pn8j.csv",
#   app_token = "g19Oad6Fq1r0KCmwHc0B14riH",
#   email     = "vvv2108@columbia.edu",
#   password  = "dymheh-1dozdo-jinGin"
# )
# restaurant <- restaurant[,c("dba","boro","latitude","longitude","zipcode","cuisine_description")]
# restaurant <- unique(restaurant)
# 
# covid <- read.csv(url("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/latest/last7days-by-modzcta.csv"),
#                   as.is = TRUE)
# colnames(covid)[1] <- "zipcode"
# restaurant <- merge(restaurant, covid, by = "zipcode")
# 
# risk <- function(percentpositivity_7day){
#   if (percentpositivity_7day <= 1){
#     return("Very Low")
#   }
#   else if (percentpositivity_7day > 1 & percentpositivity_7day <= 3){
#     return("Low")
#   }
#   else if (percentpositivity_7day > 3 & percentpositivity_7day <= 5){
#     return("Moderate")
#   }
#   else{
#     return("High")
#   }
# }
# restaurant$risk <- unlist(map(restaurant$percentpositivity_7day, risk))
# restaurant$risk <- factor(restaurant$risk, levels = c("Very Low", "Low", "Moderate", "High"), ordered = T)
# 
# write.csv(restaurant,"../data/restaurant.csv", row.names = FALSE)

#========================================================================================================================================================================================

shinyServer(function(input, output) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addProviderTiles("CartoDB.Voyager") %>%  
      setView(lng = -73.961035, lat = 40.744436, zoom = 12)
  })
  palette <- c("chartreuse4", "palegreen", "lightpink1", "indianred4")
  color <- colorFactor(palette =palette, restaurant$risk)
  leafletProxy("map", data = restaurant)%>%
    clearShapes() %>%
    addProviderTiles("CartoDB.Voyager") %>%
    addCircleMarkers(~longitude, ~latitude, radius=2,
                     color = ~color(risk),
                     label = paste(restaurant$dba, "-", restaurant$zipcode)) %>% 
    addLegend("bottomright",
              pal = color,
              values = restaurant$risk,
              title = "Covid Positive Rate",
              opacity = 0.85) 
})

packages.used=c("shiny","dplyr","tidyverse","DT","ggplot2",
                "lubridate","plotly","hrbrthemes","highcharter",
                "RColorBrewer","geojsonio","readr","leaflet",
                "RSocrata","purrr")
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
# restaurant <- restaurant[,c("dba","boro","latitude","longitude","building","street","phone", "zipcode","cuisine_description")]
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
# remove_blanks <- function(column){
#   if (column == ""){
#     return("Not Available")
#   }
#   else{
#     return(column)
#   }
# }
# 
# restaurant$cuisine_description <- unlist(map(restaurant$cuisine_description, remove_blanks))
# restaurant$dba <- unlist(map(restaurant$dba, remove_blanks))
# restaurant$boro <- unlist(map(restaurant$boro, remove_blanks))
# restaurant$street <- unlist(map(restaurant$street, remove_blanks))
# restaurant$phone <- unlist(map(restaurant$phone, remove_blanks))
# 
# restaurant <- restaurant %>% drop_na(latitude)
# write.csv(restaurant,"../data/restaurant.csv", row.names = FALSE)

#========================================================================================================================================================================================

shinyServer(function(input, output) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addProviderTiles("CartoDB.Voyager") %>%  
      setView(lng = -73.961035, lat = 40.744436, zoom = 13)
  })
  palette <- c("chartreuse4", "palegreen", "lightpink1", "indianred4")
  color <- colorFactor(palette =palette, restaurant$risk)
  
  leafletProxy("map", data = restaurant)%>%
    clearShapes() %>%
    addProviderTiles("CartoDB.Voyager") %>%
    addCircleMarkers(~longitude, ~latitude, radius=6,
                     stroke=F,
                     color = ~color(risk),
                     popup = paste(sep="<br/>",paste("<b>",restaurant$dba,"</b>"), 
                                   paste("Cuisine: ", restaurant$cuisine_description),
                                   " ",
                                   paste("Address: "),
                                   paste(sep = " ", restaurant$building,restaurant$street),
                                   restaurant$modzcta_name,
                                   restaurant$boro,
                                   paste("NY ",restaurant$zipcode),
                                   " ",
                                   paste("Phone: ", restaurant$phone),
                                   " ",
                                   paste(sep="","Covid Positivity Rate: ", restaurant$percentpositivity_7day, ", ", restaurant$risk)
                                   )) %>% 
    addLegend("bottomright",
              pal = color,
              values = restaurant$risk,
              title = "Covid Positive Rate",
              opacity = 0.85) 
})

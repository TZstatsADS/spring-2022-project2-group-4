#========================================================================================================================================================================================
#Package

packages.used=c("shiny","dplyr","tidyverse","purrr","RSocrata","zipcodeR")
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
library(ggplot2)
library(lubridate)
library(tidyverse)
library(RSocrata)
library(purrr)
library(plotly)
library(hrbrthemes)
library(highcharter)
library(RColorBrewer)
library(zipcodeR)

#========================================================================================================================================================================================
# Data Loading
skate_parks <- read.csv(url("https://data.cityofnewyork.us/resource/pvvr-75zk.csv"), 
                        as.is = TRUE)
dog_runs <- read.csv(url("https://data.cityofnewyork.us/resource/wswf-9pts.csv"), 
                     as.is = TRUE)
playgrounds <- read.csv(url("https://data.cityofnewyork.us/resource/a4qt-mpr5.csv"), 
                        as.is = TRUE)
adult_exe <- read.csv(url("https://data.cityofnewyork.us/resource/tkzt-zfpz.csv"), 
                      as.is = TRUE)
ath_faci <- read.csv(url("https://data.cityofnewyork.us/resource/g3xg-qtbc.csv"), 
                     as.is = TRUE)
ath_faci <- ath_faci[ath_faci$status!='',]
comfort <- read.csv(url("https://data.cityofnewyork.us/resource/i5n2-q8ck.csv"), 
                    as.is = TRUE)
event <- read.csv("../data/NYC_Permitted_Event_Information.csv",as.is = TRUE)


#========================================================================================================================================================================================

zipcodeData <- search_state('NY')
df_sd <- read.csv(url("https://data.cityofnewyork.us/resource/2nwg-uqyg.csv"), 
                  as.is = TRUE)

# Merging data to fetch the lat and log details
df_sd <- na.omit(df_sd) 
df_sd$zipcode <- df_sd$mod_zcta
symptoms_data <- merge(zipcodeData,df_sd)

symptoms_data$date <- as.Date(symptoms_data$date)

frequency_plot_data_all <- symptoms_data %>%
  mutate(month_year = format(date, "%Y-%m-25")) %>%
  group_by(county,month_year) %>%
  summarise(case = sum(total_ed_visits))

selectComplaint <- function(month, county){
  symptoms_data[(substr(symptoms_data$date, 1, 7) == month  & symptoms_data$county == county), ]
}

#========================================================================================================================================================================================




# Extract Longitude & Latitude
latlong <- function(x){
    x$point <- gsub("POINT |[()]", "", x$point)
    x <- x %>%
        separate(point, c("longitude", "latitude"), " ") %>%
        mutate(longitude = as.numeric(longitude)) %>%
        mutate(latitude = as.numeric(latitude))
    return(x)
}
latlong2 <- function(x){
    x <- x %>%
        mutate(longitude = as.numeric(str_extract(x$polygon, "-[0-9][0-9].[0-9]+"))) %>%
        mutate(latitude = as.numeric(substring(str_extract(x$polygon, " [0-9][0-9].[0-9]+"),2)))
    return(x)
}
playgrounds <- latlong(playgrounds)
adult_exe <- latlong(adult_exe)

ath_faci <- latlong2(ath_faci)
dog_runs <- latlong2(dog_runs)
comfort <- latlong2(comfort)
skate_parks <- latlong2(skate_parks)

# Covid Peak Status Check
covid_peak <- as.Date("2020-05-15")
peakstatus <- function(x){
    x <- x %>%
        mutate(approx_date_closed = as.Date(approx_date_closed)) %>%
        mutate(approx_date_reopened = as.Date(approx_date_reopened)) %>%
        mutate(
            peak_status = 
                ifelse(covid_peak > approx_date_closed & covid_peak < approx_date_reopened, 
                       "COVID-19 Closure", "Active")) %>%
        mutate(peak_status = ifelse(is.na(peak_status), status, peak_status))
    return(x)
}
dog_runs <- peakstatus(dog_runs)
skate_parks <- peakstatus(skate_parks)
playgrounds <- peakstatus(playgrounds)
comfort <- peakstatus(comfort)
ath_faci <- peakstatus(ath_faci)
adult_exe <- adult_exe %>%
    mutate(approx_date_closed = as.Date(approx_date_closed,format = '%Y-%m-%d'))%>%
    mutate(approx_date_reopened = as.Date(approx_date_reopened, format = '%Y-%m-%d')) %>%
    mutate(
        peak_status = 
            ifelse(covid_peak > approx_date_closed & covid_peak < approx_date_reopened, 
                   "COVID-19 Closure", "Active")) %>%
    mutate(peak_status = ifelse(is.na(peak_status), status, peak_status))

# Event Data Process
event <- event %>%
    mutate(propname = substring(event$Event.Location,1,regexpr(":",event$Event.Location) - 1))
ath_faci <- ath_faci %>%
    rename(propname = propertyname)
playgrounds <- playgrounds %>%
    rename(propname = name)
skate_parks <- skate_parks %>%
    rename(propname = name)
all <- rbind(adult_exe[,c("propname", "longitude", "latitude")],
             ath_faci[,c("propname", "longitude", "latitude")],
             skate_parks[,c("propname", "longitude", "latitude")],
             playgrounds[,c("propname", "longitude", "latitude")])
event <- merge(event, all, by = "propname")%>%
    distinct(Event.ID,.keep_all = TRUE)

# Add Popup Content
playgrounds <- playgrounds %>%
    mutate(popup = paste(sep = "<br/>",
                         paste0(location),
                         paste0("<b>Accessibility: </b>", playgrounds$accessibilityLevel)
    ))

ath_faci <- ath_faci %>%
    mutate(popup = paste(sep = "<br/>",
                         paste0(propname),
                         paste0("<b>Sports: </b>", ath_faci$primarysport)
    ))

dog_runs <- dog_runs %>%
    mutate(popup = paste(sep = "<br/>",
                         paste0(propertyname),
                         paste0("<b>Red Sign: </b>", 
                                ifelse(dog_runs$red_sign_installed == "true", 
                                       "Installed", "Not Installed"))
    ))

adult_exe <- adult_exe %>%
    mutate(popup = paste(sep = "<br/>",
                         paste0(sitename),
                         paste0("<b>Feature Type: </b>",
                                adult_exe$featuretype)
    ))

skate_parks <- skate_parks %>%
    mutate(popup = paste(sep = "<br/>",
                         paste0(propname),
                         paste0("<b>Red Sign: </b>",
                                ifelse(skate_parks$red_sign_installed == "true",
                                       "Installed", "Not Installed"))
    ))

event <- event %>%
    mutate(popup = paste(sep = "<br/>",
                         paste0("<b>Event Name: </b>",event$Event.Name),
                         paste0("<b>Event Datae: </b>",substr(event$Start.Date.Time,1,10)),
                         paste0("<b>Event Type: </b>",event$Event.Type),
                         paste0("<b>Event Location: </b>",event$Event.Location)
    ))
#========================================================================================================================================================================================
#========================================================================================================================================================================================

# Restaurant data processing

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
# set.seed(1)
# ratings <- sample(x = c(1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0), prob = c(0.05,0.05,0.1,0.1,0.1,0.1,0.1,0.15,0.25), size = nrow(restaurant),replace=T)
# restaurant$rating <- ratings
# write.csv(restaurant,"../data/restaurant.csv", row.names = FALSE)

#========================================================================================================================================================================================

#Park Tab
shinyServer(function(input, output) {
  
    # NYC symptoms map
    output$map_sd <- renderLeaflet({
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        htmlwidgets::onRender(
          "function(el, x) {
                      L.control.zoom({ position: 'bottomright' }).addTo(this)
                  }"
        ) %>%
        addProviderTiles("CartoDB.Voyager") %>%
        setView(lng = -73.935242, lat = 40.730610, zoom = 10)
    })
    
    #Map
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addProviderTiles("CartoDB.Positron") %>%  
            setView(lng = -73.935242, lat = 40.730610, zoom = 11) %>%
            addMarkers(lng = -73.9618416, lat = 40.8081563,popup = "Here's Columbia!") 
    })
    
    #Dog Runs Button
    observeEvent(input$dogruns,{
        #For Present Map
        if(input$time=="Present"){
            #Set Color
            palette_dog = c("black","blue", "yellow")
            color <- colorFactor(palette =palette_dog, dog_runs$status)
            #Add Markers
            leafletProxy("map", data = dog_runs) %>%
                clearShapes()  %>% 
                clearControls()%>%
                clearMarkers() %>%
                addProviderTiles("CartoDB.Positron") %>%
                setView(lng = -73.935242, lat = 40.730610, zoom = 11)%>%    
                addCircleMarkers(~longitude, ~latitude, radius=6,
                                 color = ~color(status),
                                 label = paste(dog_runs$propertyname, '-', dog_runs$name),
                                 popup = ~popup)%>%
                addLegend("bottomleft",
                          pal = color,
                          values = dog_runs$status,
                          title = "Status",
                          opacity = 0.85)
        }
        #For Covid-19 Peak Map
        else{
            #Set Color
            palette_dog = c("black","yellow")
            color <- colorFactor(palette =palette_dog, dog_runs$peak_status)
            #Add Markers
            leafletProxy("map", data = dog_runs) %>%
                clearShapes() %>%
                clearMarkers() %>% 
                clearControls()%>%
                addProviderTiles("CartoDB.Positron") %>%
                setView(lng = -73.935242, lat = 40.730610, zoom = 11)%>%    
                addCircleMarkers(~longitude, ~latitude, radius=6,
                                 color = ~color(peak_status),
                                 label = paste(dog_runs$propertyname, '-', dog_runs$name),
                                 popup = ~popup)%>%
                addLegend("bottomleft",
                          pal = color,
                          values = dog_runs$peak_status,
                          title = "Status",
                          opacity = 0.85) 
        }
    })
    
    #Athletic Facilities Button
    observeEvent(input$ath,{
        #For Present Map
        if(input$time=="Present"){
            #Set Color
            palette <-  c("green","black","blue", "yellow")
            color <- colorFactor(palette =palette, ath_faci$status)
            #Add Markers
            leafletProxy("map", data = ath_faci) %>%
                clearShapes() %>%
                clearMarkers() %>% 
                clearControls()%>%
                addProviderTiles("CartoDB.Positron") %>%
                setView(lng = -73.935242, lat = 40.730610, zoom = 11)%>%    
                addCircleMarkers(~longitude, ~latitude, radius=6,
                                 color = ~color(status),
                                 label = paste(ath_faci$propname,",",ath_faci$primarysport),
                                 popup = ~popup)%>%
                addLegend("bottomleft",
                          pal = color,
                          values = ath_faci$status,
                          title = "status",
                          opacity = 0.85)
        }
        #For Covid-19 Peak Map
        else{
            #Set Color
            palette <-  c("green","black","blue", "yellow")
            color <- colorFactor(palette =palette, ath_faci$peak_status)
            #Add Markers
            leafletProxy("map", data = ath_faci) %>%
                clearShapes() %>%
                clearMarkers() %>% 
                clearControls()%>%
                addProviderTiles("CartoDB.Positron") %>%
                setView(lng = -73.935242, lat = 40.730610, zoom = 11)%>%    
                addCircleMarkers(~longitude, ~latitude, radius=6,
                                 color = ~color(peak_status),
                                 label = paste(ath_faci$propname,",",ath_faci$primarysport),
                                 popup = ~popup)%>%
                addLegend("bottomleft",
                          pal = color,
                          values = ath_faci$peak_status,
                          title = "status",
                          opacity = 0.85)
        }
    })
    
    #Comfort Stations Button
    observeEvent(input$comfort,{
        #For Present Map
        if(input$time=="Present"){
            #Set Color
            palette <-  c("green","black","blue", "yellow")
            color <- colorFactor(palette =palette, comfort$status)
            #Add Markers
            leafletProxy("map", data = comfort) %>%
                clearShapes() %>% 
                clearControls()%>%
                clearMarkers() %>%
                addProviderTiles("CartoDB.Positron") %>%
                setView(lng = -73.935242, lat = 40.730610, zoom = 11)%>%    
                addCircleMarkers(~longitude, ~latitude, radius=6,
                                 color = ~color(status),
                                 label = paste('(',comfort$longitude, ',', comfort$latitude,")"))%>%
                addLegend("bottomleft",
                          pal = color,
                          values = comfort$status,
                          title = "Status",
                          opacity = 0.85) 
        }
        #For Covid-19 Peak Map
        else{
            #Set Color
            palette <-  c("green","black","blue", "yellow")
            color <- colorFactor(palette =palette, comfort$peak_status)
            #Add Markers
            leafletProxy("map", data = comfort) %>%
                clearShapes() %>%
                clearMarkers() %>% 
                clearControls()%>%
                addProviderTiles("CartoDB.Positron") %>%
                setView(lng = -73.935242, lat = 40.730610, zoom = 11) %>%    
                addCircleMarkers(~longitude, ~latitude, radius=6,
                                 color = ~color(peak_status),
                                 label = paste('(',comfort$longitude, ',', comfort$latitude,")"))%>%
                addLegend("bottomleft",
                          pal = color,
                          values = comfort$peak_status,
                          title = "Status",
                          opacity = 0.85) 
        }
    })
    
    #Skate Parks Button
    observeEvent(input$skate,{
        #For Present Map
        if(input$time=="Present"){
            #Set Color
            palette <-  c("green", "yellow")
            color <- colorFactor(palette =palette, skate_parks$status)
            #Add Markers
            leafletProxy("map", data = skate_parks) %>%
                clearShapes() %>%
                clearMarkers() %>% 
                clearControls() %>%
                addProviderTiles("CartoDB.Positron") %>%
                setView(lng = -73.935242, lat = 40.730610, zoom = 11) %>%    
                addCircleMarkers(~longitude, ~latitude, radius=6,
                                 color = ~color(status),
                                 label = paste(skate_parks$propname),
                                 popup = ~popup)%>%
                addLegend("bottomleft",
                          pal = color,
                          values = skate_parks$status,
                          title = "Status",
                          opacity = 0.85) 
        }
        #For Covid-19 Peak Map
        else{
            #Set Color
            palette <-  c("black","green", "yellow")
            color <- colorFactor(palette =palette, skate_parks$peak_status)
            #Add Markers
            leafletProxy("map", data = skate_parks) %>%
                clearShapes() %>% 
                clearControls()%>%
                clearMarkers() %>%
                addProviderTiles("CartoDB.Positron") %>%
                setView(lng = -73.935242, lat = 40.730610, zoom = 11) %>%    
                addCircleMarkers(~longitude, ~latitude, radius=6,
                                 color = ~color(peak_status),
                                 label = paste(skate_parks$propname),
                                 popup = ~popup)%>%
                addLegend("bottomleft",
                          pal = color,
                          values = skate_parks$peak_status,
                          title = "Status",
                          opacity = 0.85)
        }
    })
    
    #Playgrounds Button
    observeEvent(input$playgrounds,{
        #For Present Map
        if(input$time=="Present"){
            #Set Color
            palette <-  c("green","black","blue", "yellow")
            color <- colorFactor(palette =palette, playgrounds$status)
            #Add Markers
            leafletProxy("map", data = playgrounds) %>%
                clearShapes() %>%
                clearMarkers() %>%
                clearControls() %>%
                addProviderTiles("CartoDB.Positron") %>%
                setView(lng = -73.935242, lat = 40.730610, zoom = 11) %>%
                addCircleMarkers(~longitude, ~latitude, radius=6,
                                 color = ~color(status),
                                 label = paste(playgrounds$propname),
                                 popup = ~popup)%>%
                addLegend("bottomleft",
                          pal = color,
                          values = playgrounds$status,
                          title = "Status",
                          opacity = 0.85)
        }
        #For Covid-19 Peak Map
        else{
            #Set Color
            palette <-  c("green","black","blue", "yellow")
            color <- colorFactor(palette =palette, playgrounds$peak_status)
            #Add Markers
            leafletProxy("map", data = playgrounds) %>%
                clearShapes() %>%
                clearMarkers() %>%
                clearControls() %>%
                addProviderTiles("CartoDB.Positron") %>%
                setView(lng = -73.935242, lat = 40.730610, zoom = 11)%>%    
                addCircleMarkers(~longitude, ~latitude, radius=6,
                                 color = ~color(peak_status),
                                 label = paste(playgrounds$propname),
                                 popup = ~popup)%>%
                addLegend("bottomleft",
                          pal = color,
                          values = playgrounds$peak_status,
                          title = "Status",
                          opacity = 0.85)
        }
    })
    
    #Adult Exercise Equipment Button
    observeEvent(input$adult,{
        #For Present Map
        if(input$time=="Present"){
            #Set Color
            palette <-  c("green","blue", "yellow")
            color <- colorFactor(palette =palette, adult_exe$status)
            #Add Markers
            leafletProxy("map", data = adult_exe) %>%
                clearShapes() %>%
                clearControls() %>%
                clearMarkers() %>%
                addProviderTiles("CartoDB.Positron") %>%
                setView(lng = -73.935242, lat = 40.730610, zoom = 11)%>%
                addCircleMarkers(~longitude, ~latitude, radius=6,
                                 color = ~color(status),
                                 label = paste(adult_exe$propname),
                                 popup = ~popup)%>%
                addLegend("bottomleft",
                          pal = color,
                          values = adult_exe$status,
                          title = "Status",
                          opacity = 0.85)
        }
        #For Covid-19 Peak Map
        else{
            #Set Color
            palette <-  c("green","black","blue", "yellow")
            color <- colorFactor(palette =palette, adult_exe$peak_status)
            #Add Markers
            leafletProxy("map", data = adult_exe) %>%
                clearShapes() %>%
                clearMarkers() %>%
                clearControls()%>%
                addProviderTiles("CartoDB.Positron") %>%
                setView(lng = -73.935242, lat = 40.730610, zoom = 11)%>%
                addCircleMarkers(~longitude, ~latitude, radius=6,
                                 color = ~color(peak_status),
                                 label = paste(adult_exe$propname),
                                 popup = ~popup)%>%
                addLegend("bottomleft",
                          pal = color,
                          values = adult_exe$peak_status,
                          title = "Status",
                          opacity = 0.85) 
        }
    })
    
    #Event Button
    observeEvent(input$event, {
        #For Present Map
        if(input$time=="Present"){
            #Set Color
            palette <-  c("pink","orange", "green")
            color <- colorFactor(palette =palette, event$Event.Type)
            #Add Markers
            leafletProxy("map", data = event) %>%
                clearShapes() %>%
                clearControls()%>%
                clearMarkers() %>%
                addProviderTiles("CartoDB.Positron") %>%
                setView(lng = -73.935242, lat = 40.730610, zoom = 11)%>%
                addCircleMarkers(~longitude, ~latitude, radius=6,
                                 color = ~color(Event.Type),
                                 label = paste(event$propname,",",event$Event.Name),
                                 popup = ~popup)%>%
                addLegend("bottomleft",
                          pal = color,
                          values = event$Event.Type,
                          title = "Event Type",
                          opacity = 0.85) 
        }
        #For Covid-19 Peak Map
        else{
            leafletProxy("map", data = event) %>%
                clearShapes() %>%
                clearControls()%>%
                clearMarkers() %>%
                addProviderTiles("CartoDB.Positron") %>%
                setView(lng = -73.935242, lat = 40.730610, zoom = 11)
        }
    })  
    
    #========================================================================================================================================================================================
    # Restaurant code
    
    palette <- c("chartreuse4", "palegreen", "lightpink1", "indianred4")
    color <- colorFactor(palette =palette, restaurant$risk)
    output$restaurant_map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addProviderTiles("CartoDB.Positron") %>%  
        setView(lng = -73.961035, lat = 40.744436, zoom = 13) %>% 
        addCircleMarkers(~longitude, ~latitude, data = restaurant, radius=6,
                         stroke=F,
                         color = ~color(risk),
                         popup = paste(sep="<br/>",paste("<b>",restaurant$dba,"</b>"), 
                                       paste(sep="","Rating: ", restaurant$rating, "/5"),
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
    
    #========================================================================================================================================================================================
    
    
    df_react_ny <- reactive({
      selectComplaint(input$Month, "New York County")
    })
    
    df_react_r <- reactive({
      selectComplaint(input$Month, "Richmond County")
    })
    
    df_react_q <- reactive({
      selectComplaint(input$Month, "Queens County")
    })
    
    df_react_b <- reactive({
      selectComplaint(input$Month, "Bronx County")
    })
    
    df_react_k <- reactive({
      selectComplaint(input$Month, "Kings County")
    })
    
    observeEvent((input$Month != '' & (input$New_York_County  | input$Richmond_County | input$Queens_County | input$Bronx_County  | input$Kings_County )),{
      leafletProxy("map_sd", data = symptoms_data) %>%
        clearShapes() %>%
        clearMarkers() %>%
        addProviderTiles("CartoDB.Voyager") %>%
        fitBounds(-74.354598, 40.919500, -73.761545, 40.520024) 
      
      if (input$Month != '') {
        if (input$New_York_County){
          leafletProxy("map_sd", data = df_react_ny()) %>%
            addCircleMarkers(
              lng=~lng,
              lat=~lat,
              color = 'red',
              weight = ~sum(total_ed_visits),
              radius = ~sqrt(total_ed_visits) * 1.4,
              stroke = FALSE,
              fillOpacity = 0.3,
              label = ~total_ed_visits
            )
        }
        if (input$Richmond_County){
          leafletProxy("map_sd", data = df_react_r()) %>%
            addCircleMarkers(
              lng=~lng,
              lat=~lat,
              color = 'red',
              weight = ~sum(total_ed_visits),
              radius = ~sqrt(total_ed_visits) * 1.4,
              stroke = FALSE,
              fillOpacity = 0.3,
              label = ~total_ed_visits
            )
        }
        if (input$Queens_County){
          leafletProxy("map_sd", data = df_react_q()) %>%
            addCircleMarkers(
              lng=~lng,
              lat=~lat,
              color = 'red',
              weight = ~sum(total_ed_visits),
              radius = ~sqrt(total_ed_visits) * 1.4,
              stroke = FALSE,
              fillOpacity = 0.3,
              label = ~total_ed_visits
            )
        }
        if (input$Bronx_County){
          leafletProxy("map_sd", data = df_react_b()) %>%
            addCircleMarkers(
              lng=~lng,
              lat=~lat,
              color = 'red',
              weight = ~sum(total_ed_visits),
              radius = ~sqrt(total_ed_visits) * 1.4,
              stroke = FALSE,
              fillOpacity = 0.3,
              label = ~total_ed_visits
            )
        }
        if (input$Kings_County){
          leafletProxy("map_sd", data = df_react_k()) %>%
            addCircleMarkers(
              lng=~lng,
              lat=~lat,
              color = 'red',
              weight = ~sum(total_ed_visits),
              radius = ~sqrt(total_ed_visits) * 1.4,
              stroke = FALSE,
              fillOpacity = 0.3,
              label = ~total_ed_visits
            )
        }
      }
    })
    
    output$t3Plot1 <- renderPlot({
      # Kernel regression to get a smooth trend
      frequency_plot_data <- frequency_plot_data_all[frequency_plot_data_all$county == input$borough,]
      frequency_plot_data <- frequency_plot_data[rev(order(frequency_plot_data$month_year)),]
      dd <- frequency_plot_data[1:12,]
      dd$month_year <- as.Date(dd$month_year)
      ggplot(dd,  aes(x=month_year, y=case), label=case) +
        geom_area(fill="#69b3a2", alpha=0.5) +
        geom_line(color="#69b3a2") +
        geom_point() +
        geom_text(aes(label=case),hjust=0,vjust=0) +
        ggtitle("Number of Symptomatic Patient Hospital Visits") +
        ylab("Symptom frequency") +
        xlab("Date") +
        theme_ipsum()
    })
    
    #========================================================================================================================================================================================
    
})


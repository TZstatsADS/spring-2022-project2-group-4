#========================================================================================================================================================================================
#Package

packages.used=c("shiny","dplyr","tidyverse","leaflet","shinythemes","shinyWidgets","shinydashboard")
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
library(leaflet)
library(shinythemes)
library(shinyWidgets)
library(ggplot2)
library(lubridate)
library(plotly)
library(hrbrthemes)
library(highcharter)
library(RColorBrewer)
if(!require(fontawesome)) devtools::install_github("rstudio/fontawesome")
library(geojsonio)
library(shinydashboard)
#========================================================================================================================================================================================
#Park Tab
shinyUI(
  fluidPage(
    #Navigation Page
    navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
               title= "",
               id="nav",inverse = TRUE,
               windowTitle = "",
               header = tagList(
                 useShinydashboard()
               ),
               tabPanel('Home Page',icon = icon("home"),
                        h2(strong("Outing Avenues in NYC in times of Covid-19"),align = "center", style="font-family:Helvetica, font-color:Black"),
                        h3("Chen Gexin,", "Khan Zaigham,", "Ma Xiangyu,", "Naik Vaishak,","Vedula Varchasvi",align = "center",style="color:gray", style="font-family:Helvetica, font-color:Black"),
                        h4("2022 Spring Project2 Group4 - Applied Data Science at Columbia University",align = "center",style="color:gray", style="font-family:Helvetica, font-color:Black"),
                        br(),
                        h3("The impact of Covid-19 has been tremendous on the entire world. It has changed the lives of people. There have been a lot of behavioral and lifestyle changes in the pandemic.  
                                 The city suffered greatly in the pandemic. Apart from the threat of life threatening infections, there were far reaching consequences of the pandemic. The businesses suffered tremendously in this pandemic. During the peak of the pandemic, most business avenues had closed. 
                                 The city witnessed a decline in all outing options. Restaurant business had almost vanished. As the city gets up to a new normal, even now the impact of the virus is quite visible. Most businesses, however, have started to show growth and improvement. It is a sign of hope and improvement for all. ", style="color:black", align = "left", style="font-family:Helvetica, font-color:Black"),
                        br(),
                        h3("This app is primarily targeted at the residents of NYC who are looking for outing avenues. Through this app, we aim to suggest the places to visit in NYC by keeping in consideration the Covid outbreak. It also shows the impact of Covid outbreak in outing options and compares the present day options with peak of Covid-19. 
                        The app suggests athletic facilities, playgrounds, adult exercise equipment, dog runs, comfort stations and skate parks. The app also displays the events happening in NYC. The app also shows the restaurants in NYC. Along with it, restuarant details and ratings are also shown to the user. Each circle on the map is a restaurant, colored by the COVID risk of the neighborhood it's in. The COVID risk is calculated on the basis of COVID Test Positivity Rate over the past 7 days (updated weekly).
                         Effort has been made to also show clusters of covid near the restaurants and map them to different zones. The app shows a comparison between the peak of Covid-19 and the present day.
                                    Finally, to make sure that users are a step ahead of the covid, the app not just tracks covid in different areas, but rather keeps a track of pneumonia like symptoms in different areas. Users can avoid those area as such areas could be a potential outbreak spot. 
                                    It may continue as the economy recovers and life returns to a semblance of normality.", style="color:black",align = "Left", style="font-family:Helvetica, font-color:Black")
               ),
               #Park Page
               tabPanel('Park Reopen Situation', icon = icon("tree"),
                        #Title
                        titlePanel("NYC Park Location and Reopen Status"),
                        #Map Input
                        leafletOutput("map", width="100%", height=700),
                        #Control Pad Input
                        absolutePanel(id = "choices", class = "panel panel-default",
                                      top = 190, left = 1150, width = 240, fixed=FALSE,
                                      draggable = TRUE, height = 600,
                                      
                                      tags$h1("Control Pad",align = "center", style = "font-size:27px"),
                                      #Time Selection Input
                                      selectInput("time", "Covid-19 Peak vs Present", choices=c("Covid-19 Peak","Present"),width = 230,selected = "Covid-19 Peak"),
                                      tags$h1("Choose the Parks",align = "left", style = "font-size:15px"),
                                      #Action Buttons
                                      actionButton("ath", "Athletic Facilities",icon=icon("bicycle", lib = "font-awesome"),width = 230,style='padding:8px; font-size:80%'),
                                      br(),
                                      br(),
                                      actionButton("playgrounds", label = "Playgrounds",icon=icon("running", lib = "font-awesome"),width = 230,style='padding:8px; font-size:80%'),
                                      br(),
                                      br(),
                                      actionButton("adult", label = "Adult Exercise Equipment", icon=icon("dumbbell", lib = "font-awesome"),width = 230,style='padding:8px; font-size:80%'),
                                      br(),
                                      br(),
                                      actionButton("dogruns", label = "Dog Runs", icon("dog", lib = "font-awesome"),width = 230,style='padding:8px; font-size:80%'),
                                      br(),
                                      br(),
                                      actionButton("comfort", label = "Comfort Stations", icon=icon("steam", lib = "font-awesome"),width = 230,style='padding:8px; font-size:80%'),
                                      br(),
                                      br(),
                                      actionButton("skate", "Skate Parks",icon=icon("skating", lib = "font-awesome"),width = 230,style='padding:8px; font-size:80%'),
                                      
                                      tags$h1("Event View (Only Present)",align = "left", style = "font-size:15px"),
                                      #Event Action Button
                                      actionButton("event", label = "Event", icon=icon("child", lib = "font-awesome"),width = 230,style='padding:8px; font-size:80%'),
                                      style = "opacity: 0.85")),
               tabPanel('Restaurants and Covid Risk',
                        icon = icon("cutlery"),
                        titlePanel("Restaurants"),
                        p("Covid Test Positivity Rate is defined as the number of positive COVID tests out of 100 COVID tests performed in the zip code. Very Low: Rate <= 1%, Low: 1% < Rate <= 3%, Moderate: 3% < Rate <= 5%, High: Rate > 5%.",style='padding:8px; font-size:80%'),
                        leafletOutput("restaurant_map", width="100%", height=700)
               ),
               
               tabPanel("Symptoms Frequency Map",
                        icon=icon("heart"),
                        div(class="outer map",
                            leafletOutput("map_sd", width="100%", height=550),
                            absolutePanel(id = "symptoms", class = "panel panel-default",
                                          top = 100, left = 25, width = 250, fixed=FALSE,
                                          draggable = TRUE, height = "auto",
                                          tags$h1("Please Select",
                                                  align = "left", style = "font-size:30px"),
                                          selectInput("Month",
                                                      label = "Month",
                                                      choices = c('2020-06', '2020-07', '2020-08', '2020-09', '2020-10', '2020-11', '2020-12', 
                                                                  '2021-01', '2021-02', '2021-03', '2021-04', '2021-05', '2021-06', '2021-07', '2021-08', '2021-09', '2021-10', '2021-11', '2021-12', 
                                                                  '2022-01')
                                          ),
                                          tags$h2("County",
                                                  align = "left",style = "font-size:15px"),
                                          checkboxInput("New_York_County",
                                                        label = "New York County", value = FALSE),
                                          checkboxInput("Richmond_County",
                                                        label = "Richmond County", value = FALSE),
                                          checkboxInput("Bronx_County",
                                                        label = "Bronx County", value = FALSE),
                                          checkboxInput("Queens_County",
                                                        label = "Queens County", value = FALSE),
                                          checkboxInput("Kings_County",
                                                        label = "Kings County", value = FALSE),
                                          style = "opacity: 0.80")
                        ),
                        sidebarPanel(top = 555, left = 25, width = 150, fixed=FALSE,
                                     draggable = TRUE, height = 100,
                                     selectInput("borough",
                                                 label = "PLease Select The County To See The Covid Symptoms Trend :",
                                                 choices = c('New York County', 'Richmond County', 'Bronx County', 'Queens County', 'Kings County')
                                     )
                        ),
                        mainPanel( left = 25, width = 150, fixed=FALSE,
                                   draggable = TRUE, height = 150,
                                   plotOutput(outputId = "t3Plot1"),
                        )
               )
    )
  )
)

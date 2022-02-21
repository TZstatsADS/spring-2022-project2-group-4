#========================================================================================================================================================================================
#Package

packages.used=c("shiny","dplyr","tidyverse","leaflet","shinythemes","shinyWidgets")
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
#========================================================================================================================================================================================
#Park Tab
shinyUI(
  fluidPage(
    #Navigation Page
    navbarPage(theme = shinytheme("sandstone"), collapsible = TRUE,
               title= "",
               id="nav",
               windowTitle = "",
               header = tagList(
                 useShinydashboard()
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
                        titlePanel("Restaurants"),
                        p("Each circle on the map is a restaurant, colored by the COVID risk of the neighborhood it's in. The COVID risk is calculated on the basis of COVID Test Positivity Rate over the past 7 days (updated weekly)."),
                        p("Covid Test Positivity Rate is defined as the number of positive COVID tests out of 100 COVID tests performed in the zip code."),
                        p("Zoom in for finer detail. Click on a restaurant for more information."),
                        leafletOutput("restaurant_map", width="100%", height=700)
               )
    )
  )
)

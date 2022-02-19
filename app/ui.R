#========================================================================================================================================================================================
#Package
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
                                      style = "opacity: 0.85"))
    )
  )
)


# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(dygraphs)
library("shinythemes")
library("lubridate")
library("mgcv")
library("htmltools")
library("htmlwidgets")
library("metricsgraphics")


# load helpers.R file
source("helpers.R")

# getting Data from csv
Dataset <- read.delim("DMFeb.csv", header = TRUE, sep = ";", dec=".")
df <- create_dataset(Dataset)

header <- dashboardHeader(title = "Aqua Data Miner")

sidebar <- dashboardSidebar(  sidebarMenu(
  menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
  menuItem("Widgets", tabName = "widgets", icon = icon("th")),
  menuItem("Charts", icon = icon("bar-chart-o"),
           menuSubItem("Sub-item 1", tabName = "subitem1"),
           menuSubItem("Sub-item 2", tabName = "subitem2")
  ),
  menuItem("Build By I2S Data Science Team", icon = icon("file-code-o"),
           href = "https://www.i2s.gr") 
))

body <- dashboardBody(tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
),
  tabItems(
  tabItem(tabName = "dashboard",
          h2("Dashboard tab content")
  ),
  
  tabItem(tabName = "widgets",
          h2("Widgets tab content")
  ),
  tabItem(tabName = "subitem1",
          fluidRow(
            valueBox(
              # The value comes from the server via uiOutput
              uiOutput("orderNum"), "New Orders", icon = icon("credit-card")
            ),
            
            valueBox(
              # The icon can also be a uiOutput
              uiOutput("progress"), "Progress", icon = uiOutput("progressIcon"),
              color = "purple"
            ),
            
            # An entire box can be in a uiOutput
            uiOutput("approvalBox")
 
))))

dashboardPage(header, sidebar, body)

library(bslib)
library(tidyverse)
library(shiny)
library(shinydashboard)

gen_ai <- read.csv("generative_ai_misinformation_dataset.csv")

ui <- dashboardPage(
  dashboardHeader(
    titleWidth = 400,
    title = "Generative AI Misinformation Research"),
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      menuItem("Sample of Data", tabName = "data", icon = icon("database")),
      menuItem("Detected Gen AI Misinformation Around the World", tabName = "map", icon = icon("map-location-dot")),
      menuItem("Misinformation Detection Frequency", tabName = "histogram", icon = icon("chart-column")),
      menuItem("Factors of Misinformation Detection", tabName = "boxplot", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    fluidRow(
      box(
        title = ""
      )
    )
  )
)

server <- function(input, output) { }

shinyApp(ui, server)
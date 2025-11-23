library(bslib)
library(tidyverse)
library(shiny)
library(shinydashboard)

gen_ai <- read.csv("generative_ai_misinformation_dataset.csv")

ui <- dashboardPage(
  dashboardHeader(title = "Generative AI Misinformation Research"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    fluidRow(
      box()
    )
  )
)

server <- function(input, output) { }

shinyApp(ui, server)
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
    tabItems(

      tabItem(
        tabName = "histogram",
        fluidRow(
          box(
            title = "Misinformation Frequency in Model Signatures",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("misinfo_hist")
          ),
      
      box(
        title = "Model Signatures",
        status = "warning",
        solidHeader = TRUE,
        selectInput("model", "Select a Model Signature",
        choices = c("All", unique(gen_ai$model_signature)))))
      )
    )
  )
)

server <- function(input, output) {

  output$misinfo_hist <- renderPlot({

    filtered <- gen_ai
    if (input$model != "All") {
      filtered <- filtered %>%
        filter(model_signature == input$model)
    }
    
    ggplot(filtered, aes(x = is_misinformation)) +
      geom_bar(width = 0.9) +
      labs(
        x = "Misinformation?",
        y = "Count",
        title = paste("Misinformation for", input$model)
      )
  })

}

shinyApp(ui, server)
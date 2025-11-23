library(bslib)
library(tidyverse)
library(shiny)
library(shinydashboard)

gen_ai <- read.csv("generative_ai_misinformation_dataset.csv")

ui <- dashboardPage(skin = "purple",
  dashboardHeader(
    titleWidth = 400,
    title = "Generative AI Misinformation Research"),
  dashboardSidebar(
    width = 375,
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
          column(
            width = 7,
            box(
              title = "Misinformation Frequency in Model Signatures",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              plotOutput("misinfo_hist")
            ),

            textOutput("keytxt")
          ),

          column(
            width = 5,

            box(
              title = "Model Signatures",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              selectInput("model", "Select a Model Signature",
                          choices = c("All", unique(gen_ai$model_signature)))
            ),

            valueBoxOutput("misinfo_count", width = 12),

            valueBoxOutput("not_misinfo_count", width = 12)
          )
        )
      )
    )
  )
)

server <- function(input, output) {

    filtered <- reactive({
      data <- gen_ai
      if (input$model != "All") {
        data <- data %>%
          filter(model_signature == input$model)
    }
    data
  })
  
  output$misinfo_hist <- renderPlot({
    ggplot(filtered(), aes(x = is_misinformation)) +
      geom_bar(fill = "grey50", width = 0.9) +
      labs(
        x = "Misinformation vs. Authentic Cases",
        y = "Count",
        title = paste("Misinformation Frequency for", input$model)
      )
  })

  output$misinfo_count <- renderValueBox({

    data <- filtered()

    count_yes <- sum(data$is_misinformation == "1")
    total <- nrow(data)

    percent_yes <- ifelse(total > 0, round((count_yes / total) * 100, 1), 0)

    valueBox(
      value = paste0(count_yes, " (", percent_yes, "%)"),
      subtitle = "Number of Misinformation Cases",
      icon = icon("triangle-exclamation"),
      color = "red"
    )
  })

  output$not_misinfo_count <- renderValueBox({
    data <- filtered ()

    count_no <- sum(data$is_misinformation == "0")
    total <- nrow(data)

    percent_no <- ifelse(total > 0, round((count_no / total) * 100, 1), 0)

    valueBox(
      value = paste0(count_no, " (", percent_no, "%)"),
      subtitle = "Number of Authentic Information Cases",
      icon = icon("circle-check"),
      color = "olive"
    )
  })

  output$keytxt <- renderText({
    paste0(
      "0 = Authentic Information | 1 = Misinformation"
    )
  })

}

shinyApp(ui, server)
library(bslib)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(leaflet)

# Load dataset
gen_ai <- read.csv("generative_ai_misinformation_dataset.csv")

# Add latitude and longitude for cities (correct longitudes for western hemisphere)
gen_ai <- gen_ai %>%
  mutate(location = case_when(
    city == "New York" ~ "40.7128, -74.006",
    city == "Berlin" ~ "52.52, 13.4050",
    city == "Chicago" ~ "41.8832, -87.6324",
    city == "Hamburg" ~ "53.4588, 9.9872",
    city == "Delhi" ~ "28.7041, 77.1025",
    city == "Bangalore" ~ "12.9629, 77.5775",
    city == "Mumbai" ~ "18.9582, 72.8321",
    city == "London" ~ "51.5072, -0.1276",
    city == "Sao Paulo" ~ "-23.5558, -46.6396",
    city == "Manchester" ~ "53.4808, -2.2426",
    city == "Birmingham" ~ "52.4823, -1.89",
    city == "Brasilia" ~ "-15.7975, -47.8919",
    city == "Los Angeles" ~ "34.0549, -118.2426",
    city == "Munich" ~ "48.1351, 11.582",
    city == "Rio de Janeiro" ~ "-22.9068, -43.1729"
  )) %>%
  separate(location, into = c("latitude", "longitude"), sep = ", ") %>%
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  )

# UI
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
      # Histogram tab
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
      ),

      # Map tab
      tabItem(
        tabName = "map",
        fluidRow(
          box(
            title = "Misinformation Detected Globally",
            status = "primary",
            width = 12,
            leafletOutput("worldmap", height = 600)
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output) {

  # Filtered data for histogram tab
  filtered <- reactive({
    data <- gen_ai
    if (input$model != "All") {
      data <- data %>% filter(model_signature == input$model)
    }
    data
  })

  # Histogram
  output$misinfo_hist <- renderPlot({
    ggplot(filtered(), aes(x = is_misinformation)) +
      geom_bar(fill = "grey50", width = 0.9) +
      labs(
        x = "Misinformation vs. Authentic Cases",
        y = "Count",
        title = paste("Misinformation Frequency for", input$model)
      )
  })

  # Value boxes with count and percentage
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
    data <- filtered()
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
    "0 = Authentic Information | 1 = Misinformation"
  })

  # Prepare city-level data for map
  city_data <- reactive({
    gen_ai %>%
      filter(!is.na(latitude) & !is.na(longitude)) %>%
      group_by(city, latitude, longitude) %>%
      summarise(count = sum(is_misinformation == "1"), .groups = "drop")
  })

  # Base map
  output$worldmap <- renderLeaflet({
    data <- city_data()

    leaflet(data) %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = ~sqrt(count)*3,    # scale by number of cases
        color = "red",
        stroke = TRUE,
        fillOpacity = 0.6,
        label = ~paste0(city, ": ", count, " misinformation cases")
      )
  })
}

# Run app
shinyApp(ui, server)
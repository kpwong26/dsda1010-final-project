library(bslib)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(leaflet)
library(shinyWidgets)
library(DT)

# load dataset
gen_ai <- read.csv("generative_ai_misinformation_dataset.csv")

# add latitude and longitude for cities
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

# ui
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
      menuItem("Attributes of Misinformation Detection", tabName = "boxplot", icon = icon("chart-line")),
      menuItem("Final Presentation & Report", tabName = "report", icon = icon("file-lines"))
    )
  ),
  dashboardBody(
    tabItems(
      # misinformation frequency histogram
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

      # world map
      tabItem(
        tabName = "map",
        fluidRow(
          column(
          width = 8,
          box(
            title = "Misinformation Detected Globally",
            status = "primary",
            width = 12,
            leafletOutput("worldmap", height = 600)
          )),
          column(
            width = 4,
            box(
              title = "Model Signatures",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              checkboxGroupInput("modelmap", "Select Model Signatures", choices = unique(gen_ai$model_signature)), selected = unique(gen_ai$model_signature)
            ),
          box(
            title = "Country",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            virtualSelectInput(
              "countryselect",
              "Select a Country",
              choices = c("All", unique(gen_ai$country)),
              multiple = FALSE,
              search = TRUE,
              placeholder = "Select a country"
            ),
            virtualSelectInput(
              "cityselect",
              label = "Select a City",
              choices = c("All"),
              multiple = TRUE,
              search = TRUE,
              placeholder = "Select city/cities"))
        )),
        fluidRow(
          column(
          width = 7,
          box(
            title = "Misinformation Frequency by Location",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotOutput("misinfo_location")),
          textOutput("keytxt2")
          ),
          column(
            width = 5,
            valueBoxOutput("location_misinfo_count", width = 12),
            valueBoxOutput("location_notmisinfo_count", width = 12)
          )
        )
      ),
      
      # boxplot
      tabItem(
        tabName = "boxplot",
        fluidRow(
          box(
            title = "Attributes of AI Writing",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotOutput("ai_writing")
          ),
          box(
            title = "Attributes of Human Writing",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotOutput("human_writing")
          ),
        fluidRow(
          column(
          width = 6,
          box(
            title = "Select an Attribute",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            selectInput("attribute", "Select an attribute", choices = c("readability_score", "sentiment_score", "toxicity_score", "token_count"))
          )),
          column(
            width = 6,
            box(
              title = "AI Writing Statistics",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              tableOutput("ai_writing_stats")
            ),
            box(
              title = "Human Writing Statistics",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              tableOutput("human_writing_stats")))
          )
        )
      ),

      # data sample
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            title = "Sample of Data",
            width = 12,
            DT::dataTableOutput("data_head")
          )
        )
      )
    )
  )
)

# server
server <- function(input, output, session) {

  # filtered data for histogram tab
  filtered <- reactive({
    data <- gen_ai
    if (input$model != "All") {
      data <- data %>% filter(model_signature == input$model)
    }
    data
  })

  # misinformation frequency histogram
  output$misinfo_hist <- renderPlot({
    ggplot(filtered(), aes(x = is_misinformation)) +
      geom_bar(fill = "grey50", width = 0.9) +
      labs(
        x = "Misinformation vs. Authentic Cases",
        y = "Count",
        title = paste("Misinformation Frequency for", input$model)
      )
  })

  # value boxes with count and percentage
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
  
  # legend for histogram
  output$keytxt <- renderText({
    "0 = Authentic Information | 1 = Misinformation"
  })

  # city and country selection
  observe({
    req(input$countryselect)

    if (input$countryselect == "All") {
      cities <- unique(gen_ai$city)
    } else {
      cities <- gen_ai %>%
        filter(country == input$countryselect) %>%
        pull(city) %>%
        unique()
    }

    cities <- c("All", as.character(sort(cities)))

    updateVirtualSelect(
      session = session,
      "cityselect",
      choices = cities,
      selected = "All"
    )
  })

  # misinformation by city
  city_data <- reactive({
    data <- gen_ai

    # model signature filter
    if (!is.null(input$modelmap)) {
      data <- data %>% filter(model_signature %in% input$modelmap)
    }

    # country filter
    if (!is.null(input$countryselect) && input$countryselect != "All") {
      data <- data %>% filter(country == input$countryselect)
    }

    # city filter
    if(!is.null(input$cityselect) && !"All" %in% input$cityselect) {
      data <- data %>% filter(city %in% input$cityselect)
    }

    data %>%
      filter(!is.na(latitude) & !is.na(longitude)) %>%
      group_by(city, latitude, longitude) %>%
      summarise(
        misinfo = sum(is_misinformation == "1"),
        authentic = sum(is_misinformation == "0"),
        .groups = "drop"
      )
  })

  # world map
  output$worldmap <- renderLeaflet({
    data <- city_data()

    leaflet(data) %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      addMarkers(
        lng = ~longitude,
        lat = ~latitude,
        label = ~paste0(
          city, "<br>",
          "Misinformation: ", misinfo, "<br>",
          "Authentic: ", authentic
        ) %>% lapply(htmltools::HTML)
      )
  })

  # histogram by location
  hist_data <- reactive({
    data <- gen_ai

    if(!is.null(input$modelmap)) {
      data <- data %>% filter(model_signature %in% input$modelmap)
    }

    if(!is.null(input$countryselect) && input$countryselect != "All") {
      data <- data %>% filter(country == input$countryselect)
    }

    if (!is.null(input$cityselect) && !"All" %in% input$cityselect) {
      data <- data %>% filter(city %in% input$cityselect)
    }

    data
  })

  output$misinfo_location <- renderPlot({
    data <- hist_data()

    title_text <- "Misinformation Frequency"

    if(!is.null(input$countryselect) && input$countryselect != "All") {
      title_text <- paste(title_text, "in", input$countryselect)
    }

    if(!is.null(input$cityselect) && !"All" %in% input$cityselect) {
      cities <- paste(input$cityselect, collapse = ", ")
      title_text <- paste(title_text, "for", cities)
    }

    ggplot(data, aes(is_misinformation)) +
      geom_bar(fill = "grey50", width = 0.8) +
      labs(
        x = "Misinformation vs. Authentic Cases",
        y = "Count",
        title = title_text
      )
  })

  # legend for location histogram
  output$keytxt2 <- renderText({
    "0 = Authentic Information | 1 = Misinformation"
  })

  # location value boxes
  output$location_misinfo_count <- renderValueBox({
    data <- hist_data()

    location_count_yes <- sum(data$is_misinformation == "1")
    location_total <- nrow(data)
    location_percent_yes <- ifelse(location_total > 0, round((location_count_yes / location_total) * 100, 1), 0)

    valueBox(
      value = paste0(location_count_yes, " (", location_percent_yes, "%)"),
      subtitle = "Number of Misinformation Cases",
      icon = icon("triangle-exclamation"),
      color = "red"
    )
  })

  output$location_notmisinfo_count <- renderValueBox({
    data <- hist_data()
    
    location_count_no <- sum(data$is_misinformation == "0")
    location_total <- nrow(data)
    location_percent_no <- ifelse(location_total > 0, round((location_count_no / location_total) * 100, 1), 0)

    valueBox(
      value = paste0(location_count_no, " (", location_percent_no, "%)"),
      subtitle = "Number of Authentic Information Cases",
      icon = icon("circle-check"),
      color = "olive"
    )
  })

  # ai writing boxplot
  output$ai_writing <- renderPlot({
    gen_ai %>%
      filter(model_signature == "GPT-like") %>%
      ggplot(aes(x = factor(is_misinformation), y = !!sym(input$attribute))) +
      geom_boxplot() +
      stat_summary()
  })

  # human writing boxplot
  output$human_writing <- renderPlot({
    gen_ai %>%
      filter(model_signature == "human") %>%
      ggplot(aes(x = factor(is_misinformation), y = !!sym(input$attribute))) +
      geom_boxplot() +
      stat_summary()
  })

  # boxplot statistics
  output$ai_writing_stats <- renderTable({
    req(input$attribute)

    ai_authentic_info_stats <- gen_ai %>%
      filter(model_signature == "GPT-like" & is_misinformation == "0") %>%
      summarize(
        Group = "Authentic Information (0)",
        Q1 = quantile(.data[[input$attribute]], 0.25),
        Median = median(.data[[input$attribute]]),
        Mean = mean(.data[[input$attribute]]),
        Q3 = quantile(.data[[input$attribute]], 0.75),
        Min = min(.data[[input$attribute]]),
        Max = max(.data[[input$attribute]])
      )
    
    ai_misinfo_stats <- gen_ai %>%
      filter(model_signature == "GPT-like" & is_misinformation == "1") %>%
      summarize(
        Group = "Misinformation (1)",
        Q1 = quantile(.data[[input$attribute]], 0.25),
        Median = median(.data[[input$attribute]]),
        Mean = mean(.data[[input$attribute]]),
        Q3 = quantile(.data[[input$attribute]], 0.75),
        Min = min(.data[[input$attribute]]),
        Max = max(.data[[input$attribute]])
      )
    
    bind_rows(ai_authentic_info_stats, ai_misinfo_stats)
  })

  output$human_writing_stats <- renderTable({
    req(input$attribute)

    human_authentic_info_stats <- gen_ai %>%
      filter(model_signature == "human" & is_misinformation == "0") %>%
      summarize(
        Group = "Authentic Information (0)",
        Q1 = quantile(.data[[input$attribute]], 0.25),
        Median = median(.data[[input$attribute]]),
        Mean = mean(.data[[input$attribute]]),
        Q3 = quantile(.data[[input$attribute]], 0.75),
        Min = min(.data[[input$attribute]]),
        Max = max(.data[[input$attribute]])
      )
    
    human_misinfo_stats <- gen_ai %>%
      filter(model_signature == "human" & is_misinformation == "1") %>%
      summarize(
        Group = "Misinformation (1)",
        Q1 = quantile(.data[[input$attribute]], 0.25),
        Median = median(.data[[input$attribute]]),
        Mean = mean(.data[[input$attribute]]),
        Q3 = quantile(.data[[input$attribute]], 0.75),
        Min = min(.data[[input$attribute]]),
        Max = max(.data[[input$attribute]])
      )
    
    bind_rows(human_authentic_info_stats, human_misinfo_stats)
  })

  # data sample
  output$data_head <- DT::renderDataTable({
    DT::datatable(
      gen_ai,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
  })
}

# run app
shinyApp(ui, server)
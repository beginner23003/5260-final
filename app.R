library(shiny)
library(leaflet)
library(dplyr)
library(plotly)
library(DT)
library(tidyr)
library(shinydashboard)

# load data sets
mineral <- read.csv("Mineral ores round the world.csv")
smartphones <- read.csv("Sales.csv")  # Adjust the path accordingly




# data set cleaning
mineral <- mineral |>
  mutate(original_commod1 = commod1) |>
  separate(original_commod1, into = c("main_commod", "rest"), sep = ",", extra = "merge") |>
  mutate(ore_split = ore) |>
  separate(ore_split, into = c("main_ore", "rest_ore"), sep = ",", extra = "merge") |>
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude))


smartphones <- smartphones %>%
  mutate(Memory = as.numeric(gsub(" GB", "", Memory)),  # Assuming Memory is in the format "32 GB"
         Storage = as.numeric(gsub(" GB", "", Storage)))  # Assuming Storage is in the format "128 GB"





# ui
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Mineral Distribution"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("info-circle")),
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("US Mineral Site Num", tabName = "us_num", icon = icon("bar-chart")),
      menuItem("Main Commod Site Num", tabName = "main_commod", icon = icon("bar-chart")),
      menuItem("Smartphone Pricing", tabName = "smartphone_pricing", icon = icon("mobile")),
      menuItem("Price vs Ratings", tabName = "price_ratings", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro",
              fluidRow(
                box(
                  width = 12,
                  tags$style(type = "text/css",
                             "
                       
                       .author-info {
                         display: flex;
                         justify-content: center;
                         align-items: center;
                         font-family: 'Chalkduster', fantasy;
                       }
                      
                       "
                  ),
                  div(
                    style = "text-align: center;",
                    img(src = "https://media.giphy.com/media/v1.Y2lkPTc5MGI3NjExMHU2ajlrNDcwa2c1dTNqejMxdzJmeGdwNGV0bjU0dGp6Zmx3c2I2aSZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/2dPo5eE97Aw5q/giphy.gif",
                        height = "230px", width = "60%", style = "border-radius: 8px;")
                  ),
                  div(
                    style = "text-align: center;",
                    h2("Welcome to the Mineral Distribution Searching App"),
                    p("Explore and analyze Mineral Distribution around the world!"),
                    p("Navigate through different tabs to discover more information."),
                    p("Use filters to customize your search."),
                    p("Enjoy exploring!"),
                    br()
                  ),
                  div(
                    style = "text-align: center;",
                    div(
                      class = "author-info",
                      p("Author: "),
                      p("5260")
                    )
                  )
                )
              )
      ),
      tabItem(tabName = "map",
              fluidRow(
                box(
                  width = 4,
                  selectInput("country", "Select Country:",
                              choices = c("all", unique(mineral$country))),
                  selectizeInput("state", "Select State:",
                                 choices = unique(mineral$state),
                                 multiple = TRUE),
                  selectInput("dev_stat", "Select Development Status:",
                              choices = c("all", unique(mineral$dev_stat))),
                  selectInput("oper_type", "Select Operation Type:",
                              choices = c("all", unique(mineral$oper_type))),
                  actionButton("reset", "Reset Selection")
                ),
                box(
                  width = 8,
                  leafletOutput("map"),
                  dataTableOutput("results_table")
                )
              )
      ),
      tabItem(tabName = "us_num",
              fluidRow(
                box(
                  width = 12,
                  title = "Number of mineral sites within the United States",
                  selectInput("commod1_plot", "Select Main Commodity:",
                              choices = c("all", unique(mineral$main_commod)),
                              multiple = TRUE,
                              selected = "all"),
                  selectInput("ore_plot", "Select Main Ore Type:",
                              choices = c("all", unique(mineral$main_ore)),
                              multiple = TRUE,
                              selected = "all"),
                  actionButton("reset_filters_plot", "Reset Filters")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  div(
                    style = "height: 600px; overflow-y: scroll;",
                    plotlyOutput("bar_chart_plot")
                  )
                )
              )
      ),
      tabItem(tabName = "main_commod",
              fluidRow(
                box(
                  width = 4,
                  title = "Distribution of different main commod types",
                  selectInput("country_plot_3", "Select Country:",
                              choices = c("all", unique(mineral$country)),
                              selected = "all"),
                  sliderInput("latitude_plot_3", "Select Latitude Range:",
                              min = -90, max = 90, value = c(-90, 90)),
                  sliderInput("longitude_plot_3", "Select Longitude Range:",
                              min = -180, max = 180, value = c(-180, 180)),
                  actionButton("reset_filters_3", "Reset Filters")
                ),
                box(
                  width = 8,
                  div(
                    style = "height: 600px; overflow-y: scroll; display: flex; justify-content: flex-start;",
                    plotlyOutput("plot_3")
                  )
                )
              )
              
      ),
      tabItem(tabName = "smartphone_pricing",
              fluidRow(
                box(
                  title = "Smartphone Pricing Filters",
                  sliderInput("memoryInput", "Memory (GB):", min = 0, max = 256, value = c(0, 256)),
                  sliderInput("storageInput", "Storage (GB):", min = 0, max = 512, value = c(0, 512)),
                  actionButton("reset_smartphone", "Reset Filters")
                ),
                box(
                  title = "Average Selling Price by Brand",
                  plotOutput("avgPricePlot")
                )
              )
      ),
      tabItem(tabName = "price_ratings",
              fluidRow(
                box(
                  title = "Filters",
                  selectizeInput("brandInput", "Select Brands:",
                                 choices = unique(smartphones$Brands), 
                                 multiple = TRUE,
                                 selected = unique(smartphones$Brands)),
                  actionButton("reset_brand", "Reset Filters")
                ),
                box(
                  title = "Selling Price vs User Ratings",
                  plotOutput("priceRatingPlot")
                )
              )
      )
      
    )
  )

)

# server
server <- function(input, output, session) {
  # page 1
  observeEvent(input$country, {
    selected_country <- input$country
    available_state <- mineral$state[mineral$country == selected_country]
    updateSelectizeInput(session, "state", choices = c("", available_state))
  })
  
  

  
  
  # reset page 1
  observeEvent(input$reset, {
    updateSelectInput(session, "country", selected = "all")
    updateSelectizeInput(session, "state", selected = "")
    updateSelectInput(session, "dev_stat", selected = "all")
    updateSelectInput(session, "oper_type", selected = "all")
    
  })
  
  # reset page 2
  observeEvent(input$reset_filters_plot, {
    updateSelectInput(session, "commod1_plot", selected = "all")
    updateSelectInput(session, "ore_plot", selected = "all")
  })
  
  # reset page 3
  observeEvent(input$reset_filters_3, {
    updateSelectInput(session, "country_plot_3", selected = "all")
    updateSliderInput(session, "latitude_plot_3", value = c(-90, 90))
    updateSliderInput(session, "longitude_plot_3", value = c(-180, 180))
  })
  
  
  # filter data page 1
  filtered_data <- reactive({
    data <- mineral
    
    
    if (input$country != "all") {
      data <- data %>% filter(country %in% input$country)
    }
    
    
    if ("" %in% input$country) {
      
    } else {
      data <- data %>% filter(country %in% input$country)
    }
    
    if ("" %in% input$state) {
      
    } else {
      data <- data %>% filter(state %in% input$state)
    }
    
    if (input$dev_stat == "all") {
      
    } else {
      data <- data %>% filter(dev_stat %in% input$dev_stat)
    }
    
    if (input$oper_type == "all") {
      
    } else {
      data <- data %>% filter(oper_type %in% input$oper_type)
    }
    
    
    return(data)
  })
  
  # map page 1
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = filtered_data(),
                       lat = ~latitude, lng = ~longitude,
                       radius = 3, clusterOptions = markerClusterOptions(),
                       popup = ~paste("Site: ", ifelse(is.na(site_name), "N/A", site_name),
                                      "<br>Country: ", ifelse(is.na(country), "N/A", country),
                                      "<br>State: ", ifelse(is.na(state), "N/A", state),
                                      "<br>Main Commod: ", ifelse(is.na(commod1), "N/A", commod1),
                      
                        
                                      '<br><a href="https://www.google.com/maps?q=', latitude, ',', longitude, '" target="_blank">Open in Google Map</a>'
                       ))
  })
  
  # table page 1
  output$results_table <- renderDataTable({
    
    filtered_data() %>% select(site_name, country, com_type, commod1, commod2, commod3)
  },
  options = list(
    lengthMenu = c(5, 10, 25, 50), 
    pageLength = 5  
  ))
  
  # filter page 2
  filtered_data_plot <- reactive({
    data <- mineral
    
    if (!"all" %in% input$commod1_plot) {
      data <- data[data$main_commod %in% input$commod1_plot, ]
    }
    
    if (!"all" %in% input$ore_plot) {
      data <- data[data$main_ore %in% input$ore_plot, ]
    }
    
    data <- data |>
      filter(country == "United States")
    
    return(data)
  })

  # group page 2
  grouped_data_plot <- reactive({
    
    grouped <- aggregate(site_name ~ state, data = filtered_data_plot(), FUN = length)
    return(grouped)
  })  
  
  # plot page 2
  output$bar_chart_plot <- renderPlotly({
    
    
    p <- ggplot(data = grouped_data_plot(), aes(x = state, y = site_name)) +
      geom_bar(stat = "identity", position = "dodge", fill = "lightblue") +
      labs(
        y = "Count",
        x = "State"
      ) +
      theme_minimal() +
      scale_fill_brewer(palette = "Set3") +
      coord_flip()
    
    p <- ggplotly(p) %>% 
      layout(height = 1000)
    
    p
    #ggplotly(p)
    #subplot(p, p2, nrows = 1)
  })
  
  # filter page 3
  filtered_data_3 <- reactive({
    data <- mineral
    
    if (!"all" %in% input$country_plot_3) {
      data <- data[data$country %in% input$country_plot_3, ]
    }
    
    
    data <- data %>% filter(latitude >= input$latitude_plot_3[1],
                            latitude <= input$latitude_plot_3[2],
                           longitude >= input$longitude_plot_3[1],
                           longitude <= input$longitude_plot_3[2])
    
    return(data)
  })
  
  # group page 3
  grouped_data_3 <- reactive({
    
    grouped <- aggregate(site_name ~ main_commod, data = filtered_data_3(), FUN = length)
    return(grouped)
  }) 
  
  
  # plot page 3
  output$plot_3 <- renderPlotly({
    p <- ggplot(data = grouped_data_3(), aes(x = main_commod, y = site_name)) +
      geom_bar(stat = "identity", position = "dodge", fill = "orange") +
      labs(
        y = "Count",
        x = "Main Commod Type"
      ) +
      theme_minimal() +
      scale_fill_brewer(palette = "Set3") +
      coord_flip()
    
    p <- ggplotly(p) %>% 
      layout(height = 2000)
    
    p
  })
  
  
  # Logic for smartphone pricing
  observeEvent(input$reset_smartphone, {
    updateSliderInput(session, "memoryInput", value = c(0, 256))
    updateSliderInput(session, "storageInput", value = c(0, 512))
  })
  
  filtered_smartphones <- reactive({
    smartphones %>%
      filter(Memory >= input$memoryInput[1], Memory <= input$memoryInput[2],
             Storage >= input$storageInput[1], Storage <= input$storageInput[2])
  })
  
  output$avgPricePlot <- renderPlot({
    data <- filtered_smartphones() %>%
      group_by(Brands) %>%
      summarise(AveragePrice = mean(Selling_Price, na.rm = TRUE)) %>%
      arrange(desc(AveragePrice))
    
    ggplot(data, aes(x = reorder(Brands, -AveragePrice), y = AveragePrice, fill = Brands)) +
      geom_col(show.legend = FALSE) +
      labs(x = "Brand", y = "Average Selling Price ($)", title = "Average Selling Price by Brand") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  # Logic for Price vs Ratings
  observeEvent(input$reset_brand, {
    updateSelectizeInput(session, "brandInput", choices = unique(smartphones$Brands), selected = unique(smartphones$Brands))
  })
  
  filtered_ratings <- reactive({
    smartphones %>%
      filter(Brands %in% input$brandInput)
  })
  
  output$priceRatingPlot <- renderPlot({
    data <- filtered_ratings()
    ggplot(data, aes(x = Rating, y = Selling_Price)) +
      geom_point(aes(color = Brands), alpha = 0.6) +
      labs(x = "User Ratings", y = "Selling Price ($)", title = "Selling Price vs User Ratings") +
      theme_minimal() +
      scale_color_discrete(name = "Brand")
  })
  
}

shinyApp(ui = ui, server = server)

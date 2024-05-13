library(shiny)
library(leaflet)
library(dplyr)
library(plotly)
library(DT)
library(tidyr)
library(shinydashboard)

# Caitlin attempt
library(sf)
library(rworldmap)

data(countriesLow)
world_map_poly <- st_as_sf(countriesLow)


# load data sets
mineral <- read.csv("Mineral ores round the world.csv")
smartphones <- read.csv("Sales.csv")  # Adjust the path accordingly
#setwd('D:/Caitlin/School/JupyterNotebooks/5260-final')
ewaste <- read.csv('ewaste cleaned.csv')
ewaste <-  world_map_poly %>%
  left_join(ewaste, by = c("NAME" = "COUNTRY"))
pal <- colorNumeric(palette = "viridis", domain = world_map_poly$ewaste_gen_total, na.color = "#808080")
#Bruce
#load facility data
facility_data <- read.csv("electronic-waste-recycling-facilities-list-1.csv", stringsAsFactors = FALSE)

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

ewaste <- ewaste %>% 
  mutate(Latitude = as.numeric(Latitude), 
         Longitude = as.numeric(Longitude), 
         ewaste_generated_capita = as.numeric(E.WASTE.GENERATED..KG.CAPITA.), 
         ewaste_gen_total = as.numeric(E.WASTE.GENERATED..MILLION.KG.))

#for facilities
# Function to extract coordinates from Location.1
extract_coords <- function(location) {
  if (grepl("\\(", location)) {  # Check if there is a parenthesis indicating coordinates
    coords <- gsub(".*\\(", "", location)  # Remove everything before the opening parenthesis
    coords <- gsub("\\).*", "", coords)    # Remove everything after the closing parenthesis
    coords <- strsplit(coords, ", ")[[1]]
    if (length(coords) == 2) {
      return(as.numeric(coords))
    }
  }
  return(c(NA, NA))  # Return NA values if coordinates are not properly formatted
}

# Apply the function to extract Latitude and Longitude
coords <- t(sapply(facility_data$Location.1, extract_coords))
facility_data$Latitude <- coords[, 1]
facility_data$Longitude <- coords[, 2]



# ui
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Tech Impacts"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("info-circle")),
      menuItem("Mineral Map", tabName = "map", icon = icon("map")),
      menuItem("US Mineral Site Num", tabName = "us_num", icon = icon("bar-chart")),
      menuItem("Main Commod Site Num", tabName = "main_commod", icon = icon("bar-chart")),
      menuItem("Smartphone Pricing", tabName = "smartphone_pricing", icon = icon("mobile")),
      menuItem("Price vs Ratings", tabName = "price_ratings", icon = icon("chart-line")), 
      menuItem('Global EWaste', tabName = 'ewaste', icon=icon('globe')),
      menuItem("E-Waste Facilities in NY", tabName = "e_waste_facilities", icon = icon("recycle"))
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
                    h2("Welcome to the Physical Impact of Tech Searching App"),
                    p("Explore and analyze different aspects of tech impact around the world!"),
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
                      p("Caitlin Tavas, Zhe Yin, Muhan Zhang, Qingyao Meng, Lirui Xiao")
                    )
                  )
                )
              )
      ),
      tabItem(tabName = "map",
              fluidRow(
                box(
                  width = 4,
                  title = "Mineral distribution around the world",
                 
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
                  width = 4,
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
                ),
                box(
                  width = 8,
                  title = "Some insights",
                  verbatimTextOutput("us_num_insight")
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
                  title = "Some insights",
                  verbatimTextOutput("main_commod_insight")
                )
                
              ),
              fluidRow(
                box(
                  width = 12,
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
                  width = 4,
                  title = "Smartphone Pricing Filters",
                  sliderInput("memoryInput", "Memory (GB):", min = 0, max = 256, value = c(0, 256)),
                  sliderInput("storageInput", "Storage (GB):", min = 0, max = 512, value = c(0, 512)),
                  actionButton("reset_smartphone", "Reset Filters")
                ),
                box(
                  width = 8,
                  title = "Average Selling Price by Brand",
                  plotOutput("avgPricePlot")
                )
              )
      ),
      tabItem(tabName = "price_ratings",
              fluidRow(
                box(
                  width = 4,
                  title = "Filters",
                  selectizeInput("brandInput", "Select Brands:",
                                 choices = unique(smartphones$Brands), 
                                 multiple = TRUE,
                                 selected = unique(smartphones$Brands)),
                  actionButton("reset_brand", "Reset Filters")
                ),
                box(
                  width = 8,
                  title = "Selling Price vs User Ratings",
                  plotOutput("priceRatingPlot")
                )
              )
      ), 
      
      tabItem(tabName = "ewaste", 
              fluidRow(
                box(title = "Filter Information", width = 4
                    , 
                    sliderInput("ewaste_range",
                                "E-Waste Generated:",
                                min = min(ewaste$ewaste_gen_total, na.rm = TRUE),
                                max = max(ewaste$ewaste_gen_total, na.rm = TRUE),
                                value = c(min(ewaste$ewaste_gen_total, na.rm = TRUE), 
                                         max(ewaste$ewaste_gen_total, na.rm = TRUE)),
                                step = 10)  
                  
                ), 
                box(title = "EWaste Produced Globally", width = 8,
                    leafletOutput("ewaste_map")
                )
              )
      ),
      tabItem(tabName = "e_waste_facilities",
              fluidRow(
                box(
                  width = 12,
                  leafletOutput("map_of_facility"),
                  uiOutput("mapMessageFacility")  # This will display the message below the map
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
  
  output$us_num_insight <- renderText({
    conclusion <- "1. California has the most mineral sites in the US with more than 42,000\n" 
    conclusion <- paste(conclusion, "2. Arizona has only 1 mineral site\n", sep = "")
    conclusion <- paste(conclusion, "3. The distribution of mining sites varies greatly from state to state, from tens of thousands to dozens or even a few\n", sep = "")
    conclusion <- paste(conclusion, "4. Gold is the most popular mineral around the world, with over 20,000 in California alone", sep = "")
    conclusion
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
  
  output$main_commod_insight <- renderText({
    conclusion <- "1. Gold, Sand and Gravel are the most common commodities, with more than 60,000 mineral sites and more than 45,000 mineral sites respectively\n" 
    conclusion <- paste(conclusion, "2. The United States has an overwhelming number of mineral sites\n", sep = "")
    conclusion
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
      labs(x = "Brand", y = "Average Selling Price", title = "Average Selling Price by Brand") +
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
  
  # Map rendering for e-waste facilities
  output$map_of_facility <- renderLeaflet({
    leaflet(data = facility_data) %>%
      addTiles() %>%
      addMarkers(
        lng = ~Longitude, lat = ~Latitude, 
        popup = ~paste(Facility.Name, "<br>", Street.Address, "<br>", City, ", ", State, "<br>", Zip.Code),
        clusterOptions = markerClusterOptions()
      ) %>%
      setView(lng = -73.7572, lat = 42.6526, zoom = 7)
  })
  output$mapMessageFacility <- renderUI({
    if (is.null(input$map_marker_click)) {
      HTML('<div class="info-message">Click on a facility or group of facilities to see more information.</div>')
    } else {
      # If you want to change or hide the message after clicking, adjust here
      HTML('<div class="info-message">Displaying information for the selected facility.</div>')
    }
  })
  
    
  # creating output for ewaste page 
  filtered_ewaste <- reactive({
    ewaste %>%
      filter(ewaste_gen_total >= input$ewaste_range[1], 
             ewaste_gen_total <= input$ewaste_range[2])
  })
  output$ewaste_map <- renderLeaflet({
    leaflet(data = filtered_ewaste()) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(ewaste_gen_total),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        popup = ~paste(NAME, "<br>", "E-Waste Generated: ", ewaste_gen_total)
      ) 
  })
  
  
}

shinyApp(ui = ui, server = server)
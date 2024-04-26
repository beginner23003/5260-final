library(shiny)
library(leaflet)
library(dplyr)
library(plotly)
library(DT)
library(tidyr)
# load data sets
mineral <- read.csv("Mineral ores round the world.csv")


# data set cleaning
mineral <- mineral |>
  mutate(original_commod1 = commod1) |>
  separate(original_commod1, into = c("main_commod", "rest"), sep = ",", extra = "merge") |>
  mutate(ore_split = ore) |>
  separate(ore_split, into = c("main_ore", "rest_ore"), sep = ",", extra = "merge") |>
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude))

# ui 
ui <- fluidPage(
  # page 0 - introduction
  navbarPage(
    "Mineral Distribution", 
    tabPanel("Introduction",
             
             tags$style(type = "text/css", 
                        "
                          body{
                          background: #11ffee00; 
                          background-size: cover;
                          background-repeat: no-repeat;
                          background-attachment: fixed;
                          color: #191919;
                          margin: 0;
                          font-family: 'Georgia', serif;
                          style: 'border-radius: 50%;'
                          }
                          .bottom-rectangle {
                            position: absolute;
                            bottom: 0;
                            left: 0;
                            width: 100%;
                            background: #333;
                            color: #fff;
                            text-align: center;
                            padding: 30px;
                            font-family: 'URW Chancery L', cursive;
                          }
                          .author-info {
                            display: flex;
                            justify-content: center; 
                            align-items: center; 
                            font-family: 'Chalkduster', fantasy;
                          }
                          .author-info-text {
                            color: rgb(8, 51, 68);
                            margin-left: 20px;
                          }
              "),
             class = "center-box",
             style = "text-align: center;",
             div(
               style = "text-align: center;",
               img(src = "https://media.giphy.com/media/v1.Y2lkPTc5MGI3NjExMHU2ajlrNDcwa2c1dTNqejMxdzJmeGdwNGV0bjU0dGp6Zmx3c2I2aSZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/2dPo5eE97Aw5q/giphy.gif", height = "230px", width = "60%", style = "border-radius: 8px;")
             ),
             h2("Welcome to the Mineral Distribution Searching App"),
             p("Explore and analyze Mineral Distribution around the world!"),
             p("Navigate through different tabs to discover more information."),
             p("Use filters to customize your search."),
             p("Enjoy exploring!"),
             br(),
             
             div(
               style = "text-align: center;",
               div(
                 class = "author-info",

                   #class = "author-info-text",
                   p("Author: "),
                   p("5260")
                 
               )
             ),
             
             absolutePanel(class = "bottom-rectangle",
                           "Disclaimer: This app is for educational purposes and does not provide real-time data."
             )
    ),
    
    # first page
    tabPanel("Map",  
             
             titlePanel("Mineral Distribution Map"),
             h4("Click on blue points on the map for site details!"),
             sidebarLayout(
               sidebarPanel(
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
               mainPanel(
                 leafletOutput("map"),
                 dataTableOutput("results_table")
               )
             )
    ),
    
    # second page
    
    tabPanel("US Mineral Site Num",
             
             titlePanel("Number of mineral sites within the United States"),
             selectInput("commod1_plot", "Select Main Commodity:",
                                choices = c("all", unique(mineral$main_commod)),
                                multiple = TRUE,
                                selected = "all"),
             
             selectInput("ore_plot", "Select Main Ore Type:",
                         choices = c("all", unique(mineral$main_ore)),
                         multiple = TRUE,
                         selected = "all"),
             actionButton("reset_filters_plot", "Reset Filters"),
             
             plotlyOutput("bar_chart_plot")
             
    ),
    # third page
    tabPanel("Main Commod Site Num",
             
             titlePanel("Distribution of different main commod types"),
             
             sidebarLayout(
               sidebarPanel(
             selectInput("country_plot_3", "Select Country:",
                         choices = c("all", unique(mineral$country)),
                         selected = "all"),
             
          
             sliderInput("latitude_plot_3", "Select Latitude Range:",
                         min = -90, max = 90, value = c(-90, 90)),
             sliderInput("longitude_plot_3", "Select Longitude Range:",
                         min = -180, max = 180, value = c(-180, 180)),
             actionButton("reset_filters_3", "Reset Filters")
               ),
             
             mainPanel(
             plotlyOutput("plot_3")
             )
             )
             
    ),
    
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
  
}

shinyApp(ui = ui, server = server)

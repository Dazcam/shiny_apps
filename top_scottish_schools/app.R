library(shiny)
library(tidyverse)
library(DT)
library(leaflet) # maps
library(sf) # geospatial data

# Useful links
# https://data.spatialhub.scot # Scottish spatial data hub

## Set variables
## If running / testing locally
#app_dir <- '~/Desktop/shiny_apps/top_scottish_schools/'

## Load data
league_table <- readRDS('league_table.rds')
league_table_srtd <- league_table |> arrange(school)
address_tbl <- readRDS('address_tbl.rds')
school_geoloc_tbl <- readRDS('school_geoloc_tbl.rds')
catchment_geo_data_subset <- readRDS('catchment_geo_data_subset.rds')

ui <- fluidPage(
  titlePanel("Scottish School League Table"),
  theme = shinythemes::shinytheme('paper'),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "school", 
                  label = "Choose a school", 
                  choices = league_table_srtd$school_id)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Plot', plotOutput('trend')), 
        tabPanel('Map', leafletOutput("scotland", width = '800px', height = '800px')),
        tabPanel('Table', DT::DTOutput("school_table")),
        tabPanel('Contact', DT::DTOutput("address_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  glasgow_coords <- c(-4.2518, 55.8642) # Fixed Glasgow coordinates
  
  # Initialize the default school selection
  updateSelectInput(session, "school", selected = "Aberdeen Grammar School | Aberdeen City")
  
  # Create the initial leaflet map
  output$scotland <- renderLeaflet({
    scotland <- leaflet() %>%
      addTiles() %>%
      setView(lat = 56.49, lng = -4.20, zoom = 7)
    
    # Add black dot for Glasgow
    scotland <- scotland %>%
      addCircleMarkers(lng = glasgow_coords[1], lat = glasgow_coords[2],
                       color = "black", radius = 5, label = "Glasgow")
    
    return(scotland)  # Return initial map
  })
  
  observeEvent(input$school, {
    # Extract the selected school name and local authority
    selected_school_info <- str_split(input$school, " \\| ")[[1]]
    selected_school_name <- selected_school_info[1]
    selected_la_name <- selected_school_info[2]
    
    # Look up coordinates from school_geoloc_tbl for the currently selected school
    school_coords <- school_geoloc_tbl %>%
      filter(SchoolName == selected_school_name & LAName == selected_la_name) %>%
      select(Latitude, Longitude)
    
    # Prepare polygons for the selected school
    selected_school_poly <- catchment_geo_data_subset %>%
      filter(school_name == toupper(selected_school_name) & local_authority == selected_la_name) %>%
      st_transform(crs = 4326)  # Transform to WGS84
    
    leafletProxy("scotland") %>%
      clearMarkers() %>%
      clearShapes() %>%
      addCircleMarkers(lng = glasgow_coords[1], lat = glasgow_coords[2],
                       color = "black", radius = 5, label = "Glasgow")
    
    # Only add the selected school's marker and polygon
    if (nrow(school_coords) > 0) {
      leafletProxy("scotland") %>%
        addCircleMarkers(lng = school_coords$Longitude, lat = school_coords$Latitude,
                         color = "blue", radius = 5, label = selected_school_name)
      
      if (nrow(selected_school_poly) > 0) {
        leafletProxy("scotland") %>%
          addPolygons(data = selected_school_poly,
                      fillColor = "red",
                      weight = 1,
                      opacity = 1,
                      color = "red",
                      fillOpacity = 0.5,
                      popup = ~school_name)
      }
    }
  })
  
  output$school_table <- DT::renderDT({
    league_table |>
      DT::datatable()
  })
  
  output$address_table <- DT::renderDT({
    # Use the complete info to get the current school data
    selected_school <- str_split(input$school, " \\| ")[[1]][1]
    address_tbl |>
      dplyr::filter(School == selected_school) |>
      DT::datatable()
  })
  
  rval_school <- reactive({
    league_table_single <- league_table %>%
      dplyr::filter(school_id ==  input$school) %>%
      pivot_longer(names_to = 'Year', values_to = 'Rank', cols = c(-school, -LAName, -school_id))
  })
  
  output$trend <- renderPlot({
    league_table |>
      dplyr::slice_head(n = 5) |>
      dplyr::mutate(across(everything(), ~replace_na(.x, 0))) |>
      pivot_longer(names_to = 'Year', values_to = 'Rank', cols = c(-school, -LAName, -school_id)) |>
      ggplot2::ggplot(aes(x = Year, y = Rank, color = factor(school), group = factor(school))) +
      geom_line(linewidth = 0.8, linetype = "dashed", alpha = 0.7) +
      geom_line(data = rval_school(), color = 'tomato2', linewidth = 1) +
      scale_y_reverse() +
      theme_minimal() +
      theme(legend.position = "none")
  })
}

shinyApp(ui = ui, server = server)
library(shiny)
library(bslib)
library(tidyverse)
library(DT)
library(leaflet)
library(sf)

## Load data
league_table              <- readRDS('league_table.rds')
league_table_srtd         <- league_table |> arrange(school)
address_tbl               <- readRDS('address_tbl.rds')
school_geoloc_tbl         <- readRDS('school_geoloc_tbl.rds')
catchment_geo_data_subset <- readRDS('catchment_geo_data_subset.rds')

# ── Colour palette ─────────────────────────────────────────────────────────────
scot_blue  <- "#003078"
scot_navy  <- "#001B4F"
scot_gold  <- "#C8A400"
accent     <- "#0066CC"

# ── Custom CSS ─────────────────────────────────────────────────────────────────
custom_css <- "
  body { background-color: #F4F6FA; font-family: 'Inter', 'Segoe UI', sans-serif; }

  .app-header {
    background: linear-gradient(135deg, #001B4F 0%, #003078 60%, #0066CC 100%);
    padding: 1.4rem 2rem 1.2rem;
    color: #fff;
    border-bottom: 4px solid #C8A400;
    margin-bottom: 0;
  }
  .app-header h1 { font-size: 1.75rem; font-weight: 700; letter-spacing: -0.5px; margin: 0 0 0.2rem; }
  .app-header .subtitle { font-size: 0.88rem; opacity: 0.82; margin: 0; }

  .sidebar-card {
    background: #fff;
    border-radius: 10px;
    padding: 1.25rem 1.1rem;
    box-shadow: 0 2px 12px rgba(0,0,0,0.08);
    border-top: 4px solid #003078;
  }
  .sidebar-card label { font-weight: 600; color: #001B4F; font-size: 0.9rem; }

  .stat-badge {
    background: #EEF2F7;
    border-radius: 8px;
    padding: 0.55rem 0.75rem;
    margin-top: 0.6rem;
    font-size: 0.82rem;
    color: #334;
    display: flex;
    align-items: center;
    gap: 0.5rem;
  }
  .stat-badge .badge-val { font-weight: 700; font-size: 1rem; color: #003078; }

  .nav-tabs { border-bottom: 2px solid #D0D8E8; margin-bottom: 0; }
  .nav-tabs .nav-link {
    color: #556; font-weight: 600; font-size: 0.88rem;
    border: none; border-bottom: 3px solid transparent; border-radius: 0;
    padding: 0.65rem 1.1rem;
    transition: color 0.15s, border-color 0.15s;
  }
  .nav-tabs .nav-link:hover { color: #003078; background: #F0F4FA; }
  .nav-tabs .nav-link.active { color: #003078; border-bottom-color: #C8A400; background: transparent; }

  .tab-content { background: #fff; border-radius: 0 0 10px 10px; box-shadow: 0 2px 12px rgba(0,0,0,0.07); padding: 1.25rem; }

  #scotland { border-radius: 8px; }

  .dataTables_wrapper { font-size: 0.86rem; }
  table.dataTable thead th {
    background: #003078 !important; color: #fff !important;
    border-bottom: 2px solid #C8A400 !important; font-size: 0.82rem;
  }
  table.dataTable tbody tr:hover { background: #EEF2F7 !important; }

  .app-footer {
    margin-top: 1.5rem; padding: 0.8rem 1rem;
    font-size: 0.76rem; color: #778;
    text-align: center; border-top: 1px solid #D5DCE8;
  }
"

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700&display=swap",
              rel = "stylesheet"),
    tags$style(HTML(custom_css))
  ),

  div(class = "app-header",
    h1(HTML("&#127988;&#917607;&#917602;&#917619;&#917603;&#917620;&#917631; Scottish School League Table")),
    p(class = "subtitle", "Secondary school rankings, catchment areas and contact details \u00b7 Data: 2022\u201323")
  ),

  br(),

  fluidRow(
    # Sidebar
    column(width = 3,
      div(class = "sidebar-card",
        tags$label("Choose a school"),
        br(),
        selectInput("school", NULL, choices = league_table_srtd$school_id, width = "100%"),
        hr(style = "border-color:#D5DCE8; margin: 0.8rem 0;"),
        div(class = "stat-badge",
          div(
            div(style = "font-size:0.78rem; color:#667;", "Local Authority"),
            div(class = "badge-val", textOutput("la_name", inline = TRUE))
          )
        ),
        div(class = "stat-badge",
          div(
            div(style = "font-size:0.78rem; color:#667;", "Best rank (across all years)"),
            div(class = "badge-val", textOutput("best_rank", inline = TRUE))
          )
        )
      )
    ),

    # Main tabs
    column(width = 9,
      tabsetPanel(id = "main_tabs",

        tabPanel("📈  Trend",
          div(class = "tab-content",
            p(style = "color:#556; font-size:0.84rem; margin-bottom:0.8rem;",
              "Ranking over time for the selected school (gold) vs. the current top 5 (grey dashed). Lower rank = better."),
            plotOutput("trend", height = "380px")
          )
        ),

        tabPanel("🗺️  Map",
          div(class = "tab-content",
            p(style = "color:#556; font-size:0.84rem; margin-bottom:0.8rem;",
              "School location (blue) and catchment area (gold shading). Glasgow reference point in black."),
            leafletOutput("scotland", width = "100%", height = "520px")
          )
        ),

        tabPanel("📋  Rankings Table",
          div(class = "tab-content",
            DT::DTOutput("school_table")
          )
        ),

        tabPanel("📬  Contact",
          div(class = "tab-content",
            DT::DTOutput("address_table")
          )
        )
      )
    )
  ),

  div(class = "app-footer",
    HTML("Data: <a href='https://www.datamap-scotland.co.uk' target='_blank'>Datamap Scotland</a> &middot;
          <a href='https://data.spatialhub.scot' target='_blank'>Spatial Hub</a> &middot;
          <a href='https://www.data.gov.uk' target='_blank'>data.gov.uk</a> &middot;
          Contains OS data &copy; Crown copyright and database right 2024")
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  glasgow_coords <- c(-4.2518, 55.8642)

  updateSelectInput(session, "school", selected = "Aberdeen Grammar School | Aberdeen City")

  selected_parts <- reactive({
    parts <- str_split(input$school, " \\| ")[[1]]
    list(name = parts[1], la = parts[2])
  })

  # Sidebar stat outputs
  output$la_name   <- renderText({ selected_parts()$la })
  output$best_rank <- renderText({
    row <- league_table |> filter(school_id == input$school)
    if (nrow(row) == 0) return("N/A")
    vals <- row |> select(where(is.numeric)) |> unlist()
    best <- min(vals, na.rm = TRUE)
    if (is.infinite(best)) "N/A" else as.character(best)
  })

  # Map
  output$scotland <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lat = 56.49, lng = -4.20, zoom = 7) |>
      addCircleMarkers(lng = glasgow_coords[1], lat = glasgow_coords[2],
                       color = "#222", fillColor = "#222", fillOpacity = 0.9,
                       radius = 6, weight = 1, label = "Glasgow (reference)")
  })

  observeEvent(input$school, {
    sp <- selected_parts()

    school_coords <- school_geoloc_tbl |>
      filter(SchoolName == sp$name, LAName == sp$la) |>
      select(Latitude, Longitude)

    selected_poly <- catchment_geo_data_subset |>
      filter(school_name == toupper(sp$name), local_authority == sp$la) |>
      st_transform(crs = 4326)

    proxy <- leafletProxy("scotland") |>
      clearMarkers() |> clearShapes() |>
      addCircleMarkers(lng = glasgow_coords[1], lat = glasgow_coords[2],
                       color = "#222", fillColor = "#222", fillOpacity = 0.9,
                       radius = 6, weight = 1, label = "Glasgow (reference)")

    if (nrow(school_coords) > 0) {
      proxy <- proxy |>
        addCircleMarkers(lng = school_coords$Longitude, lat = school_coords$Latitude,
                         color = scot_blue, fillColor = accent, fillOpacity = 0.95,
                         radius = 8, weight = 2, label = sp$name,
                         popup = paste0("<b>", sp$name, "</b><br>", sp$la)) |>
        flyTo(lng = school_coords$Longitude, lat = school_coords$Latitude, zoom = 12)
    }

    if (nrow(selected_poly) > 0) {
      proxy |> addPolygons(data = selected_poly,
                           fillColor = scot_gold, fillOpacity = 0.25,
                           color = scot_blue, weight = 2, opacity = 0.7,
                           popup = ~school_name, smoothFactor = 1)
    }
  })

  # Trend plot
  rval_school <- reactive({
    league_table |>
      filter(school_id == input$school) |>
      pivot_longer(names_to = "Year", values_to = "Rank",
                   cols = c(-school, -LAName, -school_id))
  })

  output$trend <- renderPlot({
    top5 <- league_table |>
      slice_head(n = 5) |>
      mutate(across(everything(), ~replace_na(.x, 0))) |>
      pivot_longer(names_to = "Year", values_to = "Rank",
                   cols = c(-school, -LAName, -school_id))

    selected     <- rval_school()
    school_label <- selected_parts()$name

    ggplot(top5, aes(x = Year, y = Rank, group = factor(school))) +
      geom_line(linewidth = 0.7, linetype = "dashed", color = "#AABBD0", alpha = 0.9) +
      geom_line(data = selected, aes(group = 1), color = scot_gold, linewidth = 1.6) +
      geom_point(data = selected, aes(group = 1),
                 color = scot_gold, fill = "#fff", shape = 21, size = 3.2, stroke = 1.8) +
      scale_y_reverse(name = "Rank  (lower = better)") +
      scale_x_discrete(name = NULL) +
      labs(title = school_label, subtitle = "Ranking trend vs. top 5 schools (grey dashed)") +
      theme_minimal(base_size = 13) +
      theme(
        plot.title       = element_text(face = "bold", color = scot_navy, size = 14),
        plot.subtitle    = element_text(color = "#556", size = 10, margin = margin(b = 10)),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#E0E8F0"),
        axis.text        = element_text(color = "#445"),
        axis.title.y     = element_text(color = "#445", size = 10),
        plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
  }, bg = "white")

  # Tables
  output$school_table <- DT::renderDT({
    league_table |>
      DT::datatable(options = list(pageLength = 15, dom = "frtip", scrollX = TRUE),
                    rownames = FALSE, class = "stripe hover")
  })

  output$address_table <- DT::renderDT({
    address_tbl |>
      filter(School == selected_parts()$name) |>
      DT::datatable(options = list(dom = "t", paging = FALSE),
                    rownames = FALSE, class = "stripe")
  })
}

shinyApp(ui = ui, server = server)

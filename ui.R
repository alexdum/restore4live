source("sections/ui_climate_scenario.R")
source("sections/ui_area_of_interest.R")
ui <- 
  page_navbar(
    title = "Danube basin",
    fillable_mobile = T,
   shinyjs::useShinyjs(),
   # selected = "About",
    nav_panel(
      title = "Climate scenario",
      climate_scenario
    ),
    nav_panel(
      title = "Remote Sensing",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          uiOutput("timestep_selector"),
          sliderInput(
            "trans_rs",
            span(
              "Transparency",
              tooltip(
                bs_icon("info-circle"),
                "Select raster opacity"
              )
            ),
            min = 0,
            max = 1,
            value = 0.8,
            step = 0.1,
            ticks = F
            
          )
        ),
        layout_columns(
          # col_widths = c(8, 4),
          card( # First card for the map
            full_screen = TRUE,
            card_header("NDVI Map"), # Changed header
            leafletOutput("ndvi_map")
          ),
          card( # Second card for the timeseries plot
            full_screen = TRUE,
            card_header("Timeseries Plot"),
            highchartOutput("timeseries_plot")
          )
        )
      )
    ),
    nav_panel(
      title = "AoI",
      area_of_interest_ui,
      icon = icon("map-marked-alt"),
    ),
    nav_panel(
      class = "bslib-page-dashboard",
      "About",
      htmlOutput("about")
    )
  )

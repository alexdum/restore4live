source("sections/ui_home.R")
source("sections/ui_climate_scenario.R")
source("sections/ui_area_of_interest.R")
source("sections/ui_climate_observational.R")
ui <-
  page_navbar(
    id = "navbar",
    title = actionLink("nav_brand", "Danube basin", class = "navbar-brand"),
    fillable_mobile = FALSE,
    shinyjs::useShinyjs(),
    nav_panel(
      title = "Home",
      value = "home",
      ui_home
    ),
    header = tagList(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
        tags$script(src = "app.js")
      ),
      tags$div(
        class = "frozen-overlay",
        tags$div(
          class = "frozen-overlay-content",
          tags$button(class = "frozen-overlay-close", icon("times")),
          tags$div(class = "spinner-border text-primary", role = "status"),
          tags$div(class = "frozen-overlay-station", ""),
          tags$p(class = "frozen-overlay-message", "Loading data...")
        )
      )
    ),
    # selected = "About",
    nav_panel(
      title = "Climate scenario",
      value = "climate_scenario",
      icon = icon("chart-line"),
      climate_scenario
    ),
    nav_panel(
      title = "Hydrological scenario",
      value = "hydrological_scenario",
      icon = icon("water"),
      hydrological_scenario
    ),
    nav_panel(
      title = "Climate observational data",
      value = "climate_observational",
      icon = icon("temperature-high"),
      climate_observational
    ),
    nav_panel(
      title = "Remote Sensing",
      value = "remote_sensing",
      icon = icon("satellite"),
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
      value = "aoi",
      area_of_interest_ui,
      icon = icon("map-marked-alt"),
    ),
    nav_panel(
      class = "bslib-page-dashboard",
      title = "About",
      value = "about",
      icon = icon("info-circle"),
      htmlOutput("about")
    )
  )

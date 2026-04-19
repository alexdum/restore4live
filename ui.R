source("sections/ui_home.R")
source("sections/ui_climate_scenario.R")
source("sections/ui_area_of_interest.R")
source("sections/ui_climate_observational.R")
source("sections/ui_remote_sensing.R")
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
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css?v=2.4"),
        tags$script(src = "app.js?v=1.2")
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
      remote_sensing_ui
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

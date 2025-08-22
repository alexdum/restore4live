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
      title = "Area of Interest",
      area_of_interest_ui
    ),
    nav_panel(
      title = "Remote Sensing",
      p("TBA")
    ),
    nav_panel(
      class = "bslib-page-dashboard",
      "About",
      htmlOutput("about")
    )
  )

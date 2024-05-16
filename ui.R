source("sections/ui_climate_scenario.R")
ui <- 
  page_navbar(
    title = "Danube basin",
    fillable_mobile = T,
    nav_panel(
      title = "Climate scenario",
      climate_scenario
    ),
    nav_panel(
      title = "Remote Sensing",
      p("TBA")
    )
  )

server <- function(input, output, session) {
  # Hide Home tab programmatically on server start
  shinyjs::runjs("$('.navbar-nav .nav-link').filter(function() { return $(this).text().trim() === 'Home'; }).parent().hide();")

  observeEvent(input$nav_brand, {
    nav_select("navbar", "home")
  })

  observeEvent(input$link_climate_scenario, {
    nav_select("navbar", "climate_scenario")
  })

  observeEvent(input$link_hydrological_scenario, {
    nav_select("navbar", "hydrological_scenario")
  })

  observeEvent(input$link_climate_observational, {
    nav_select("navbar", "climate_observational")
  })

  observeEvent(input$link_remote_sensing, {
    nav_select("navbar", "remote_sensing")
  })

  observeEvent(input$link_aoi, {
    nav_select("navbar", "aoi")
  })

  observeEvent(input$link_about, {
    nav_select("navbar", "about")
  })

  source(file = "sections/server_climate_scenario.R", local = T)
  source(file = "sections/server_climate_observational.R", local = T)
  source(file = "sections/server_rs.R", local = T)
  source(file = "sections/server_area_of_interest.R", local = T)
  source(file = "sections/server_hydrological_scenario.R", local = T)
  source(file = "sections/server_about.R", local = T)
}

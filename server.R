server <- function(input, output) {
  source(file = "sections/server_climate_scenario.R", local = T)
  source(file = "sections/server_climate_observational.R", local = T)
  source(file = "sections/server_rs.R", local = T)
  source(file = "sections/server_area_of_interest.R", local = T)
  source(file = "sections/server_about.R", local = T)
}

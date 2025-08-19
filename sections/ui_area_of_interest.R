area_of_interest_ui <- layout_sidebar(
  sidebar = sidebar(
    title = "Area of Interest Controls",
    selectInput(
      inputId = "aoi_area",
      label = "Select an area:",
      choices = select_area,
      selected = "at1"
    ),
    card(
      card_header("Information"),
      card_body("This section displays annual time series plots for all available climate parameters and scenarios for the selected area of interest. Please note that generating all plots may take a moment, especially for larger areas.")
    )
  ),
  # A placeholder for the dynamic grid of plots
  uiOutput("aoi_plots_grid")
)
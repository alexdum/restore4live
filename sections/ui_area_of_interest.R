area_of_interest_ui <- layout_sidebar(
  sidebar = sidebar(
    title = "Area of Interest Controls",
    radioButtons(
      inputId = "aoi_selection_method",
      label = "Select Area of Interest By:",
      choices = c("Predefined Area" = "predefined", "Draw on Map" = "draw"),
      selected = "predefined"
    ),
    uiOutput("aoi_predefined_selection_ui"), # Placeholder for conditional UI
    div(id = "clear_drawn_area_div",
        actionButton("clear_drawn_area", "Draw New Area", class = "btn-warning w-100 mb-3")
    ),
    radioButtons(
      "report_format",
      label = "Select Report Format:",
      choices = list("HTML" = "html", "Word (DOCX)" = "docx"),
      selected = "html",
      inline = TRUE
    ),
    downloadButton(
      "download_aoi_report",
      "Download Report",
      class = "btn-primary w-100 mb-3"
    ),
    card(
      card_header("Information"),
      card_body(
        "This section displays annual time series plots for all available climate parameters and scenarios for the selected area of interest. Please note that generating all plots may take a moment, especially for larger areas."
      )
    )
  ),
  card(id = "aoi_map_card",
       leafletOutput("aoi_map", height = "400px"),
       helpText("Draw a polygon on the map to define your area of interest. Only the last drawn polygon will be used.")
  ),
  # A placeholder for the dynamic grid of plots
  uiOutput("selected_area_display_name"),
  uiOutput("aoi_plots_grid")
)

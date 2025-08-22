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
    uiOutput("aoi_map_ui"), # Placeholder for conditional UI
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
  # A placeholder for the dynamic grid of plots
  uiOutput("aoi_plots_grid")
)

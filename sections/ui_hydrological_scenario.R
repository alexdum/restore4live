hydrological_scenario_sidebar <- list(
  selectInput(
    inputId = "hydro_area",
    label = span(
      "Area of interest",
      tooltip(
        bs_icon("info-circle"),
        "Choose the spatial context for hydrological analysis. Danube River Basin keeps the chart in point mode, while selecting a specific area zooms to that polygon and switches the chart to an area-average series."
      )
    ),
    choices = select_area,
    selected = "drb"
  ),
  selectInput(
    "hydro_param", "Parameter",
    c("River Discharge" = "rdis")
  ),
  selectInput(
    "hydro_quant",
    span(
      "Quantity",
      tooltip(
        bs_icon("info-circle"),
        "Compute either the climatology or the change versus the baseline for the selected parameter."
      )
    ),
    c(
      "Climatology" = "climate",
      "Change" = "change"
    )
  ),
  selectInput(
    "hydro_scen",
    span(
      "Scenario",
      tooltip(
        bs_icon("info-circle"),
        "Different hydrological forcing scenario (RCPs)."
      )
    ),
    c(
      "RCP2.6" = "rcp_2_6",
      "RCP4.5" = "rcp_4_5",
      "RCP8.5" = "rcp_8_5"
    )
  ),
  selectInput(
    inputId = "hydro_season",
    label = "Season",
    choices = c("Annual" = "ANN")
  ),
  conditionalPanel(
    condition = "input.hydro_quant == 'climate'",
    sliderTextInput(
      inputId = "hydro_period_climate",
      span(
        "Climate",
        tooltip(
          bs_icon("info-circle"),
          "Aggregated values of the selected parameter for the chosen period."
        )
      ),
      selected = c("2041-2060"),
      choices = c("1971-2000", "2021-2040", "2041-2060", "2081-2100")
    )
  ),
  conditionalPanel(
    condition = "input.hydro_quant == 'change'",
    sliderTextInput(
      inputId = "hydro_period_change",
      span(
        "Change",
        tooltip(
          bs_icon("info-circle"),
          "Mean differences between the selected future period and the baseline period."
        )
      ),
      selected = c("1971-2000", "2041-2060"),
      choices = c("1971-2000", "2021-2040", "2041-2060", "2081-2100"),
      from_min = "1971-2000",
      from_max = "1971-2000",
      to_min = "2021-2040",
      to_max = "2081-2100"
    )
  ),
  sliderInput(
    "hydro_transp",
    span(
      "Transparency",
      tooltip(
        bs_icon("info-circle"),
        "Select raster opacity."
      )
    ),
    min = 0, max = 1, ticks = FALSE,
    value = 0.8, step = 0.1
  )
)


hydrological_scenario <-
  div(
    class = "climate-scenario-page",
    layout_sidebar(
      sidebar = sidebar(
        hydrological_scenario_sidebar
      ),
      div(
        class = "climate-scenario-main",
        div(
          class = "climate-scenario-shared-title",
          textOutput("hydro_map_titl")
        ),
        div(
          class = "climate-scenario-layout",
          div(
            class = "climate-scenario-map-slot",
            card(
              class = "climate-scenario-map-card",
              full_screen = TRUE,
              div(
                class = "climate-scenario-map-panel",
                mapgl::maplibreOutput("hydro_map", height = "100%"),
                absolutePanel(
                  top = 130,
                  left = 10,
                  class = "map-layer-control",
                  style = "z-index: 1000;",
                  div(class = "control-icon", icon("layer-group")),
                  div(
                    class = "control-content",
                    radioButtons(
                      inputId = "basemap_hydro",
                      label = "Basemap",
                      choices = c(
                        "OpenFreeMap Positron" = "ofm_positron",
                        "OpenFreeMap Bright" = "ofm_bright",
                        "Satellite (Sentinel-2)" = "sentinel"
                      ),
                      selected = "ofm_positron"
                    ),
                    hr(style = "margin: 8px 0;"),
                    checkboxInput(
                      inputId = "show_labels_hydro",
                      label = "Show Labels",
                      value = TRUE
                    )
                  )
                ),
                absolutePanel(
                  id = "zoom_home_hydro_panel",
                  top = 80,
                  left = 10,
                  style = "z-index: 1000;",
                  actionButton(
                    "zoom_home_hydro",
                    bsicons::bs_icon("house-fill"),
                    class = "btn-home",
                    title = "Zoom to Danube basin"
                  )
                )
              )
            )
          ),
          div(
            class = "climate-scenario-side-column",
            div(
              class = "climate-scenario-plot-slot",
              card(
                class = "climate-scenario-plot-card",
                full_screen = TRUE,
                div(
                  class = "climate-scenario-chart-panel",
                  highchartOutput("hydro_chart_scen", height = "100%")
                )
              )
            ),
            div(
              class = "climate-scenario-stats-slot",
              card(
                class = "climate-scenario-stats-card",
                div(
                  class = "climate-scenario-info-panel",
                  uiOutput("hydro_context_panel")
                )
              )
            )
          )
        )
      )
    )
  )

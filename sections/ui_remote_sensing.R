remote_sensing_sidebar <- list(
  uiOutput("timestep_selector"),
  sliderInput(
    "trans_rs",
    span(
      "Transparency",
      tooltip(
        bs_icon("info-circle"),
        "Select raster opacity."
      )
    ),
    min = 0,
    max = 1,
    value = 0.8,
    step = 0.1,
    ticks = FALSE
  ),
  div(
    class = "rs-sidebar-export",
    div(
      class = "rs-sidebar-export-label",
      span("Export time series"),
      tooltip(
        bs_icon("info-circle"),
        "Exports the currently displayed NDVI timeseries for the selected point as a CSV file with date, NDVI, longitude, and latitude columns."
      )
    ),
    uiOutput("rs_download_control")
  )
)


remote_sensing_ui <-
  div(
    class = "climate-scenario-page",
    layout_sidebar(
      sidebar = sidebar(
        remote_sensing_sidebar
      ),
      div(
        class = "climate-scenario-main",
        div(
          class = "climate-scenario-shared-title",
          textOutput("rs_map_titl")
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
                mapgl::maplibreOutput("ndvi_map", height = "100%"),
                absolutePanel(
                  top = 130,
                  left = 10,
                  class = "map-layer-control",
                  style = "z-index: 1000;",
                  div(class = "control-icon", icon("layer-group")),
                  div(
                    class = "control-content",
                    radioButtons(
                      inputId = "basemap_rs",
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
                      inputId = "show_labels_rs",
                      label = "Show Labels",
                      value = TRUE
                    )
                  )
                ),
                absolutePanel(
                  id = "zoom_home_rs_panel",
                  top = 80,
                  left = 10,
                  style = "z-index: 1000;",
                  actionButton(
                    "zoom_home_rs",
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
                  highchartOutput("timeseries_plot", height = "100%")
                )
              )
            ),
            div(
              class = "climate-scenario-stats-slot",
              card(
                class = "climate-scenario-stats-card",
                div(
                  class = "climate-scenario-info-panel",
                  uiOutput("rs_context_panel")
                )
              )
            )
          )
        )
      )
    )
  )

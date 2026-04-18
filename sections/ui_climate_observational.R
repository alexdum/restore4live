climate_observational_sidebar <- list(
    selectInput(
        inputId = "test_area_obs",
        label = "Area of interest",
        choices = select_area,
        selected = "drb"
    ),
    div(
        style = "margin-top: 20px; padding-top: 10px; border-top: 1px solid #ccc;",
        tags$h6(icon("info-circle"), " Usage Info"),
        tags$p(
            style = "font-size: 0.9em;",
            "1. Click a station marker on the map.", tags$br(),
            "2. View detailed data in the Dashboard tab."
        ),
        tags$hr(),
        tags$h6(icon("database"), " Data info"),
        tags$p(
            style = "font-size: 0.9em;",
            "The data represents monthly climatological reports (CLIMAT) from weather stations, including mean temperature, total precipitation, and other parameters. See more info in the About section."
        )
    )
)


climate_observational <-
    layout_sidebar(
        sidebar = sidebar(
            climate_observational_sidebar
        ),
        tabsetPanel(
            id = "obs_tab",
            tabPanel(
                title = "Map View",
                div(
                    class = "obs-map-wrap",
                    style = "height:85vh; position:relative;",
                    mapgl::maplibreOutput("map_obs", width = "100%", height = "100%"),
                    absolutePanel(
                        top = 130,
                        left = 10,
                        class = "map-layer-control",
                        style = "z-index: 1000;",
                        div(class = "control-icon", icon("layer-group")),
                        div(
                            class = "control-content",
                            radioButtons(
                                inputId = "basemap_obs",
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
                                inputId = "show_labels_obs",
                                label = "Show Labels",
                                value = TRUE
                            )
                        )
                    ),
                    absolutePanel(
                        id = "zoom_home_obs_panel",
                        top = 80,
                        left = 10,
                        style = "z-index: 1000;",
                        actionButton(
                            "zoom_home_obs",
                            bsicons::bs_icon("house-fill"),
                            class = "btn-home",
                            title = "Zoom to Danube basin"
                        )
                    )
                )
            ),
            tabPanel(
                title = "Station Info",
                DT::dataTableOutput("stations_table")
            ),
            tabPanel(
                "Dashboard",
                card(
                    full_screen = TRUE,
                    card_header(
                        div(
                            class = "d-flex justify-content-between align-items-center w-100",
                            uiOutput("obs_station_meta"),
                            downloadButton("download_obs_data", "Download CSV", class = "btn-sm btn-primary")
                        )
                    ),
                    bslib::navset_card_pill(
                        nav_panel(
                            "Plots",
                            layout_columns(
                                col_widths = breakpoints(sm = 12, md = 6, lg = 6, xl = 6),
                                row_heights = c("32vh", "32vh"),
                                card(
                                    full_screen = TRUE,
                                    card_header("Temperature"),
                                    plotly::plotlyOutput("obs_plot_temp", height = "100%")
                                ),
                                card(
                                    full_screen = TRUE,
                                    card_header("Precipitation"),
                                    plotly::plotlyOutput("obs_plot_precip", height = "100%")
                                ),
                                card(
                                    full_screen = TRUE,
                                    card_header("Sunshine Duration"),
                                    plotly::plotlyOutput("obs_plot_sun", height = "100%")
                                ),
                                card(
                                    full_screen = TRUE,
                                    card_header("Pressure"),
                                    plotly::plotlyOutput("obs_plot_pressure", height = "100%")
                                )
                            )
                        ),
                        nav_panel(
                            "Data",
                            DT::dataTableOutput("weather_data_table")
                        )
                    )
                )
            )
        )
    )

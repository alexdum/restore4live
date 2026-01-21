climate_observational_sidebar <- list(
    selectInput(
        inputId = "test_area_obs",
        label = "Area of interest",
        choices = select_area,
        selected = "drb"
    ),
    div(
        style = "margin-top: 20px; padding-top: 10px; border-top: 1px solid #ccc;",
        tags$h5(icon("info-circle"), " Usage Info"),
        tags$p(
            style = "font-size: 0.9em;",
            "1. Click a station marker on the map.", tags$br(),
            "2. View detailed data in the Dashboard tab."
        ),
        tags$hr(),
        tags$h5(icon("database"), " Data info"),
        tags$p(
            style = "font-size: 0.9em;",
            "The data represents monthly climatological reports (CLIMAT) from weathet stations, including mean temperature, total precipitation, and other parameters."
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
                div(style = "height:85vh;", leafletOutput("map_obs", width = "100%", height = "100%"))
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
                            style = "display: flex; justify-content: space-between; align_items: center;",
                            uiOutput("obs_station_meta"),
                            downloadButton("download_obs_data", "Download CSV", class = "btn-sm btn-primary")
                        )
                    ),
                    bslib::navset_card_pill(
                        nav_panel(
                            "Plots",
                            layout_columns(
                                col_widths = c(6, 6),
                                row_heights = c("calc(50vh - 130px)", "calc(50vh - 130px)"),
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

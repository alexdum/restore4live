hydrological_scenario_sidebar <- list(
    selectInput(
        inputId = "hydro_area",
        label = "Area of interest",
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
                "Compute either the climatology or the change versus the baseline for a given parameter"
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
                "Different emission scenario (RCPs)"
            )
        ),
        c(
            "RCP2.6" = "rcp_2_6",
            "RCP4.5" = "rcp_4_5",
            "RCP8.5" = "rcp_8_5"
        )
    ),

    # Only annual season is available for now based on user description
    selectInput(
        inputId = "hydro_season",
        label = "Season",
        choices = c("Annual" = "ANN")
    ),

    # afiseaza inputul conditional
    conditionalPanel(
        condition = "input.hydro_quant == 'climate'",
        sliderTextInput(
            inputId = "hydro_period_climate",
            span(
                "Climate",
                tooltip(
                    bs_icon("info-circle"),
                    "Aggregated values of the selected parameter for the given period"
                )
            ),
            selected = c("2041-2060"),
            choices = c("1971-2000", "2021-2040", "2041-2060", "2081-2100")
        ), # Adjusted choices based on likely data availability, will need to verify
    ),
    conditionalPanel(
        condition = "input.hydro_quant == 'change'",
        sliderTextInput(
            inputId = "hydro_period_change",
            span(
                "Change",
                tooltip(
                    bs_icon("info-circle"),
                    "Mean differences between the values of the reference period and a selected baseline"
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
                "Select raster opacity"
            )
        ),
        min = 0, max = 1, ticks = F,
        value = 0.8, step = 0.1
    )
)


hydrological_scenario <-
    layout_sidebar(
        sidebar = sidebar(
            hydrological_scenario_sidebar
        ),
        layout_columns(
            # width = 1/2,
            # heights_equal = "row",
            # Show results
            card(
                card_header(textOutput("hydro_map_titl")),
                full_screen = T,
                leafletOutput("hydro_map", height = "450px")
            ),
            card(
                card_header(textOutput("hydro_graph_titl")),
                full_screen = T,
                highchartOutput("hydro_chart_scen", height = "450px")
            )
        )
    )

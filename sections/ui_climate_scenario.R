climate_scenario_sidebar <- list(
    selectInput(
    inputId = "test_area",
    label = "Area of interest",
    choices = select_area,
    selected = "drb"),
  selectInput(
    "param", "Parameter",
    c("Mean temperature" = "tas",
      "Maximum temperature" = "tasmax",
      "Minimum temperature" = "tasmin",
      "Precipitation" = "pr",
      "Tropical nights" = "tr")),
  selectInput(
    "quant", 
    span(
      "Quantity",
      tooltip(
        bs_icon("info-circle"),
        "Compute either the climatology or the change versus the baseline for a given parameter"
      )
    ), 
    c("Climatology" = "climate",
      "Change" = "change")),
  selectInput(
    "scen", 
    span(
      "Scenario",
      tooltip(
        bs_icon("info-circle"),
        "Different emission scenario (see About section)"
      )
    ), 
    c("SSP1" = "ssp1",
      "SSP2" = "ssp2",
      "SSP3" = "ssp3",
      "SSP5" = "ssp5")),

  # dipslay only annual if indices are selected
  conditionalPanel(
    condition = "input.param == 'tr'",
    selectInput(inputId = "season_ind", label = "Season", 
    choices = c("Annual" = "ANN")
  )
),
    # display seasons if no indices are selected
  conditionalPanel(
    condition = "input.param != 'tr'",
    selectInput(
    inputId = "season",
    label = "Season",
    choices = select_seas,
    selected =  select_seas[17])
  ),
  # afiseaza inputul conditional
  conditionalPanel(
    condition = "input.quant == 'climate'",
    sliderTextInput(
      inputId = "period_climate",
      span(
        "Climate",
        tooltip(
          bs_icon("info-circle"),
          "Aggregated values of the selected parameter for the given period"
        )
      ), 
      selected = c("2041-2060"),
      choices = c("1961-1990", "1981-2010", "2021-2040", "2041-2060", "2081-2100")),
  ),
  conditionalPanel(
    condition = "input.quant == 'change'",
    sliderTextInput(
      inputId = "period_change",
      span(
        "Change",
        tooltip(
          bs_icon("info-circle"),
          "Mean differences between the values of the reference period and a selected baseline"
        )
      ), 
      selected = c("1981-2010", "2041-2060"),
      choices = c("1961-1990", "1981-2010", "2021-2040", "2041-2060", "2081-2100"),
      from_min = "1961-1990", 
      from_max = "1981-2010",
      to_min =  "2021-2040",
      to_max = "2081-2100")
  ),
  
  sliderInput(
    "transp",
    span(
      "Transparency",
      tooltip(
        bs_icon("info-circle"),
        "Select raster CMIP6 opacity"
      ) 
    ),
    min = 0, max = 1, ticks = F,
    value = 0.8, step = 0.1)
)



climate_scenario <-
  
  layout_sidebar(
    sidebar = sidebar(
      climate_scenario_sidebar
    ),
    layout_columns(
      # width = 1/2,
      # heights_equal = "row",
      # Show results
      card(
        card_header(textOutput("map_titl")),
        full_screen = T,
        leafletOutput("map", height = "450px")
      ),
      card(
        card_header(textOutput("graph_titl")),
        full_screen = T,
        highchartOutput("chart_scen", height = "450px")
      )
    )
  )



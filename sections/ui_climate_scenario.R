climate_scenario_sidebar <- list(
  selectInput(
    "param", "Parameter",
    c("Mean temperature" = "tas",
      "Maximum temperature" = "tasmax",
      "Minimum temperature" = "tasmin",
      "Precipitation" = "pr")),
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
        "Different emission scenario acoording to https://en.wikipedia.org/wiki/Shared_Socioeconomic_Pathways"
      )
    ), 
    c("SSP1" = "ssp1",
      "SSP2" = "ssp2",
      "SSP3" = "ssp3",
      "SSP5" = "ssp5")),
  selectInput(
    inputId = "season",
    label = "Season",
    choices = select_seas,
    selected =  select_seas[17]),
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
      selected = c("1981-2010"),
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
      selected = c("1981-2010", "2021-2040"),
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
    # Show results
    card(
      card_header(textOutput("map_titl")),
      full_screen = T,
      leafletOutput("map")
    )
  )



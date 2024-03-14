
ui <- 
  page_fillable(
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          "param", "Parameter",
          c("Mean temperature" = "tas",
            "Maximum temperature" = "tasmax",
            "Minimum temperature" = "tasmin",
            "Precipitation" = "pr")),
        selectInput(
          "quant", "Quantity",
          c("Climatology" = "climate",
            "Change" = "change")),
        selectInput(
          "scen", "Scenario",
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
            label = "Climate", 
            selected = c("1981-2010"),
            choices = c("1961-1990", "1981-2010", "2021-2040", "2041-2060", "2081-2100")),
        ),
        conditionalPanel(
          condition = "input.quant == 'change'",
          sliderTextInput(
            inputId = "period_change",
            label = "Change", 
            selected = c("1981-2010", "2021-2040"),
            choices = c("1961-1990", "1981-2010", "2021-2040", "2041-2060", "2081-2100"),
            from_min = "1961-1990", 
            from_max = "1981-2010",
            to_min =  "2021-2040",
            to_max = "2081-2100")
        ),
        
        sliderInput(
          "transp", "Transparency",
          min = 0, max = 1, ticks = F,
          value = 0.8, step = 0.1)),
      # Show results
      card(
        card_header(textOutput("map_titl")),
        full_screen = T,
        leafletOutput("map")
      )
      
      
      # )
    )
  )
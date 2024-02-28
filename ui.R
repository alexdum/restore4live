
ui <- 
  page_fillable(
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          "param", "Parameter",
          c("Mean temperature" = "tas",
            "Maximum temperature" = "tasmax",
            "Minimum temperature" = "tasmin",
            "Precipitation" = "pr")
          ),
        selectInput(
          "quant", "Quantity",
          c("Climatology" = "climate",
            "Change" = "change")
        ),
        selectInput(
          "scen", "Scenario",
          c("Historical" = "hist",
            "SSP1" = "ssp1",
            "SSP2" = "ssp2",
            "SSP3" = "ssp3",
            "SSP5" = "ssp5")
        ),
        selectInput(
          inputId = "season",
          label = "Season",
          choices = select_seas,
          selected =  select_seas[17]
        ),
        
        sliderInput(
          "transp", "Transparency",
          min = 0, max = 1, ticks = F,
          value = 0.8, step = 0.1,
        )
      ),
      # Show results
      card(
        full_screen = T,
        leafletOutput("map")
      )
      
      
      # )
    )
  )
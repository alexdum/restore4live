data_sel <- reactive({
  prepare_climate_data(
    param = input$param,
    season = input$season,
    season_ind = input$season_ind,
    scen = input$scen,
    quant = input$quant,
    period_climate = input$period_climate,
    period_change = input$period_change,
    transp = input$transp,
    files_cmip6 = files_cmip6,
    params_def = params_def,
    select_seas = select_seas
  )
})


# functie leaflet de start
output$map <- renderLeaflet({
  leaflet_fun()
})

# update leaflet outputuri
observe({
  req(input$navbar == "climate_scenario")

  r <- data_sel()$r
  pal <- data_sel()$pal
  min_max <- data_sel()$min_max
  opacy <- data_sel()$opacy

  leafletProxy("map") |>
    clearImages() |>
    addRasterImage(r, opacity = opacy, color = pal$pal) |>
    clearControls() |>
    addLegend(
      title = pal$tit_leg,
      position = "bottomright",
      opacity = opacy,
      pal = pal$pal_rev, values = min_max,
      labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
    )
})


# zoom to selected area
observeEvent(input$test_area, {
  req(input$test_area)

  shape_to_zoom <- switch(input$test_area,
    "drb" = dun,
    "at1" = is1_austria[1, ],
    "at2" = is1_austria[2, ],
    "at3" = is1_austria[3, ],
    "at4" = is1_austria[4, ],
    "at5" = is1_austria[5, ],
    "sk1" = is2_slovakia,
    "rs1" = is3_serbia,
    "ro1" = is4_romania,
    "de1" = ms1_germany,
    "sk2" = ms2_slovakia,
    "rs2" = ms3_serbia,
    "ro2" = ms4_romania,
    "ro3" = ms5_romania,
    "ro4" = ms6_romania
  )


  bbox <- sf::st_bbox(sf::st_buffer(shape_to_zoom, dist = 10000))

  proxy <- leafletProxy("map") %>%
    removeShape(layerId = "highlighted_polygon")


  if (input$test_area != "drb") {
    print(shape_to_zoom)
    proxy <- proxy %>%
      addPolygons(
        data = shape_to_zoom,
        layerId = "highlighted_polygon",
        fillColor = "yellow",
        fillOpacity = 0.5,
        color = "orange",
        weight = 3,
        stroke = TRUE,
        label = names(select_area[select_area == input$test_area]),
        labelOptions = labelOptions(
          style = list(
            "color" = "black",
            "font-family" = "Arial",
            "font-weight" = "bold",
            "font-size" = "12px"
          ),
          textsize = "12px",
          direction = "auto"
        )
      )
  }

  proxy %>%
    fitBounds(
      lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
      lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
    )
})


output$map_titl <- renderText({
  param <- data_sel()$param_name
  season <- data_sel()$season_name
  if (input$quant %in% "climate") {
    paste(param, season, toupper(input$scen), " - multiannual mean", input$period_climate)
  } else {
    paste(param, season, toupper(input$scen), " - change in multiannual mean", input$period_change[2], "vs.", input$period_change[1])
  }
})

# reactive values pentru plot lst time series din raster
values_plot_na <- reactiveValues(input = NULL, title = NULL, lon = 25, lat = 46, mode = "point")

observeEvent(input$test_area, {
  if (input$test_area %in% c("at1", "at2", "at3", "at4", "at5", "sk1", "rs1", "ro1", "de1", "sk2", "rs2", "ro2", "ro3", "ro4")) {
    values_plot_na$mode <- "zonal"
  } else {
    values_plot_na$mode <- "point"
  }
})

# interactivitate raster
observeEvent(input$map_click, {
  req(input$map_click)
  # Do not trigger point extraction when a country is selected
  if (input$test_area %in% "drb") {
    values_plot_na$mode <- "point"
    values_plot_na$lon <- input$map_click$lng
    values_plot_na$lat <- input$map_click$lat
  }
})

observe({
  # Make it depend on all relevant inputs
  req(input$param, input$scen, input$season, input$quant, input$period_change, input$period_climate, values_plot_na$mode)

  if (values_plot_na$mode == "zonal") {
    req(input$test_area %in% c("drb", "at1", "at2", "at3", "at4", "at5", "sk1", "rs1", "ro1", "de1", "sk2", "rs2", "ro2", "ro3", "ro4"))
    shape_to_extract <- switch(input$test_area,
      "at1" = is1_austria[1, ],
      "at2" = is1_austria[2, ],
      "at3" = is1_austria[3, ],
      "at4" = is1_austria[4, ],
      "at5" = is1_austria[5, ],
      "sk1" = is2_slovakia,
      "rs1" = is3_serbia,
      "ro1" = is4_romania,
      "de1" = ms1_germany,
      "sk2" = ms2_slovakia,
      "rs2" = ms3_serbia,
      "ro2" = ms4_romania,
      "ro3" = ms5_romania,
      "ro4" = ms6_romania
    )
    area_choices <- c(
      "Danube River Basin" = "drb", "Austria" = "at1", "Austria" = "at2", "Austria" = "at3", "Austria" = "at4", "Slovakia" = "sk1", "Serbia" = "rs1", "Romania" = "ro1", "Germany" = "de1",
      "Slovakia" = "sk2", "Serbia" = "rs2", "Romania" = "ro2", "Romania" = "ro3", "Romania" = "ro4"
    )

    country_name <- names(area_choices)[area_choices == input$test_area]

    ddf <- extract_zonal_data(
      file_hist = data_sel()$file_hist,
      file_scen = data_sel()$file_scen,
      shape = shape_to_extract,
      season_subset = data_sel()$season_subset,
      param = input$param,
      quant = input$quant,
      period_change = input$period_change,
      file_ind = data_sel()$file_ind
    )

    values_plot_na$input <- ddf
    values_plot_na$title <- paste(data_sel()$param_name, "for", country_name, "test area", toString(shape_to_extract$Name))
  } else {
    lon <- values_plot_na$lon
    lat <- values_plot_na$lat


    ddf <- extract_data(data_sel()$file_hist, data_sel()$file_scen, extract_point, lon, lat, input$param, data_sel()$season_subset, input$quant, input$period_change, input$period_climate, data_sel()$file_ind)


    if (is.data.frame(ddf)) {
      if (!all(is.na(ddf$value))) {
        values_plot_na$input <- ddf
        values_plot_na$title <- graph_title_climate(data_sel()$param_name, input$quant, input$param, input$period_change, lon, lat)
      } else {
        values_plot_na$input <- "No data available for the selected point"
      }
    } else {
      values_plot_na$input <- ddf
    }
  }
})


output$chart_scen <- renderHighchart({
  create_timeseries_chart(
    data_input = values_plot_na$input,
    param = input$param,
    params_def = params_def
  )
})


output$graph_titl <- renderText({
  values_plot_na$title
})

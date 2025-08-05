data_sel <- reactive({
  
  param <- input$param
  
  season <- strsplit(input$season, "-")[[1]][2]
  season_subset <- strsplit(input$season, "-")[[1]][1]
  
  # uneste istricul cu scenariiul
  file_hist <-  grep(paste0("www/data/ncs/cmip6/",input$param,"/hist/",input$param,"_hist_",season, "-50_"), files_cmip6, value = T)
  file_scen <- grep(paste0("www/data/ncs/cmip6/",input$param,"/",input$scen,"/",input$param,"_",input$scen,"_",season, "-50_"), files_cmip6, value = T)
  r <- c(rast(file_hist), rast(file_scen))
  dats <- time(r)
  
  # subseteaza sezoniere si lunare
  if (season_subset != "year") r <- r[[which(format(dats, "%m") %in% season_subset)]]
  
  # pentru subsetare harta
  dats_sub <- time(r)
  
  # pentru selectie tip de calcul
  if (input$quant %in% "climate") {
    
    # pentru subsetare
    an1 <- strsplit(input$period_climate, "-")[[1]][1] |> as.numeric()
    an2 <- strsplit(input$period_climate, "-")[[1]][2] |> as.numeric()
    
    # mediaza duop input perioada
    r_mean <- mean(r[[format(dats_sub, "%Y") %in% an1:an2]])
    
    setMinMax(r_mean)
    
    
  } else {
    
    # pentru subsetare
    an1_hist <- strsplit(input$period_change[1], "-")[[1]][1] |> as.numeric()
    an2_hist <- strsplit(input$period_change[1], "-")[[1]][2] |> as.numeric()
    an1_scen <- strsplit(input$period_change[2], "-")[[1]][1] |> as.numeric()
    an2_scen <- strsplit(input$period_change[2], "-")[[1]][2] |> as.numeric()
    
    
    # mediaza duop input
    r_hist <- mean(r[[format(dats_sub, "%Y") %in% an1_hist:an2_hist]])
    r_scen <- mean(r[[format(dats_sub, "%Y") %in% an1_scen:an2_scen]])
    
    # schimbarea
    if (input$param %in% "pr") {
      r_mean <- ((r_scen * 100)/r_hist) - 100
    } else {
      r_mean <- r_scen - r_hist
    }
    setMinMax(r_mean)
    
    
  }
  
  
  pal <- map_cols_cmip_fun(indic = input$param, type = input$quant, domain = minmax(r_mean))
  # pentru reclasificare extreme raster cu min/max din paleta de culori
  r_mean[r_mean < pal$minmax[1]] <- pal$minmax[1]
  r_mean[r_mean > pal$minmax[2]] <- pal$minmax[2]
  pal <- map_cols_cmip_fun(indic = input$param, type = input$quant, domain = minmax(r_mean))
  
  # titlu cu unitate de masura extrasa din - map_cols_cmip_fun
  param_name <- paste0(params_def$parm[params_def$input %in% input$param], " (",strsplit(pal$tit_leg, ";|<")[[1]][7],")")
  season_name <- names(select_seas[select_seas %in% input$season])
  
  list(r = r_mean, pal = pal, min_max = minmax(r_mean), opacy = input$transp, file_hist = file_hist, file_scen = file_scen,
       season_subset = season_subset, param_name = param_name, season_name = season_name)
  
}) 



# functie leaflet de start
output$map <- renderLeaflet({
  leaflet_fun()
})

# update leaflet outputuri
observe({
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
      labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
})


# zoom to selected area
observeEvent(input$test_area, {
  req(input$test_area)
  
  shape_to_zoom <- switch(
    input$test_area,
    "drb" = dun,
    "ro" = romania_ro,
    "at" = austria_at,
    "rs" = serbia_rs
  )
  
  bbox <- sf::st_bbox(shape_to_zoom)
  
  leafletProxy("map") %>%
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
    paste(param, season, toupper(input$scen), " - change in multiannual mean", input$period_change[2], "vs.",  input$period_change[1]) 
  }
})

# reactive values pentru plot lst time series din raster
values_plot_na <- reactiveValues(input = NULL, title = NULL, lon = 25, lat = 46, mode = "point")

observeEvent(input$test_area, {
  if (input$test_area %in% c("ro", "at", "rs")) {
    values_plot_na$mode <- "zonal"
  } else {
    values_plot_na$mode <- "point"
  }
})

# interactivitate raster
observeEvent(input$map_click,{
  req(input$map_click)
  # Do not trigger point extraction when a country is selected
  if (input$test_area %in% 'drb') {
    values_plot_na$mode <- "point"
    values_plot_na$lon <- input$map_click$lng
    values_plot_na$lat <- input$map_click$lat
  }
})

observe({
  # Make it depend on all relevant inputs
  req(input$param, input$scen, input$season, input$quant, input$period_change, input$period_climate, values_plot_na$mode)
  
  if (values_plot_na$mode == "zonal") {
    req(input$test_area %in% c("ro", "at", "rs"))
    shape_to_extract <- switch(
      input$test_area,
      "ro" = romania_ro,
      "at" = austria_at,
      "rs" = serbia_rs
    )
    
    area_choices <- c("Danube River Basin" = "drb", "Romania" = "ro", "Austria" = "at", "Serbia" = "rs")
    country_name <- names(area_choices)[area_choices == input$test_area]
    
    ddf <- extract_zonal_data(
      file_hist = data_sel()$file_hist,
      file_scen = data_sel()$file_scen,
      shape = shape_to_extract,
      season_subset = data_sel()$season_subset,
      param = input$param,
      quant = input$quant,
      period_change = input$period_change
    )
    
    values_plot_na$input <- ddf
    values_plot_na$title <- paste( data_sel()$param_name, "for", country_name, "test area",   toString(shape_to_extract$Name) )
  } else {
    lon <- values_plot_na$lon
    lat <- values_plot_na$lat

  

    ddf <- extract_data(data_sel()$file_hist, data_sel()$file_scen, extract_point, lon, lat, input$param, data_sel()$season_subset,input$quant, input$period_change,input$period_climate)
  

    if (is.data.frame(ddf)) {
      if(!all(is.na(ddf$value))) {
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
  data_input <- values_plot_na$input
  
  param_name <- params_def$parm[params_def$input %in% input$param]
  col_line <- ifelse(input$param == "pr", "blue", "red")
  
  
  #data_input$value90[data_input$value90 > 999] <- NA
  if (!is.character(data_input)) {
    # Calculate trendline using linear regression
    trendline <- lm(value ~ as.numeric(date), data = data_input)
    data_input$trendline <- predict(trendline) |> round(3)
    highchart() %>%
      #hc_title(text = "Value Trends Over Years") %>%
      hc_xAxis(categories = format(data_input$date, "%Y")) %>%
      hc_yAxis(title = list(text = "Value")) %>%
      hc_add_series(color =  col_line,name = param_name, data = data_input$value, type = 'line', marker = list(enabled = FALSE)) %>%
      hc_add_series(name = "P10 - P90", data = list_parse2(data.frame(low = data_input$value10, high = data_input$value90)), type = 'arearange', color = '#CCCCCC', lineWidth = 0, fillOpacity = 0.3, zIndex = 0) |>
      hc_add_series(name = "Trendline", data = data_input$trendline, type = 'line', color = 'grey', dashStyle = 'shortdash')
    
  } else {
    highchart() |> 
      hc_title(
        text = data_input,
        style = list(fontSize = "14px", color = "grey"))
  }
})


output$graph_titl <- renderText({
  values_plot_na$title 
})

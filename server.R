server <- function(input, output) {
  
  data_sel <- reactive({
    
    param <- input$param
    
    season <- strsplit(input$season, "-")[[1]][2]
    season_subset <- strsplit(input$season, "-")[[1]][1]
    
    # uneste istricul cu scenariiul
    file_hist <-  paste0("www/data/ncs/cmip6/",input$param,"/hist/",input$param,"_hist_",season, "-50_19601201-20141130.nc")
    file_scen <- paste0("www/data/ncs/cmip6/",input$param,"/",input$scen,"/",input$param,"_",input$scen,"_",season, "-50_20150101-21001231.nc")
    r <- c(rast(file_hist), rast(file_scen))
    
    dats <- time(r)
    print(summary(dats))
    
    # subseteaza sezoniere si lunare
    if (season_subset != "year") r <- r[[which(format(dats, "%m") %in% season_subset)]]
    
    # pentru subsetare harta
    dats_sub <- time(r)
    
    # pentur selectie tip de calcul
    if (input$quant %in% "climate") {
      
      
      # pentru subsetare
      an1 <- strsplit(input$period_climate, "-")[[1]][1] |> as.numeric()
      an2 <- strsplit(input$period_climate, "-")[[1]][2] |> as.numeric()
      
      print(c(an1, an2))
      
      # mediaza duop input
      r_mean <- mean(r[[format(dats_sub, "%Y") %in% an1:an2]])
      
      # r <- mask(r, dun)
      setMinMax(r_mean)
      print(r_mean)
      
    } else {
      
      # pentru subsetare
      an1_hist <- strsplit(input$period_change[1], "-")[[1]][1] |> as.numeric()
      an2_hist <- strsplit(input$period_climate[1], "-")[[1]][2] |> as.numeric()
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
      print(r_mean)
      
    }
    
    
    pal <- map_cols_cmip_fun(indic = input$param, type = input$quant, domain = minmax(r_mean))
    # pentru reclasificare extreme raster cu min/max din paleta de culori
    r_mean[r_mean < pal$minmax[1]] <- pal$minmax[1]
    r_mean[r_mean > pal$minmax[2]] <- pal$minmax[2]
    pal <- map_cols_cmip_fun(indic = input$param, type = input$quant, domain = minmax(r_mean))
    
    list(r = r_mean, pal = pal, min_max = minmax(r_mean), opacy = input$transp)
    
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
  
}


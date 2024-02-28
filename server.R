server <- function(input, output) {
    
    data_sel <- reactive({
        
        param <- input$param
        
        season <- strsplit(input$season, "-")[[1]][2]
        season_subset <- strsplit(input$season, "-")[[1]][1]
        
        sufix <- ifelse(input$scen %in% "hist", "-50_19601201-20141130.nc", "-50_20150101-21001231.nc")
        
        file <- paste0("www/data/ncs/cmip6/",input$param,"/",input$scen,"/",input$param,"_",input$scen,"_",season, sufix)
        
        r <- rast(file) 
        dats <- time(r)
        
        # subseteaza sezoniere si lunare
        if (season_subset != "year") r <- r[[which(format(dats, "%m") %in% season_subset)]]
      
        r <- mean(r)
        
        r <- mask(r, dun)
        setMinMax(r)
        
        pal <- map_cols_cmip_fun(indic = input$param, domain = minmax(r))
        
        list(r = r, pal = pal, min_max = minmax(r), opacy = input$transp)
        
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


server <- function(input, output) {
    
    data_sel <- reactive({
        
        param <- input$param
        
        season <- strsplit(input$season, "-")[[1]][2]
        sufix <- ifelse(input$scen %in% "hist", "-50_19601201-20141130.nc", "-50_20150101-21001231.nc")
        
        file <- paste0("www/data/ncs/cmip6/",input$param,"/",input$scen,"/",input$param,"_",input$scen,"_",season, sufix)
        
        print(file)
        r <- rast(file) |> mean()
        
        setMinMax(r)
        
        pal <- map_cols_cmip_fun(indic = input$param, domain = minmax(r))
        
        list(r = r, pal = pal, min_max = minmax(r))
        
    }) 
    
    observe({
        
        print(data_sel()$r)
    })
    
    
    #functie leaflet de start
    output$map <- renderLeaflet({
        leaflet_fun()
    })
    
    observe({
        
        r <- data_sel()$r
        pal <- data_sel()$pal
        min_max <- data_sel()$min_max
        leafletProxy("map") |>
            addRasterImage(r, opacity = 0.8, color = pal$pal) |>
            clearControls() |>
            addLegend(
                title = pal$tit_leg,
                position = "bottomright",
                pal = pal$pal_rev, values = min_max,
                opacity = 1,
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
        
        
    })
    
}


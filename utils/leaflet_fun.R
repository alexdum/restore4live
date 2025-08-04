leaflet_fun <- function() {
  
  map <- leaflet(
    #data = data,
    options = leafletOptions(
      minZoom = 5, maxZoom = 16
    ) 
  ) %>%
    leaflet.extras::addBootstrapDependency() %>%
    setView(19, 46, zoom = 5) %>%
    setMaxBounds(1, 25, 43, 65) |>
    #addMapPane(name = "SUHI", zIndex = 420) %>%
    addMapPane(name = "danube", zIndex = 440) %>%
    addMapPane(name = "maplabels", zIndex = 450) %>%
    addProviderTiles( "CartoDB.Positron", group = "CartoDB")  %>% 
    addProviderTiles( "Esri.WorldGrayCanvas", group = "EsriWorldGray") |> 
    addProviderTiles( "Esri.WorldImagery", group = "EsriWorldImagery") |> 
    addEasyButton(
      easyButton(
        icon    = "glyphicon glyphicon-home", title = "Reset zoom",
        onClick = JS("function(btn, map){ map.setView([46, 19], 5); }")
      )
    ) |>
    # addPolylines(
    #     data =  jud,
    #     color = "#444444", weight = 1, smoothFactor = 0.5,
    #     options = pathOptions(pane = "judete"),
    #     group = "Județe") |>
    addPolygons(
      data = dun,
      stroke = TRUE,color = "black",opacity = 1,  fillOpacity = 0,
      fillColor =  "#444444", weight = 1, smoothFactor = 0.5,
     # options = pathOptions(pane = "danube"),
      group = "Danube") |>
    addPolygons(
      data = austria_at,
      stroke = TRUE,
      color = "red",
      opacity = 1,
      fillOpacity = 0.3,
      fillColor = "#444444",
      weight = 1,
      smoothFactor = 0.5,
      group = "Austria",
      highlightOptions = highlightOptions(
        weight = 2,
        color = "blue",
        fillOpacity = 0.5,
        bringToFront = TRUE
      ),
      label = ~Name,
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
    ) |> addPolygons(
      data = romania_ro,
      stroke = TRUE,
      color = "red",
      opacity = 1,
      fillOpacity = 0.3,
      fillColor = "#444444",
      weight = 1,
      smoothFactor = 0.5,
      group = "Romania",
      highlightOptions = highlightOptions(
        weight = 2,
        color = "blue",
        fillOpacity = 0.5,
        bringToFront = TRUE
      ),
      label = ~Name,
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
    ) |>
    addPolygons(
      data = serbia_rs,
      stroke = TRUE,
      color = "red",
      opacity = 1,
      fillOpacity = 0.3,
      fillColor = "#444444",
      weight = 1,
      smoothFactor = 0.5,
      group = "Serbia",
      highlightOptions = highlightOptions(
        weight = 2,
        color = "blue",
        fillOpacity = 0.5,
        bringToFront = TRUE
      ),
      label = ~Name,
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
    ) |>
    addLayersControl(
      baseGroups = c("CartoDB", "EsriWorldGray", "EsriWorldImagery"),
      overlayGroups = c("Labels","Danube","Austria", "Romania", "Serbia")) |> 
    #hideGroup("SUHI") |>
    # pentru poztionare raster mereu in top
    htmlwidgets::onRender(" 
    function(el, x) {
      this.on('baselayerchange', function(e) {
        e.layer.bringToBack();
      })
    }
  ") |>
    
    addProviderTiles(
      "CartoDB.PositronOnlyLabels",
      options = pathOptions(pane = "maplabels"),
      group = "Labels"
    ) %>%
    addScaleBar(
      position = c("bottomleft"),
      options = scaleBarOptions(metric = TRUE)) 
  
  
  return(map)
}
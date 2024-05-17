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
    #addMapPane(name = "judete", zIndex = 430) %>%
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
      options = pathOptions(pane = "danube"),
      group = "Danube") |>
  addLayersControl(
    baseGroups = c("CartoDB", "EsriWorldGray", "EsriWorldImagery"),
    overlayGroups = c("Labels","Danube")) |> 
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
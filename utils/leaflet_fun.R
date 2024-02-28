leaflet_fun <- function() {
  
  map <- leaflet(
    #data = data,
    options = leafletOptions(
      minZoom = 6, maxZoom = 16
    ) 
  ) %>%
    leaflet.extras::addBootstrapDependency() %>%
    setView(20, 46, zoom = 6) %>%
    setMaxBounds(7, 41.3, 31, 50.9) |>
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
        onClick = JS("function(btn, map){ map.setView([46, 20], 6); }")
      )
    ) |>
    # addPolylines(
    #     data =  jud,
    #     color = "#444444", weight = 1, smoothFactor = 0.5,
    #     options = pathOptions(pane = "judete"),
    #     group = "Județe") |>
    addPolygons(
      data = dun,
      stroke = TRUE,color = "yellow",opacity = 1,
      fillColor =  "#444444", weight = 1, smoothFactor = 0.5,
      options = pathOptions(pane = "danube"),
      group = "Danube") |>
  addLayersControl(
    baseGroups = c("EsriWorldImagery","CartoDB","EsriWorldGray"),
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
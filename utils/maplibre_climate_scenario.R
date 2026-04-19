scenario_maplibre_vector_layer_ids <- c(
  "scen-danube-fill", "scen-danube-line",
  "scen-aoi-fill", "scen-aoi-line",
  "scen-selected-fill", "scen-selected-line",
  "scen-selected-point"
)

scenario_maplibre_top_line_layer_ids <- c(
  "scen-danube-line",
  "scen-aoi-line",
  "scen-selected-line"
)

scenario_maplibre_draw_area_layers <- function(
    proxy,
    danube_shape,
    all_areas,
    selected_shape = NULL,
    before_id = "waterway_line_label") {
  proxy <- proxy %>%
    mapgl::clear_layer(scenario_maplibre_vector_layer_ids)

  proxy <- maplibre_add_polygon_layers(
    proxy = proxy,
    fill_id = "scen-danube-fill",
    line_id = "scen-danube-line",
    source = danube_shape,
    fill_color = "#1d4ed8",
    fill_opacity = 0.02,
    line_color = "#0f172a",
    line_width = 1.1,
    line_opacity = 0.85,
    before_id = before_id
  )

  proxy <- maplibre_add_polygon_layers(
    proxy = proxy,
    fill_id = "scen-aoi-fill",
    line_id = "scen-aoi-line",
    source = all_areas,
    fill_color = "#ef4444",
    fill_opacity = 0.08,
    line_color = "#b91c1c",
    line_width = 1,
    line_opacity = 0.5,
    tooltip = "area_label",
    before_id = before_id
  )

  if (!is.null(selected_shape)) {
    proxy <- maplibre_add_polygon_layers(
      proxy = proxy,
      fill_id = "scen-selected-fill",
      line_id = "scen-selected-line",
      source = selected_shape,
      fill_color = "#facc15",
      fill_opacity = 0.26,
      line_color = "#f59e0b",
      line_width = 3,
      line_opacity = 0.95,
      tooltip = "area_label",
      before_id = before_id
    )
  }

  proxy
}

scenario_maplibre_update_raster <- function(
    proxy,
    raster,
    palette,
    opacity,
    layer_id = "scenario-raster",
    legend_id = "scenario-raster-legend",
    before_id = "waterway_line_label") {
  maplibre_update_categorical_raster(
    proxy = proxy,
    raster = raster,
    palette = palette,
    opacity = opacity,
    layer_id = layer_id,
    legend_id = legend_id,
    before_id = before_id
  )
}

scenario_maplibre_highlight_point <- function(
    proxy,
    lon,
    lat,
    before_id = "waterway_line_label") {
  maplibre_highlight_point(
    proxy = proxy,
    layer_id = "scen-selected-point",
    lon = lon,
    lat = lat,
    before_id = before_id
  )
}

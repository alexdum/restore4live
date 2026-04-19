hydro_maplibre_vector_layer_ids <- c(
  "hydro-danube-fill", "hydro-danube-line",
  "hydro-aoi-fill", "hydro-aoi-line",
  "hydro-selected-fill", "hydro-selected-line"
)

hydro_maplibre_top_line_layer_ids <- c(
  "hydro-danube-line",
  "hydro-aoi-line",
  "hydro-selected-line"
)

hydro_maplibre_draw_area_layers <- function(
    proxy,
    danube_shape,
    all_areas,
    selected_shape = NULL,
    before_id = "waterway_line_label") {
  proxy <- proxy %>%
    mapgl::clear_layer(hydro_maplibre_vector_layer_ids)

  proxy <- maplibre_add_polygon_layers(
    proxy = proxy,
    fill_id = "hydro-danube-fill",
    line_id = "hydro-danube-line",
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
    fill_id = "hydro-aoi-fill",
    line_id = "hydro-aoi-line",
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
      fill_id = "hydro-selected-fill",
      line_id = "hydro-selected-line",
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

hydro_maplibre_update_raster <- function(
    proxy,
    raster,
    palette,
    opacity,
    layer_id = "hydro-raster",
    legend_id = "hydro-raster-legend",
    before_id = "waterway_line_label") {
  maplibre_update_categorical_raster(
    proxy = proxy,
    raster = raster,
    palette = palette,
    opacity = opacity,
    layer_id = layer_id,
    legend_id = legend_id,
    legend_separator = "-",
    before_id = before_id
  )
}

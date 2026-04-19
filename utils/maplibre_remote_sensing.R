rs_maplibre_overlay_layer_ids <- c(
  "rs-danube-fill", "rs-danube-line",
  "rs-selected-point"
)

rs_maplibre_draw_boundary_layers <- function(
    proxy,
    danube_shape,
    before_id = "waterway_line_label") {
  proxy <- proxy %>%
    mapgl::clear_layer(c("rs-danube-fill", "rs-danube-line"))

  maplibre_add_polygon_layers(
    proxy = proxy,
    fill_id = "rs-danube-fill",
    line_id = "rs-danube-line",
    source = danube_shape,
    fill_color = "#1d4ed8",
    fill_opacity = 0.02,
    line_color = "#0f172a",
    line_width = 1.1,
    line_opacity = 0.85,
    before_id = before_id
  )
}

rs_maplibre_highlight_point <- function(
    proxy,
    lon,
    lat,
    before_id = "waterway_line_label") {
  maplibre_highlight_point(
    proxy = proxy,
    layer_id = "rs-selected-point",
    lon = lon,
    lat = lat,
    before_id = before_id
  )
}

rs_maplibre_build_palette <- function(raster, legend_title, n_bins = 10) {
  raster_range <- as.numeric(terra::minmax(raster))
  raster_range <- raster_range[is.finite(raster_range)]

  if (length(raster_range) < 2) {
    raster_range <- c(0, 1)
  }

  if (isTRUE(all.equal(raster_range[1], raster_range[2]))) {
    delta <- max(abs(raster_range[1]) * 0.01, 0.01)
    raster_range <- c(raster_range[1] - delta, raster_range[2] + delta)
  }

  bins <- seq(raster_range[1], raster_range[2], length.out = n_bins + 1)

  list(
    tit_leg = legend_title,
    bins = bins,
    cols = rev(hcl.colors(n_bins, "viridis"))
  )
}

rs_maplibre_update_raster <- function(
    proxy,
    raster,
    timestep_label,
    opacity,
    layer_id = "ndvi-raster",
    legend_id = "ndvi-raster-legend",
    before_id = "waterway_line_label") {
  palette <- rs_maplibre_build_palette(
    raster = raster,
    legend_title = paste("NDVI for", timestep_label)
  )

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

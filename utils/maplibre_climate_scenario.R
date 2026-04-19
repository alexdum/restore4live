scenario_maplibre_vector_layer_ids <- c(
  "scen-danube-fill", "scen-danube-line",
  "scen-aoi-fill", "scen-aoi-line",
  "scen-selected-fill", "scen-selected-line"
)

scenario_maplibre_top_line_layer_ids <- c(
  "scen-danube-line",
  "scen-aoi-line",
  "scen-selected-line"
)

scenario_maplibre_format_legend_value <- function(x) {
  if (is.na(x)) {
    return("NA")
  }

  rounded <- if (abs(x) >= 100 || abs(x - round(x)) < 1e-8) {
    round(x, 0)
  } else if (abs(x) >= 10) {
    round(x, 1)
  } else {
    round(x, 2)
  }

  format(rounded, trim = TRUE, scientific = FALSE)
}

scenario_maplibre_build_interval_legend <- function(bins, colors) {
  stopifnot(length(bins) == length(colors) + 1)

  labels <- vapply(seq_along(colors), function(i) {
    paste0(
      scenario_maplibre_format_legend_value(bins[i]),
      " - ",
      scenario_maplibre_format_legend_value(bins[i + 1])
    )
  }, character(1))

  ord <- rev(seq_along(colors))
  list(
    labels = labels[ord],
    colors = colors[ord]
  )
}

scenario_maplibre_classify_raster <- function(r, bins) {
  terra::app(
    r,
    fun = function(x) {
      as.integer(cut(
        x,
        breaks = bins,
        labels = FALSE,
        include.lowest = TRUE,
        right = FALSE
      ))
    }
  )
}

scenario_maplibre_build_classified_raster <- function(r, bins, colors) {
  class_raster <- scenario_maplibre_classify_raster(r, bins)
  rgba <- grDevices::col2rgb(colors, alpha = TRUE)

  terra::coltab(class_raster) <- data.frame(
    value = seq_along(colors),
    red = as.integer(rgba["red", ]),
    green = as.integer(rgba["green", ]),
    blue = as.integer(rgba["blue", ]),
    alpha = as.integer(rgba["alpha", ])
  )

  class_raster
}

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
  legend_title <- trimws(gsub("<[^>]+>", "", palette$tit_leg))
  legend_items <- scenario_maplibre_build_interval_legend(palette$bins, palette$cols)
  classified_raster <- scenario_maplibre_build_classified_raster(raster, palette$bins, palette$cols)

  proxy %>%
    mapgl::clear_layer(layer_id) %>%
    mapgl::clear_legend(legend_id) %>%
    mapgl::add_image_source(
      id = layer_id,
      data = classified_raster
    ) %>%
    mapgl::add_raster_layer(
      id = layer_id,
      source = layer_id,
      raster_opacity = opacity,
      raster_resampling = "nearest",
      before_id = before_id
    ) %>%
    mapgl::add_categorical_legend(
      legend_title = legend_title,
      values = legend_items$labels,
      colors = legend_items$colors,
      position = "bottom-right",
      unique_id = legend_id
    )
}

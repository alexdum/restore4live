maplibre_label_layer_ids <- c(
  "waterway_line_label", "water_name_point_label", "water_name_line_label",
  "highway-name-path", "highway-name-minor", "highway-name-major",
  "highway-shield-non-us", "highway-shield-us-interstate", "road_shield_us",
  "airport", "label_other", "label_village", "label_town", "label_state",
  "label_city", "label_city_capital", "label_country_3", "label_country_2", "label_country_1",
  "road_oneway", "road_oneway_opposite", "poi_r20", "poi_r7", "poi_r1", "poi_transit",
  "waterway-line-label", "water-name-point-label", "water-name-line-label",
  "road-shield-us", "label-other", "label-village", "label-town", "label-state",
  "label-city", "label-city-capital", "label-country-3", "label-country-2", "label-country-1",
  "place_villages", "place_town", "place_country_2", "place_country_1",
  "place_state", "place_continent", "place_city_r6", "place_city_r5",
  "place_city_dot_r7", "place_city_dot_r4", "place_city_dot_r2", "place_city_dot_z7",
  "place_capital_dot_z7", "place_capital", "roadname_minor", "roadname_sec",
  "roadname_pri", "roadname_major", "motorway_name", "watername_ocean",
  "watername_sea", "watername_lake", "watername_lake_line", "poi_stadium",
  "poi_park", "poi_zoo", "airport_label", "country-label", "state-label",
  "settlement-major-label", "settlement-minor-label", "settlement-subdivision-label",
  "road-label", "waterway-label", "natural-point-label", "poi-label", "airport-label"
)

maplibre_boundary_layer_ids <- c(
  "boundary_3", "boundary_2", "boundary_disputed",
  "boundary-3", "boundary-2", "boundary-disputed"
)

maplibre_non_label_layer_ids <- c(
  "background", "park", "water", "landcover_ice_shelf", "landcover_glacier",
  "landuse_residential", "landcover_wood", "waterway", "building",
  "tunnel_motorway_casing", "tunnel_motorway_inner", "aeroway-taxiway",
  "aeroway-runway-casing", "aeroway-area", "aeroway-runway",
  "road_area_pier", "road_pier", "highway_path", "highway_minor",
  "highway_major_casing", "highway_major_inner", "highway_major_subtle",
  "highway_motorway_casing", "highway_motorway_inner", "highway_motorway_subtle",
  "railway_transit", "railway_transit_dashline", "railway_service",
  "railway_service_dashline", "railway", "railway_dashline",
  "highway_motorway_bridge_casing", "highway_motorway_bridge_inner"
)

maplibre_create_base_map <- function(
    style = ofm_positron_style,
    center = c(19, 46),
    zoom = 5,
    navigation_position = "top-left",
    show_compass = FALSE,
    visualize_pitch = FALSE) {
  mapgl::maplibre(
    style = style,
    center = center,
    zoom = zoom
  ) %>%
    mapgl::add_navigation_control(
      show_compass = show_compass,
      visualize_pitch = visualize_pitch,
      position = navigation_position
    )
}

maplibre_resize_script <- function(map_dom_id, delay_ms = 200) {
  sprintf(
    "
      setTimeout(function() {
        var map = document.getElementById('%s');
        if (map && map.__mapgl) {
          map.__mapgl.resize();
        }
      }, %d);
    ",
    map_dom_id,
    delay_ms
  )
}

maplibre_get_area_shape <- function(
    area_id,
    dun,
    is1_austria,
    is2_slovakia,
    is3_serbia,
    is4_romania,
    ms1_germany,
    ms2_slovakia,
    ms3_serbia,
    ms4_romania,
    ms5_romania,
    ms6_romania) {
  switch(area_id,
    "drb" = dun,
    "at1" = is1_austria[1, ],
    "at2" = is1_austria[2, ],
    "at3" = is1_austria[3, ],
    "at4" = is1_austria[4, ],
    "at5" = is1_austria[5, ],
    "sk1" = is2_slovakia,
    "rs1" = is3_serbia,
    "ro1" = is4_romania,
    "de1" = ms1_germany,
    "sk2" = ms2_slovakia,
    "rs2" = ms3_serbia,
    "ro2" = ms4_romania,
    "ro3" = ms5_romania,
    "ro4" = ms6_romania,
    dun
  )
}

maplibre_get_area_label <- function(area_id, select_area, default_label = "Danube River Basin") {
  label <- names(select_area[select_area == area_id])
  if (length(label) == 0) {
    return(default_label)
  }

  label[[1]]
}

maplibre_get_bbox <- function(shape, buffer_m = 10000) {
  shape_metric <- sf::st_transform(sf::st_make_valid(shape), 3035)
  if (!is.null(buffer_m) && buffer_m > 0) {
    shape_metric <- sf::st_buffer(shape_metric, dist = buffer_m)
  }

  bbox <- sf::st_bbox(sf::st_transform(shape_metric, 4326))
  unname(c(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]]))
}

maplibre_fit_shape <- function(proxy, shape, animate = TRUE, buffer_m = 10000) {
  proxy %>%
    mapgl::fit_bounds(
      maplibre_get_bbox(shape, buffer_m = buffer_m),
      animate = animate
    )
}

maplibre_build_all_areas <- function(country_layers) {
  dplyr::bind_rows(lapply(country_layers, function(layer_info) {
    sf::st_as_sf(layer_info$data) %>%
      dplyr::mutate(area_label = layer_info$name) %>%
      dplyr::select(area_label)
  }))
}

maplibre_add_polygon_layers <- function(
    proxy,
    fill_id,
    line_id,
    source,
    fill_color,
    fill_opacity,
    line_color,
    line_width,
    line_opacity,
    tooltip = NULL,
    before_id = "waterway_line_label") {
  fill_args <- list(
    proxy,
    id = fill_id,
    source = source,
    fill_color = fill_color,
    fill_opacity = fill_opacity,
    before_id = before_id
  )

  if (!is.null(tooltip)) {
    fill_args$tooltip <- tooltip
  }

  proxy <- do.call(mapgl::add_fill_layer, fill_args)

  do.call(
    mapgl::add_line_layer,
    list(
      proxy,
      id = line_id,
      source = source,
      line_color = line_color,
      line_width = line_width,
      line_opacity = line_opacity,
      before_id = before_id
    )
  )
}

maplibre_format_legend_value <- function(x) {
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

maplibre_build_interval_legend <- function(bins, colors, separator = " - ") {
  stopifnot(length(bins) == length(colors) + 1)

  labels <- vapply(seq_along(colors), function(i) {
    paste0(
      maplibre_format_legend_value(bins[i]),
      separator,
      maplibre_format_legend_value(bins[i + 1])
    )
  }, character(1))

  ord <- rev(seq_along(colors))
  list(
    labels = labels[ord],
    colors = colors[ord]
  )
}

maplibre_classify_raster <- function(r, bins) {
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

maplibre_build_classified_raster <- function(r, bins, colors) {
  class_raster <- maplibre_classify_raster(r, bins)
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

maplibre_update_categorical_raster <- function(
    proxy,
    raster,
    palette,
    opacity,
    layer_id,
    legend_id,
    legend_separator = " - ",
    before_id = "waterway_line_label") {
  legend_title <- trimws(gsub("<[^>]+>", "", palette$tit_leg))
  legend_items <- maplibre_build_interval_legend(
    bins = palette$bins,
    colors = palette$cols,
    separator = legend_separator
  )
  classified_raster <- maplibre_build_classified_raster(raster, palette$bins, palette$cols)

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
      position = "bottom-left",
      unique_id = legend_id
    )
}

maplibre_apply_label_visibility <- function(proxy, show_labels, label_layer_ids = maplibre_label_layer_ids) {
  visibility <- if (isTRUE(show_labels)) "visible" else "none"

  for (layer_id in label_layer_ids) {
    tryCatch(
      {
        proxy <- proxy %>% mapgl::set_layout_property(layer_id, "visibility", visibility)
      },
      error = function(e) {
        proxy
      }
    )
  }

  proxy
}

maplibre_bring_layers_to_front <- function(proxy, layer_ids, before_id = "waterway_line_label") {
  for (layer_id in layer_ids) {
    proxy <- tryCatch(
      {
        proxy %>% mapgl::move_layer(layer_id, before_id = before_id)
      },
      error = function(e) {
        proxy
      }
    )
  }

  proxy
}

maplibre_place_layer_below_anchors <- function(proxy, layer_id, anchor_ids) {
  for (anchor_id in anchor_ids) {
    moved <- FALSE
    proxy <- tryCatch(
      {
        moved <- TRUE
        proxy %>% mapgl::move_layer(layer_id, before_id = anchor_id)
      },
      error = function(e) {
        proxy
      }
    )

    if (moved) {
      break
    }
  }

  proxy
}

maplibre_switch_basemap <- function(
    map_id,
    basemap,
    current_basemap,
    show_labels,
    current_raster_layers,
    style_change_trigger,
    sentinel_ids,
    label_layer_ids = maplibre_label_layer_ids,
    non_label_layer_ids = maplibre_non_label_layer_ids,
    delay = 0.5) {
  proxy <- mapgl::maplibre_proxy(map_id)

  old_layers <- shiny::isolate(current_raster_layers())
  if (length(old_layers) > 0) {
    for (layer_id in old_layers) {
      proxy <- proxy %>% mapgl::clear_layer(layer_id)
    }
    current_raster_layers(character(0))
  }

  if (basemap %in% c("ofm_positron", "ofm_bright")) {
    style_url <- switch(basemap,
      "ofm_positron" = ofm_positron_style,
      "ofm_bright" = ofm_bright_style
    )

    proxy %>%
      mapgl::set_style(style_url, preserve_layers = FALSE)

    current_session <- shiny::getDefaultReactiveDomain()
    selected_basemap <- basemap

    later::later(function() {
      shiny::withReactiveDomain(current_session, {
        if (!identical(shiny::isolate(current_basemap()), selected_basemap)) {
          return()
        }

        maplibre_apply_label_visibility(
          mapgl::maplibre_proxy(map_id),
          shiny::isolate(show_labels()),
          label_layer_ids
        )
        style_change_trigger(shiny::isolate(style_change_trigger()) + 1)
      })
    }, delay = delay)
  } else if (identical(basemap, "sentinel")) {
    proxy %>%
      mapgl::set_style(ofm_positron_style, preserve_layers = FALSE)

    current_session <- shiny::getDefaultReactiveDomain()
    selected_basemap <- basemap

    later::later(function() {
      shiny::withReactiveDomain(current_session, {
        if (!identical(shiny::isolate(current_basemap()), selected_basemap)) {
          return()
        }

        ids <- sentinel_ids()

        mapgl::maplibre_proxy(map_id) %>%
          mapgl::add_raster_source(
            id = ids$source_id,
            tiles = sentinel_url,
            tileSize = 256,
            attribution = sentinel_attribution
          ) %>%
          mapgl::add_layer(
            id = ids$layer_id,
            type = "raster",
            source = ids$source_id,
            paint = list("raster-opacity" = 1),
            before_id = "background"
          )

        for (layer_id in non_label_layer_ids) {
          tryCatch(
            {
              mapgl::maplibre_proxy(map_id) %>%
                mapgl::set_layout_property(layer_id, "visibility", "none")
            },
            error = function(e) {
              NULL
            }
          )
        }

        maplibre_apply_label_visibility(
          mapgl::maplibre_proxy(map_id),
          shiny::isolate(show_labels()),
          label_layer_ids
        )
        current_raster_layers(c(ids$layer_id))
        style_change_trigger(shiny::isolate(style_change_trigger()) + 1)
      })
    }, delay = delay)
  }
}

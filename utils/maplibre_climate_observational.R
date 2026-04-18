observational_maplibre_overlay_layer_ids <- c(
  "obs-danube-fill", "obs-danube-line",
  "obs-area-fill", "obs-area-line"
)

observational_maplibre_build_station_label <- function(station_row) {
  lat_val <- round(station_row$Latitude[[1]], 4)
  lon_val <- round(station_row$Longitude[[1]], 4)

  paste0(
    "<div style='font-size:14px; min-width: 220px;'>",
    "<b>WMO Station ID:</b> ", htmltools::htmlEscape(as.character(station_row$StationID[[1]])), "<br>",
    "<b>Station Name:</b> ", htmltools::htmlEscape(as.character(station_row$StationName[[1]])), "<br>",
    "<b>Latitude:</b> ", lat_val, "<br>",
    "<b>Longitude:</b> ", lon_val, "<br>",
    "<b>Altitude (Height):</b> ", station_row$Height[[1]], " m<br>",
    "<b>Country:</b> ", htmltools::htmlEscape(as.character(station_row$Country[[1]])),
    "</div>"
  )
}

observational_maplibre_highlight_station <- function(
    proxy,
    station_row,
    move_map = TRUE,
    before_id = "waterway_line_label") {
  highlight_data <- sf::st_as_sf(
    data.frame(
      Longitude = station_row$Longitude[[1]],
      Latitude = station_row$Latitude[[1]],
      station_label = observational_maplibre_build_station_label(station_row)
    ),
    coords = c("Longitude", "Latitude"),
    crs = 4326
  )

  proxy <- proxy %>%
    mapgl::clear_layer("selected-highlight") %>%
    mapgl::add_circle_layer(
      id = "selected-highlight",
      source = highlight_data,
      circle_color = "#dc2626",
      circle_radius = 11,
      circle_stroke_color = "#b91c1c",
      circle_stroke_width = 3,
      circle_opacity = 0.35,
      tooltip = "station_label",
      before_id = before_id
    )

  if (isTRUE(move_map)) {
    proxy <- proxy %>%
      mapgl::fly_to(
        center = c(station_row$Longitude[[1]], station_row$Latitude[[1]]),
        zoom = 8
      )
  }

  proxy
}

observational_maplibre_draw_area_layers <- function(
    proxy,
    danube_shape,
    selected_shape = NULL,
    before_id = "waterway_line_label") {
  proxy <- proxy %>%
    mapgl::clear_layer(observational_maplibre_overlay_layer_ids) %>%
    mapgl::add_fill_layer(
      id = "obs-danube-fill",
      source = danube_shape,
      fill_color = "#1d4ed8",
      fill_opacity = 0.025,
      before_id = before_id
    ) %>%
    mapgl::add_line_layer(
      id = "obs-danube-line",
      source = danube_shape,
      line_color = "#0f172a",
      line_width = 1.2,
      line_opacity = 0.8,
      before_id = before_id
    )

  if (!is.null(selected_shape)) {
    proxy <- proxy %>%
      mapgl::add_fill_layer(
        id = "obs-area-fill",
        source = selected_shape,
        fill_color = "#facc15",
        fill_opacity = 0.28,
        tooltip = "area_label",
        before_id = before_id
      ) %>%
      mapgl::add_line_layer(
        id = "obs-area-line",
        source = selected_shape,
        line_color = "#f59e0b",
        line_width = 3,
        line_opacity = 0.95,
        before_id = before_id
      )
  }

  proxy
}

observational_maplibre_draw_station_layers <- function(
    proxy,
    stations_in_box,
    before_id = "waterway_line_label") {
  proxy <- proxy %>%
    mapgl::clear_layer("stations")

  if (!is.null(stations_in_box) && nrow(stations_in_box) > 0) {
    stations_map <- stations_in_box %>%
      dplyr::mutate(
        StationID = as.character(StationID),
        station_label = vapply(
          seq_len(nrow(stations_in_box)),
          function(i) observational_maplibre_build_station_label(stations_in_box[i, ]),
          character(1)
        )
      )

    proxy <- proxy %>%
      mapgl::add_circle_layer(
        id = "stations",
        source = stations_map,
        circle_color = "#2563eb",
        circle_radius = 5,
        circle_stroke_color = "#1e3a8a",
        circle_stroke_width = 1,
        circle_opacity = 0.85,
        tooltip = "station_label",
        before_id = before_id
      )
  }

  proxy
}

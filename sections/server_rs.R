# Define the zarr URL
zarr_url <- "https://r4l-data.unibuc.ro/data/eo_indicies/ndvi/zarr/ndvi_monthly_4326_v2.zarr/"

# Source the Python script for reading Zarr data
source_python("utils/read_zarr_data.py")

bounds_are_similar <- function(bounds1, bounds2, tolerance = 0.001) {
  if (is.null(bounds1) || is.null(bounds2)) {
    return(FALSE)
  }

  abs(bounds1$ymax - bounds2$ymax) < tolerance &&
    abs(bounds1$ymin - bounds2$ymin) < tolerance &&
    abs(bounds1$xmax - bounds2$xmax) < tolerance &&
    abs(bounds1$xmin - bounds2$xmin) < tolerance
}

rs_render_mode_label <- function(mode) {
  if (identical(mode, "high-res")) {
    "High-resolution view"
  } else if (identical(mode, "aggregated")) {
    "Overview view"
  } else {
    "Waiting for map state"
  }
}

# Reactive values to track the current render state and map position
last_rendered_state <- reactiveVal("initial")
last_rendered_bounds <- reactiveVal(NULL)
last_rendered_transparency <- reactiveVal(NULL)
last_rendered_style_token <- reactiveVal(NULL)

rs_map_initialized <- reactiveVal(FALSE)
rs_style_change_trigger <- reactiveVal(0)
rs_current_raster_layers <- reactiveVal(character(0))
rs_label_layer_ids <- maplibre_label_layer_ids
rs_non_label_layer_ids <- maplibre_non_label_layer_ids
rs_boundary_layer_ids <- maplibre_boundary_layer_ids
rs_initial_point_loaded <- reactiveVal(FALSE)
rs_default_point <- list(lon = 25, lat = 46)

# Call the Python function to get all available timesteps
all_timesteps <- reactive({
  req(zarr_url)
  get_zarr_timesteps(zarr_url)
})

output$timestep_selector <- renderUI({
  timesteps <- all_timesteps()
  req(timesteps)
  selectInput(
    "selected_timestep",
    "Select Timestep",
    choices = timesteps,
    selected = timesteps[length(timesteps)]
  )
})

full_resolution_raster <- eventReactive(input$selected_timestep, {
  last_rendered_state("initial")
  last_rendered_bounds(NULL)
  last_rendered_style_token(NULL)

  req(input$selected_timestep)
  selected_timestep_str <- input$selected_timestep
  timesteps <- all_timesteps()
  timestep_index <- as.integer(which(timesteps == selected_timestep_str) - 1)

  withProgress(message = "Fetching raster data", value = 0.2, {
    incProgress(0.1, detail = "Fetching data from Zarr store...")
    py_result <- get_timestep_data(zarr_url, timestep_index = timestep_index)

    data_values <- py_result[[1]]
    x_coords <- py_result[[2]]
    y_coords <- py_result[[3]]
    if (is.null(data_values)) {
      showNotification("Could not load data from Zarr for the selected timestep.", type = "error")
      return(NULL)
    }

    incProgress(0.5, detail = "Creating full-resolution raster...")
    ext <- terra::ext(min(x_coords), max(x_coords), min(y_coords), max(y_coords))
    raster_data <- terra::rast(data_values, extent = ext, crs = "EPSG:4326")
    raster_data[raster_data == -9999] <- NA

    setProgress(1, detail = "Done.")
    raster_data
  })
})

debounced_bounds <- reactive({
  req(input$ndvi_map_bbox)
  input$ndvi_map_bbox
}) %>% debounce(800)

output$ndvi_map <- mapgl::renderMaplibre({
  maplibre_create_base_map()
})

observe({
  req(!rs_map_initialized())
  req(input$ndvi_map_zoom)

  maplibre_fit_shape(
    proxy = mapgl::maplibre_proxy("ndvi_map"),
    shape = dun,
    animate = FALSE,
    buffer_m = 0
  )

  rs_map_initialized(TRUE)
})

observeEvent(input$navbar, {
  if (input$navbar == "remote_sensing") {
    shinyjs::runjs(maplibre_resize_script("ndvi_map"))
  }
})

observeEvent(input$zoom_home_rs, {
  req(rs_map_initialized())

  maplibre_fit_shape(
    proxy = mapgl::maplibre_proxy("ndvi_map"),
    shape = dun,
    animate = TRUE,
    buffer_m = 0
  )
})

observeEvent(input$basemap_rs, {
  req(rs_map_initialized())
  maplibre_switch_basemap(
    map_id = "ndvi_map",
    basemap = input$basemap_rs,
    current_basemap = function() input$basemap_rs,
    show_labels = function() input$show_labels_rs,
    current_raster_layers = rs_current_raster_layers,
    style_change_trigger = rs_style_change_trigger,
    sentinel_ids = function() {
      list(
        source_id = "rs-sentinel",
        layer_id = "rs-sentinel"
      )
    },
    label_layer_ids = rs_label_layer_ids,
    non_label_layer_ids = rs_non_label_layer_ids
  )
})

observeEvent(input$show_labels_rs,
  {
    req(rs_map_initialized())
    maplibre_apply_label_visibility(
      mapgl::maplibre_proxy("ndvi_map"),
      input$show_labels_rs,
      rs_label_layer_ids
    )
  },
  ignoreInit = TRUE
)

# Reactive values for clicked point and timeseries data
clicked_point <- reactiveVal(rs_default_point)
timeseries_data <- reactiveVal(NULL)
last_plotted_point <- reactiveVal(list(lon = NA, lat = NA))

rs_timeseries_export <- reactive({
  ts_df <- timeseries_data()
  point <- clicked_point()

  req(is.data.frame(ts_df), nrow(ts_df) > 0, !is.null(point))

  data.frame(
    date = ts_df$time,
    ndvi = ts_df$value,
    longitude = point$lon,
    latitude = point$lat
  )
})

rs_load_timeseries_for_point <- function(lon, lat, show_success_notification = FALSE) {
  clicked_sf_point <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)

  if (exists("dun") && !is.null(dun)) {
    dun_wgs84 <- sf::st_transform(dun, crs = 4326)

    if (any(sf::st_intersects(clicked_sf_point, dun_wgs84, sparse = FALSE))) {
      if (isTRUE(show_success_notification)) {
        showNotification(
          paste("Clicked within Danube Basin at:", round(lon, 2), ",", round(lat, 2)),
          type = "message"
        )
      }

      withProgress(message = "Extracting timeseries data", value = 0.5, {
        ts_result <- get_point_timeseries(zarr_url, lon, lat)

        if (!is.null(ts_result)) {
          df_ts <- data.frame(
            time = as.Date(ts_result$time),
            value = unlist(ts_result$values)
          )
          timeseries_data(df_ts)
          clicked_point(list(lon = lon, lat = lat))
          last_plotted_point(list(lon = lon, lat = lat))
        } else {
          showNotification("Could not extract timeseries data for the selected point.", type = "error")
          timeseries_data(NULL)
          clicked_point(NULL)
          last_plotted_point(list(lon = NA, lat = NA))
        }
      })
    } else {
      showNotification("Clicked outside Danube Basin. Please click inside the basin to get timeseries data.", type = "warning")
      timeseries_data(NULL)
      clicked_point(NULL)
      last_plotted_point(list(lon = NA, lat = NA))
    }
  } else {
    showNotification("Danube basin boundary data (dun) not available.", type = "error")
    timeseries_data(NULL)
    clicked_point(NULL)
    last_plotted_point(list(lon = NA, lat = NA))
  }
}

observe({
  req(rs_map_initialized())

  rs_style_change_trigger()
  current_point <- clicked_point()

  proxy <- rs_maplibre_draw_boundary_layers(
    proxy = mapgl::maplibre_proxy("ndvi_map"),
    danube_shape = dun
  )

  if (!is.null(current_point)) {
    proxy <- rs_maplibre_highlight_point(
      proxy = proxy,
      lon = current_point$lon,
      lat = current_point$lat
    )
  } else {
    proxy <- proxy %>% mapgl::clear_layer("rs-selected-point")
  }

  proxy %>%
    maplibre_place_layer_below_anchors("ndvi-raster", c(rs_boundary_layer_ids, rs_label_layer_ids)) %>%
    maplibre_bring_layers_to_front(c("rs-danube-fill", "rs-danube-line", "rs-selected-point"), before_id = NULL)
})

observe({
  req(input$navbar == "remote_sensing")
  req(rs_map_initialized())
  req(last_rendered_state() != "initial")
  req(!rs_initial_point_loaded())

  rs_initial_point_loaded(TRUE)
  rs_load_timeseries_for_point(
    lon = rs_default_point$lon,
    lat = rs_default_point$lat,
    show_success_notification = FALSE
  )
})

observe({
  req(input$navbar == "remote_sensing")
  req(rs_map_initialized())

  full_raster <- full_resolution_raster()
  current_bounds <- debounced_bounds()
  transparency_val <- input$trans_rs
  style_token <- rs_style_change_trigger()

  req(full_raster, current_bounds, transparency_val)

  map_extent <- terra::ext(current_bounds$xmin, current_bounds$xmax, current_bounds$ymin, current_bounds$ymax)
  map_polygon <- terra::as.polygons(map_extent, crs = "EPSG:4326")
  area_km2 <- terra::expanse(map_polygon, unit = "m") / 1e6

  current_view_state <- if (area_km2 < 100000) "high-res" else "aggregated"

  should_render <- FALSE
  previous_state <- last_rendered_state()
  previous_bounds <- last_rendered_bounds()
  previous_transparency <- last_rendered_transparency()
  previous_style_token <- last_rendered_style_token()

  if (!identical(style_token, previous_style_token)) {
    should_render <- TRUE
  } else if (!identical(current_view_state, previous_state)) {
    should_render <- TRUE
  } else if (identical(current_view_state, "high-res")) {
    if (is.null(previous_bounds) || !bounds_are_similar(previous_bounds, current_bounds)) {
      should_render <- TRUE
    }
  }

  if (is.null(previous_transparency) || transparency_val != previous_transparency) {
    should_render <- TRUE
  }

  if (should_render) {
    raster_to_draw <- if (identical(current_view_state, "high-res")) {
      tryCatch(
        terra::crop(full_raster, map_extent),
        error = function(e) NULL
      )
    } else {
      terra::aggregate(full_raster, fact = 3, fun = "mean")
    }

    proxy <- mapgl::maplibre_proxy("ndvi_map")

    if (is.null(raster_to_draw) || !any(is.finite(terra::values(raster_to_draw)))) {
      proxy %>%
        mapgl::clear_layer("ndvi-raster") %>%
        mapgl::clear_legend("ndvi-raster-legend")
    } else {
      rs_maplibre_update_raster(
        proxy = proxy,
        raster = raster_to_draw,
        timestep_label = input$selected_timestep,
        opacity = transparency_val
      ) %>%
        maplibre_place_layer_below_anchors("ndvi-raster", c(rs_boundary_layer_ids, rs_label_layer_ids)) %>%
        maplibre_bring_layers_to_front(c("rs-danube-fill", "rs-danube-line", "rs-selected-point"), before_id = NULL)
    }

    last_rendered_state(current_view_state)
    last_rendered_bounds(current_bounds)
    last_rendered_transparency(transparency_val)
    last_rendered_style_token(style_token)
  }
})

observeEvent(input$ndvi_map_click, {
  click_lon <- input$ndvi_map_click$lng
  click_lat <- input$ndvi_map_click$lat

  lon_tolerance <- 0.01
  lat_tolerance <- 0.01

  if (!is.na(last_plotted_point()$lon) &&
      abs(click_lon - last_plotted_point()$lon) < lon_tolerance &&
      abs(click_lat - last_plotted_point()$lat) < lat_tolerance) {
    return()
  }

  rs_load_timeseries_for_point(
    lon = click_lon,
    lat = click_lat,
    show_success_notification = TRUE
  )
})

output$rs_map_titl <- renderText({
  req(input$selected_timestep)
  paste("NDVI for", input$selected_timestep)
})

output$rs_context_panel <- renderUI({
  bounds <- debounced_bounds()
  current_mode <- if (!is.null(bounds)) {
    map_extent <- terra::ext(bounds$xmin, bounds$xmax, bounds$ymin, bounds$ymax)
    map_polygon <- terra::as.polygons(map_extent, crs = "EPSG:4326")
    area_km2 <- terra::expanse(map_polygon, unit = "m") / 1e6
    if (area_km2 < 100000) "high-res" else "aggregated"
  } else {
    NULL
  }

  point <- clicked_point()
  point_label <- if (!is.null(point)) {
    sprintf("%.3f°E, %.3f°N", point$lon, point$lat)
  } else {
    "No point selected"
  }
  status_value <- if (is.data.frame(timeseries_data())) {
    "Timeseries ready"
  } else {
    "Click a point inside the basin"
  }
  timestep_value <- if (!is.null(input$selected_timestep)) {
    input$selected_timestep
  } else {
    "Waiting for selection"
  }
  transparency_value <- if (!is.null(input$trans_rs)) {
    sprintf("%.1f", input$trans_rs)
  } else {
    "0.8"
  }

  shiny::tagList(
    tags$div(
      class = "scenario-context-section",
      tags$div(
        class = "scenario-context-grid",
        lapply(
          list(
            c("Dataset", "NDVI"),
            c("Timestep", timestep_value),
            c("Rendering mode", rs_render_mode_label(current_mode)),
            c("Transparency", transparency_value),
            c("Selected point", point_label),
            c("Status", status_value)
          ),
          function(row) {
            tags$div(
              class = "scenario-context-row",
              tags$span(class = "scenario-context-label", row[[1]]),
              tags$span(class = "scenario-context-value", row[[2]])
            )
          }
        )
      )
    ),
    tags$div(
      class = "scenario-context-section scenario-context-note",
      tags$div(class = "scenario-context-title", "Tip"),
      tags$p("Zoom in for a high-resolution raster view, or click a point inside the Danube basin to load the NDVI timeseries.")
    )
  )
})

output$rs_download_control <- renderUI({
  ts_df <- timeseries_data()
  point <- clicked_point()

  if (!is.data.frame(ts_df) || nrow(ts_df) == 0 || is.null(point)) {
    return(
      tags$p(
        class = "rs-sidebar-export-note",
        "Available after the timeseries is loaded."
      )
    )
  }

  downloadButton(
    outputId = "download_rs_timeseries",
    label = "Export CSV",
    class = "btn-sm btn-primary rs-sidebar-export-button"
  )
})

output$download_rs_timeseries <- downloadHandler(
  filename = function() {
    point <- clicked_point()
    req(!is.null(point))

    safe_lon <- gsub("[^0-9A-Za-z_-]", "_", sprintf("%.3f", point$lon))
    safe_lat <- gsub("[^0-9A-Za-z_-]", "_", sprintf("%.3f", point$lat))

    paste0("ndvi_timeseries_", safe_lon, "_", safe_lat, "_", Sys.Date(), ".csv")
  },
  content = function(file) {
    write.csv(rs_timeseries_export(), file, row.names = FALSE)
  }
)

output$timeseries_plot <- renderHighchart({
  ts_df <- timeseries_data()
  point <- clicked_point()

  if (!is.null(ts_df) && !is.null(point)) {
    hchart(ts_df, "line", hcaes(x = time, y = value)) %>%
      hc_xAxis(title = list(text = "Date")) %>%
      hc_yAxis(title = list(text = "NDVI Value")) %>%
      hc_tooltip(pointFormat = "<b>Date:</b> {point.x:%Y-%m-%d}<br><b>NDVI:</b> {point.y:.2f}") %>%
      hc_add_theme(hc_theme_flat())
  } else {
    highchart() %>%
      hc_chart(type = "area") %>%
      hc_xAxis(visible = FALSE) %>%
      hc_yAxis(visible = FALSE) %>%
      hc_add_series(data = list())
  }
})

# Define the zarr URL
zarr_url <- "https://r4l-data.unibuc.ro/data/eo_indicies/ndvi/zarr/ndvi_monthly_4326_v2.zarr/"

# Source the Python script for reading Zarr data
source_python("utils/read_zarr_data.py")

# Add this helper function at the beginning of your server code
bounds_are_similar <- function(bounds1, bounds2, tolerance = 0.001) {
  if (is.null(bounds1) || is.null(bounds2)) return(FALSE)
  
  abs(bounds1$north - bounds2$north) < tolerance &&
  abs(bounds1$south - bounds2$south) < tolerance &&
  abs(bounds1$east - bounds2$east) < tolerance &&
  abs(bounds1$west - bounds2$west) < tolerance
}

# --- Server logic for the Remote Sensing tab ---

# Reactive values to track the current render state and map position
last_rendered_state <- reactiveVal("initial")
last_rendered_bounds <- reactiveVal(NULL) # NEW: Stores the bounds of the last successful render
last_rendered_transparency <- reactiveVal(NULL) # NEW: Stores the transparency of the last successful render

# Call the Python function to get all available timesteps
all_timesteps <- reactive({
  req(zarr_url)
  get_zarr_timesteps(zarr_url)
})

# Render the UI for timestep selection dropdown
output$timestep_selector <- renderUI({
  timesteps <- all_timesteps()
  req(timesteps)
  selectInput("selected_timestep", "Select Timestep", choices = timesteps, selected = timesteps[length(timesteps)])
})

# Reactive to hold the full-resolution raster for the selected timestep.
full_resolution_raster <- eventReactive(input$selected_timestep, {
  # When a new timestep is selected, we must reset the render state and bounds
  # to force a re-render of the aggregated view for the new data.
  last_rendered_state("initial")
  last_rendered_bounds(NULL) # MODIFIED: Reset bounds as well
  
  req(input$selected_timestep)
  selected_timestep_str <- input$selected_timestep
  timesteps <- all_timesteps()
  timestep_index <- as.integer(which(timesteps == selected_timestep_str) - 1)

  withProgress(message = 'Loading data for selected timestep', value = 0.2, {
    incProgress(0.1, detail = "Fetching data from Zarr store...")
    py_result <- get_timestep_data(zarr_url, timestep_index = timestep_index)
    
    data_values <- py_result[[1]]; x_coords <- py_result[[2]]; y_coords <- py_result[[3]]
    if (is.null(data_values)) {
      showNotification("Could not load data from Zarr for the selected timestep.", type = "error")
      return(NULL)
    }
    
    incProgress(0.5, detail = "Creating full-resolution raster...")
    ext <- terra::ext(min(x_coords), max(x_coords), min(y_coords), max(y_coords))
    raster_data <- terra::rast(data_values, extent = ext, crs = "EPSG:4326")
    raster_data[raster_data == -9999] <- NA
    
    setProgress(1, detail = "Done.")
    return(raster_data)
  })
})

# Create a debounced reactive for the map bounds to prevent rapid firing.
debounced_bounds <- reactive({
  req(input$ndvi_map_bounds)
  input$ndvi_map_bounds
}) %>% debounce(800) # Wait 800ms after user stops moving

# Render the initial Leaflet map
output$ndvi_map <- renderLeaflet({
  leaflet_fun()
})

# This is the main observer that intelligently updates the map.
observe({
  # Dependencies: Runs when user stops moving OR new raster is ready.
  full_raster <- full_resolution_raster()
  current_bounds <- debounced_bounds()
  transparency_val <- input$trans_rs # Add transparency as a dependency
  
  req(full_raster, current_bounds, transparency_val)

  # 1. Determine the DESIRED state based on the current view
  map_extent <- terra::ext(current_bounds$west, current_bounds$east, current_bounds$south, current_bounds$north)
  map_polygon <- terra::as.polygons(map_extent, crs = "EPSG:4326")
  area_km2 <- terra::expanse(map_polygon, unit = "m") / 1e6
  
  current_view_state <- if (area_km2 < 100000) "high-res" else "aggregated"

  # 2. Decide if a re-render is necessary (THE CORE FIX)
  should_render <- FALSE
  previous_state <- last_rendered_state()
  previous_bounds <- last_rendered_bounds()
  previous_transparency <- last_rendered_transparency()

  if (current_view_state != previous_state) {
    # If the state itself has changed (e.g., aggregated -> high-res), we MUST render.
    should_render <- TRUE
  } else if (current_view_state == "high-res") {
    # If we are staying in high-res, only render if the map has actually moved.
    # The `!identical()` check is what prevents the feedback loop from causing a re-render.
    if (is.null(previous_bounds) || !bounds_are_similar(previous_bounds, current_bounds)) {
       should_render <- TRUE
    }
  }
  
  # Also re-render if transparency has changed
  if (is.null(previous_transparency) || transparency_val != previous_transparency) {
    should_render <- TRUE
  }
  # Note: If current_view_state == "aggregated" and previous_state == "aggregated",
  # should_render remains FALSE, preventing panning from re-rendering the overview.

  # 3. Perform the render if needed
  if (should_render) {
    raster_to_draw <- if (current_view_state == "high-res") {
      tryCatch(terra::crop(full_raster, map_extent), error = function(e) NULL)
    } else {
      terra::aggregate(full_raster, fact = 3, fun = "mean")
    }
    
    if (is.null(raster_to_draw)) {
      leafletProxy("ndvi_map") %>% clearImages()
    } else {
      pal <- colorNumeric(hcl.colors(100, "viridis"), values(raster_to_draw), na.color = "transparent", reverse = TRUE)
      rev_pal <- colorNumeric(palette = hcl.colors(100, "viridis"), domain = values(raster_to_draw))

      # Clear images first
      leafletProxy("ndvi_map") %>% clearImages()

      # Then add the new raster and legend
      leafletProxy("ndvi_map", data = raster_to_draw) %>%
        addRasterImage(raster_to_draw, colors = pal, opacity = input$trans_rs, project = FALSE) %>%
        clearControls() %>%
        leaflet::addLegend(
          pal = rev_pal,
          values = na.omit(values(raster_to_draw)),
          title = paste("NDVI for", input$selected_timestep),
          labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
          na.label = ""
        )
    }
    
    # 4. Update the state trackers *after* a successful render
    last_rendered_state(current_view_state)
    last_rendered_bounds(current_bounds)
    last_rendered_transparency(transparency_val)
  }
})

# --- Your code for timeseries plotting remains unchanged below ---

# Reactive values for clicked point and timeseries data
clicked_point <- reactiveVal(NULL)
timeseries_data <- reactiveVal(NULL)
last_plotted_point <- reactiveVal(list(lon = NA, lat = NA)) # Store last plotted point

# Observe map clicks
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

  clicked_sf_point <- st_point(c(click_lon, click_lat)) %>%
    st_sfc(crs = 4326)

  if (exists("dun") && !is.null(dun)) {
    dun_wgs84 <- st_transform(dun, crs = 4326)
    
    if (any(st_intersects(clicked_sf_point, dun_wgs84, sparse = FALSE))) {
      showNotification(paste("Clicked within Danube Basin at:", round(click_lon, 2), ",", round(click_lat, 2)), type = "message")
      
      withProgress(message = 'Extracting timeseries data', value = 0.5, {
        ts_result <- get_point_timeseries(zarr_url, click_lon, click_lat)
        
        if (!is.null(ts_result)) {
          df_ts <- data.frame(
            time = as.Date(ts_result$time),
            value = unlist(ts_result$values)
          )
          timeseries_data(df_ts)
          clicked_point(list(lon = click_lon, lat = click_lat))
          last_plotted_point(list(lon = click_lon, lat = click_lat))
        } else {
          showNotification("Could not extract timeseries data for the clicked point.", type = "error")
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
})

# Render the timeseries plot
output$timeseries_plot <- renderHighchart({
  ts_df <- timeseries_data()
  point <- clicked_point()

  if (!is.null(ts_df) && !is.null(point)) {
    hchart(ts_df, "line", hcaes(x = time, y = value)) %>%
      hc_title(text = paste("NDVI Timeseries at Lon:", round(point$lon, 2), "Lat:", round(point$lat, 2))) %>%
      hc_xAxis(title = list(text = "Date")) %>%
      hc_yAxis(title = list(text = "NDVI Value")) %>%
      hc_tooltip(pointFormat = "<b>Date:</b> {point.x:%Y-%m-%d}<br><b>NDVI:</b> {point.y:.2f}") %>%
      hc_add_theme(hc_theme_flat())
  } else {
    highchart() %>%
      hc_title(text = "Click on the map within the Danube Basin to view timeseries data.") %>%
      hc_chart(type = "area") %>%
      hc_xAxis(visible = FALSE) %>%
      hc_yAxis(visible = FALSE) %>%
      hc_add_series(data = list())
  }
})
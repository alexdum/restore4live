# Climate Observational Data Server Logic

# Create a reactive to fetch stations once (shared between map and table)
stations_data <- reactive({
    get_dwd_stations(shape = dun, buffer_km = 25)
})

# MapLibre state for climate observational map
obs_style_change_trigger <- reactiveVal(0)
obs_map_initialized <- reactiveVal(FALSE)
obs_current_raster_layers <- reactiveVal(character(0))
selected_station_obs_id <- reactiveVal(NULL)

obs_label_layer_ids <- c(
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

obs_non_label_layer_ids <- c(
    "background", "park", "water", "landcover_ice_shelf", "landcover_glacier",
    "landuse_residential", "landcover_wood", "waterway", "building",
    "tunnel_motorway_casing", "tunnel_motorway_inner", "aeroway-taxiway",
    "aeroway-runway-casing", "aeroway-area", "aeroway-runway",
    "road_area_pier", "road_pier", "highway_path", "highway_minor",
    "highway_major_casing", "highway_major_inner", "highway_major_subtle",
    "highway_motorway_casing", "highway_motorway_inner", "highway_motorway_subtle",
    "railway_transit", "railway_transit_dashline", "railway_service",
    "railway_service_dashline", "railway", "railway_dashline",
    "highway_motorway_bridge_casing", "highway_motorway_bridge_inner",
    "boundary_3", "boundary_2", "boundary_disputed"
)

get_obs_shape <- function(area_id) {
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

get_obs_area_label <- function(area_id) {
    label <- names(select_area[select_area == area_id])
    if (length(label) == 0) {
        return("Danube River Basin")
    }
    label[[1]]
}

get_obs_bbox <- function(shape, buffer_m = 10000) {
    shape_metric <- sf::st_transform(sf::st_make_valid(shape), 3035)
    if (!is.null(buffer_m) && buffer_m > 0) {
        shape_metric <- sf::st_buffer(shape_metric, dist = buffer_m)
    }
    bbox <- sf::st_bbox(sf::st_transform(shape_metric, 4326))
    unname(c(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]]))
}

build_obs_station_label <- function(station_row) {
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

apply_obs_label_visibility <- function(proxy, show_labels) {
    visibility <- if (isTRUE(show_labels)) "visible" else "none"
    for (layer_id in obs_label_layer_ids) {
        tryCatch(
            {
                proxy %>% mapgl::set_layout_property(layer_id, "visibility", visibility)
            },
            error = function(e) {
                NULL
            }
        )
    }
}

highlight_selected_obs_station <- function(proxy, station_row, move_map = TRUE) {
    highlight_data <- sf::st_as_sf(
        data.frame(
            Longitude = station_row$Longitude[[1]],
            Latitude = station_row$Latitude[[1]],
            station_label = build_obs_station_label(station_row)
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
            before_id = "waterway_line_label"
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

# Invalidate map size when switching tabs (fixes rendering issues)
observeEvent(input$obs_tab, {
    if (input$obs_tab == "Map View") {
        shinyjs::runjs("
            setTimeout(function() {
                var map = document.getElementById('map_obs');
                if (map && map.__mapgl) {
                    map.__mapgl.resize();
                }
            }, 200);
        ")
    }
})

# Render the initial map
output$map_obs <- mapgl::renderMaplibre({
    mapgl::maplibre(
        style = ofm_positron_style,
        center = c(19, 46),
        zoom = 5
    ) %>%
        mapgl::add_navigation_control(show_compass = FALSE, visualize_pitch = FALSE, position = "top-left")
})

observe({
    req(!obs_map_initialized())
    req(input$map_obs_zoom)

    mapgl::maplibre_proxy("map_obs") %>%
        mapgl::fit_bounds(get_obs_bbox(dun, buffer_m = 0), animate = FALSE)

    obs_map_initialized(TRUE)
})

observeEvent(input$zoom_home_obs, {
    req(obs_map_initialized())

    mapgl::maplibre_proxy("map_obs") %>%
        mapgl::fit_bounds(get_obs_bbox(dun, buffer_m = 0), animate = TRUE)
})

observeEvent(input$basemap_obs, {
    req(obs_map_initialized())

    proxy <- mapgl::maplibre_proxy("map_obs")
    basemap <- input$basemap_obs

    old_layers <- isolate(obs_current_raster_layers())
    if (length(old_layers) > 0) {
        for (layer_id in old_layers) {
            proxy %>% mapgl::clear_layer(layer_id)
        }
        obs_current_raster_layers(character(0))
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
                if (!identical(isolate(input$basemap_obs), selected_basemap)) {
                    return()
                }

                apply_obs_label_visibility(mapgl::maplibre_proxy("map_obs"), isolate(input$show_labels_obs))
                obs_style_change_trigger(isolate(obs_style_change_trigger()) + 1)
            })
        }, delay = 0.5)
    } else if (basemap == "sentinel") {
        proxy %>%
            mapgl::set_style(ofm_positron_style, preserve_layers = FALSE)

        current_session <- shiny::getDefaultReactiveDomain()
        selected_basemap <- basemap

        later::later(function() {
            shiny::withReactiveDomain(current_session, {
                if (!identical(isolate(input$basemap_obs), selected_basemap)) {
                    return()
                }

                unique_suffix <- as.integer(as.numeric(Sys.time()) * 1000)
                source_id <- paste0("obs_sentinel_source_", unique_suffix)
                layer_id <- paste0("obs_sentinel_layer_", unique_suffix)

                mapgl::maplibre_proxy("map_obs") %>%
                    mapgl::add_raster_source(
                        id = source_id,
                        tiles = sentinel_url,
                        tileSize = 256,
                        attribution = sentinel_attribution
                    ) %>%
                    mapgl::add_layer(
                        id = layer_id,
                        type = "raster",
                        source = source_id,
                        paint = list("raster-opacity" = 1),
                        before_id = "background"
                    )

                for (layer_id_kill in obs_non_label_layer_ids) {
                    tryCatch(
                        {
                            mapgl::maplibre_proxy("map_obs") %>%
                                mapgl::set_layout_property(layer_id_kill, "visibility", "none")
                        },
                        error = function(e) {
                            NULL
                        }
                    )
                }

                apply_obs_label_visibility(mapgl::maplibre_proxy("map_obs"), isolate(input$show_labels_obs))
                obs_current_raster_layers(c(layer_id))
                obs_style_change_trigger(isolate(obs_style_change_trigger()) + 1)
            })
        }, delay = 0.5)
    }
})

observeEvent(input$show_labels_obs,
    {
        req(obs_map_initialized())
        apply_obs_label_visibility(mapgl::maplibre_proxy("map_obs"), input$show_labels_obs)
    },
    ignoreInit = TRUE
)

observe({
    req(obs_map_initialized())

    obs_style_change_trigger()
    selected_shape <- get_obs_shape(input$test_area_obs)
    selected_label <- get_obs_area_label(input$test_area_obs)

    proxy <- mapgl::maplibre_proxy("map_obs") %>%
        mapgl::clear_layer("obs-danube-fill") %>%
        mapgl::clear_layer("obs-danube-line") %>%
        mapgl::clear_layer("obs-area-fill") %>%
        mapgl::clear_layer("obs-area-line")

    proxy <- proxy %>%
        mapgl::add_fill_layer(
            id = "obs-danube-fill",
            source = dun,
            fill_color = "#1d4ed8",
            fill_opacity = 0.025,
            before_id = "waterway_line_label"
        ) %>%
        mapgl::add_line_layer(
            id = "obs-danube-line",
            source = dun,
            line_color = "#0f172a",
            line_width = 1.2,
            line_opacity = 0.8,
            before_id = "waterway_line_label"
        )

    if (!identical(input$test_area_obs, "drb")) {
        selected_shape <- dplyr::mutate(sf::st_as_sf(selected_shape), area_label = selected_label)

        proxy %>%
            mapgl::add_fill_layer(
                id = "obs-area-fill",
                source = selected_shape,
                fill_color = "#facc15",
                fill_opacity = 0.28,
                tooltip = "area_label",
                before_id = "waterway_line_label"
            ) %>%
            mapgl::add_line_layer(
                id = "obs-area-line",
                source = selected_shape,
                line_color = "#f59e0b",
                line_width = 3,
                line_opacity = 0.95,
                before_id = "waterway_line_label"
            )
    }
})

observe({
    req(obs_map_initialized())

    obs_style_change_trigger()
    stations_in_box <- stations_data()
    proxy <- mapgl::maplibre_proxy("map_obs") %>%
        mapgl::clear_layer("stations")

    if (!is.null(stations_in_box) && nrow(stations_in_box) > 0) {
        stations_map <- stations_in_box %>%
            dplyr::mutate(
                StationID = as.character(StationID),
                station_label = vapply(
                    seq_len(nrow(stations_in_box)),
                    function(i) build_obs_station_label(stations_in_box[i, ]),
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
                before_id = "waterway_line_label"
            )
    }

    selected_id <- isolate(selected_station_obs_id())
    if (!is.null(selected_id) && !is.null(stations_in_box)) {
        selected_station <- stations_in_box[as.character(stations_in_box$StationID) == as.character(selected_id), ]
        if (nrow(selected_station) > 0) {
            highlight_selected_obs_station(proxy, selected_station, move_map = FALSE)
        } else {
            proxy %>% mapgl::clear_layer("selected-highlight")
        }
    } else {
        proxy %>% mapgl::clear_layer("selected-highlight")
    }
})

# Reactive value to store downloaded weather data
weather_data_obs <- reactiveVal(NULL)

# ============================================================================
# ASYNC FETCH STATE MACHINE (DWD-Style)
# ============================================================================

# State Variables
fetch_stage <- reactiveVal(0) # 0=Idle, 1=Init, 2=NextParam, 3=Fetch, 4=Merge
fetch_token <- reactiveVal(NULL) # Cancellation token
fetch_queue <- reactiveVal(list()) # List of parameters to fetch
fetch_queue_idx <- reactiveVal(0)
fetch_parsed_data <- reactiveVal(list()) # Accumulated parsed data
fetch_station_id <- reactiveVal(NULL)
fetch_station_name <- reactiveVal(NULL)

# Helper: Reset fetch state
reset_fetch <- function(session, msg = NULL) {
    fetch_stage(0)
    fetch_token(as.numeric(Sys.time())) # Invalidate current token
    fetch_queue(list())
    fetch_queue_idx(0)
    fetch_parsed_data(list())

    session$sendCustomMessage("unfreezeUI", list())
    if (!is.null(msg)) {
        showNotification(msg, type = "warning", duration = 3)
    }
}

# Handle Cancel from Freeze Window
observeEvent(input$cancel_loading, {
    session <- getDefaultReactiveDomain()
    reset_fetch(session, "Loading cancelled by user.")
})

# Observe marker clicks to START fetch
observeEvent(input$map_obs_feature_click, {
    click <- input$map_obs_feature_click
    if (!is.null(click$layer) && identical(click$layer, "stations")) {
        props <- click$properties
        station_id <- props$StationID
        if (is.null(station_id) && !is.null(click$id)) {
            station_id <- click$id
        }

        if (is.null(station_id)) {
            return()
        }

        station_id <- as.character(station_id)
        session <- getDefaultReactiveDomain()

        print(paste("Clicked station:", station_id))

        # Get station metadata for display
        all_stations <- stations_data()
        meta <- all_stations[as.character(all_stations$StationID) == station_id, ]

        station_name <- if (nrow(meta) > 0) meta$StationName[1] else station_id
        country <- if (nrow(meta) > 0) meta$Country[1] else ""
        display_info <- paste0(station_name, " (", country, ")")

        selected_station_obs_id(station_id)
        if (nrow(meta) > 0) {
            highlight_selected_obs_station(mapgl::maplibre_proxy("map_obs"), meta)
        }

        # Initialize fetch state
        fetch_station_id(station_id)
        fetch_station_name(display_info)
        fetch_parsed_data(list())

        # Build parameter queue
        params <- list(
            "air_temperature_mean" = "MeanTemp",
            "air_temperature_absolute_max" = "MaxTempAbs",
            "air_temperature_absolute_min" = "MinTempAbs",
            "air_temperature_mean_of_daily_max" = "MeanMaxTemp",
            "air_temperature_mean_of_daily_min" = "MeanMinTemp",
            "precipitation_total" = "Precipitation",
            "precipGE1mm_days" = "PrecipDays",
            "sunshine_duration" = "SunshineDuration",
            "mean_sea_level_pressure" = "MeanSeaLevelPressure",
            "vapour_pressure" = "VapourPressure"
        )
        fetch_queue(params)
        fetch_queue_idx(1)

        # Create new token
        fetch_token(as.numeric(Sys.time()))

        # Freeze UI
        session$sendCustomMessage("freezeUI", list(
            text = "Initializing download...",
            station = display_info
        ))

        # Start state machine
        fetch_stage(2) # Go to NextParam
    }
})

# Stage 2: Next Parameter
observe({
    req(fetch_stage() == 2)

    session <- getDefaultReactiveDomain()
    token <- fetch_token()
    idx <- fetch_queue_idx()
    params <- fetch_queue()

    if (idx > length(params)) {
        # All done -> Merge
        fetch_stage(4)
        return()
    }

    param_names <- names(params)
    param_dir <- param_names[idx]
    col_name <- params[[param_dir]]

    # Update UI
    session$sendCustomMessage("freezeUI", list(
        text = paste0("Fetching ", col_name, " (", idx, "/", length(params), ")..."),
        station = fetch_station_name()
    ))

    # Move to Fetch stage after yielding control
    later::later(function() {
        # Check if cancelled
        if (!identical(isolate(fetch_token()), token)) {
            return() # Token invalidated, stop
        }
        isolate({
            fetch_stage(3)
        })
    }, 0.1)

    fetch_stage(-1) # Hold
})

# Stage 3: Fetch current parameter
observe({
    req(fetch_stage() == 3)

    session <- getDefaultReactiveDomain()
    token <- fetch_token()
    idx <- fetch_queue_idx()
    params <- fetch_queue()
    station_id <- fetch_station_id()

    param_names <- names(params)
    param_dir <- param_names[idx]
    col_name <- params[[param_dir]]

    later::later(function() {
        # Check if cancelled
        if (!identical(isolate(fetch_token()), token)) {
            return()
        }

        isolate({
            base_url <- "https://opendata.dwd.de/climate_environment/CDC/observations_global/CLIMAT/monthly/qc/"
            month_map <- c(
                "Jan" = 1, "Feb" = 2, "Mrz" = 3, "Apr" = 4, "Mai" = 5, "Jun" = 6,
                "Jul" = 7, "Aug" = 8, "Sep" = 9, "Okt" = 10, "Nov" = 11, "Dez" = 12
            )

            # Fetch recent
            url_recent <- paste0(base_url, param_dir, "/recent/", sprintf("%05d", as.integer(station_id)), ".txt")
            df_recent <- tryCatch(parse_dwd_file(url_recent, col_name, month_map), error = function(e) NULL)

            # Check again
            if (!identical(fetch_token(), token)) {
                return()
            }

            # Fetch historical
            url_hist <- tryCatch(get_historical_url(param_dir, sprintf("%05d", as.integer(station_id)), base_url, session), error = function(e) NULL)
            df_hist <- if (!is.null(url_hist)) tryCatch(parse_dwd_file(url_hist, col_name, month_map), error = function(e) NULL) else NULL

            # Check again
            if (!identical(fetch_token(), token)) {
                return()
            }

            # Combine
            df_combined <- rbind(df_recent, df_hist)
            if (!is.null(df_combined) && nrow(df_combined) > 0) {
                df_combined <- df_combined[!duplicated(df_combined$Date), ]

                current_data <- fetch_parsed_data()
                current_data[[col_name]] <- df_combined
                fetch_parsed_data(current_data)
            }

            # Move to next parameter
            fetch_queue_idx(idx + 1)
            fetch_stage(2)
        })
    }, 0.1)

    fetch_stage(-1) # Hold
})

# Stage 4: Merge and Finalize
observe({
    req(fetch_stage() == 4)

    session <- getDefaultReactiveDomain()
    token <- fetch_token()

    session$sendCustomMessage("freezeUI", list(
        text = "Merging data...",
        station = fetch_station_name()
    ))

    later::later(function() {
        if (!identical(isolate(fetch_token()), token)) {
            return()
        }

        isolate({
            all_data <- fetch_parsed_data()

            if (length(all_data) == 0) {
                session$sendCustomMessage("unfreezeUI", list())
                showNotification("No data found for this station.", type = "error", duration = 5)
                fetch_stage(0)
                return()
            }

            # Merge all parameters
            first_key <- names(all_data)[1]
            final_df <- all_data[[first_key]]

            if (length(all_data) > 1) {
                for (key in names(all_data)[-1]) {
                    final_df <- merge(final_df, all_data[[key]], by = "Date", all = TRUE)
                }
            }

            final_df <- final_df[order(final_df$Date), ]

            # Create complete date sequence to show gaps in data
            min_date <- min(final_df$Date, na.rm = TRUE)
            max_date <- max(final_df$Date, na.rm = TRUE)
            complete_dates <- seq.Date(
                from = as.Date(paste0(format(min_date, "%Y-%m"), "-01")),
                to = as.Date(paste0(format(max_date, "%Y-%m"), "-01")),
                by = "month"
            )
            complete_df <- data.frame(Date = complete_dates)
            final_df <- merge(complete_df, final_df, by = "Date", all.x = TRUE)
            final_df <- final_df[order(final_df$Date), ]

            final_df$Year <- as.numeric(format(final_df$Date, "%Y"))
            final_df$Month <- as.numeric(format(final_df$Date, "%m"))

            # Reorder columns
            params <- fetch_queue()
            cols <- c("Date", "Year", "Month", unlist(params, use.names = FALSE))
            cols <- intersect(cols, names(final_df))
            final_df <- final_df[, cols]

            weather_data_obs(final_df)

            weather_data_obs(final_df)

            # Switch tab to Dashboard
            updateTabsetPanel(session = session, inputId = "obs_tab", selected = "Dashboard")

            session$sendCustomMessage("unfreezeUI", list())
            fetch_stage(0)
        })
    }, 0.1)

    fetch_stage(-1)
})

# ============================================================================
# END ASYNC STATE MACHINE
# ============================================================================

# Render weather plots - create reactive for shared plot list
obs_plot_list <- reactive({
    data <- weather_data_obs()
    station_name <- fetch_station_name()
    if (is.null(station_name)) station_name <- "Station Data"
    plot_climat_obs_data(data, station_name)
})

# Render 4 separate plot outputs
output$obs_plot_temp <- renderPlotly({
    plots <- obs_plot_list()
    plots$temp
})

output$obs_plot_precip <- renderPlotly({
    plots <- obs_plot_list()
    plots$precip
})

output$obs_plot_sun <- renderPlotly({
    plots <- obs_plot_list()
    plots$sun
})

output$obs_plot_pressure <- renderPlotly({
    plots <- obs_plot_list()
    plots$pressure
})

# Render Station Metadata Header
output$obs_station_meta <- renderUI({
    id <- fetch_station_id()
    stations <- stations_data()

    if (is.null(id) || is.null(stations)) {
        return(span("No station selected", style = "font-weight:bold; font-size:1.2em;"))
    }

    # Find station in data
    st <- stations[stations$StationID == id, ]
    if (nrow(st) == 0) {
        return(span("Station not found", style = "font-weight:bold; font-size:1.2em;"))
    }

    # Format coordinates
    lat <- round(st$Latitude, 4)
    lon <- round(st$Longitude, 4)
    lat_dir <- if (lat >= 0) "°N" else "°S"
    lon_dir <- if (lon >= 0) "°E" else "°W"
    coords <- paste0(abs(lat), lat_dir, ", ", abs(lon), lon_dir)

    # Create styled metadata display
    div(
        style = "display: flex; flex-direction: column; gap: 0.2rem;",
        div(
            style = "font-weight: bold; font-size: 1.2em;",
            st$StationName,
            span(
                style = "font-weight: normal; color: #666; font-size: 0.9em; margin-left: 0.5rem;",
                paste0("ID: ", st$StationID, " | ", st$Country)
            )
        ),
        div(
            style = "font-size: 0.95em; color: #555;",
            span(style = "margin-right: 1rem;", paste0("⛰️ ", st$Height, "m")),
            span(paste0("📍 ", coords))
        )
    )
})

# Download Handler
output$download_obs_data <- downloadHandler(
    filename = function() {
        station_name <- fetch_station_name()
        if (is.null(station_name)) station_name <- "station"
        # Sanitize filename
        safe_name <- gsub("[^a-zA-Z0-9]", "_", station_name)
        paste0("climat_", safe_name, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
        data <- weather_data_obs()
        if (!is.null(data)) {
            write.csv(data, file, row.names = FALSE)
        }
    }
)

# Render weather data table
output$weather_data_table <- DT::renderDataTable({
    data <- weather_data_obs()
    print(paste("Rendering weather_data_table. Data is null?", is.null(data))) # Debugging
    if (is.null(data)) {
        DT::datatable(data.frame(Message = "No data selected. Click a station on the map to load data."),
            options = list(pageLength = 5, dom = "t")
        )
    } else {
        print(paste("Weather data rows:", nrow(data))) # Debugging
        DT::datatable(data, options = list(pageLength = 15, scrollX = TRUE))
    }
})

# Render station info table
output$stations_table <- DT::renderDataTable({
    stations_in_box <- stations_data()
    if (is.null(stations_in_box) || nrow(stations_in_box) == 0) {
        DT::datatable(data.frame(Message = "No station data available"), options = list(pageLength = 5))
    } else {
        # Convert sf to data.frame for DT display (drop geometry column)
        stations_df <- sf::st_drop_geometry(stations_in_box)
        DT::datatable(stations_df, options = list(pageLength = 15))
    }
})

# Zoom to selected area
observeEvent(input$test_area_obs, {
    req(input$test_area_obs)

    req(obs_map_initialized())

    shape_to_zoom <- get_obs_shape(input$test_area_obs)

    mapgl::maplibre_proxy("map_obs") %>%
        mapgl::fit_bounds(
            get_obs_bbox(shape_to_zoom),
            animate = TRUE
        )
})

output$map_titl_obs <- renderText({
    "Climate Observational Data Map"
})

output$graph_titl_obs <- renderText({
    "Timeseries Plot - Placeholder"
})

# Climate Observational Data Server Logic

# Create a reactive to fetch stations once (shared between map and table)
stations_data <- reactive({
    get_dwd_stations(shape = dun, buffer_km = 25)
})

# Invalidate map size when switching tabs (fixes rendering issues)
observeEvent(input$obs_tab, {
    if (input$obs_tab == "Map View") {
        leafletProxy("map_obs") %>%
            leaflet.extras::removeDrawToolbar(clearFeatures = FALSE)
        # Force map to recalculate its size
        shinyjs::runjs('setTimeout(function(){ window.dispatchEvent(new Event("resize")); }, 100);')
    }
})

# Render the initial map
output$map_obs <- renderLeaflet({
    # Calculate bounding box of the Danube basin
    bbox <- sf::st_bbox(dun)

    # Get all stations filtered by buffer
    stations_in_box <- stations_data()

    map <- leaflet_fun() |>
        fitBounds(lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]], lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]])

    if (!is.null(stations_in_box) && nrow(stations_in_box) > 0) {
        map <- map |>
            addCircleMarkers(
                data = stations_in_box,
                layerId = ~ as.character(StationID), # Essential for click events!
                label = lapply(seq_len(nrow(stations_in_box)), function(i) {
                    htmltools::HTML(paste0(
                        "<b>WMO Station ID: </b>", stations_in_box$StationID[i], "<br>",
                        "<b>Station Name: </b>", stations_in_box$StationName[i], "<br>",
                        "<b>Latitude: </b>", stations_in_box$Latitude[i], "<br>",
                        "<b>Longitude: </b>", stations_in_box$Longitude[i], "<br>",
                        "<b>Altitude (Height): </b>", stations_in_box$Height[i], " m<br>",
                        "<b>Country: </b>", stations_in_box$Country[i]
                    ))
                }),
                labelOptions = labelOptions(
                    textsize = "15px",
                    direction = "auto",
                    style = list(
                        "color" = "black",
                        "font-family" = "Arial",
                        "font-size" = "15px"
                    )
                ),
                radius = 5,
                color = "blue",
                stroke = FALSE,
                fillOpacity = 0.8,
                group = "Stations"
            )
    }

    map
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
observeEvent(input$map_obs_marker_click, {
    click <- input$map_obs_marker_click
    if (!is.null(click$id)) {
        station_id <- click$id
        session <- getDefaultReactiveDomain()

        print(paste("Clicked station:", station_id))

        # Get station metadata for display
        all_stations <- stations_data()
        meta <- all_stations[all_stations$StationID == station_id, ]

        station_name <- if (nrow(meta) > 0) meta$StationName[1] else station_id
        country <- if (nrow(meta) > 0) meta$Country[1] else ""
        display_info <- paste0(station_name, " (", country, ")")

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

    shape_to_zoom <- switch(input$test_area_obs,
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
        "ro4" = ms6_romania
    )

    bbox <- sf::st_bbox(sf::st_buffer(shape_to_zoom, dist = 10000))

    proxy <- leafletProxy("map_obs") %>%
        removeShape(layerId = "highlighted_polygon_obs")

    if (input$test_area_obs != "drb") {
        proxy <- proxy %>%
            addPolygons(
                data = shape_to_zoom,
                layerId = "highlighted_polygon_obs",
                fillColor = "yellow",
                fillOpacity = 0.5,
                color = "orange",
                weight = 3,
                stroke = TRUE,
                label = names(select_area[select_area == input$test_area_obs]),
                labelOptions = labelOptions(
                    style = list(
                        "color" = "black",
                        "font-family" = "Arial",
                        "font-weight" = "bold",
                        "font-size" = "12px"
                    ),
                    textsize = "12px",
                    direction = "auto"
                )
            )
    }

    proxy %>%
        fitBounds(
            lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
            lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
        )
})

output$map_titl_obs <- renderText({
    "Climate Observational Data Map"
})

output$graph_titl_obs <- renderText({
    "Timeseries Plot - Placeholder"
})

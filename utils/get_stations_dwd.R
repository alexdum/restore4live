get_dwd_stations <- function(shape = NULL, buffer_km = 25) {
    url <- "https://opendata.dwd.de/climate_environment/CDC/help/stations_list_CLIMAT_data.txt"

    # Define cache file path (store in www/data/tabs)
    cache_dir <- "www/data/tabs"
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
    cache_file <- file.path(cache_dir, "stations_list_CLIMAT_data.txt")

    # Try to download the file only if cache is older than 31 days
    download_success <- FALSE
    if (file.exists(cache_file)) {
        age_secs <- as.numeric(difftime(Sys.time(), file.info(cache_file)$mtime, units = "secs"))
        if (age_secs < 31 * 24 * 60 * 60) {
            download_success <- TRUE
            warning("Using cached DWD station list (younger than 31 days).")
        }
    }
    if (!download_success) {
        tryCatch(
            {
                download.file(url, cache_file, method = "curl", quiet = TRUE)
                download_success <- TRUE
            },
            error = function(e) {
                warning("Failed to download DWD station list: ", e$message)
            }
        )
    }

    # If download failed, check if we have a cached version
    if (!download_success) {
        if (file.exists(cache_file)) {
            warning("Using cached DWD station list.")
        } else {
            warning("No cached DWD station list available. Returning empty dataset.")
            return(NULL) # Or return empty sf object structure
        }
    }

    # Read the data
    # Use tryCatch during read as well just in case the file is corrupt
    stations <- tryCatch(
        {
            read.table(cache_file, sep = ";", header = TRUE, fill = TRUE, quote = "", stringsAsFactors = FALSE, fileEncoding = "latin1")
        },
        error = function(e) {
            warning("Failed to read DWD station list file: ", e$message)
            return(NULL)
        }
    )

    if (is.null(stations)) {
        return(NULL)
    }

    # Clean up column names
    names(stations) <- c("StationID", "StationName", "Latitude", "Longitude", "Height", "Country")

    # Data cleaning
    stations <- stations |>
        dplyr::mutate(
            Latitude = as.numeric(Latitude),
            Longitude = as.numeric(Longitude),
            Height = as.numeric(Height)
        ) |>
        dplyr::filter(!is.na(Latitude) & !is.na(Longitude))

    # Convert to sf object
    stations_sf <- sf::st_as_sf(stations, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

    if (!is.null(shape)) {
        # Ensure shape is valid and try to union if it consists of multiple features
        # to avoid buffering each feature individually which can be slow and create overlaps
        shape_union <- sf::st_union(shape) |> sf::st_make_valid()

        # Transform to a metric CRS for buffering (EPSG:3035 - ETRS89-extended / LAEA Europe is good for Europe)
        # If the shape is roughly in Europe this is fine.
        shape_metric <- sf::st_transform(shape_union, 3035)

        # Buffer in meters
        shape_buffered <- sf::st_buffer(shape_metric, dist = buffer_km * 1000)

        # Transform buffer back to WGS84 to match stations
        shape_buffered_4326 <- sf::st_transform(shape_buffered, 4326)

        # Filter stations
        stations_sf <- sf::st_filter(stations_sf, shape_buffered_4326)
    }

    return(stations_sf)
}

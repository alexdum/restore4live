#' Parse DWD CLIMAT Weather Data for a Station
#'
#' Downloads and parses monthly CLIMAT weather data from DWD Open Data
#' for a given WMO station ID, fetching both RECENT and HISTORICAL data.
#'
#' @param station_id Character or numeric WMO station ID
#' @param session Shiny session for progress updates (optional)
#' @param station_name Station name for display (optional)
#' @param check_cancel Function that returns TRUE if cancelled (optional)
#' @return A data.frame with parsed weather data, or NULL if download fails
#'

# Cancellation flag environment
.dwd_state <- new.env(parent = emptyenv())
.dwd_state$cancelled <- FALSE

# Function to set cancellation
cancel_dwd_download <- function() {
    .dwd_state$cancelled <- TRUE
}

# Function to reset cancellation
reset_dwd_cancel <- function() {
    .dwd_state$cancelled <- FALSE
}

# Function to check cancellation
is_dwd_cancelled <- function() {
    .dwd_state$cancelled
}

# Helper: Parse a single DWD CLIMAT file
parse_dwd_file <- function(file_url, col_name, month_map) {
    temp_file <- tempfile(fileext = ".txt")

    download_success <- tryCatch(
        {
            # Use curl with timeout (10 seconds)
            h <- curl::new_handle()
            curl::handle_setopt(h, timeout = 10, connecttimeout = 5)
            req <- curl::curl_fetch_disk(file_url, temp_file, handle = h)
            req$status_code == 200
        },
        error = function(e) FALSE
    )

    if (!download_success) {
        return(NULL)
    }

    raw_data <- tryCatch(
        {
            read.table(temp_file, sep = ";", header = TRUE, stringsAsFactors = FALSE, na.strings = "-999")
        },
        error = function(e) NULL
    )

    unlink(temp_file)

    if (is.null(raw_data) || nrow(raw_data) == 0) {
        return(NULL)
    }

    # Reshape
    years <- raw_data$JAHR
    valid_months <- intersect(names(raw_data), names(month_map))

    if (length(valid_months) == 0) {
        return(NULL)
    }

    date_list <- list()
    val_list <- list()

    for (m_name in valid_months) {
        m_num <- month_map[m_name]
        vals <- raw_data[[m_name]]
        current_dates <- as.Date(paste(years, m_num, "1", sep = "-"))

        date_list[[m_name]] <- current_dates
        val_list[[m_name]] <- vals
    }

    long_df <- data.frame(
        Date = do.call(c, date_list),
        Value = do.call(c, val_list)
    )

    names(long_df)[2] <- col_name
    long_df <- long_df[!is.na(long_df$Date) & !is.na(long_df[[col_name]]), ]
    # Filter out future dates
    long_df <- long_df[long_df$Date <= Sys.Date(), ]

    return(long_df)
}

# Helper: Get Historical File URL using cached index
get_historical_url <- function(param_dir, station_id, base_url, session = NULL) {
    cache_dir <- "www/data/indices"
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

    cache_file <- file.path(cache_dir, paste0(param_dir, "_hist_index.rds"))

    index <- NULL
    if (file.exists(cache_file) && difftime(Sys.time(), file.info(cache_file)$mtime, units = "days") < 30) {
        index <- readRDS(cache_file)
    } else {
        # Update message if session available
        if (!is.null(session)) {
            session$sendCustomMessage("freezeUI", list(text = paste("Downloading index for", param_dir, "...")))
        }

        hist_url <- paste0(base_url, param_dir, "/historical/")

        # Use curl with timeout instead of blocking readLines
        html_content <- tryCatch(
            {
                h <- curl::new_handle()
                curl::handle_setopt(h, timeout = 15, connecttimeout = 5)
                req <- curl::curl_fetch_memory(hist_url, handle = h)
                if (req$status_code == 200) {
                    strsplit(rawToChar(req$content), "\n")[[1]]
                } else {
                    NULL
                }
            },
            error = function(e) NULL
        )

        if (!is.null(html_content)) {
            matches <- regmatches(html_content, regexec('href="?([0-9]{5}_[0-9]{6}_[0-9]{6}\\.txt)"?', html_content))
            files <- sapply(matches, function(m) if (length(m) > 1) m[2] else NA)
            files <- na.omit(files)

            ids <- substr(files, 1, 5)
            index <- data.frame(ID = ids, File = files, stringsAsFactors = FALSE)

            saveRDS(index, cache_file)
        }
    }

    if (is.null(index)) {
        return(NULL)
    }

    match <- index[index$ID == station_id, "File"]
    if (length(match) > 0) {
        return(paste0(base_url, param_dir, "/historical/", tail(match, 1)))
    }

    return(NULL)
}

# Helper: Get Recent File URL using substring matching (directory listing)
get_recent_url <- function(param_dir, station_id, base_url) {
    # We don't cache recent index as it changes frequently
    recent_url <- paste0(base_url, param_dir, "/recent/")

    # Use curl for timeout
    html_content <- tryCatch(
        {
            h <- curl::new_handle()
            curl::handle_setopt(h, timeout = 10, connecttimeout = 5)
            req <- curl::curl_fetch_memory(recent_url, handle = h)
            if (req$status_code == 200) {
                strsplit(rawToChar(req$content), "\n")[[1]]
            } else {
                NULL
            }
        },
        error = function(e) NULL
    )

    if (is.null(html_content)) {
        return(NULL)
    }

    # Regex to find file: ID_start_end.txt
    pattern <- paste0('href="?', station_id, "_[0-9]{6}_[0-9]{6}\\.txt")
    matches <- regmatches(html_content, regexec(pattern, html_content))
    files <- sapply(matches, function(m) if (length(m) > 0) m[1] else NA)

    # Clean up href=" part if present
    files <- gsub('href="?', "", na.omit(files))
    # Remove closing quote if present
    files <- gsub('"', "", files)

    if (length(files) > 0) {
        # Return the last one (most recent)
        return(paste0(recent_url, tail(files, 1)))
    }

    return(NULL)
}

parse_weather_dwd <- function(station_id, session = NULL, station_name = NULL) {
    station_id <- sprintf("%05d", as.integer(station_id))

    # Reset cancellation flag at start
    reset_dwd_cancel()

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

    base_url <- "https://opendata.dwd.de/climate_environment/CDC/observations_global/CLIMAT/monthly/qc/"

    month_map <- c(
        "Jan" = 1, "Feb" = 2, "Mrz" = 3, "Apr" = 4, "Mai" = 5, "Jun" = 6,
        "Jul" = 7, "Aug" = 8, "Sep" = 9, "Okt" = 10, "Nov" = 11, "Dez" = 12
    )

    all_data <- list()
    param_names <- names(params)
    total_params <- length(param_names)

    for (i in seq_along(param_names)) {
        # Check for cancellation
        if (is_dwd_cancelled()) {
            message("Download cancelled by user")
            return(NULL)
        }

        param_dir <- param_names[i]
        col_name <- params[[param_dir]]

        # Update progress
        if (!is.null(session)) {
            progress_msg <- paste0("Fetching ", col_name, " (", i, "/", total_params, ")...")
            session$sendCustomMessage("freezeUI", list(
                text = progress_msg,
                station = if (!is.null(station_name)) station_name else paste("Station", station_id)
            ))
        }

        # 1. Recent
        url_recent <- get_recent_url(param_dir, station_id, base_url)
        df_recent <- if (!is.null(url_recent)) parse_dwd_file(url_recent, col_name, month_map) else NULL

        # Check cancellation again
        if (is_dwd_cancelled()) {
            message("Download cancelled by user")
            return(NULL)
        }

        # 2. Historical
        url_hist <- get_historical_url(param_dir, station_id, base_url, session)
        df_hist <- if (!is.null(url_hist)) parse_dwd_file(url_hist, col_name, month_map) else NULL

        # 3. Combine
        df_combined <- rbind(df_recent, df_hist)

        if (!is.null(df_combined) && nrow(df_combined) > 0) {
            df_combined <- df_combined[!duplicated(df_combined$Date), ]
            all_data[[col_name]] <- df_combined
        }
    }

    if (length(all_data) == 0) {
        return(NULL)
    }

    # Merge
    if (!is.null(session)) {
        session$sendCustomMessage("freezeUI", list(text = "Merging data..."))
    }

    first_key <- names(all_data)[1]
    final_df <- all_data[[first_key]]

    if (length(all_data) > 1) {
        for (key in names(all_data)[-1]) {
            final_df <- merge(final_df, all_data[[key]], by = "Date", all = TRUE)
        }
    }

    final_df <- final_df[order(final_df$Date), ]
    final_df$Year <- as.numeric(format(final_df$Date, "%Y"))
    final_df$Month <- as.numeric(format(final_df$Date, "%m"))

    cols <- c("Date", "Year", "Month", unlist(params, use.names = FALSE))
    cols <- intersect(cols, names(final_df))
    final_df <- final_df[, cols]

    return(final_df)
}

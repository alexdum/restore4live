extract_data <- function(file_hist, file_scen, dataset_function, lon, lat, param, season, quant, period_change, period_climate, file_ind) {
  
  # 1. Differentiate logic based on input type (file path vs SpatRaster)
  if (is.character(file_hist)) {
    # --- Logic for main parameters (e.g., tas, pr) using Python ---
    dd_hist <- dataset_function(fname = file_hist, lon = lon, lat = lat, variable = param)

    if (is.character(dd_hist)) {
      return(dd_hist) # Return error message from Python
    }

    dd_scen <- dataset_function(fname = file_scen, lon = lon, lat = lat, variable = param)
    dd_hist_90 <- dataset_function(fname = gsub("-50_", "-90_", file_hist), lon = lon, lat = lat, variable = param)
    dd_scen_90 <- dataset_function(fname = gsub("-50_", "-90_", file_scen), lon = lon, lat = lat, variable = param)
    dd_hist_10 <- dataset_function(fname = gsub("-50_", "-10_", file_hist), lon = lon, lat = lat, variable = param)
    dd_scen_10 <- dataset_function(fname = gsub("-50_", "-10_", file_scen), lon = lon, lat = lat, variable = param)

    # Combine data into a single data frame
    dd <- data.frame(
      date = as.Date(names(append(dd_hist, dd_scen))),
      value = round(append(dd_hist, dd_scen), 1),
      value10 = round(append(dd_hist_10, dd_scen_10), 1),
      value90 = round(append(dd_hist_90, dd_scen_90), 1)
    )

  } else {
    # --- New logic for climate indices (e.g., tr) using R/terra ---
    
    # Combine historical and scenario SpatRasters
    r <- c(file_hist, file_scen)

    
    r90 <- rast(gsub("ensmean","ensp90",file_ind))
    r10 <- rast(gsub("ensmean","ensp10",file_ind))
    


    
    # Create a point geometry for extraction
    point_sf <- sf::st_as_sf(data.frame(lon = lon, lat = lat), coords = c("lon", "lat"), crs = 4326)
    
    # Extract time series data
    extracted_values <- terra::extract(r, point_sf, ID = FALSE)
    extracted_values90 <- terra::extract(r90, point_sf, ID = FALSE)
    extracted_values10 <- terra::extract(r10, point_sf, ID = FALSE)

    
    if (all(is.na(extracted_values))) {
      return("No data available for the selected point")
    }
    
    # Create data frame
    dd <- data.frame(
      date = as.Date(time(r)),
      value = round(as.numeric(extracted_values), 1),
      value10 = round(as.numeric(extracted_values10), 1),
      value90 = round(as.numeric(extracted_values90), 1)
    )
  }

  # --- Common processing logic for all parameter types ---
  
  # Filter by season if it's not annual
  # Note: This might need a more robust way to handle seasonal abbreviations like "DJF"
  if (!season %in% c("year", "ANN")) {
    dd <- dd |> dplyr::filter(format(date, "%m") %in% season)
  }
  
  # Filter data based on the time period
  if (quant %in% "climate") {
    end_year <- as.numeric(strsplit(period_climate, "-")[[1]][2])
    ddf <- dd |> dplyr::filter(as.numeric(format(date, "%Y")) <= end_year)
  } else { # quant is "change"
    end_year <- as.numeric(strsplit(period_change[2], "-")[[1]][2])
    ddf <- dd |> dplyr::filter(as.numeric(format(date, "%Y")) <= end_year)
  }

  if (nrow(ddf) == 0) {
    return("No data available for the selected period and season")
  }

  # Calculate change if requested
  if (quant %in% "change") {
    
    # Baseline period
    an1_hist <- as.numeric(strsplit(period_change[1], "-")[[1]][1])
    an2_hist <- as.numeric(strsplit(period_change[1], "-")[[1]][2])
    
    # Calculate mean of the baseline period
    baseline_mean <- mean(
      ddf$value[as.numeric(format(ddf$date, "%Y")) >= an1_hist & as.numeric(format(ddf$date, "%Y")) <= an2_hist],
      na.rm = TRUE
    )
    
    if (is.na(baseline_mean) || baseline_mean == 0) {
      return("Could not calculate baseline for change analysis.")
    }
    
    # Calculate the change relative to the baseline
    if (param %in% "pr") {
      ddf$value <- round(((ddf$value * 100) / baseline_mean) - 100, 1)
      ddf$value90 <- round(((ddf$value90 * 100) / baseline_mean) - 100, 1)
      ddf$value10 <- round(((ddf$value10 * 100) / baseline_mean) - 100, 1)
    } else {
      ddf$value <- round(ddf$value - baseline_mean, 1)
      ddf$value90 <- round(ddf$value90 - baseline_mean, 1)
      ddf$value10 <- round(ddf$value10 - baseline_mean, 1)
    }
  }
  
  return(ddf)
}

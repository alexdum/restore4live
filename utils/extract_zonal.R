#' Extract Zonal Time Series Data
#'
#' Extracts and processes time series data for a given polygon. It calculates the
#' spatial mean for the 10th, 50th, and 90th percentiles of the model ensemble.
#' If requested, it computes the change relative to a baseline period.
#'
#' @param file_hist Path to the historical NetCDF file.
#' @param file_scen Path to the scenario NetCDF file.
#' @param shape An 'sf' object representing the polygon for zonal statistics.
#' @param season_subset A string indicating the months for seasonal subsetting.
#' @param param The climate parameter (e.g., "tas", "pr").
#' @param quant The quantity to compute ("climate" or "change").
#' @param period_change A vector of two strings for the baseline and future periods
#'   (e.g., c("1981-2010", "2041-2060")).
#'
#' @return A data frame with date, value (mean of 50th percentile),
#'   value10 (mean of 10th percentile), and value90 (mean of 90th percentile).
#' @param file_ind Path to the main index file (e.g., ensmean). Only used for
#'   climate indices when `file_hist` is a SpatRaster.
extract_zonal_data <- function(file_hist, file_scen, shape, season_subset, param, quant, period_change, file_ind = NA) {
  
  # Helper function to subset by season and extract zonal mean
  get_zonal_mean <- function(r, shp, season) {
    dats <- time(r)
    
    # Subset for season if necessary
    if (!season %in% c("year", "ANN")) {
      r <- r[[which(format(dats, "%m") %in% season)]]
      dats <- time(r)
    }
    
    # terra::extract is efficient for this task, we only need the mean
    zonal_mean <- terra::extract(r, vect(shp), fun = mean, na.rm = TRUE, ID = FALSE)
    
    if (nrow(zonal_mean) > 0 && ncol(zonal_mean) > 0) {
      return(list(dates = dats, values = as.numeric(zonal_mean[1, ])))
    } else {
      return(list(dates = as.Date(character()), values = numeric()))
    }
  }

  if (is.character(file_hist)) {
    # --- Logic for main parameters (e.g., tas, pr) from file paths ---
    r_50 <- c(rast(file_hist), rast(file_scen))
    r_10 <- c(rast(gsub("-50_", "-10_", file_hist)), rast(gsub("-50_", "-10_", file_scen)))
    r_90 <- c(rast(gsub("-50_", "-90_", file_hist)), rast(gsub("-50_", "-90_", file_scen)))
  } else {
    # --- New logic for climate indices (e.g., tr) from SpatRaster objects ---
    r_50 <- c(file_hist, file_scen)
    
    # For indices, p10 and p90 are in separate files derived from file_ind
    r_10 <- rast(gsub("ensmean", "ensp10", file_ind))
    r_90 <- rast(gsub("ensmean", "ensp90", file_ind))
    
    # Ensure time is set correctly for all rasters
    time(r_10) <- time(r_50)
    time(r_90) <- time(r_50)
  }
  
  # Extract zonal data for each percentile
  data_50 <- get_zonal_mean(r_50, shape, season_subset)
  data_10 <- get_zonal_mean(r_10, shape, season_subset)
  data_90 <- get_zonal_mean(r_90, shape, season_subset)
  
  # --- Common processing logic ---
  if (length(data_50$dates) == 0) {
    return(data.frame(date = as.Date(character()), value = numeric(), value10 = numeric(), value90 = numeric()))
  }
  
  ddf <- data.frame(
    date = data_50$dates,
    value = round(data_50$values, 1),
    value10 = round(data_10$values, 1),
    value90 = round(data_90$values, 1)
  )
  
  # If "change" is selected, calculate anomalies relative to the baseline
  if (quant %in% "change") {
    # Baseline period start and end years from the first element of period_change
    an1_hist <- as.numeric(strsplit(period_change[1], "-")[[1]][1])
    an2_hist <- as.numeric(strsplit(period_change[1], "-")[[1]][2])
    
    # Calculate the mean for the baseline period from the 50th percentile data
    baseline_mean <- mean(
      ddf$value[as.numeric(format(ddf$date, "%Y")) >= an1_hist & as.numeric(format(ddf$date, "%Y")) <= an2_hist],
      na.rm = TRUE
    )
    
    # Calculate change (anomaly)
    if (param %in% "pr") {
      # Percentage change for precipitation
      ddf$value <- round(((ddf$value * 100) / baseline_mean) - 100, 1)
      ddf$value90 <- round(((ddf$value90 * 100) / baseline_mean) - 100, 1)
      ddf$value10 <- round(((ddf$value10 * 100) / baseline_mean) - 100, 1)
    } else {
      # Absolute change for temperature
      ddf$value <- round(ddf$value - baseline_mean, 1)
      ddf$value90 <- round(ddf$value90 - baseline_mean, 1)
      ddf$value10 <- round(ddf$value10 - baseline_mean, 1)
    }
  }
  
  return(ddf)
}
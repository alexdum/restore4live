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
extract_zonal_data <- function(file_hist, file_scen, shape, season_subset, param, quant, period_change) {
  
  # Helper function to load, combine, subset, and extract zonal mean
  get_zonal_mean <- function(hist_path, scen_path, shp, season) {
    # Combine historical and scenario rasters
    r <- c(rast(hist_path), rast(scen_path))
    dats <- time(r)
    
    # Subset for season if necessary
    if (season != "year") {
      r <- r[[which(format(dats, "%m") %in% season)]]
      dats <- time(r)
    }
    
    # terra::extract is efficient for this task, we only need the mean
    zonal_mean <- terra::extract(r, vect(shp), fun = mean, na.rm = TRUE, ID = FALSE)
    
    # The result from terra::extract is a wide data.frame with 1 row.
    # We need to transpose it to a single column.
    if (nrow(zonal_mean) > 0 && ncol(zonal_mean) > 0) {
      return(list(dates = dats, values = as.numeric(zonal_mean[1, ])))
    } else {
      return(list(dates = as.Date(character()), values = numeric()))
    }
  }
  
  # Extract data for each percentile by loading the corresponding files
  data_50 <- get_zonal_mean(file_hist, file_scen, shape, season_subset)
  data_10 <- get_zonal_mean(gsub("-50_", "-10_", file_hist), gsub("-50_", "-10_", file_scen), shape, season_subset)
  data_90 <- get_zonal_mean(gsub("-50_", "-90_", file_hist), gsub("-50_", "-90_", file_scen), shape, season_subset)
  
  # Check if we got any data from the main file
  if (length(data_50$dates) > 0) {
    # Create the initial data frame
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
  } else {
    # Handle cases with no data (e.g., no overlap)
    data.frame(date = as.Date(character()), value = numeric(), value10 = numeric(), value90 = numeric())
  }
}
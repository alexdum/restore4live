extract_data <- function(file_hist, file_scen, dataset_function, lon, lat, param, season) {
  # Extract data points using the dataset_function
  dd_hist <- dataset_function(fname = file_hist, lon = lon, lat = lat, variable = param)
  dd_scen <- dataset_function(fname = file_scen, lon = lon, lat = lat, variable = param)
  dd_hist_90 <- dataset_function(fname = gsub("-50_", "-90_", file_hist), lon = lon, lat = lat, variable = param)
  dd_scen_90 <- dataset_function(fname = gsub("-50_", "-90_", file_scen), lon = lon, lat = lat, variable = param)
  dd_hist_10 <- dataset_function(fname = gsub("-50_", "-10_", file_hist), lon = lon, lat = lat, variable = param)
  dd_scen_10 <- dataset_function(fname = gsub("-50_", "-10_", file_scen), lon = lon, lat = lat, variable = param)
  
  # Combine data
  dd <- append(dd_hist, dd_scen)
  dd90 <- append(dd_hist_90, dd_scen_90)
  dd10 <- append(dd_hist_10, dd_scen_10)
  
  # Create data frame
  ddf <- data.frame(date = as.Date(names(dd)), value = round(dd, 1), value10 = round(dd10, 1), value90 = round(dd90,1))
  
  # Filter data based on season subset
  if (season != "year")  ddf <- ddf |> dplyr::filter(format(date, "%m") %in% season)
  
  return(ddf)
}
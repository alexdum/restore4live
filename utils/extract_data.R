extract_data <- function(file_hist, file_scen, dataset_function, lon, lat, param, season, quant, period_climate) {
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
  
  print(summary(ddf))
  # ggrafic cu evolutia schimbarii
  if (quant %in% "change") {
    
    # pentru subsetare
    an1 <- strsplit(period_climate, "-")[[1]][1] |> as.numeric()
    an2 <- strsplit(period_climate, "-")[[1]][2] |> as.numeric()
  
    print(an1)
    print(an2)
   
    # mediaza duop input perioada
    df_mean <- mean(ddf$value[as.numeric(format(ddf$date, "%Y")) >= an1 & as.numeric(format(ddf$date, "%Y")) <= an2])
    df_mean90 <- mean(ddf$value90[as.numeric(format(ddf$date, "%Y")) >= an1 & as.numeric(format(ddf$date, "%Y")) <= an2])
    df_mean10 <- mean(ddf$value10[as.numeric(format(ddf$date, "%Y")) >= an1 & as.numeric(format(ddf$date, "%Y")) <= an2])
   
       # schimbarea
    if (param %in% "pr") {
      ddf$value <- ((ddf$value * 100)/df_mean) - 100
      ddf$value90 <- ((ddf$value90 * 100)/df_mean90) - 100
      ddf$value10 <- ((ddf$value10 * 100)/df_mean10) - 100
    
    } else {
      ddf$value <- ddf$value - df_mean 
      ddf$value90 <- ddf$value90 - df_mean90 
      ddf$value10 <- ddf$value10 - df_mean10 
    }
  }
  
  print(summary(ddf))
  return(ddf)
}
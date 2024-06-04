extract_data <- function(file_hist, file_scen, dataset_function, lon, lat, param, season, quant, period_change, period_climate) {
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
  dd <- data.frame(date = as.Date(names(dd)), value = round(dd, 1), value10 = round(dd10, 1), value90 = round(dd90,1))
  
  # Filter data based on season subset
  if (season != "year")  dd <- dd |> dplyr::filter(format(date, "%m") %in% season)
  
  # filtreaza dupa periaoda vizualizat in harta
  an2 <- strsplit(period_climate, "-")[[1]][2] |> as.numeric()
  ddf <- dd |> dplyr::filter(as.numeric(format(date,"%Y")) <= an2)

  # ggrafic cu evolutia schimbarii
  if (quant %in% "change") {
    
    # pentru subsetare
    an11 <- strsplit(period_change, "-")[[1]][1] |> as.numeric()
    an12 <- strsplit(period_change, "-")[[1]][2] |> as.numeric()
    
    an21 <- strsplit(period_change, "-")[[2]][1] |> as.numeric()
    an22 <- strsplit(period_change, "-")[[2]][2] |> as.numeric()
  
   
    ddf <- dd |> dplyr::filter(as.numeric(format(date,"%Y")) <= an22)
    
    # mediaza duop input perioada
    df_mean <- mean(ddf$value[as.numeric(format(ddf$date, "%Y")) >= an11 & as.numeric(format(ddf$date, "%Y")) <= an12])
    # df_mean90 <- mean(ddf$value90[as.numeric(format(ddf$date, "%Y")) >= an1 & as.numeric(format(ddf$date, "%Y")) <= an2])
    # df_mean10 <- mean(ddf$value10[as.numeric(format(ddf$date, "%Y")) >= an1 & as.numeric(format(ddf$date, "%Y")) <= an2])
   
       # schimbarea
    if (param %in% "pr") {
      ddf$value <- round(((ddf$value * 100)/df_mean) - 100, 1)
      ddf$value90 <- round(((ddf$value90 * 100)/df_mean) - 100, 1)
      ddf$value10 <- round(((ddf$value10 * 100)/df_mean) - 100, 1)
    
    } else {
      ddf$value <- round(ddf$value - df_mean, 1)
      ddf$value90 <- round(ddf$value90 - df_mean, 1)
      ddf$value10 <- round(ddf$value10 - df_mean, 1)
    }
  }
  
  return(ddf)
}
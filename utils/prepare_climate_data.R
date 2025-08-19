prepare_climate_data <- function(param, season, season_ind, scen, quant, period_climate, period_change, transp,
                                files_cmip6, params_def, select_seas) {
  
  if (param %in% c("tas", "tasmax", "tasmin", "pr")) {
    
    season_code <- strsplit(season, "-")[[1]][2]
    season_subset <- strsplit(season, "-")[[1]][1]
    
    # uneste istricul cu scenariiul
    file_hist <-  grep(paste0("www/data/ncs/cmip6/",param,"/hist/",param,"_hist_",season_code, "-50_"), files_cmip6, value = T)
    file_scen <- grep(paste0("www/data/ncs/cmip6/",param,"/",scen,"/",param,"_",scen,"_",season_code, "-50_"), files_cmip6, value = T)
    r <- c(rast(file_hist), rast(file_scen))
    dats <- time(r)
    
    file_ind <- NA 
  } else {
    
    file_ind <- paste0("www/data/ncs/cmip6/indices/",param,"_",season_ind,"_",scen,"_1961-2100_ensmean.nc")
    r <-  rast( file_ind )
    dats <- seq(as.Date("1961-07-01"), as.Date("2100-07-01"),'year')
    time(r) <- dats
    
    
    file_hist <- r[[format(dats, "%Y") %in% 1961:2014]]
    file_scen <- r[[format(dats, "%Y") %in% 2015:2100]]
    
    season_subset <- season_ind
    
  }
  
  # subseteaza sezoniere si lunare
  if (!season_subset %in% c("ANN","year")) r <- r[[which(format(dats, "%m") %in% season_subset)]]
  
  # pentru subsetare harta
  dats_sub <- time(r)
  
  
  # pentru selectie tip de calcul
  if (quant %in% "climate") {
    
    # pentru subsetare
    an1 <- strsplit(period_climate, "-")[[1]][1] |> as.numeric()
    an2 <- strsplit(period_climate, "-")[[1]][2] |> as.numeric()
    
    
    # mediaza duop input perioada
    r_mean <- mean(r[[format(dats_sub, "%Y") %in% an1:an2]])
    
    
    setMinMax(r_mean)
    
  } else {
    
    # pentru subsetare
    an1_hist <- strsplit(period_change[1], "-")[[1]][1] |> as.numeric()
    an2_hist <- strsplit(period_change[1], "-")[[1]][2] |> as.numeric()
    an1_scen <- strsplit(period_change[2], "-")[[1]][1] |> as.numeric()
    an2_scen <- strsplit(period_change[2], "-")[[1]][2] |> as.numeric()
    
    
    # mediaza duop input
    r_hist <- mean(r[[format(dats_sub, "%Y") %in% an1_hist:an2_hist]])
    r_scen <- mean(r[[format(dats_sub, "%Y") %in% an1_scen:an2_scen]])
    
    # schimbarea
    if (param %in% "pr") {
      r_mean <- ((r_scen * 100)/r_hist) - 100
    } else {
      r_mean <- r_scen - r_hist
    }
    setMinMax(r_mean)
  }
  
  pal <- map_cols_cmip_fun(indic = param, type = quant, domain = minmax(r_mean))
  # pentru reclasificare extreme raster cu min/max din paleta de culori
  r_mean[r_mean < pal$minmax[1]] <- pal$minmax[1]
  r_mean[r_mean > pal$minmax[2]] <- pal$minmax[2]
  pal <- map_cols_cmip_fun(indic = param, type = quant, domain = minmax(r_mean))
  
  # titlu cu unitate de masura extrasa din - map_cols_cmip_fun
  param_name <- paste0(params_def$parm[params_def$input %in% param], " (",strsplit(pal$tit_leg, ";|<")[[1]][7],")")
  season_name <- if (param %in% c("tas", "tasmax", "tasmin", "pr")) names(select_seas[select_seas %in% season]) else "Annual"
  
  list(r = r_mean, pal = pal, min_max = minmax(r_mean), opacy = transp, file_hist = file_hist, file_scen = file_scen,
       season_subset = season_subset, param_name = param_name, season_name = season_name, file_ind = file_ind)
}
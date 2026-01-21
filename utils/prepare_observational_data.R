prepare_observational_data <- function(param, season, season_ind, period_obs, transp,
                                       files_cmip6, params_def, select_seas) {
    if (param %in% c("tas", "tasmax", "tasmin", "pr")) {
        season_code <- strsplit(season, "-")[[1]][2]
        season_subset <- strsplit(season, "-")[[1]][1]

        # uneste istricul cu scenariiul
        file_hist <- grep(paste0("www/data/ncs/cmip6/", param, "/hist/", param, "_hist_", season_code, "-50_"), files_cmip6, value = T)
        r <- rast(file_hist)
        dats <- time(r)

        file_ind <- NA
    } else {
        # For indices, we need to find a way to get historical only.
        # The prepare_climate_data uses:
        # file_ind <- paste0("www/data/ncs/cmip6/indices/",param,"_",season_ind,"_",scen,"_1961-2100_ensmean.nc")
        # This implies indices are combined.
        # We might need to pick one scenario file and just extract the historical part
        # IF the historical part is identical across scenarios (which it should be for CMIP6 stitched files).
        # Or maybe there is a 'hist' file for indices?
        # Let's assume we can use ssp1 as a proxy since the historical part (up to 2014) is the same.

        file_ind <- paste0("www/data/ncs/cmip6/indices/", param, "_", season_ind, "_ssp1_1961-2100_ensmean.nc")

        if (!file.exists(file_ind)) {
            # Try to find any file matching the pattern if specific one doesn't exist
            pattern <- paste0("www/data/ncs/cmip6/indices/", param, "_", season_ind, "_.*_1961-2100_ensmean.nc")
            avail_files <- Sys.glob(pattern)
            if (length(avail_files) > 0) file_ind <- avail_files[1]
        }

        r <- rast(file_ind)
        dats <- seq(as.Date("1961-07-01"), as.Date("2100-07-01"), "year")
        time(r) <- dats

        # Filter for historical period only (up to 2014)
        r <- r[[format(dats, "%Y") %in% 1961:2014]]
        dats <- time(r) # Update dats to match filtered r

        season_subset <- season_ind
    }

    # subseteaza sezoniere si lunare
    if (!season_subset %in% c("ANN", "year")) r <- r[[which(format(dats, "%m") %in% season_subset)]]

    # pentru subsetare harta
    dats_sub <- time(r)

    # pentru subsetare period
    an1 <- strsplit(period_obs, "-")[[1]][1] |> as.numeric()
    an2 <- strsplit(period_obs, "-")[[1]][2] |> as.numeric()


    # mediaza duop input perioada
    r_mean <- mean(r[[format(dats_sub, "%Y") %in% an1:an2]])

    setMinMax(r_mean)

    pal <- map_cols_cmip_fun(indic = param, type = "climate", domain = minmax(r_mean))
    # pentru reclasificare extreme raster cu min/max din paleta de culori
    r_mean[r_mean < pal$minmax[1]] <- pal$minmax[1]
    r_mean[r_mean > pal$minmax[2]] <- pal$minmax[2]
    pal <- map_cols_cmip_fun(indic = param, type = "climate", domain = minmax(r_mean))

    # titlu cu unitate de masura extrasa din - map_cols_cmip_fun
    param_name <- paste0(params_def$parm[params_def$input %in% param], " (", strsplit(pal$tit_leg, ";|<")[[1]][7], ")")
    season_name <- if (param %in% c("tas", "tasmax", "tasmin", "pr")) names(select_seas[select_seas %in% season]) else "Annual"

    list(
        r = r_mean, pal = pal, min_max = minmax(r_mean), opacy = transp, file_hist = NA, file_scen = NA, # passing NA for history/scen files as we don't do extraction for now or need updates
        season_subset = season_subset, param_name = param_name, season_name = season_name, file_ind = file_ind,
        r_full = r
    ) # Return full raster for point extraction
}

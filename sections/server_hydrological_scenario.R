# Helper function to prepare hydrological data
prepare_hydrological_data <- function(param, season, scen, quant, period_climate, period_change, transp, files_rdis, params_def) {
    # param is "rdis"
    # season is "ANN" (hardcoded in UI for now)

    # Valid stats: mean, max, min. For now defaulting to mean as user only asked for "River Discharge".
    # If we want to support max/min, we'd need a UI selector or assume mean.
    stat <- "mean"

    # File patterns
    # Historical: rdis_mean_ANN_historical_1971-2005_ensmean.nc
    # Scenario: rdis_mean_ANN_rcp_2_6_2006-2099_ensmean.nc

    # Construct file paths
    file_hist_base <- paste0("rdis_", stat, "_", season, "_historical_1971-2005_ensmean.nc")
    file_scen_base <- paste0("rdis_", stat, "_", season, "_", scen, "_2006-2099_ensmean.nc")

    file_hist <- grep(file_hist_base, files_rdis, value = TRUE, fixed = TRUE)
    file_scen <- grep(file_scen_base, files_rdis, value = TRUE, fixed = TRUE)

    if (length(file_hist) == 0 || length(file_scen) == 0) {
        validate(need(FALSE, "Data files not found."))
    }

    # Load rasters
    r_hist <- terra::rast(file_hist)
    r_scen <- terra::rast(file_scen)

    # Assign dates (assuming annual)
    # Historical: 1971-2005 (35 years)
    time(r_hist) <- seq(as.Date("1971-01-01"), by = "years", length.out = nlyr(r_hist))

    # Scenario: 2006-2099 (94 years)
    time(r_scen) <- seq(as.Date("2006-01-01"), by = "years", length.out = nlyr(r_scen))

    r <- c(r_hist, r_scen)
    dats <- time(r)

    # No season subsetting needed as data is already ANN
    dats_sub <- dats

    # Calculate based on input type
    if (quant == "climate") {
        an1 <- as.numeric(strsplit(period_climate, "-")[[1]][1])
        an2 <- as.numeric(strsplit(period_climate, "-")[[1]][2])

        # Subset and mean
        indices <- which(as.numeric(format(dats_sub, "%Y")) >= an1 & as.numeric(format(dats_sub, "%Y")) <= an2)
        if (length(indices) == 0) validate(need(FALSE, "No data for selected period"))

        r_mean <- mean(r[[indices]])

        # Mask very low discharge values (< 1 m³/s) for cleaner display
        r_mean[r_mean < 1] <- NA

        setMinMax(r_mean)
    } else { # Change
        an1_hist <- as.numeric(strsplit(period_change[1], "-")[[1]][1])
        an2_hist <- as.numeric(strsplit(period_change[1], "-")[[1]][2])
        an1_scen <- as.numeric(strsplit(period_change[2], "-")[[1]][1])
        an2_scen <- as.numeric(strsplit(period_change[2], "-")[[1]][2])

        idx_hist <- which(as.numeric(format(dats_sub, "%Y")) >= an1_hist & as.numeric(format(dats_sub, "%Y")) <= an2_hist)
        idx_scen <- which(as.numeric(format(dats_sub, "%Y")) >= an1_scen & as.numeric(format(dats_sub, "%Y")) <= an2_scen)

        if (length(idx_hist) == 0 || length(idx_scen) == 0) validate(need(FALSE, "No data for selected periods"))

        r_base <- mean(r[[idx_hist]])
        r_fut <- mean(r[[idx_scen]])

        # Mask areas with very low baseline discharge (< 1 m³/s) to avoid division artifacts
        r_base[r_base < 1] <- NA

        # Calculate percentage change
        r_mean <- ((r_fut * 100) / r_base) - 100

        # Clamp extreme values to scale limits instead of masking
        r_mean[r_mean > 50] <- 50
        r_mean[r_mean < -50] <- -50

        setMinMax(r_mean)
    }

    # Crop to Danube basin extent
    r_mean <- terra::crop(r_mean, terra::ext(dun))

    # Get palette
    domain_val <- minmax(r_mean)

    # Force full domain for river discharge climatology to ensure correct legend display
    if (param == "rdis" && quant == "climate") {
        domain_val <- c(1, 10000)
    }

    pal <- map_cols_cmip_fun(indic = param, type = quant, domain = domain_val)

    # Reclassify extremes for visualization
    r_mean[r_mean < pal$minmax[1]] <- pal$minmax[1]
    r_mean[r_mean > pal$minmax[2]] <- pal$minmax[2]

    # Update palette with potentially new domain (clamped)
    # pal <- map_cols_cmip_fun(indic = param, type = quant, domain = minmax(r_mean))
    # Actually `prepare_climate_data` calls it again, probably to ensure bins cover the range.

    # Title
    # params_def needs to have rdis
    param_label <- if (param == "rdis") "River Discharge" else param
    unit_label <- strsplit(pal$tit_leg, ";|<")[[1]][7] # Hacky extraction from HTML, same as original

    param_name <- paste0(param_label, " (", unit_label, ")")
    season_name <- "Annual"

    # File Ind is for indexing ensemble spread files (p10/p90)
    # rdis logic puts mean/p10/p90 in same folder names but different suffix
    # Construct a strict file_ind pattern for extraction functions
    # They expect something they can gsub "ensmean" -> "ensp10"
    # existing logic: file_ind <- paste0("www/data/ncs/cmip6/indices/",param,"_",season_ind,"_",scen,"_1961-2100_ensmean.nc")
    # We should provide the full path to the mean file so gsub works
    file_ind <- file.path(getwd(), "www/data/ncs/rdis", paste0("rdis_", stat, "_", season, "_", scen, "_2006-2099_ensmean.nc"))

    list(
        r = r_mean, pal = pal, min_max = minmax(r_mean), opacy = transp,
        file_hist = r_hist, file_scen = r_scen,
        file_hist_path = file_hist, file_scen_path = file_scen,
        season_subset = season, param_name = param_name, season_name = season_name, file_ind = file_ind
    )
}


data_hydro_sel <- reactive({
    prepare_hydrological_data(
        param = input$hydro_param,
        season = input$hydro_season,
        scen = input$hydro_scen,
        quant = input$hydro_quant,
        period_climate = input$hydro_period_climate,
        period_change = input$hydro_period_change,
        transp = input$hydro_transp,
        files_rdis = files_rdis,
        params_def = params_def
    )
})


# functii leaflet de start
output$hydro_map <- renderLeaflet({
    leaflet_fun()
})

# update leaflet outputuri
observe({
    r <- data_hydro_sel()$r
    pal <- data_hydro_sel()$pal
    min_max <- data_hydro_sel()$min_max
    opacy <- data_hydro_sel()$opacy

    leafletProxy("hydro_map") |>
        clearImages() |>
        addRasterImage(r, opacity = opacy, color = pal$pal) |>
        clearControls() |>
        addLegend(
            title = pal$tit_leg,
            position = "bottomright",
            opacity = opacy,
            pal = pal$pal_rev, values = min_max,
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
        )
})

# Also trigger map update when map bounds become available (handles initial render on tab switch)
observeEvent(input$hydro_map_bounds,
    {
        req(data_hydro_sel())
        r <- data_hydro_sel()$r
        pal <- data_hydro_sel()$pal
        min_max <- data_hydro_sel()$min_max
        opacy <- data_hydro_sel()$opacy

        leafletProxy("hydro_map") |>
            clearImages() |>
            addRasterImage(r, opacity = opacy, color = pal$pal) |>
            clearControls() |>
            addLegend(
                title = pal$tit_leg,
                position = "bottomright",
                opacity = opacy,
                pal = pal$pal_rev, values = min_max,
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
            )
    },
    once = TRUE
)


# zoom to selected area
observeEvent(input$hydro_area, {
    req(input$hydro_area)

    shape_to_zoom <- switch(input$hydro_area,
        "drb" = dun,
        "at1" = is1_austria[1, ],
        "at2" = is1_austria[2, ],
        "at3" = is1_austria[3, ],
        "at4" = is1_austria[4, ],
        "at5" = is1_austria[5, ],
        "sk1" = is2_slovakia,
        "rs1" = is3_serbia,
        "ro1" = is4_romania,
        "de1" = ms1_germany,
        "sk2" = ms2_slovakia,
        "rs2" = ms3_serbia,
        "ro2" = ms4_romania,
        "ro3" = ms5_romania,
        "ro4" = ms6_romania
    )

    bbox <- sf::st_bbox(sf::st_buffer(shape_to_zoom, dist = 10000))

    proxy <- leafletProxy("hydro_map") %>%
        removeShape(layerId = "highlighted_polygon")

    if (input$hydro_area != "drb") {
        proxy <- proxy %>%
            addPolygons(
                data = shape_to_zoom,
                layerId = "highlighted_polygon",
                fillColor = "yellow",
                fillOpacity = 0.5,
                color = "orange",
                weight = 3,
                stroke = TRUE,
                label = names(select_area[select_area == input$hydro_area]),
                labelOptions = labelOptions(
                    style = list(
                        "color" = "black",
                        "font-family" = "Arial",
                        "font-weight" = "bold",
                        "font-size" = "12px"
                    ),
                    textsize = "12px",
                    direction = "auto"
                )
            )
    }

    proxy %>%
        fitBounds(
            lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
            lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
        )
})


output$hydro_map_titl <- renderText({
    req(data_hydro_sel())
    param <- data_hydro_sel()$param_name
    season <- data_hydro_sel()$season_name
    if (input$hydro_quant %in% "climate") {
        paste(param, season, toupper(input$hydro_scen), " - multiannual mean", input$hydro_period_climate)
    } else {
        paste(param, season, toupper(input$hydro_scen), " - change in multiannual mean", input$hydro_period_change[2], "vs.", input$hydro_period_change[1])
    }
})

# reactive values pentru plot lst time series din raster
hydro_values_plot <- reactiveValues(input = NULL, title = NULL, lon = 28.389, lat = 45.433, mode = "point")

observeEvent(input$hydro_area, {
    if (input$hydro_area %in% c("at1", "at2", "at3", "at4", "at5", "sk1", "rs1", "ro1", "de1", "sk2", "rs2", "ro2", "ro3", "ro4")) {
        hydro_values_plot$mode <- "zonal"
    } else {
        hydro_values_plot$mode <- "point"
    }
})

# interactivitate raster
observeEvent(input$hydro_map_click, {
    req(input$hydro_map_click)
    # Do not trigger point extraction when a country is selected
    if (input$hydro_area %in% "drb") {
        hydro_values_plot$mode <- "point"
        hydro_values_plot$lon <- input$hydro_map_click$lng
        hydro_values_plot$lat <- input$hydro_map_click$lat
    }
})

observe({
    # Make it depend on all relevant inputs
    req(input$hydro_param, input$hydro_scen, input$hydro_season, input$hydro_quant, input$hydro_period_change, input$hydro_period_climate, hydro_values_plot$mode)

    if (hydro_values_plot$mode == "zonal") {
        req(input$hydro_area %in% c("drb", "at1", "at2", "at3", "at4", "at5", "sk1", "rs1", "ro1", "de1", "sk2", "rs2", "ro2", "ro3", "ro4"))
        shape_to_extract <- switch(input$hydro_area,
            "at1" = is1_austria[1, ],
            "at2" = is1_austria[2, ],
            "at3" = is1_austria[3, ],
            "at4" = is1_austria[4, ],
            "at5" = is1_austria[5, ],
            "sk1" = is2_slovakia,
            "rs1" = is3_serbia,
            "ro1" = is4_romania,
            "de1" = ms1_germany,
            "sk2" = ms2_slovakia,
            "rs2" = ms3_serbia,
            "ro2" = ms4_romania,
            "ro3" = ms5_romania,
            "ro4" = ms6_romania
        )

        area_choices <- c(
            "Danube River Basin" = "drb", "Austria" = "at1", "Austria" = "at2", "Austria" = "at3", "Austria" = "at4", "Slovakia" = "sk1", "Serbia" = "rs1", "Romania" = "ro1", "Germany" = "de1",
            "Slovakia" = "sk2", "Serbia" = "rs2", "Romania" = "ro2", "Romania" = "ro3", "Romania" = "ro4"
        )

        country_name <- names(area_choices)[area_choices == input$hydro_area]

        # Custom zonal extraction for hydrology - handles separate hist/scen p10/p90 files
        r_hist <- data_hydro_sel()$file_hist
        r_scen <- data_hydro_sel()$file_scen
        r_50 <- c(r_hist, r_scen)

        # Load p10/p90 from file paths
        file_hist_p90 <- gsub("ensmean", "ensp90", data_hydro_sel()$file_hist_path)
        file_scen_p90 <- gsub("ensmean", "ensp90", data_hydro_sel()$file_scen_path)
        file_hist_p10 <- gsub("ensmean", "ensp10", data_hydro_sel()$file_hist_path)
        file_scen_p10 <- gsub("ensmean", "ensp10", data_hydro_sel()$file_scen_path)

        r_hist_p90 <- terra::rast(file_hist_p90)
        r_scen_p90 <- terra::rast(file_scen_p90)
        r_hist_p10 <- terra::rast(file_hist_p10)
        r_scen_p10 <- terra::rast(file_scen_p10)

        # Set times for p10/p90
        time(r_hist_p90) <- time(r_hist)
        time(r_scen_p90) <- time(r_scen)
        time(r_hist_p10) <- time(r_hist)
        time(r_scen_p10) <- time(r_scen)

        r_90 <- c(r_hist_p90, r_scen_p90)
        r_10 <- c(r_hist_p10, r_scen_p10)

        # Extract zonal means
        zonal_50 <- terra::extract(r_50, terra::vect(shape_to_extract), fun = mean, na.rm = TRUE, ID = FALSE)
        zonal_10 <- terra::extract(r_10, terra::vect(shape_to_extract), fun = mean, na.rm = TRUE, ID = FALSE)
        zonal_90 <- terra::extract(r_90, terra::vect(shape_to_extract), fun = mean, na.rm = TRUE, ID = FALSE)

        ddf <- data.frame(
            date = as.Date(time(r_50)),
            value = round(as.numeric(zonal_50[1, ]), 1),
            value10 = round(as.numeric(zonal_10[1, ]), 1),
            value90 = round(as.numeric(zonal_90[1, ]), 1)
        )

        # Calculate change if requested
        quant <- input$hydro_quant
        period_change <- input$hydro_period_change
        if (quant %in% "change") {
            an1_hist <- as.numeric(strsplit(period_change[1], "-")[[1]][1])
            an2_hist <- as.numeric(strsplit(period_change[1], "-")[[1]][2])
            baseline_mean <- mean(ddf$value[as.numeric(format(ddf$date, "%Y")) >= an1_hist & as.numeric(format(ddf$date, "%Y")) <= an2_hist], na.rm = TRUE)

            if (!is.na(baseline_mean) && baseline_mean != 0) {
                ddf$value <- round(((ddf$value * 100) / baseline_mean) - 100, 1)
                ddf$value90 <- round(((ddf$value90 * 100) / baseline_mean) - 100, 1)
                ddf$value10 <- round(((ddf$value10 * 100) / baseline_mean) - 100, 1)
            }
        }

        hydro_values_plot$input <- ddf
        hydro_values_plot$title <- paste(data_hydro_sel()$param_name, "for", country_name, "test area", toString(shape_to_extract$Name))
    } else {
        lon <- hydro_values_plot$lon
        lat <- hydro_values_plot$lat

        # Custom extraction for hydrology - handles separate hist/scen p10/p90 files
        r_hist <- data_hydro_sel()$file_hist
        r_scen <- data_hydro_sel()$file_scen
        r <- c(r_hist, r_scen)

        # Load p10/p90 from file paths
        file_hist_p90 <- gsub("ensmean", "ensp90", data_hydro_sel()$file_hist_path)
        file_scen_p90 <- gsub("ensmean", "ensp90", data_hydro_sel()$file_scen_path)
        file_hist_p10 <- gsub("ensmean", "ensp10", data_hydro_sel()$file_hist_path)
        file_scen_p10 <- gsub("ensmean", "ensp10", data_hydro_sel()$file_scen_path)

        r_hist_p90 <- terra::rast(file_hist_p90)
        r_scen_p90 <- terra::rast(file_scen_p90)
        r_hist_p10 <- terra::rast(file_hist_p10)
        r_scen_p10 <- terra::rast(file_scen_p10)

        # Set times for p10/p90
        time(r_hist_p90) <- time(r_hist)
        time(r_scen_p90) <- time(r_scen)
        time(r_hist_p10) <- time(r_hist)
        time(r_scen_p10) <- time(r_scen)

        r90 <- c(r_hist_p90, r_scen_p90)
        r10 <- c(r_hist_p10, r_scen_p10)

        # Create point for extraction
        point_sf <- sf::st_as_sf(data.frame(lon = lon, lat = lat), coords = c("lon", "lat"), crs = 4326)

        # Extract values
        extracted_values <- terra::extract(r, point_sf, ID = FALSE)
        extracted_values90 <- terra::extract(r90, point_sf, ID = FALSE)
        extracted_values10 <- terra::extract(r10, point_sf, ID = FALSE)

        if (all(is.na(extracted_values))) {
            hydro_values_plot$input <- "No data available for the selected point"
        } else {
            # Create data frame
            ddf <- data.frame(
                date = as.Date(time(r)),
                value = round(as.numeric(extracted_values), 1),
                value10 = round(as.numeric(extracted_values10), 1),
                value90 = round(as.numeric(extracted_values90), 1)
            )

            # Filter by period
            season <- data_hydro_sel()$season_subset
            quant <- input$hydro_quant
            period_change <- input$hydro_period_change
            period_climate <- input$hydro_period_climate

            if (quant %in% "climate") {
                end_year <- as.numeric(strsplit(period_climate, "-")[[1]][2])
                ddf <- ddf |> dplyr::filter(as.numeric(format(date, "%Y")) <= end_year)
            } else {
                end_year <- as.numeric(strsplit(period_change[2], "-")[[1]][2])
                ddf <- ddf |> dplyr::filter(as.numeric(format(date, "%Y")) <= end_year)

                # Calculate change
                an1_hist <- as.numeric(strsplit(period_change[1], "-")[[1]][1])
                an2_hist <- as.numeric(strsplit(period_change[1], "-")[[1]][2])
                baseline_mean <- mean(ddf$value[as.numeric(format(ddf$date, "%Y")) >= an1_hist & as.numeric(format(ddf$date, "%Y")) <= an2_hist], na.rm = TRUE)

                if (!is.na(baseline_mean) && baseline_mean != 0) {
                    # Percentage change for rdis
                    ddf$value <- round(((ddf$value * 100) / baseline_mean) - 100, 1)
                    ddf$value90 <- round(((ddf$value90 * 100) / baseline_mean) - 100, 1)
                    ddf$value10 <- round(((ddf$value10 * 100) / baseline_mean) - 100, 1)
                }
            }

            hydro_values_plot$input <- ddf
            hydro_values_plot$title <- graph_title_hydro(data_hydro_sel()$param_name, input$hydro_quant, input$hydro_param, input$hydro_period_change, lon, lat)
        }
    }
})


output$hydro_chart_scen <- renderHighchart({
    create_timeseries_chart(
        data_input = hydro_values_plot$input,
        param = input$hydro_param,
        params_def = params_def
    )
})


output$hydro_graph_titl <- renderText({
    hydro_values_plot$title
})

library(tidyr)

# Define parameters and scenarios to loop through
# The column names in params_def are 'input' (for the code) and 'parm' (for the name).
# Let's select them and rename for consistency within this script.
aoi_params_df <- setNames(params_def[, c("input", "parm")], c("param", "name"))
aoi_scenarios <- c("ssp1", "ssp2", "ssp3", "ssp5")

# Conditional UI for predefined area selection
output$aoi_predefined_selection_ui <- renderUI({
  req(input$aoi_selection_method == "predefined")
  selectInput(
    inputId = "aoi_area",
    label = "Select an area:",
    choices = select_area,
    selected = "at1"
  )
})

observeEvent(input$aoi_selection_method, {
  if (input$aoi_selection_method == "draw") {
    shinyjs::show("aoi_map_card")
    shinyjs::show("clear_drawn_area_div")
  } else {
    shinyjs::hide("aoi_map_card")
    shinyjs::hide("clear_drawn_area_div")
  }
})

observeEvent(input$aoi_selection_method, {
  if (input$aoi_selection_method == "draw") {
    # Clear any previously drawn polygon when "Draw on Map" is selected
    drawn_polygon_sf(NULL)
  }
}, ignoreInit = TRUE)

observeEvent(input$clear_drawn_area, {
  # Clear the drawn polygon when the "Draw New Area" button is clicked
  drawn_polygon_sf(NULL)
  showNotification("Previous drawing cleared. You can now draw a new area.", type = "message")
})

# Conditional UI for map drawing


# Reactive value to store the drawn polygon
drawn_polygon_sf <- reactiveVal(NULL)

# Render the Leaflet map with drawing tools
output$aoi_map <- renderLeaflet({
  req(input$aoi_selection_method == "draw")
  leaflet() %>%
    setView(lng = 15, lat = 48, zoom = 5) %>%
    addProviderTiles( "CartoDB.Positron", group = "CartoDB")  %>%
    addProviderTiles( "Esri.WorldGrayCanvas", group = "EsriWorldGray") |>
    addProviderTiles( "Esri.WorldImagery", group = "EsriWorldImagery") |>
    addEasyButton(
      easyButton(
        icon    = "glyphicon glyphicon-home", title = "Zoom to Danube Basin",
        onClick = JS("function(btn, map){ map.setView([45, 22], 4); }")
      )
    ) |>
    
    addDrawToolbar(
      targetGroup = "drawn_aoi",
      polylineOptions = FALSE,
      polygonOptions = drawPolygonOptions(shapeOptions = drawShapeOptions(fillOpacity = 0.4, weight = 2)),
      circleOptions = FALSE,
      rectangleOptions = FALSE,
      markerOptions = FALSE,
      circleMarkerOptions = FALSE,
      editOptions = editToolbarOptions(
        selectedPathOptions = selectedPathOptions(),
        edit = FALSE, # Disable editing existing shapes
        remove = TRUE # Allow removing shapes
      )
    ) %>%
    addPolygons(
      data = dun,
      fillColor = "blue",
      color = "blue",
      weight = 2,
      fillOpacity = 0.2,
      group = "Danube Basin",
      popup = "Danube River Basin"
    ) %>%
    addLayersControl(
      baseGroups = c("CartoDB", "EsriWorldGray", "EsriWorldImagery"),
      overlayGroups = c("drawn_aoi", "Danube Basin"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addStyleEditor() # For debugging/styling if needed
})

# Observe drawn features
observeEvent(input$aoi_map_draw_new_feature, {
  feature <- input$aoi_map_draw_new_feature
  # Convert drawn feature to sf object
  drawn_sf <- sf::st_read(jsonlite::toJSON(feature$geometry, auto_unbox = TRUE), quiet = TRUE)
  
  # Check for overlap with Danube basin
  if (any(sf::st_intersects(drawn_sf, dun, sparse = FALSE))) {
    drawn_polygon_sf(drawn_sf)
    showNotification("Polygon drawn successfully! Data will be prepared using this area.", type = "message")
  } else {
    showNotification("Error: Your drawn area must overlap with the Danube Basin. Please draw a new area.", type = "error", duration = 10)
    # Optionally, clear the drawn feature from the map if possible, though not directly supported by shinyjs
  }
})

# Observe deleted features (clear the drawn polygon)
observeEvent(input$aoi_map_draw_deleted_features, {
  drawn_polygon_sf(NULL)
  showNotification("Drawn polygon cleared.", type = "message")
})

# Helper to get the sf object for the selected area
get_shape_for_aoi <- function(aoi_id) {
  switch(
    aoi_id,
    "drb" = dun,
    "at1" = is1_austria[1, ], "at2" = is1_austria[2, ], "at3" = is1_austria[3, ],
    "at4" = is1_austria[4, ], "at5" = is1_austria[5, ], "sk1" = is2_slovakia,
    "rs1" = is3_serbia, "ro1" = is4_romania, "de1" = ms1_germany,
    "sk2" = ms2_slovakia, "rs2" = ms3_serbia, "ro2" = ms4_romania,
    "ro3" = ms5_romania, "ro4" = ms6_romania,
    NULL
  )
}

all_plots_data <- reactiveVal(NULL)

# --- MODIFIED SECTION ---
# Render the UI grid with open cards for each scenario
output$aoi_plots_grid <- renderUI({
  # Require either a predefined area or a drawn polygon
  if (input$aoi_selection_method == "predefined") {
    req(input$aoi_area)
  } else if (input$aoi_selection_method == "draw") {
    req(drawn_polygon_sf())
  } else {
    return(NULL)
  }
  
  # Create a list of UI sections, one for each parameter
  parameter_sections <- lapply(1:nrow(aoi_params_df), function(i) {
    param_code <- aoi_params_df$param[i]
    param_name <- aoi_params_df$name[i]
    
    # Create a list of individual plot cards for the 4 scenarios
    plot_outputs <- lapply(aoi_scenarios, function(scen_code) {
      output_id <- paste("aoi_plot", param_code, scen_code, sep = "_")
      card(
        card_header(paste("Scenario:", toupper(scen_code))),
        highchartOutput(output_id, height = "300px")
      )
    })
    
    # Anomaly plots
    anomaly_plot_outputs <- lapply(aoi_scenarios, function(scen_code) {
      output_id <- paste("aoi_anomaly_plot", param_code, scen_code, sep = "_")
      card(
        card_header(paste("Anomaly vs 1981-2010 - Scenario:", toupper(scen_code))),
        highchartOutput(output_id, height = "300px")
      )
    })
    
    # Summary table
    summary_table_output <- tableOutput(paste("summary_table", param_code, sep = "_"))
    
    # For each parameter, return a header followed by the grid of scenario plots
    # This removes the single collapsible parent card
    tagList(
      h5(param_name, style = "text-align: left; margin-top: 25px; margin-bottom: 15px;"),
      h6("Absolute values", style = "text-align: left; margin-top: 15px; margin-bottom: 15px;"),
      layout_columns(
        col_widths = c(6, 6, 6, 6), # Creates a 2x2 grid
        !!!plot_outputs
      ),
      h6("Anomaly values", style = "text-align: left; margin-top: 15px; margin-bottom: 15px;"),
      layout_columns(
        col_widths = c(6, 6, 6, 6), # Creates a 2x2 grid
        !!!anomaly_plot_outputs
      ),
      h6("Summary table", style = "text-align: left; margin-top: 25px; margin-bottom: 15px;"),
      summary_table_output,
      hr(style = "margin-top: 20px;") # Adds a line to separate parameter groups
    )
  })
  
  # Combine all parameter sections into a single UI
  tagList(parameter_sections)
})
# --- END OF MODIFIED SECTION ---

# Main observer for data generation and plot rendering
observeEvent(list(input$aoi_selection_method, input$aoi_area, drawn_polygon_sf()), {
  
  # --- 1. Get the area of interest ---
  shape_to_extract <- if (input$aoi_selection_method == "predefined") {
    req(input$aoi_area)
    get_shape_for_aoi(input$aoi_area)
  } else if (input$aoi_selection_method == "draw") {
    req(drawn_polygon_sf())
    drawn_polygon_sf()
  } else {
    return(NULL)
  }
  req(shape_to_extract)

  # --- 2. Setup Progress Bar ---
  n_params <- nrow(aoi_params_df)
  n_scenarios <- length(aoi_scenarios)
  total_steps <- (n_params * n_scenarios) + n_params
  
  withProgress(message = 'Generating graphs and tables', value = 0, {

    # --- 3. Data Generation and Main Plot Rendering ---
    data_list <- list()
    climate_indices <- c("gsl", "tr", "wsdi", "csdi")

    for (i in 1:n_params) {
      param_code <- aoi_params_df$param[i]
      param_name <- aoi_params_df$name[i]

      if (param_code %in% climate_indices) {
        season_arg <- NULL; season_ind_arg <- "ANN"
      } else {
        season_arg <- "year-year"; season_ind_arg <- NULL
      }

      for (scen_code in aoi_scenarios) {
        incProgress(1 / total_steps, detail = paste("Processing", param_name, "-", toupper(scen_code)))

        plot_id <- paste(param_code, scen_code, sep = "_")

        ddf <- try({
          data_prepared <- prepare_climate_data(
            param = param_code, season = season_arg, season_ind = season_ind_arg,
            scen = scen_code, quant = "timeseries", period_climate = "2081-2100",
            period_change = c("1981-2010", "2041-2060"), transp = 0.8,
            files_cmip6 = files_cmip6, params_def = params_def, select_seas = select_seas
          )
          extract_zonal_data(
            file_hist = data_prepared$file_hist, file_scen = data_prepared$file_scen,
            shape = shape_to_extract, season_subset = data_prepared$season_subset,
            param = param_code, quant = "climate", period_change = c("1981-2010", "2041-2060"),
            file_ind = data_prepared$file_ind
          )
        }, silent = TRUE)
        data_list[[plot_id]] <- ddf

        local({
          local_plot_id <- plot_id
          local_param_code <- param_code
          output_id <- paste0("aoi_plot_", local_plot_id)
          output[[output_id]] <- renderHighchart({
            df_plot <- data_list[[local_plot_id]]
            if (is.data.frame(df_plot) && nrow(df_plot) > 0 && !all(is.na(df_plot$value))) {
              create_timeseries_chart(data_input = df_plot, param = local_param_code, params_def = params_def)
            } else {
              highchart() %>% hc_title(text = "Data not available")
            }
          })
        })
      }
    }
    
    all_plots_data(data_list)
    
    # --- 4. Anomaly and Summary Table Generation ---
    plot_data <- data_list
    for (i in 1:n_params) {
      local({
        param_code <- aoi_params_df$param[i]
        param_name <- aoi_params_df$name[i]
        incProgress(1 / total_steps, detail = paste("Creating anomalies for", param_name))

        all_scenarios_data <- lapply(aoi_scenarios, function(scen_code) {
          plot_id <- paste(param_code, scen_code, sep = "_")
          df <- plot_data[[plot_id]]
          if (is.data.frame(df) && nrow(df) > 0 && !all(is.na(df$value))) {
            df$scenario <- toupper(scen_code)
            return(df)
          }
          return(NULL)
        })
        combined_df <- dplyr::bind_rows(all_scenarios_data)

        if (is.data.frame(combined_df) && nrow(combined_df) > 0) {
          for (scen_code in aoi_scenarios) {
            local({
              local_scen_code <- scen_code
              df_scen <- combined_df %>% filter(scenario == toupper(local_scen_code))
              historical_mean_scen <- df_scen %>%
                filter(as.numeric(format(date, "%Y")) >= 1981 & as.numeric(format(date, "%Y")) <= 2010) %>%
                summarise(mean_value = mean(value, na.rm = TRUE)) %>%
                pull(mean_value)
              if (!is.na(historical_mean_scen)) {
                if (param_code == "pr") {
                  df_scen <- df_scen %>% mutate(
                    anomaly = if (historical_mean_scen != 0) ((value - historical_mean_scen) / historical_mean_scen) * 100 else NA,
                    anomaly10 = if (historical_mean_scen != 0) ((value10 - historical_mean_scen) / historical_mean_scen) * 100 else NA,
                    anomaly90 = if (historical_mean_scen != 0) ((value90 - historical_mean_scen) / historical_mean_scen) * 100 else NA
                  )
                } else {
                  df_scen <- df_scen %>% mutate(
                    anomaly = value - historical_mean_scen,
                    anomaly10 = value10 - historical_mean_scen,
                    anomaly90 = value90 - historical_mean_scen
                  )
                }
                output_id <- paste("aoi_anomaly_plot", param_code, local_scen_code, sep = "_")
                output[[output_id]] <- renderHighchart({
                  if (is.data.frame(df_scen) && nrow(df_scen) > 0 && !all(is.na(df_scen$anomaly))) {
                    create_timeseries_chart(data_input = df_scen, param = param_code, params_def = params_def, is_anomaly = TRUE)
                  } else {
                    highchart() %>% hc_title(text = "Anomaly data not available")
                  }
                })
              }
            })
          }

          hist_period <- 1981:2010
          future_periods <- list("2041-2060" = 2041:2060, "2061-2080" = 2061:2080, "2081-2100" = 2081:2100)
          summary_list <- list()
          for (scen_code in aoi_scenarios) {
            plot_id <- paste(param_code, scen_code, sep = "_")
            df <- plot_data[[plot_id]]
            if (is.data.frame(df) && nrow(df) > 0) {
              if ("date" %in% colnames(df) && !is.numeric(df$date)) {
                df <- df %>% mutate(date = as.numeric(format(as.Date(date), "%Y")))
              }
              historical_mean <- df %>% filter(date %in% hist_period) %>% summarise(mean_value = mean(value, na.rm = TRUE)) %>% pull(mean_value)
              if (length(historical_mean) > 0 && !is.na(historical_mean)) {
                for (period_name in names(future_periods)) {
                  future_period <- future_periods[[period_name]]
                  future_mean <- df %>% filter(date %in% future_period) %>% summarise(mean_value = mean(value, na.rm = TRUE)) %>% pull(mean_value)
                  if (length(future_mean) > 0 && !is.na(future_mean)) {
                    change <- if (param_code == "pr") {
                      if (historical_mean != 0) ((future_mean - historical_mean) / historical_mean) * 100 else NA
                    } else {
                      future_mean - historical_mean
                    }
                    summary_list[[length(summary_list) + 1]] <- data.frame(
                      Scenario = toupper(scen_code), Period = period_name,
                      Historical_Mean = round(historical_mean, 2), Future_Mean = round(future_mean, 2), Change = round(change, 2)
                    )
                  }
                }
              }
            }
          }
          if (length(summary_list) > 0) {
            summary_df <- do.call(rbind, summary_list)
            summary_wide <- summary_df %>% tidyr::pivot_wider(names_from = Period, values_from = c("Future_Mean", "Change"), names_sep = "_")
            output_id <- paste("summary_table", param_code, sep = "_")
            output[[output_id]] <- renderTable(summary_wide)
          }
        }
      })
    }
  })
}, ignoreInit = TRUE)


# Handler for downloading the report
output$download_aoi_report <- downloadHandler(
  filename = function() {
    format <- if (is.null(input$report_format)) "html" else input$report_format
    ext <- switch(format,
                  html = ".html",
                  docx = ".docx",
                  ".html") # Default to .html
    area_id <- if (input$aoi_selection_method == "predefined") input$aoi_area else "drawn_area"
    paste0("report-", area_id, "-", Sys.Date(), ext)
  },
  content = function(file) {
    withProgress(message = 'Generating report, please wait...', {
      # --- CLEAN the data going into the report ---
    clean_plot_data <- lapply(all_plots_data(), function(x) {
      if (inherits(x, "try-error") || is.null(x) || !is.data.frame(x)) {
        return(data.frame(
          date = as.Date(character()),
          value = numeric(0),
          value10 = numeric(0),
          value90 = numeric(0)
        ))
      }
      req_cols <- c("date", "value", "value10", "value90")
      missing <- setdiff(req_cols, names(x))
      for (nm in missing) {
        x[[nm]] <- numeric(0)
      }
      x
    })

    # Build local params with plain R objects
    local_params <- list(
      area_name = if (input$aoi_selection_method == "predefined") names(select_area[select_area == input$aoi_area]) else "User Drawn Area",
      plot_data = clean_plot_data,
      aoi_params = as.data.frame(aoi_params_df, stringsAsFactors = FALSE),
      aoi_scenarios = as.character(aoi_scenarios),
      params_def = as.data.frame(params_def, stringsAsFactors = FALSE)
    )

    # Copy Rmd template to a temporary directory for rendering
    temp_report <- file.path(tempdir(), "report_template.Rmd")
    file.copy("sections/report_template.Rmd", temp_report, overwrite = TRUE)

    # Determine the output format
    format <- if (is.null(input$report_format)) "html" else input$report_format
    output_format <- switch(format,
                           html = "html_document",
                           docx = "word_document",
                           "html_document") # Default to html

    # Render the RMarkdown report
    rmarkdown::render(
      input = temp_report,
      output_file = file,
      output_format = output_format,
      params = local_params,
      envir = new.env(parent = globalenv())
    )
    })
  }
)

# Reactive expression for the selected area's display name
output$selected_area_display_name <- renderUI({
  if (input$aoi_selection_method == "predefined") {
    req(input$aoi_area)
    h5(names(select_area[select_area == input$aoi_area]))
  } else if (input$aoi_selection_method == "draw") {
    req(drawn_polygon_sf())
    h5("User Drawn Area")
  } else {
    NULL
  }
})
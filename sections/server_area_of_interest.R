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
  drawn_polygon_sf(drawn_sf)
  showNotification("Polygon drawn successfully! Data will be prepared using this area.", type = "message")
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

# Reactive that prepares all dataframes when the area changes
all_plots_data <- eventReactive(list(input$aoi_selection_method, input$aoi_area, drawn_polygon_sf()), {
  id <- showNotification("Preparing data for all plots...", duration = NULL, closeButton = FALSE, type = "message")
  on.exit(removeNotification(id), add = TRUE)

  if (input$aoi_selection_method == "predefined") {
    req(input$aoi_area)
    shape_to_extract <- get_shape_for_aoi(input$aoi_area)
  } else if (input$aoi_selection_method == "draw") {
    req(drawn_polygon_sf())
    shape_to_extract <- drawn_polygon_sf()
  } else {
    return(NULL) # Or handle error/no selection
  }
  req(shape_to_extract)

  data_list <- list()

  # Define which parameters are climate indices, as they are handled differently
  climate_indices <- c("gsl", "tr", "wsdi", "csdi")

  for (i in 1:nrow(aoi_params_df)) {
    param_code <- aoi_params_df$param[i]

    if (param_code %in% climate_indices) {
      season_arg <- NULL
      season_ind_arg <- "ANN"
    } else {
      season_arg <- "year-year"
      season_ind_arg <- NULL
    }

    for (scen_code in aoi_scenarios) {
      plot_id <- paste(param_code, scen_code, sep = "_")

      ddf <- try({
        data_prepared <- prepare_climate_data(
          param = param_code,
          season = season_arg,
          season_ind = season_ind_arg,
          scen = scen_code,
          quant = "climate",
          period_climate = "2081-2100",
          period_change = c("1981-2010", "2041-2060"),
          transp = 0.8,
          files_cmip6 = files_cmip6,
          params_def = params_def,
          select_seas = select_seas
        )
        
        extract_zonal_data(
          file_hist = data_prepared$file_hist,
          file_scen = data_prepared$file_scen,
          shape = shape_to_extract,
          season_subset = data_prepared$season_subset,
          param = param_code,
          quant = "climate",
          period_change = c("1981-2010", "2041-2060"),
          file_ind = data_prepared$file_ind
        )
      }, silent = TRUE)

      data_list[[plot_id]] <- ddf
    }
  }
  
  data_list
})

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
    
    # For each parameter, return a header followed by the grid of scenario plots
    # This removes the single collapsible parent card
    tagList(
      h5(param_name, style = "text-align: center; margin-top: 25px; margin-bottom: 15px;"),
      layout_columns(
        col_widths = c(6, 6, 6, 6), # Creates a 2x2 grid
        !!!plot_outputs
      ),
      hr(style = "margin-top: 20px;") # Adds a line to separate parameter groups
    )
  })
  
  # Combine all parameter sections into a single UI
  tagList(parameter_sections)
})
# --- END OF MODIFIED SECTION ---

# Render the individual plots
observeEvent(all_plots_data(), {
  plot_data <- all_plots_data()
  
  for (plot_id in names(plot_data)) {
    local({
      local_plot_id <- plot_id
      output_id <- paste0("aoi_plot_", local_plot_id)
      param_code <- strsplit(local_plot_id, "_")[[1]][1]
      
      output[[output_id]] <- renderHighchart({
        df <- plot_data[[local_plot_id]]
        
        if (is.data.frame(df) && nrow(df) > 0 && !all(is.na(df$value))) {
          create_timeseries_chart(
            data_input = df,
            param = param_code,
            params_def = params_def
          )
        } else {
          highchart() %>% hc_title(text = "Data not available")
        }
      })
    })
  }
})

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
    id <- showNotification(
      "Generating report...",
      duration = NULL,
      closeButton = FALSE,
      type = "message"
    )
    on.exit(removeNotification(id))

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
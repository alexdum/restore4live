data_sel <- reactive({
  prepare_climate_data(
    param = input$param,
    season = input$season,
    season_ind = input$season_ind,
    scen = input$scen,
    quant = input$quant,
    period_climate = input$period_climate,
    period_change = input$period_change,
    transp = input$transp,
    files_cmip6 = files_cmip6,
    params_def = params_def,
    select_seas = select_seas
  )
})

# MapLibre state for climate scenario map
scen_style_change_trigger <- reactiveVal(0)
scen_map_initialized <- reactiveVal(FALSE)
scen_current_raster_layers <- reactiveVal(character(0))
scen_label_layer_ids <- maplibre_label_layer_ids
scen_non_label_layer_ids <- maplibre_non_label_layer_ids
scen_boundary_layer_ids <- maplibre_boundary_layer_ids
scen_vector_layer_ids <- scenario_maplibre_vector_layer_ids
scen_all_areas <- maplibre_build_all_areas(country_layers)


# functie harta de start
output$map <- mapgl::renderMaplibre({
  mapgl::maplibre(
    style = ofm_positron_style,
    center = c(19, 46),
    zoom = 5
  ) %>%
    mapgl::add_navigation_control(show_compass = FALSE, visualize_pitch = FALSE, position = "top-left")
})

observe({
  req(!scen_map_initialized())
  req(input$map_zoom)

  mapgl::maplibre_proxy("map") %>%
    mapgl::fit_bounds(maplibre_get_bbox(dun, buffer_m = 0), animate = FALSE)

  scen_map_initialized(TRUE)
})

observeEvent(input$navbar, {
  if (input$navbar == "climate_scenario") {
    shinyjs::runjs(maplibre_resize_script("map"))
  }
})

observeEvent(input$zoom_home_scen, {
  req(scen_map_initialized())

  mapgl::maplibre_proxy("map") %>%
    mapgl::fit_bounds(maplibre_get_bbox(dun, buffer_m = 0), animate = TRUE)
})

observeEvent(input$basemap_scen, {
  req(scen_map_initialized())
  maplibre_switch_basemap(
    map_id = "map",
    basemap = input$basemap_scen,
    current_basemap = function() input$basemap_scen,
    show_labels = function() input$show_labels_scen,
    current_raster_layers = scen_current_raster_layers,
    style_change_trigger = scen_style_change_trigger,
    sentinel_ids = function() {
      list(
        source_id = "scenario-sentinel",
        layer_id = "scenario-sentinel"
      )
    },
    label_layer_ids = scen_label_layer_ids,
    non_label_layer_ids = scen_non_label_layer_ids
  )
})

observeEvent(input$show_labels_scen,
  {
    req(scen_map_initialized())
    maplibre_apply_label_visibility(mapgl::maplibre_proxy("map"), input$show_labels_scen, scen_label_layer_ids)
  },
  ignoreInit = TRUE
)

observe({
  req(scen_map_initialized())

  scen_style_change_trigger()
  selected_shape <- maplibre_get_area_shape(
    input$test_area,
    dun,
    is1_austria,
    is2_slovakia,
    is3_serbia,
    is4_romania,
    ms1_germany,
    ms2_slovakia,
    ms3_serbia,
    ms4_romania,
    ms5_romania,
    ms6_romania
  )
  selected_shape <- if (!identical(input$test_area, "drb")) {
    sf::st_as_sf(selected_shape) %>%
      dplyr::mutate(area_label = maplibre_get_area_label(input$test_area, select_area)) %>%
      dplyr::select(area_label)
  } else {
    NULL
  }

  proxy <- scenario_maplibre_draw_area_layers(
    proxy = mapgl::maplibre_proxy("map"),
    danube_shape = dun,
    all_areas = scen_all_areas,
    selected_shape = selected_shape
  )

  proxy %>%
    maplibre_place_layer_below_anchors("scenario-raster", c(scen_boundary_layer_ids, scen_vector_layer_ids, scen_label_layer_ids)) %>%
    maplibre_bring_layers_to_front(scen_vector_layer_ids) %>%
    maplibre_bring_layers_to_front(scenario_maplibre_top_line_layer_ids, before_id = NULL)
})

# update map outputuri
observe({
  req(input$navbar == "climate_scenario")
  req(scen_map_initialized())

  r <- data_sel()$r
  pal <- data_sel()$pal
  opacy <- data_sel()$opacy

  scen_style_change_trigger()

  scenario_maplibre_update_raster(
    proxy = mapgl::maplibre_proxy("map"),
    raster = r,
    palette = pal,
    opacity = opacy
  ) %>%
    maplibre_place_layer_below_anchors("scenario-raster", c(scen_boundary_layer_ids, scen_vector_layer_ids, scen_label_layer_ids)) %>%
    maplibre_bring_layers_to_front(scen_vector_layer_ids) %>%
    maplibre_bring_layers_to_front(scenario_maplibre_top_line_layer_ids, before_id = NULL)
})


# zoom to selected area
observeEvent(input$test_area, {
  req(input$test_area)

  shape_to_zoom <- maplibre_get_area_shape(
    input$test_area,
    dun,
    is1_austria,
    is2_slovakia,
    is3_serbia,
    is4_romania,
    ms1_germany,
    ms2_slovakia,
    ms3_serbia,
    ms4_romania,
    ms5_romania,
    ms6_romania
  )


  req(scen_map_initialized())

  mapgl::maplibre_proxy("map") %>%
    mapgl::fit_bounds(
      maplibre_get_bbox(shape_to_zoom),
      animate = TRUE
    )
})


output$map_titl <- renderText({
  param <- data_sel()$param_name
  season <- data_sel()$season_name
  if (input$quant %in% "climate") {
    paste(param, season, toupper(input$scen), " - multiannual mean", input$period_climate)
  } else {
    paste(param, season, toupper(input$scen), " - change in multiannual mean", input$period_change[2], "vs.", input$period_change[1])
  }
})

# reactive values pentru plot lst time series din raster
values_plot_na <- reactiveValues(input = NULL, title = NULL, lon = 25, lat = 46, mode = "point")

observeEvent(input$test_area, {
  if (input$test_area %in% c("at1", "at2", "at3", "at4", "at5", "sk1", "rs1", "ro1", "de1", "sk2", "rs2", "ro2", "ro3", "ro4")) {
    values_plot_na$mode <- "zonal"
  } else {
    values_plot_na$mode <- "point"
  }
})

# interactivitate raster
observeEvent(input$map_click, {
  req(input$map_click)
  # Do not trigger point extraction when a country is selected
  if (input$test_area %in% "drb") {
    values_plot_na$mode <- "point"
    values_plot_na$lon <- input$map_click$lng
    values_plot_na$lat <- input$map_click$lat
  }
})

observe({
  # Make it depend on all relevant inputs
  req(input$param, input$scen, input$season, input$quant, input$period_change, input$period_climate, values_plot_na$mode)

  if (values_plot_na$mode == "zonal") {
    req(input$test_area %in% c("drb", "at1", "at2", "at3", "at4", "at5", "sk1", "rs1", "ro1", "de1", "sk2", "rs2", "ro2", "ro3", "ro4"))
    shape_to_extract <- switch(input$test_area,
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

    country_name <- names(area_choices)[area_choices == input$test_area]

    ddf <- extract_zonal_data(
      file_hist = data_sel()$file_hist,
      file_scen = data_sel()$file_scen,
      shape = shape_to_extract,
      season_subset = data_sel()$season_subset,
      param = input$param,
      quant = input$quant,
      period_change = input$period_change,
      file_ind = data_sel()$file_ind
    )

    values_plot_na$input <- ddf
    values_plot_na$title <- paste(data_sel()$param_name, "for", country_name, "test area", toString(shape_to_extract$Name))
  } else {
    lon <- values_plot_na$lon
    lat <- values_plot_na$lat


    ddf <- extract_data(data_sel()$file_hist, data_sel()$file_scen, extract_point, lon, lat, input$param, data_sel()$season_subset, input$quant, input$period_change, input$period_climate, data_sel()$file_ind)


    if (is.data.frame(ddf)) {
      if (!all(is.na(ddf$value))) {
        values_plot_na$input <- ddf
        values_plot_na$title <- graph_title_climate(data_sel()$param_name, input$quant, input$param, input$period_change, lon, lat)
      } else {
        values_plot_na$input <- "No data available for the selected point"
      }
    } else {
      values_plot_na$input <- ddf
    }
  }
})


output$chart_scen <- renderHighchart({
  create_timeseries_chart(
    data_input = values_plot_na$input,
    param = input$param,
    params_def = params_def,
    use_percentile_axis = TRUE,
    percentile_axis_padding = 0.01,
    hide_hover_markers = TRUE
  )
})

output$scenario_context_panel <- renderUI({
  req(input$param, input$scen, input$quant)

  area_label <- maplibre_get_area_label(input$test_area, select_area)
  is_zonal <- identical(values_plot_na$mode, "zonal")
  metric_label <- if (identical(input$quant, "climate")) {
    "Climatology"
  } else {
    "Change vs baseline"
  }
  period_label <- if (identical(input$quant, "climate")) {
    input$period_climate
  } else {
    paste(input$period_change[2], "vs.", input$period_change[1])
  }
  selection_value <- if (is_zonal) {
    area_label
  } else {
    sprintf("%.3f°E, %.3f°N", values_plot_na$lon, values_plot_na$lat)
  }
  point_in_area <- if (!is_zonal) {
    selected_shape <- maplibre_get_area_shape(
      input$test_area,
      dun,
      is1_austria,
      is2_slovakia,
      is3_serbia,
      is4_romania,
      ms1_germany,
      ms2_slovakia,
      ms3_serbia,
      ms4_romania,
      ms5_romania,
      ms6_romania
    )

    selected_shape <- sf::st_make_valid(sf::st_as_sf(selected_shape))
    selected_point <- sf::st_sf(
      geometry = sf::st_sfc(
        sf::st_point(c(values_plot_na$lon, values_plot_na$lat)),
        crs = 4326
      )
    )

    tryCatch(
      {
        selected_point <- sf::st_transform(selected_point, sf::st_crs(selected_shape))
        any(lengths(sf::st_intersects(selected_point, selected_shape)) > 0)
      },
      error = function(e) {
        FALSE
      }
    )
  } else {
    NA
  }
  status_value <- if (is.data.frame(values_plot_na$input)) {
    "Series ready"
  } else if (is.character(values_plot_na$input) && length(values_plot_na$input) == 1) {
    values_plot_na$input
  } else {
    "Waiting for data"
  }
  tip_text <- if (is_zonal) {
    "Select the Danube basin to return to point-based inspection on the map."
  } else {
    "Click another map location to update the series for a new point."
  }

  info_rows <- if (is_zonal) {
    list(
      c("View", "Area-average series"),
      c("Area of interest", area_label),
      c("Parameter", data_sel()$param_name),
      c("Season", data_sel()$season_name),
      c("Scenario", toupper(input$scen)),
      c("Metric", metric_label),
      c("Period", period_label),
      c("Status", status_value)
    )
  } else {
    list(
      c("View", "Point series"),
      c("Area of interest", area_label),
      c(
        "Point location",
        if (isTRUE(point_in_area)) paste("Inside", area_label) else paste("Outside", area_label)
      ),
      c("Selected point", selection_value),
      c("Parameter", data_sel()$param_name),
      c("Season", data_sel()$season_name),
      c("Scenario", toupper(input$scen)),
      c("Metric", metric_label),
      c("Period", period_label),
      c("Status", status_value)
    )
  }

  shiny::tagList(
    tags$div(
      class = "scenario-context-section",
      tags$div(
        class = "scenario-context-grid",
        lapply(info_rows, function(row) {
          tags$div(
            class = "scenario-context-row",
            tags$span(class = "scenario-context-label", row[[1]]),
            tags$span(class = "scenario-context-value", row[[2]])
          )
        })
      )
    ),
    tags$div(
      class = "scenario-context-section scenario-context-note",
      tags$div(class = "scenario-context-title", "Tip"),
      tags$p(tip_text)
    )
  )
})

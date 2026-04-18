create_timeseries_chart <- function(
    data_input,
    param,
    params_def,
    is_anomaly = FALSE,
    use_percentile_axis = FALSE,
    percentile_axis_padding = 0.05,
    hide_hover_markers = FALSE) {
  param_info <- params_def[params_def$input == param, ]
  param_name <- param_info$parm
  param_unit <- param_info$unit

  if (is_anomaly) {
    y_label <- if (param == "pr") "Anomaly (%)" else paste0("Anomaly (", param_unit, ")")
    series_name <- "Anomaly"
    value_col <- "anomaly"
    low_col <- "anomaly10"
    high_col <- "anomaly90"
  } else {
    y_label <- if (length(param_unit) > 0 && !is.na(param_unit) && nzchar(trimws(param_unit))) {
      param_unit
    } else {
      "Value"
    }
    series_name <- param_name
    value_col <- "value"
    low_col <- "value10"
    high_col <- "value90"
  }

  col_line <- ifelse(param %in% c("pr", "csdi"), "blue", "red")
  marker_options <- if (isTRUE(hide_hover_markers)) {
    list(
      enabled = FALSE,
      states = list(
        hover = list(
          enabled = FALSE
        )
      )
    )
  } else {
    list(enabled = FALSE)
  }

  if (!is.character(data_input)) {
    # Calculate trendline using linear regression
    trendline <- lm(data_input[[value_col]] ~ as.numeric(data_input$date), data = data_input)
    data_input$trendline <- predict(trendline) |> round(3)

    y_axis_limits <- NULL
    if (isTRUE(use_percentile_axis) && all(c(value_col, low_col, high_col) %in% names(data_input))) {
      y_values <- c(data_input[[low_col]], data_input[[value_col]], data_input[[high_col]])
      y_values <- y_values[is.finite(y_values)]

      if (length(y_values) > 0) {
        y_min <- min(y_values)
        y_max <- max(y_values)
        y_span <- y_max - y_min
        padding <- if (isTRUE(all.equal(y_span, 0))) {
          max(abs(y_min) * percentile_axis_padding, 1)
        } else {
          y_span * percentile_axis_padding
        }

        y_axis_limits <- list(
          min = y_min - padding,
          max = y_max + padding
        )
      }
    }

    chart <- highchart() %>%
      hc_xAxis(categories = format(data_input$date, "%Y")) %>%
      hc_add_series(color = col_line, name = series_name, data = data_input[[value_col]], type = "line", marker = marker_options) %>%
      hc_add_series(name = "P10 - P90", data = list_parse2(data.frame(low = data_input[[low_col]], high = data_input[[high_col]])), type = "arearange", color = "#CCCCCC", lineWidth = 0, fillOpacity = 0.3, zIndex = 0, marker = marker_options) %>%
      hc_add_series(name = "Trendline", data = data_input$trendline, type = "line", color = "grey", dashStyle = "shortdash", marker = marker_options) %>%
      hc_exporting(
        enabled = TRUE,
        buttons = list(
          contextButton = list(
            menuItems = list(
              "downloadPNG",
              "downloadJPEG",
              "downloadPDF",
              "downloadSVG",
              "separator",
              "downloadCSV",
              "downloadXLS"
            )
          )
        )
      )

    if (is.null(y_axis_limits)) {
      chart <- chart %>%
        hc_yAxis(title = list(text = y_label))
    } else {
      chart <- chart %>%
        hc_yAxis(
          title = list(text = y_label),
          min = y_axis_limits$min,
          max = y_axis_limits$max
        )
    }

    chart
  } else {
    highchart() |>
      hc_title(
        text = data_input,
        style = list(fontSize = "14px", color = "grey")
      ) |>
      hc_exporting(enabled = TRUE)
  }
}

create_timeseries_chart <- function(data_input, param, params_def, is_anomaly = FALSE) {
  
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
  
  col_line <- ifelse(param %in% c("pr","csdi"), "blue", "red")
  
  if (!is.character(data_input)) {
    # Calculate trendline using linear regression
    trendline <- lm(data_input[[value_col]] ~ as.numeric(data_input$date), data = data_input)
    data_input$trendline <- predict(trendline) |> round(3)
    
    highchart() %>%
      hc_xAxis(categories = format(data_input$date, "%Y")) %>%
      hc_yAxis(title = list(text = y_label)) %>%
      hc_add_series(color = col_line, name = series_name, data = data_input[[value_col]], type = 'line', marker = list(enabled = FALSE)) %>%
      hc_add_series(name = "P10 - P90", data = list_parse2(data.frame(low = data_input[[low_col]], high = data_input[[high_col]])), type = 'arearange', color = '#CCCCCC', lineWidth = 0, fillOpacity = 0.3, zIndex = 0) |>
      hc_add_series(name = "Trendline", data = data_input$trendline, type = 'line', color = 'grey', dashStyle = 'shortdash')
      
  } else {
    highchart() |> 
      hc_title(
        text = data_input,
        style = list(fontSize = "14px", color = "grey"))
  }
}
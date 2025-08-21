create_timeseries_chart <- function(data_input, param, params_def) {
  
  param_info <- params_def[params_def$input == param, ]
  param_name <- param_info$parm
  param_unit <- param_info$unit
  
  # Set y-axis label to the unit, with a fallback
  y_label <- if (length(param_unit) > 0 && !is.na(param_unit) && nzchar(trimws(param_unit))) {
    param_unit
  } else {
    "Value" # Fallback if unit is not available
  }
  
  col_line <- ifelse(param %in% c("pr","csdi"), "blue", "red")
  
  if (!is.character(data_input)) {
    # Calculate trendline using linear regression
    trendline <- lm(value ~ as.numeric(date), data = data_input)
    data_input$trendline <- predict(trendline) |> round(3)
    highchart() %>%
      #hc_title(text = "Value Trends Over Years") %>%
      hc_xAxis(categories = format(data_input$date, "%Y")) %>%
      hc_yAxis(title = list(text = y_label)) %>%
      hc_add_series(color =  col_line,name = param_name, data = data_input$value, type = 'line', marker = list(enabled = FALSE)) %>%
      hc_add_series(name = "P10 - P90", data = list_parse2(data.frame(low = data_input$value10, high = data_input$value90)), type = 'arearange', color = '#CCCCCC', lineWidth = 0, fillOpacity = 0.3, zIndex = 0) |>
      hc_add_series(name = "Trendline", data = data_input$trendline, type = 'line', color = 'grey', dashStyle = 'shortdash')
    
  } else {
    highchart() |> 
      hc_title(
        text = data_input,
        style = list(fontSize = "14px", color = "grey"))
  }
}
create_timeseries_chart <- function(data_input, param, params_def) {
  
  param_name <- params_def$parm[params_def$input %in% param]
  col_line <- ifelse(param %in% c("pr","csdi"), "blue", "red")
  
  if (!is.character(data_input)) {
    # Calculate trendline using linear regression
    trendline <- lm(value ~ as.numeric(date), data = data_input)
    data_input$trendline <- predict(trendline) |> round(3)
    highchart() %>%
      #hc_title(text = "Value Trends Over Years") %>%
      hc_xAxis(categories = format(data_input$date, "%Y")) %>%
      hc_yAxis(title = list(text = "Value")) %>%
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
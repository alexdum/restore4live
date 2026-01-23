graph_title_hydro <- function(param_name, quant, param, period_change, lon, lat) {
    if (quant %in% "climate") {
        values_plot_na <- paste0(param_name, " values for point lon = ", round(lon, 3), " lat = ", round(lat, 3), " (click on map to update the graph)")
    } else {
        values_plot_na <- paste0(param_name, " % change relative to the ", period_change[1], " mean; values for point lon = ", round(lon, 5), " lat = ", round(lat, 5), " (click on map to update the graph)")
    }
    return(values_plot_na)
}

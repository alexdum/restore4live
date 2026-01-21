#' Plot CLIMAT Observational Data
#'
#' Creates 4 separate Plotly visualizations for observational climate data.
#' Returns a list of 4 plots that can be displayed in separate cards.
#'
#' @param df Data frame with columns: Date, MeanTemp, MaxTempAbs, MinTempAbs, Precipitation, SunshineDuration, etc.
#' @param station_name Title for the plot
#' @return A list of 4 plotly objects: temperature, precipitation, sunshine, pressure
#' @export
plot_climat_obs_data <- function(df, station_name = "") {
    library(plotly)
    library(dplyr)

    empty_plot <- function(msg = "No data available") {
        plotly::plot_ly() %>%
            plotly::add_annotations(
                text = msg,
                showarrow = FALSE,
                xref = "paper", yref = "paper",
                x = 0.5, y = 0.5
            ) %>%
            plotly::layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
    }

    if (is.null(df) || nrow(df) == 0) {
        return(list(
            temp = empty_plot(),
            precip = empty_plot(),
            sun = empty_plot(),
            pressure = empty_plot()
        ))
    }

    # Ensure Date is Date object
    df$Date <- as.Date(df$Date)

    # Calculate date range for display
    data_max <- max(df$Date, na.rm = TRUE)
    data_min <- min(df$Date, na.rm = TRUE)
    view_min <- data_max - (365 * 5)
    if (view_min < data_min) view_min <- data_min

    # Common x-axis configuration
    common_xaxis <- list(
        title = "",
        range = c(view_min, data_max),
        rangeselector = list(
            buttons = list(
                list(count = 1, label = "1y", step = "year", stepmode = "backward"),
                list(count = 5, label = "5y", step = "year", stepmode = "backward"),
                list(count = 10, label = "10y", step = "year", stepmode = "backward"),
                list(step = "all")
            )
        ),
        rangeslider = list(visible = FALSE),
        type = "date"
    )

    # --- Plot 1: Temperature ---
    has_temp <- "MeanTemp" %in% names(df) && any(!is.na(df$MeanTemp))
    p_temp <- plotly::plot_ly(data = df)

    if (has_temp) {
        if ("MinTempAbs" %in% names(df) && "MaxTempAbs" %in% names(df)) {
            p_temp <- p_temp %>%
                add_trace(
                    x = ~Date, y = ~MinTempAbs, type = "scatter", mode = "lines",
                    line = list(color = "rgba(211, 47, 47, 0.1)", width = 0),
                    showlegend = FALSE, name = "Abs Min",
                    hovertemplate = "Abs Min: %{y:.1f}°C<extra></extra>"
                ) %>%
                add_trace(
                    x = ~Date, y = ~MaxTempAbs, type = "scatter", mode = "lines",
                    fill = "tonexty", fillcolor = "rgba(211, 47, 47, 0.1)",
                    line = list(color = "rgba(211, 47, 47, 0.1)", width = 0),
                    name = "Abs Max", showlegend = TRUE,
                    hovertemplate = "Abs Max: %{y:.1f}°C<extra></extra>"
                )
        }

        if ("MeanMinTemp" %in% names(df) && "MeanMaxTemp" %in% names(df)) {
            p_temp <- p_temp %>%
                add_trace(
                    x = ~Date, y = ~MeanMinTemp, type = "scatter", mode = "lines",
                    line = list(color = "rgba(211, 47, 47, 0.3)", width = 0),
                    showlegend = FALSE, name = "Mean Min",
                    hovertemplate = "Mean Min: %{y:.1f}°C<extra></extra>"
                ) %>%
                add_trace(
                    x = ~Date, y = ~MeanMaxTemp, type = "scatter", mode = "lines",
                    fill = "tonexty", fillcolor = "rgba(211, 47, 47, 0.3)",
                    line = list(color = "rgba(211, 47, 47, 0.3)", width = 0),
                    name = "Mean Max", showlegend = TRUE,
                    hovertemplate = "Mean Max: %{y:.1f}°C<extra></extra>"
                )
        }

        p_temp <- p_temp %>% add_lines(
            x = ~Date, y = ~MeanTemp, name = "Mean Temp",
            line = list(color = "#b71c1c", width = 2),
            hovertemplate = "Mean Temp: %{y:.1f}°C<extra></extra>"
        )
    } else {
        p_temp <- empty_plot("No temperature data")
    }

    p_temp <- p_temp %>%
        layout(
            title = list(text = "Temperature", font = list(size = 12)),
            yaxis = list(title = "Temp (°C)"),
            xaxis = common_xaxis,
            hovermode = "x unified",
            legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15),
            margin = list(t = 40, r = 10, b = 60, l = 50)
        ) %>%
        config(displaylogo = FALSE)

    # --- Plot 2: Precipitation ---
    has_precip <- "Precipitation" %in% names(df) && any(!is.na(df$Precipitation))
    has_precip_days <- "PrecipDays" %in% names(df) && any(!is.na(df$PrecipDays))
    p_precip <- plotly::plot_ly(data = df)

    if (has_precip) {
        p_precip <- p_precip %>% add_bars(
            x = ~Date, y = ~Precipitation, name = "Precipitation",
            marker = list(color = "#1976d2"),
            hovertemplate = "Precip: %{y:.1f} mm<extra></extra>"
        )

        if (has_precip_days) {
            p_precip <- p_precip %>% add_trace(
                x = ~Date, y = ~PrecipDays, name = "Precip Days",
                yaxis = "y2", type = "scatter", mode = "markers",
                marker = list(color = "#388e3c", size = 5, line = list(color = "yellow", width = 1)),
                hovertemplate = "Days >1mm: %{y:.0f}<extra></extra>"
            )
        }
    } else {
        p_precip <- empty_plot("No precipitation data")
    }

    p_precip <- p_precip %>%
        layout(
            title = list(text = "Precipitation", font = list(size = 12)),
            yaxis = list(title = "Precip (mm)", rangemode = "tozero"),
            yaxis2 = list(title = "Days", overlaying = "y", side = "right", showgrid = FALSE, rangemode = "tozero"),
            xaxis = common_xaxis,
            hovermode = "x unified",
            legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15),
            margin = list(t = 40, r = 50, b = 60, l = 50)
        ) %>%
        config(displaylogo = FALSE)

    # --- Plot 3: Sunshine Duration ---
    has_sun <- "SunshineDuration" %in% names(df) && any(!is.na(df$SunshineDuration))
    p_sun <- plotly::plot_ly(data = df)

    if (has_sun) {
        p_sun <- p_sun %>% add_bars(
            x = ~Date, y = ~SunshineDuration, name = "Sunshine",
            marker = list(color = "#fbc02d"),
            hovertemplate = "Sun: %{y:.1f} h<extra></extra>"
        )
    } else {
        p_sun <- empty_plot("No sunshine data")
    }

    p_sun <- p_sun %>%
        layout(
            title = list(text = "Sunshine Duration", font = list(size = 12)),
            yaxis = list(title = "Sun (h)"),
            xaxis = common_xaxis,
            hovermode = "x unified",
            legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15),
            margin = list(t = 40, r = 10, b = 60, l = 50)
        ) %>%
        config(displaylogo = FALSE)

    # --- Plot 4: Pressure (MSLP + Vapour Pressure) ---
    has_vapour <- "VapourPressure" %in% names(df) && any(!is.na(df$VapourPressure))
    has_press <- "MeanSeaLevelPressure" %in% names(df) && any(!is.na(df$MeanSeaLevelPressure))
    p_pressure <- plotly::plot_ly(data = df)

    if (has_press) {
        p_pressure <- p_pressure %>%
            add_lines(
                x = ~Date, y = ~MeanSeaLevelPressure, name = "MSLP",
                line = list(color = "#7b1fa2", width = 2),
                hovertemplate = "MSLP: %{y:.1f} hPa<extra></extra>"
            ) %>%
            add_lines(
                x = ~Date, y = rep(1013.25, nrow(df)), name = "Std (1013.25)",
                line = list(color = "grey", dash = "dash", width = 1),
                hoverinfo = "skip"
            )
    }

    if (has_vapour) {
        p_pressure <- p_pressure %>% add_lines(
            x = ~Date, y = ~VapourPressure, name = "Vapour Pressure",
            yaxis = "y2", line = list(color = "#2e7d32", width = 2),
            hovertemplate = "VapPres: %{y:.1f} hPa<extra></extra>"
        )
    }

    if (!has_press && !has_vapour) {
        p_pressure <- empty_plot("No pressure data")
    }

    p_pressure <- p_pressure %>%
        layout(
            title = list(text = "Pressure", font = list(size = 12)),
            yaxis = list(title = "MSLP (hPa)"),
            yaxis2 = list(title = "VP (hPa)", overlaying = "y", side = "right", showgrid = FALSE),
            xaxis = common_xaxis,
            hovermode = "x unified",
            legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15),
            margin = list(t = 40, r = 50, b = 60, l = 50)
        ) %>%
        config(displaylogo = FALSE)

    # Return list of 4 plots
    return(list(
        temp = p_temp,
        precip = p_precip,
        sun = p_sun,
        pressure = p_pressure
    ))
}

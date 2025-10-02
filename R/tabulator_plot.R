# =============================================================================
# TABULATOR PLOT FORMATTERS
# =============================================================================

#' Create Tabulator plot formatters for plot_tt()
#' @param plot_type Character string: "histogram", "density", "bar", "line"
#' @param data Data for the plot
#' @param color Color for the plot (single value or length-2 vector)
#' @param xlim X-axis limits
#' @return List with formatter configuration
#' @keywords internal
#' @noRd
tabulator_plot_formatter <- function(
    plot_type,
    data,
    color = "black",
    xlim = NULL) {

  formatter_config <- list()

  # Handle color as single value or length-2 vector
  bar_color <- color[1]
  bg_color <- if (length(color) == 2) color[2] else NULL

  if (plot_type == "line") {
    # Use custom SVG sparkline formatter
    if (is.data.frame(data) && "x" %in% names(data) && "y" %in% names(data)) {
      y_values <- data$y
    } else if (is.numeric(data)) {
      y_values <- data
    } else {
      stop("Line data must be a data frame with x and y columns or a numeric vector", call. = FALSE)
    }

    formatter_config$formatter <- "tinytable_sparkline"
    formatter_config$formatterParams <- list(
      color = bar_color,
      width = 120,
      height = 30,
      strokeWidth = 1.5
    )
    formatter_config$sorter <- "tinytable_sparkline_sorter"
    return(list(
      config = formatter_config,
      data = y_values,
      requires_custom_js = TRUE
    ))

  } else if (plot_type == "bar") {
    # Use Tabulator's built-in progress formatter
    if (!is.numeric(data) || length(data) != 1) {
      stop("Bar data must be a single numeric value", call. = FALSE)
    }

    max_val <- if (!is.null(xlim)) xlim[2] else max(data, na.rm = TRUE)

    formatter_config$formatter <- "progress"
    formatter_config$formatterParams <- list(
      min = 0,
      max = max_val,
      color = bar_color,
      legendColor = "#000000",
      legendAlign = "left"
    )

    return(list(config = formatter_config, data = data))

  } else if (plot_type == "histogram") {
    # For histogram, use custom formatter with canvas
    # Data will be stored in cell value, not formatterParams
    if (!is.numeric(data)) {
      stop("Histogram data must be numeric", call. = FALSE)
    }

    return(list(
      config = list(
        formatter = "tinytable_histogram",
        formatterParams = list(
          color = bar_color,
          width = 120,
          height = 30
        ),
        sorter = "tinytable_histogram_sorter"
      ),
      data = as.numeric(data),
      requires_custom_js = TRUE
    ))

  } else if (plot_type == "density") {
    # For density, calculate density points and use custom sparkline with fill
    if (!is.numeric(data)) {
      stop("Density data must be numeric", call. = FALSE)
    }

    dens <- stats::density(stats::na.omit(data))
    y_values <- dens$y

    formatter_config$formatter <- "tinytable_sparkline"
    formatter_config$formatterParams <- list(
      color = bar_color,
      fillArea = TRUE,
      width = 120,
      height = 30,
      strokeWidth = 1.5
    )
    formatter_config$sorter <- "tinytable_density_sorter"
    return(list(
      config = formatter_config,
      data = y_values,
      requires_custom_js = TRUE
    ))
  }

  stop(paste("Unknown plot type:", plot_type), call. = FALSE)
}



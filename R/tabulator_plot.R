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
        )
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
    return(list(
      config = formatter_config,
      data = y_values,
      requires_custom_js = TRUE
    ))
  }

  stop(paste("Unknown plot type:", plot_type), call. = FALSE)
}


#' Generate custom JavaScript formatter for histograms
#' @return JavaScript code string
#' @keywords internal
#' @noRd
tabulator_histogram_js <- function() {
  js <- '
// Custom histogram formatter
function tinytable_histogram(cell, formatterParams) {
  const data = formatterParams.data;
  const color = formatterParams.color || "black";
  const width = formatterParams.width || 120;
  const height = formatterParams.height || 30;

  if (!data || data.length === 0) return "";

  // Calculate histogram bins
  const numBins = Math.min(10, Math.floor(data.length / 2));
  const min = Math.min(...data);
  const max = Math.max(...data);
  const binWidth = (max - min) / numBins;

  const bins = new Array(numBins).fill(0);
  for (let val of data) {
    let binIndex = Math.floor((val - min) / binWidth);
    if (binIndex === numBins) binIndex--; // Edge case for max value
    bins[binIndex]++;
  }

  const maxCount = Math.max(...bins);

  // Create canvas
  const canvas = document.createElement("canvas");
  canvas.width = width;
  canvas.height = height;
  const ctx = canvas.getContext("2d");

  // Draw histogram
  ctx.fillStyle = color;
  const barWidth = width / numBins;

  for (let i = 0; i < numBins; i++) {
    const barHeight = (bins[i] / maxCount) * height;
    ctx.fillRect(
      i * barWidth,
      height - barHeight,
      barWidth - 1,
      barHeight
    );
  }

  return canvas;
}
'
  return(js)
}


#' Generate custom JavaScript formatter for sparklines
#' @return JavaScript code string
#' @keywords internal
#' @noRd
tabulator_sparkline_js <- function() {
  js <- '
// Custom SVG sparkline formatter
function tinytable_sparkline(cell, formatterParams) {
  const values = cell.getValue() || [];
  const color = formatterParams.color || "currentColor";
  const width = formatterParams.width || 120;
  const height = formatterParams.height || 30;
  const strokeWidth = formatterParams.strokeWidth || 1.5;
  const fillArea = formatterParams.fillArea || false;

  if (!values || values.length === 0) return "";

  const svgNS = "http://www.w3.org/2000/svg";
  const pad = 2;
  const n = values.length;

  const min = Math.min(...values);
  const max = Math.max(...values);
  const span = (max - min) || 1;

  // scale functions
  const x = i => pad + (i * (width - 2*pad)) / (n - 1 || 1);
  const y = v => height - pad - ((v - min) * (height - 2*pad)) / span;

  // build path
  let d = `M ${x(0)},${y(values[0])}`;
  for (let i = 1; i < n; i++) {
    d += ` L ${x(i)},${y(values[i])}`;
  }

  // create SVG
  const svg = document.createElementNS(svgNS, "svg");
  svg.setAttribute("viewBox", `0 0 ${width} ${height}`);
  svg.setAttribute("width", width);
  svg.setAttribute("height", height);
  svg.setAttribute("class", "sparkline");
  svg.setAttribute("title", values.join(", "));

  // path
  const path = document.createElementNS(svgNS, "path");
  path.setAttribute("d", d);
  path.setAttribute("stroke", color);
  path.setAttribute("stroke-width", strokeWidth);

  if (fillArea) {
    // Close the path for fill
    path.setAttribute("d", d + ` L ${x(n-1)},${height-pad} L ${x(0)},${height-pad} Z`);
    path.setAttribute("fill", color);
    path.setAttribute("fill-opacity", "0.3");
  } else {
    path.setAttribute("fill", "none");
  }

  svg.appendChild(path);

  // dot at last value (for line only)
  if (!fillArea) {
    const cx = x(n - 1), cy = y(values[n - 1]);
    const dot = document.createElementNS(svgNS, "circle");
    dot.setAttribute("cx", cx);
    dot.setAttribute("cy", cy);
    dot.setAttribute("r", 2.2);
    dot.setAttribute("fill", color);
    svg.appendChild(dot);
  }

  return svg;
}
'
  return(js)
}


#' Inject histogram JavaScript into table
#' @param x tinytable object
#' @return Modified tinytable object
#' @keywords internal
#' @noRd
tabulator_inject_histogram_js <- function(x) {
  histogram_js <- tabulator_histogram_js()

  # Find the location to inject JS (before table initialization)
  # Look for the script tag that contains the table initialization
  pattern <- "<script>\\s*\\(function\\(\\) \\{"

  # Inject the histogram function before the table IIFE
  injection <- paste0("<script>\n", histogram_js, "\n")

  x@table_string <- sub(
    pattern,
    paste0(injection, "(function() {"),
    x@table_string
  )

  return(x)
}


#' Inject sparkline JavaScript into table
#' @param x tinytable object
#' @return Modified tinytable object
#' @keywords internal
#' @noRd
tabulator_inject_sparkline_js <- function(x) {
  sparkline_js <- tabulator_sparkline_js()

  # Find the location to inject JS (before table initialization)
  pattern <- "<script>\\s*\\(function\\(\\) \\{"

  # Inject the sparkline function before the table IIFE
  injection <- paste0("<script>\n", sparkline_js, "\n")

  x@table_string <- sub(
    pattern,
    paste0(injection, "(function() {"),
    x@table_string
  )

  return(x)
}
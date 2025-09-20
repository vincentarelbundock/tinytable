#' Create Plot Vector
#'
#' @details
#' This function creates plots by creating a temporary `tt()` object and applying `plot_tt()`. It returns a character vector containing the image paths or HTML tags for the plots.
#'
#' @param output Output format. One of "html", "latex", "typst", "markdown", etc.
#' @param fun String or function to generate inline plots (same as in plot_tt).
#' @param data A list of data frames or vectors to be used by the plotting functions in `fun`.
#' @param color String name of color to use for inline plots.
#' @param xlim Numeric vector of length 2.
#' @param height Numeric, the height of the images in the table in em units.
#' @param height_plot Numeric, the height of generated plot images in pixels (default: 400).
#' @param width_plot Numeric, the width of generated plot images in pixels (default: 1200).
#' @param images Character vector, the paths to the images to be inserted.
#' @param assets Path to the directory where generated assets are stored.
#' @param ... Extra arguments are passed to the function in `fun`.
#' @return A character vector with plot file paths or HTML tags.
#' @export
#' @examples
#' \dontrun{
#' # Create histogram plots
#' plot_data <- list(rnorm(100), rnorm(50))
#' plot_vector(fun = "histogram", data = plot_data, output = "html")
#'
#' # Create density plots
#' plot_vector(fun = "density", data = plot_data, output = "latex")
#'
#' # Create bar plots from single values
#' bar_data <- list(0.5, 0.8, 0.3)
#' plot_vector(fun = "barpct", data = bar_data, output = "html")
#' }
plot_vector <- function(
    output = "html",
    fun = NULL,
    data = NULL,
    color = "black",
    xlim = NULL,
    height = 1,
    height_plot = 400,
    width_plot = 1200,
    images = NULL,
    assets = "tinytable_assets",
    ...) {
  # Determine the length based on data or images
  if (!is.null(data)) {
    len <- length(data)
  } else if (!is.null(images)) {
    len <- length(images)
  } else {
    stop("Either `data` or `images` must be provided.", call. = FALSE)
  }

  # Create a temporary data frame with empty strings
  df <- data.frame(x = rep("", len), stringsAsFactors = FALSE)
  tt_obj <- tt(df, output = output)

  # Apply plotting (specify j = 1 to target the single column)
  result <- plot_tt(
    tt_obj,
    j = 1,
    fun = fun,
    data = data,
    color = color,
    xlim = xlim,
    height = height,
    height_plot = height_plot,
    width_plot = width_plot,
    images = images,
    assets = assets,
    ...
  )

  # Build the table to trigger plot generation
  built_result <- build_tt(result, output = output)

  # Extract the plot paths/HTML from the built table
  plot_data <- built_result@data_body$x
  return(plot_data)
}


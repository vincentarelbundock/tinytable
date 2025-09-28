#' Create Plot Vector
#'
#' @details
#' This function creates plots by creating a temporary `tt()` object and applying `plot_tt()`. It returns a character vector containing the image paths or HTML tags for the plots.
#'
#' @param output Output format. One of "html", "latex", "typst", "markdown", etc.
#' @inheritParams plot_tt
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
    sprintf = "%s",
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
    sprintf = sprintf,
    assets = assets,
    ...
  )

  # Build the table to trigger plot generation
  built_result <- build_tt(result, output = output)

  # Extract the plot paths/HTML from the built table
  plot_data <- built_result@data_body$x
  return(plot_data)
}

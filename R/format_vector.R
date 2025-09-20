#' Format a Vector
#'
#' @details
#' This function formats a vector by passing it to `format_tt()`. All formatting arguments must be of length 1 or `length(x)`.
#'
#' @param x A vector to be formatted.
#' @param output Output format. One of "html", "latex", "typst", "markdown", etc.
#' @inheritParams format_tt
#' @return A character vector with formatted values.
#' @export
#' @examples
#' # Format numeric vector
#' format_vector(c(1234.567, 9876.543), digits = 2, num_mark_big = ",")
#'
#' # Format dates
#' dates <- as.Date(c("2023-01-01", "2023-12-31"))
#' format_vector(dates, date = "%B %d, %Y")
#'
#' # Format logical values
#' format_vector(c(TRUE, FALSE, TRUE), bool = function(x) ifelse(x, "Yes", "No"))
format_vector <- function(
    x,
    output = "html",
    digits = NULL,
    num_fmt = "significant",
    num_zero = FALSE,
    num_suffix = FALSE,
    num_mark_big = "",
    num_mark_dec = getOption("OutDec", default = "."),
    date = NULL,
    bool = NULL,
    math = FALSE,
    other = NULL,
    replace = FALSE,
    escape = FALSE,
    markdown = FALSE,
    quarto = FALSE,
    fn = NULL,
    sprintf = NULL,
    linebreak = NULL) {
  assert_atomic_vector(x)

  # Convert vector to data frame and then to tinytable
  df <- data.frame(x = x, stringsAsFactors = FALSE)
  tt_obj <- tt(df, output = output)

  # Apply formatting (specify j = 1 to target the single column)
  result <- format_tt(
    tt_obj,
    j = 1,
    digits = digits,
    num_fmt = num_fmt,
    num_zero = num_zero,
    num_suffix = num_suffix,
    num_mark_big = num_mark_big,
    num_mark_dec = num_mark_dec,
    date = date,
    bool = bool,
    math = math,
    other = other,
    replace = replace,
    escape = escape,
    markdown = markdown,
    quarto = quarto,
    fn = fn,
    sprintf = sprintf,
    linebreak = linebreak
  )

  # Build the table to trigger rendering (including markdown)
  built_result <- build_tt(result, output = output)

  # Extract the formatted vector values from the built table
  formatted_data <- built_result@data_body$x
  return(formatted_data)
}

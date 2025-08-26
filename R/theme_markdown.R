#' Markdown theme with optional ANSI color support and grid customization
#'
#' @param x A `tinytable` object.
#' @param ansi Logical. If TRUE, enables ANSI color codes for grid styling. Default is FALSE.
#' @param hline Logical. Enable/disable horizontal lines. Default is TRUE.
#' @param hline_header Logical. Enable/disable the special header separator line below column names. Default is TRUE.
#' @param vline Logical. Enable/disable vertical lines. Default is TRUE.
#' @param ... Additional arguments (ignored).
#' @return A modified `tinytable` object.
#' @export
theme_markdown <- function(x,
                           ansi = FALSE,
                           hline = TRUE,
                           hline_header = TRUE,
                           vline = TRUE,
                           ...) {
  # Validate logical arguments
  assert_flag(ansi)
  assert_flag(hline)
  assert_flag(hline_header)
  assert_flag(vline)

  # Set the ansi slot based on the argument
  x@ansi <- ansi

  # Set the grid formatting slots
  x@grid_hline <- hline
  x@grid_hline_header <- hline_header
  x@grid_vline <- vline

  return(x)
}


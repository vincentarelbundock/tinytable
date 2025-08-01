#' LaTeX-specific styles and options
#'
#' @param x A `tinytable` object.
#' @param inner A string that specifies the "inner" settings of a tabularray LaTeX table.
#' @param outer A string that specifies the "outer" settings of a tabularray LaTeX table.
#'
#' @export
theme_latex <- function(x, inner = NULL, outer = NULL, environment = "tblr") {
  # tabularray: inner + outer options
  assert_string(inner, null.ok = TRUE)
  assert_string(outer, null.ok = TRUE)
  if (!is.null(inner)) x@latex_inner <- c(x@latex_inner, inner)
  if (!is.null(outer)) x@latex_outer <- c(x@latex_outer, outer)

  return(x)
}

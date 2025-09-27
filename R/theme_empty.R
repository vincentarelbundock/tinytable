#' Theme for a void table
#'
#' This function calls `strip_tt()` to remove all the styles, groups, and formatting applied to a `tinytable` object. It returns a nearly blank table, with only the cell information. Warning: since this function strips the `tinytable` object, the order in which it is called in a pipeline matters.
#' @inheritParams theme_default
#' @export
theme_empty <- function(x, ...) {
  strip <- names(formals(strip_tt))
  # do not strip these arguments
  strip <- setdiff(strip, c("group", "width"))
  strip <- stats::setNames(c(list(x), rep(list(TRUE), length(strip) - 1)), strip)
  x <- do.call(strip_tt, strip)
  return(x)
}

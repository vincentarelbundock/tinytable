#' Striped theme with alternating row colors
#'
#' @param x A `tinytable` object.
#' @param ... Additional arguments (ignored).
#' @return A modified `tinytable` object.
#' @export
theme_striped <- function(x, ...) {
  fn <- function(x) {
    x <- theme_latex(x, inner = "row{even}={bg=black!5!white}")
    x <- style_tt(
      x,
      i = seq(1, nrow(x), by = 2),
      background = "#ededed")
    return(x)
  }
  x <- build_prepare(x, fn)
  return(x)
}

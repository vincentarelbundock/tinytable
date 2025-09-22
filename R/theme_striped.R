#' Striped theme with alternating row colors
#'
#' @param x A `tinytable` object.
#' @param ... Additional arguments (ignored).
#' @return A modified `tinytable` object.
#' @export
theme_striped <- function(x, ...) {
  x <- theme_latex(x, inner = "row{even}={bg=black!5!white}")

  # prepare
  fn <- function(x) {
    x <- style_tt(
      x,
      i = seq(1, nrow(x), by = 2),
      background = "#ededed")
    return(x)
  }
  x <- build_prepare(x, fn, output = c("html", "typst", "grid"))

  return(x)
}

#' Striped theme with alternating row colors
#'
#' @param x A `tinytable` object.
#' @param ... Additional arguments (ignored).
#' @return A modified `tinytable` object.
#' @export
theme_striped <- function(x, ...) {
  x <- theme_drop_default(x)

  # now: all formats
  x <- theme_html(x, engine = "bootstrap", class = "table table-striped")
  x <- theme_latex(x, inner = "row{even}={bg=black!5!white}")
  x <- style_tt(
    x,
    i = seq(1, nrow(x), by = 2),
    background = "#ededed")

  # prepare
  fn <- function(x) {
    x <- style_tt(
      x,
      i = nrow(x),
      line = "b",
      line_color = "#d3d8dc",
      line_width = 0.1
    )
    x <- style_tt(
      x,
      i = 0,
      line = "bt",
      line_color = "#d3d8dc",
      line_width = 0.1
    )
    return(x)
  }
  x <- build_prepare(x, fn, output = c("html", "bootstrap", "typst", "grid"))

  return(x)
}

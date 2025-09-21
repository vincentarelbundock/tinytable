#' Grid theme with borders around all cells
#'
#' @param x A `tinytable` object.
#' @param ... Additional arguments (ignored).
#' @return A modified `tinytable` object.
#' @export
theme_grid <- function(x, ...) {
  # now
  x <- theme_empty(x)

  # prepare: before table is drawn
  fn <- function(x) theme_latex(x, inner = "hlines, vlines,")
  x <- build_prepare(x, fn, output = "latex")

  fn <- function(x) theme_html(x, engine = "bootstrap", class = "table table-bordered")
  x <- build_prepare(x, fn, output = "html")

  # finalize: after table is drawn
  fn <- function(x) {
    x@table_string <- sub(
      "stroke: none,",
      "stroke: (paint: black),",
      x@table_string
    )
    return(x)
  }
  x <- build_finalize(x, fn, output = "typst")

  return(x)
}

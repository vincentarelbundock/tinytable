#' Grid theme with borders around all cells
#'
#' @param x A `tinytable` object.
#' @param ... Additional arguments (ignored).
#' @return A modified `tinytable` object.
#' @export
theme_grid <- function(x, ...) {
  # prepare: before table is drawn
  x <- theme_latex(x, inner = "hlines, vlines,")
  x <- theme_html(x, engine = "bootstrap", class = "table table-bordered")

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

theme_grid <- function(x, ...) {
  # now
  x <- theme_void(x)

  # prepare: before table is drawn
  fn <- function(x) theme_latex(x, inner = "hlines, vlines,")
  x <- build_prepare(x, fn, output = "latex")

  fn <- function(x) theme_bootstrap(x, class = "table table-bordered")
  x <- build_prepare(x, fn, output = c("html", "bootstrap"))

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

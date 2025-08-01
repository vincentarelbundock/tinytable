theme_grid <- function(x, ...) {
  # prepare: before table is drawn
  fn <- function(x) {
    if (identical(x@output, "latex")) {
      x <- theme_latex(x, inner = "hlines, vlines,")
    } else if (identical(x@output, "html")) {
      x <- theme_bootstrap(x, class = "table table-bordered")
    }
    return(x)
  }
  x@lazy_prepare <- c(x@lazy_prepare, list(fn))

  # finalize: after table is drawn
  fn <- function(table) {
    if (isTRUE(table@output == "typst")) {
      table@table_string <- sub(
        "stroke: none,",
        "stroke: (paint: black),",
        table@table_string
      )
    }
    return(table)
  }
  x@lazy_finalize <- c(x@lazy_finalize, list(fn, finalize_theme_void))

  return(x)
}

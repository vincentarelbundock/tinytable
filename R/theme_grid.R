theme_grid <- function(x, ...) {
  # now
  x <- theme_latex(x, inner = "hlines, vlines,")
  x <- theme_bootstrap(x, class = "table table-bordered")

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

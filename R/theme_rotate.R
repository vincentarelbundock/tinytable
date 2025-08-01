theme_rotate <- function(x, angle = 90, ...) {
  assert_numeric(angle, len = 1, lower = 0, upper = 360)

  # latex
  fn <- function(table) {
    rot <- sprintf("\\begin{table}\n\\rotatebox{%s}{", angle)
    table@table_string <- sub(
      "\\begin{table}",
      rot,
      table@table_string,
      fixed = TRUE
    )
    table@table_string <- sub(
      "\\end{table}",
      "}\n\\end{table}",
      table@table_string,
      fixed = TRUE
    )
    return(table)
  }
  attr(fn, "output") <- "latex"
  x@lazy_finalize <- c(x@lazy_finalize, list(fn))

  # typst
  fn <- function(table) {
    rot <- sprintf("#rotate(-%sdeg, reflow: true, [\n  #figure(", angle)
    table@table_string <- sub(
      "#figure(",
      rot,
      table@table_string,
      fixed = TRUE
    )
    table@table_string <- sub(
      ") // end figure",
      ") ]) // end figure",
      table@table_string,
      fixed = TRUE
    )
    return(table)
  }
  attr(fn, "output") <- "typst"
  x@lazy_finalize <- c(x@lazy_finalize, list(fn))

  return(x)
}

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
  x <- build_finalize(x, fn, output = "latex")

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
  x <- build_finalize(x, fn, output = "typst")

  return(x)
}

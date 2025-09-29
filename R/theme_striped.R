#' Striped theme with alternating row colors
#'
#' @param x A `tinytable` object.
#' @param ... Additional arguments (ignored).
#' @return A modified `tinytable` object.
#' @export
theme_striped <- function(x, ...) {
  x <- theme_latex(x, inner = "row{even}={bg=black!5!white}")

  # For Tabulator, use CSS rules to create striped rows
  x <- theme_html(
    x,
    tabulator_css_rule = "$TINYTABLE_ID .tabulator-row:nth-child(odd) .tabulator-cell { background-color: #ededed !important; }"
  )

  # For other HTML engines, use style_tt
  x <- style_tt(
    x,
    i = seq(1, nrow(x), by = 2),
    background = "#ededed")
  return(x)
}

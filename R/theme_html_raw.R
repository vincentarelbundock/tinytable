#' Internal HTML Raw theme
#' @param x A `tinytable` object.
#' @param i Row indices.
#' @param j Column indices.
#' @param class String. Bootstrap table class.
#' @param css Character vector. CSS style declarations.
#' @param css_rule String. Complete CSS rules.
#' @param ... Additional arguments.
#' @keywords internal
#' @noRd
theme_html_raw <- function(x, ...) {
  # Apply bootstrap styling first
  x <- theme_html_bootstrap(x)

  # Then strip bootstrap styling
  fn <- function(table) {
    tab <- table@table_string
    tab <- lines_drop(tab, regex = "<table class", position = "before")
    tab <- lines_drop(tab, regex = "<\\/table>", position = "after")
    table@table_string <- tab
    return(table)
  }
  x <- build_finalize(x, fn, output = c("html", "bootstrap"))

  return(x)
}

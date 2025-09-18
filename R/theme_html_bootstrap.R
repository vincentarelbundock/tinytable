#' Internal HTML Bootstrap theme
#' @param x A `tinytable` object.
#' @param i Row indices.
#' @param j Column indices.
#' @param class String. Bootstrap table class.
#' @param css Character vector. CSS style declarations.
#' @param css_rule String. Complete CSS rules.
#' @param ... Additional arguments.
#' @keywords internal
#' @noRd
theme_html_bootstrap <- function(x, i = NULL, j = NULL, class = NULL, css = NULL, css_rule = NULL, ...) {
  assert_string(class, null.ok = TRUE)
  assert_character(css, null.ok = TRUE)
  assert_string(css_rule, null.ok = TRUE)

  if (!is.null(class)) {
    x@bootstrap_class <- class
  } else if (length(x@bootstrap_class) == 0) {
    x@bootstrap_class <- "table"
  }

  if (!is.null(css_rule)) {
    x@bootstrap_css_rule <- css_rule
  }

  # this must still be handled by style_tt() because it is cell-specific
  if (!is.null(css)) {
    x <- style_tt(x, i, j, bootstrap_css = css)
  }

  return(x)
}

#' HTML-specific styles and options
#'
#' @param class String. Bootstrap table class such as `"table"`, `"table table-dark"` or `"table table-dark table-hover"`. See the bootstrap documentation.
#' @param css Character vector. CSS style declarations to be applied to every cell defined by `i` and `j` (ex: `"font-weight: bold"`).
#' @param css_rule String. Complete CSS rules (with curly braces, semicolon, etc.) that apply to the table class specified by the `bootstrap_class` argument.
#' @inheritParams style_tt
#'
#' @export
theme_bootstrap <- function(x, i = NULL, j = NULL, class = NULL, css = NULL, css_rule = NULL, ...) {
  assert_string(class, null.ok = TRUE)
  assert_character(css, null.ok = TRUE)
  assert_string(css_rule, null.ok = TRUE)

  if (!is.null(class)) {
    x@bootstrap_class <- class
  } else {
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

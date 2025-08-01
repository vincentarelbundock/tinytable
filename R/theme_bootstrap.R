theme_bootstrap <- function(x, class = NULL, css_rule = NULL, ...) {
  assert_string(class, null.ok = TRUE)
  assert_string(css_rule, null.ok = TRUE)
  if (!is.null(class)) {
    x@bootstrap_class <- class
  } else {
    x@bootstrap_class <- "table"
  }
  if (!is.null(css_rule)) {
    x@bootstrap_css_rule <- css_rule
  }
  return(x)
}

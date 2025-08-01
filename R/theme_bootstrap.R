theme_bootstrap <- function(
    x,
    i = NULL,
    j = NULL,
    bootstrap_class = get_option("tinytable_theme_bootstrap_class", default = "table"),
    bootstrap_css = get_option("tinytable_theme_bootstrap_css", default = NULL),
    bootstrap_css_rule = get_option("tinytable_theme_bootstrap_css_rule", default = NULL),
    ...) {
  # Extract bootstrap_css from ... (undocumented argument)
  dots <- list(...)

  # Validate arguments
  assert_string(bootstrap_class, null.ok = TRUE)
  assert_character(bootstrap_css, null.ok = TRUE)
  assert_string(bootstrap_css_rule, null.ok = TRUE)

  # Apply bootstrap CSS to cells if specified with i/j
  # need to go through style_tt() because this argument is cell-specific
  if (!is.null(bootstrap_css)) {
    table <- style_tt(table, i = i, j = j, bootstrap_css = bootstrap_css)
  }

  # Apply bootstrap class
  if (!is.null(bootstrap_class)) {
    table@bootstrap_class <- bootstrap_class
  }

  # Apply bootstrap CSS rule
  if (!is.null(bootstrap_css_rule)) {
    table@bootstrap_css_rule <- bootstrap_css_rule
  }

  return(x)
}

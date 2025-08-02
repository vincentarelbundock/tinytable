#' HTML-specific styles and options
#'
#' @param x A `tinytable` object.
#' @param engine Character string specifying the HTML engine: "bootstrap", "raw", or "tabulator".
#' @param i Row indices (shared across engines).
#' @param j Column indices (shared across engines).
#' @param class String. Bootstrap table class (shared across engines).
#' @param css Character vector. CSS style declarations (shared across engines).
#' @param css_rule String. Complete CSS rules (shared across engines).
#' @param tabulator_stylesheet Tabulator CSS theme (tabulator engine only).
#' @param tabulator_layout Table layout algorithm (tabulator engine only).
#' @param tabulator_pagination Pagination settings (tabulator engine only).
#' @param tabulator_search Enable/disable search functionality (tabulator engine only).
#' @param tabulator_options Custom Tabulator.js configuration options (tabulator engine only).
#' @param tabulator_css_rule Custom CSS rules (tabulator engine only).
#' @param tabulator_columns Custom column definitions (tabulator engine only).
#' @param ... Additional arguments.
#'
#' @export
theme_html <- function(x, 
                       engine = "bootstrap", 
                       i = NULL, 
                       j = NULL, 
                       class = NULL, 
                       css = NULL, 
                       css_rule = NULL,
                       tabulator_stylesheet = get_option("tinytable_theme_tabulator_stylesheet", default = "bootstrap5"),
                       tabulator_layout = get_option("tinytable_theme_tabulator_layout", default = "fitDataTable"),
                       tabulator_pagination = TRUE,
                       tabulator_search = TRUE,
                       tabulator_options = get_option("tinytable_theme_tabulator_options", default = NULL),
                       tabulator_css_rule = NULL,
                       tabulator_columns = NULL,
                       ...) {
  assert_choice(engine, c("bootstrap", "raw", "tabulator"))
  
  if (engine == "bootstrap") {
    x <- theme_html_bootstrap(x, i = i, j = j, class = class, css = css, css_rule = css_rule, ...)
  } else if (engine == "raw") {
    x <- theme_html_raw(x, i = i, j = j, class = class, css = css, css_rule = css_rule, ...)
  } else if (engine == "tabulator") {
    x <- theme_html_tabulator(x, 
                              tabulator_stylesheet = tabulator_stylesheet,
                              tabulator_layout = tabulator_layout,
                              tabulator_pagination = tabulator_pagination,
                              tabulator_search = tabulator_search,
                              tabulator_options = tabulator_options,
                              tabulator_css_rule = tabulator_css_rule,
                              tabulator_columns = tabulator_columns,
                              ...)
  }
  
  return(x)
}
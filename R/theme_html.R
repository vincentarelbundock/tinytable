#' HTML-specific styles and options
#' @param x A `tinytable` object.
#' @param engine Character string specifying the HTML engine: "tinytable", "bootstrap", or "tabulator".
#' @param i Row indices.
#' @param j Column indices.
#' @param class String. HTML table class.
#' @param css Character vector. CSS style declarations.
#' @param css_rule String. CSS rules to customize table styling. Can be one of:
#'   - Shortcut strings: "tinytable" (default) or "dark" for dark theme
#'   - A file path to a local CSS file (e.g., "custom.css" or "path/to/styles.css")
#'   - A URL to an external stylesheet (must start with http/https and end with .css)
#'   - Inline CSS content as a string
#' @param script String. Custom JavaScript code or script tags to inject into the HTML output. Useful for loading external JavaScript libraries like Plotly, D3, etc.
#' @param portable Logical. Sets whether to create portable HTML output with embedded Javascript, CSS, and base64-encoded images.
#' @param tabulator_stylesheet Character string. CSS stylesheet theme for Tabulator.js tables.
#'   Default is "bootstrap5". Available options: "default", "simple", "midnight", "modern",
#'   "site", "site_dark", "bootstrap3", "bootstrap4", "bootstrap5", "semanticui", "bulma",
#'   "materialize", or a custom HTTP URL starting with "http".
#' @param tabulator_layout Character string. Table layout algorithm for column sizing.
#'   Default is "fitDataTable". Available options: "fitDataTable", "fitData", "fitDataFill",
#'   "fitDataStretch", "fitColumns".
#' @param tabulator_pagination Logical or numeric vector. Pagination settings for large tables.
#'   - NULL (default): Preserves existing pagination settings, does not change previous configuration
#'   - FALSE: Explicitly disable pagination
#'   - TRUE: Enable pagination with automatic page sizes (10, 25, 50, 100, 250) filtered by row count
#'   - Numeric vector: First element is default page size, full vector provides page size options
#' @param tabulator_search Character or NULL. Search functionality position.
#'   - NULL (default): Preserves existing search settings, does not change previous configuration
#'   - "top": Adds search box above the table
#'   - "bottom": Adds search box below the table
#'   - "column": Adds header filters to each column for per-column searching
#' @param tabulator_options Custom Tabulator.js configuration options.
#' @param tabulator_css_rule Complete CSS rules.
#' @param tabulator_columns Custom column definitions.
#' @param ... Additional arguments are ignored.
#'
#' @examples
#' \dontrun{
#' # Load an external JavaScript library
#' library(tinytable)
#' x <- tt(mtcars[1:5, 1:5])
#' x <- theme_html(x, script = '<script src="https://cdn.plot.ly/plotly-2.27.0.min.js"></script>')
#'
#' # Add custom inline JavaScript
#' x <- tt(mtcars[1:5, 1:5])
#' x <- theme_html(x, script = '<script>console.log("Table loaded");</script>')
#' }
#'
#' @export
theme_html <- function(
    x,
    engine = get_option("tinytable_html_engine", default = NULL),
    i = NULL,
    j = NULL,
    class = get_option("tinytable_html_class", default = NULL),
    css = get_option("tinytable_html_css", default = NULL),
    css_rule = get_option("tinytable_html_css_rule", default = NULL),
    script = get_option("tinytable_html_script", default = NULL),
    portable = get_option("tinytable_html_portable"),
    tabulator_columns = get_option("tinytable_html_tabulator_columns"),
    tabulator_css_rule = get_option("tinytable_html_tabulator_css_rule"),
    tabulator_layout = get_option("tinytable_html_tabulator_layout", default = "fitDataTable"),
    tabulator_options = get_option("tinytable_html_tabulator_options"),
    tabulator_pagination = get_option("tinytable_html_tabulator_pagination"),
    tabulator_search = get_option("tinytable_html_tabulator_search"),
    tabulator_stylesheet = get_option("tinytable_html_tabulator_stylesheet"),
    ...) {
  assert_string(class, null.ok = TRUE)
  assert_choice(engine, c("tinytable", "bootstrap", "tabulator"), null.ok = TRUE)
  assert_choice(tabulator_search, c("top", "bottom", "column"), null.ok = TRUE)
  sanity_tabulator_css_rule(tabulator_css_rule)
  sanity_tabulator_columns(tabulator_columns)

  if (!is.null(engine)) {
    x@html_engine <- engine
    if (engine == "bootstrap" && is.null(class) && identical(x@html_class, "tinytable")) {
      x@html_class <- "table"
    }
  }

  if (!is.null(css_rule)) {
    assert_string(css_rule)
    # Handle shortcut strings
    if (css_rule == "tinytable") {
      css_rule <- "https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.css"
    } else if (css_rule == "dark") {
      css_rule <- "https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable_dark.css"
    }
    x@html_css_rule <- css_rule
  }

  if (!is.null(script)) {
    assert_string(script)
    x@html_script <- script
  }

  if (!is.null(class)) {
    x@html_class <- class
  }

  if (!is.null(portable)) {
    assert_flag(portable)
    x@html_portable <- portable
  }

  # Handle css parameter for styling
  if (!is.null(css)) {
    # Use style_tt with html_css parameter - let style_tt handle the expansion logic
    x <- style_tt(x, i = i, j = j, html_css = css)
  }

  x <- theme_html_tabulator(
    x,
    tabulator_stylesheet = tabulator_stylesheet,
    tabulator_layout = tabulator_layout,
    tabulator_pagination = tabulator_pagination,
    tabulator_search = tabulator_search,
    tabulator_options = tabulator_options,
    tabulator_css_rule = tabulator_css_rule,
    tabulator_columns = tabulator_columns,
    ...
  )

  return(x)
}

#' HTML-specific styles and options
#' @param x A `tinytable` object.
#' @param engine Character string specifying the HTML engine: "bootstrap", "raw", or "tabulator".
#' @param i Row indices.
#' @param j Column indices.
#' @param class String. Bootstrap table class.
#' @param css Character vector. CSS style declarations.
#' @param css_rule String. Complete CSS rules.
#' @param portable Logical. If not NULL, sets whether to create portable HTML output with base64-encoded images (bootstrap engine only).
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
#' @param tabulator_options Custom Tabulator.js configuration options.
#' @param tabulator_css_rule Complete CSS rules.
#' @param tabulator_columns Custom column definitions.
#' @param ... Additional arguments are ignored.
#'
#' @export
theme_html <- function(
    x,
    engine = get_option("tinytable_html_engine", default = "bootstrap"),
    i = NULL,
    j = NULL,
    class = get_option("tinytable_html_class", default = NULL),
    css = get_option("tinytable_html_css", default = NULL),
    css_rule = get_option("tinytable_html_css_rule", default = NULL),
    portable = get_option("tinytable_html_portable", default = NULL),
    tabulator_columns = get_option(
      "tinytable_html_tabulator_columns",
      default = NULL
    ),
    tabulator_css_rule = get_option(
      "tinytable_html_tabulator_css_rule",
      default = NULL
    ),
    tabulator_layout = get_option(
      "tinytable_html_tabulator_layout",
      default = "fitDataTable"
    ),
    tabulator_options = get_option(
      "tinytable_html_tabulator_options",
      default = NULL
    ),
    tabulator_pagination = get_option(
      "tinytable_html_tabulator_pagination",
      default = NULL
    ),
    tabulator_search = get_option(
      "tinytable_html_tabulator_search",
      default = NULL
    ),
    tabulator_stylesheet = get_option(
      "tinytable_html_tabulator_stylesheet",
      default = NULL
    ),
    ...) {
  assert_choice(engine, c("bootstrap", "raw", "tabulator"))
  assert_choice(tabulator_search, c("top", "bottom"), null.ok = TRUE)
  sanity_tabulator_css_rule(tabulator_css_rule)
  sanity_tabulator_columns(tabulator_columns)

  if (!is.null(portable)) {
    assert_flag(portable)
    if (isTRUE(portable)) {
      assert_dependency("base64enc")
    }
    x@html_portable <- portable
  }

  if (engine == "raw") {
    x <- theme_html_raw(
      x,
      i = i,
      j = j,
      class = class,
      css = css,
      css_rule = css_rule,
      ...
    )
    return(x)
  }

  x <- theme_html_bootstrap(
    x,
    i = i,
    j = j,
    class = class,
    css = css,
    css_rule = css_rule,
    ...
  )

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

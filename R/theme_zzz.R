theme_dictionary <- list(
  "default" = theme_default,
  "bootstrap" = theme_bootstrap,
  "grid" = theme_grid,
  "latex" = theme_latex,
  "placement" = theme_placement,
  "revealjs" = theme_revealjs,
  "resize" = theme_resize,
  "rotate" = theme_rotate,
  "striped" = theme_striped,
  "tabulator" = theme_tabulator,
  "void" = theme_void
)

#' Themes for `tinytable`
#'
#' @description
#' A theme is a function which applies a collection of transformations to a `tinytable` object. Whereas the other `tinytable` functions such as `format_tt()` and `style_tt()` aim to be output-agnostic, themes can be output-specific, only applying to LaTeX, HTML, or Typst, as needed.
#'
#' Each theme can have specific arguments, which are passed to the `theme_tt()` function. See the "Arguments" section below.
#'
#' @param x A `tinytable` object
#' @param theme String. Name of the theme to apply. One of:
#'   + "bootstrap": Similar appearance to the default Bootstrap theme in HTML
#'   + "grid": Vertical and horizontal rules around each cell.
#'   + "placement": Position of the table environment (LaTeX)
#'   + "revealjs": Tables optimized for Reveal.js presentations with light/dark theme support
#'   + "rotate": Rotate a LaTeX or Typst table.
#'   + "resize": Scale a LaTeX `tinytable` to fit the `width` argument.
#'   + "striped": Grey stripes on alternating rows
#'   + "tabulator": Customize Tabulator.js tables **Experimental**
#'   + "void": No rules
#' @param ... Additional arguments passed the themeing function. See the "Arguments" section below for a list of supported arguments for each theme.
#' @section Arguments:
#'
#' revealjs
#'
#' + `css`: String. "light" (default) or "dark" for light or dark theme colors.
#'   - `options("tinytable_theme_revealjs_css" = "dark")`
#' + `fontsize`: Numeric. Font size multiplier for table content.
#'   - `options("tinytable_theme_revealjs_fontsize" = 0.8)`
#' + `fontsize_caption`: Numeric. Font size multiplier for table captions.
#'   - `options("tinytable_theme_revealjs_fontsize_caption" = 1)`
#'
#' multipage
#'
#' + `rowhead`: Non-negative integer. The number of header rows to repeat on each page.
#'   - `options("tinytable_theme_multipage_rowhead" = 1L)`
#' + `rowfoot`: Non-negative integer. The number of footer rows to repeat on each page.
#'   - `options("tinytable_theme_multipage_rowfoot" = 1L)`
#'
#' placement
#'
#' + `horizontal` (Typst only): "l", "c", or "r" to align the table horizontally in the page.
#'    - `options("tinytable_theme_placement_horizontal" = "l")`
#' + `latex_float`: String to insert in square brackets after the LaTeX table environment, ex: "H", "htbp". The default value is controlled by a global option:
#'    - `options("tinytable_theme_placement_latex_float" = "H")`
#'
#' resize
#'
#' + `width`: A numeric value between 0.01 and 1, representing the proportion of the line width to use
#'   - `options("tinytable_theme_resize_width" = 0.9)`
#' + `direction`: "down", "up", "both" A string indicating if the table should be scaled in one direction. For example, "down" will only resize the table if it exceeds `\linewidth`
#'   - `options("tinytable_theme_resize_direction" = "down")`
#'
#' rotate
#'
#' + `angle`: Angle of the rotation. For example, `angle=90`` applies a half counter-clockwise turn.
#' + Caveats:
#'   - LaTeX and Typst only.
#'   - Typst: In Quarto documents, rotation does not work because Quarto takes over the figure environment.
#'   - LaTeX: In Quarto documents, captions must be specified using the `caption` argument in `tt()` rather than via Quarto chunk options.
#'
#' tabulator
#'
#' + `stylesheet`: String. Tabulator CSS theme. One of: "default", "simple", "midnight", "modern", "site", "site_dark", "bootstrap3", "bootstrap4", "bootstrap5", "semanticui", "bulma", "materialize".
#'   - `options("tinytable_theme_tabulator_stylesheet" = "midnight")`
#' + `layout`: String. Table layout algorithm. One of: "fitDataTable" (default), "fitData", "fitDataFill", "fitDataStretch", "fitColumns".
#'   - `options("tinytable_theme_tabulator_layout" = "fitColumns")`
#' + `pagination`: Logical or numeric vector. If TRUE, enables pagination with automatic page sizes. If FALSE, disables pagination. If numeric vector, first element is page size, full vector provides page size options.
#'   - `options("tinytable_theme_tabulator_pagination" = c(25, 50, 100))`
#' + `search`: Logical. Enable/disable search functionality across all columns.
#'   - `options("tinytable_theme_tabulator_search" = FALSE)`
#' + `options`: String or NULL. Custom Tabulator.js configuration options as JavaScript string. Overrides individual arguments when provided.
#'   - `options("tinytable_theme_tabulator_options" = "pagination: 'local', paginationSize: 50")`
#' + `css_rule`: String or NULL. Custom CSS rules that must include `$TINYTABLE_ID` placeholder for table-specific scoping.
#'   - Example: `"$TINYTABLE_ID .tabulator-header { background: black; }"`
#' + `columns`: String or NULL. Custom column definitions as JavaScript array string. Completely overrides default column configuration.
#'   - Example: `'[{"title": "Name", "field": "name", "formatter": "link"}]'`
#'
#' @examples
#' library(tinytable)
#'
#' x <- mtcars[1:4, 1:4]
#'
#' # equivalent calls
#' tt(x, theme = "striped")
#'
#' tt(x) |> theme_tt("striped")
#'
#' # resize w/ argument
#' x <- cbind(mtcars[1:10, ], mtcars[1:10, ])
#' tt(x) |>
#'   theme_tt("resize", width = .9) |>
#'   print("latex")
#'
#' @return A modified `tinytable` object
#' @export
theme_tt <- function(x, theme, ...) {
  assert_class(x, "tinytable")
  if (is.null(theme)) {
    return(x)
  }
  if (is.function(theme)) {
    return(theme(x, ...))
  }
  td <- theme_dictionary
  na <- unique(sort(names(td)))
  assert_choice(theme, na)
  if (identical(theme, "void")) {
    if (isTRUE(nrow(x@style$line) > 0)) {
      x@style$line <- NA
    }
  }
  fn <- td[[theme]]
  out <- list(list(fn, list(...)))
  x@lazy_theme <- c(x@lazy_theme, out)
  return(x)
}

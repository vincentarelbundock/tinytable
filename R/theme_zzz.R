theme_dictionary <- list(
  "default" = theme_default,
  "bootstrap" = theme_bootstrap,
  "grid" = theme_grid,
  "multipage" = theme_multipage,
  "placement" = theme_placement,
  "revealjs" = theme_revealjs,
  "resize" = theme_resize,
  "rotate" = theme_rotate,
  "spacing" = theme_spacing,
  "striped" = theme_striped,
  "tabular" = theme_tabular,
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
#'   + "multipage": Long tables continue on the next page (LaTeX only)
#'   + "placement": Position of the table environment (LaTeX)
#'   + "revealjs": Tables optimized for Reveal.js presentations with light/dark theme support
#'   + "rotate": Rotate a LaTeX or Typst table.
#'   + "resize": Scale a LaTeX `tinytable` to fit the `width` argument.
#'   + "spacing": Draw more compact or airy tables.
#'   + "striped": Grey stripes on alternating rows
#'   + "tabular": Remove table environment (LaTeX) or Javascript/CSS (HTML)
#'   + "void": No rules
#' @param ... Additional arguments passed the themeing function. See the "Arguments" section below for a list of supported arguments for each theme.
#' @section Arguments:
#'
#' revealjs
#'
#' + `css`: String. "light" (default) or "dark" for light or dark theme colors.
#'   - Set globally with `options("tinytable_theme_revealjs_css" = "dark")`
#' + `fontsize`: Numeric. Font size multiplier for table content.
#'   - Set globally with `options("tinytable_theme_revealjs_fontsize" = 0.8)`
#' + `fontsize_caption`: Numeric. Font size multiplier for table captions.
#'   - Set globally with `options("tinytable_theme_revealjs_fontsize_caption" = 1)`
#'
#' multipage
#'
#' + `rowhead`: Non-negative integer. The number of header rows to repeat on each page.
#'   - Set globally with `options("tinytable_theme_multipage_rowhead" = 1L)`
#' + `rowfoot`: Non-negative integer. The number of footer rows to repeat on each page.
#'   - Set globally with `options("tinytable_theme_multipage_rowfoot" = 1L)`
#'
#' tabular
#'
#' + `style`:
#'   - "tabular": Drop all LaTeX dependencies and floating environments, except `\\begin{tabular}`
#'   - "tabularray": Drop all LaTeX dependencies and floating environments, except `\\begin{tblr}`
#'   - Set globally with `options("tinytable_theme_tabular_style" = "tblr")`
#'
#' placement
#'
#' + `horizontal` (Typst only): "l", "c", or "r" to align the table horizontally in the page.
#'    - Set globally with `options("tinytable_theme_placement_horizontal" = "l")`
#' + `latex_float`: String to insert in square brackets after the LaTeX table environment, ex: "H", "htbp". The default value is controlled by a global option:
#'    - Set globally with `options("tinytable_theme_placement_latex_float" = "H")`
#'
#' resize
#'
#' + `width`: A numeric value between 0.01 and 1, representing the proportion of the line width to use
#'   - Set globally with `options("tinytable_theme_resize_width" = 0.9)`
#' + `direction`: "down", "up", "both" A string indicating if the table should be scaled in one direction. For example, "down" will only resize the table if it exceeds `\linewidth`
#'   - Set globally with `options("tinytable_theme_resize_direction" = "down")`
#'
#' rotate
#'
#' + `angle`: Angle of the rotation. For example, `angle=90`` applies a half counter-clockwise turn.
#' + Caveats:
#'   - LaTeX and Typst only.
#'   - Typst: In Quarto documents, rotation does not work because Quarto takes over the figure environment.
#'   - LaTeX: In Quarto documents, captions must be specified using the `caption` argument in `tt()` rather than via Quarto chunk options.
#'
#' spacing
#'
#' + `rowsep`: Row spacing
#' + `colsep`: Column spacing
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

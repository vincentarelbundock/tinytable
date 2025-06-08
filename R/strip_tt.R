#' Strip elements from a Tiny Table
#'
#' @param x A table object created by `tt()`.
#' @param format `TRUE` reverts `format_tt()`.
#' @param group `TRUE` reverts `group_tt()`.
#' @param style `TRUE` reverts `style_tt()`.
#' @param theme `TRUE` reverts `theme_tt()`.
#' @param notes `TRUE` reverts the effect of the `notes` argument from `tt()`.
#' @param caption `TRUE` reverts the effect of the `caption` argument from `tt()`.
#' @param width `TRUE` reverts the effect of the `width` argument from `tt()`.
#' @param bold `TRUE` reverts the effect of the `bold` argument from `style_tt()`.
#' @param italic `TRUE` reverts the effect of the `italic` argument from `style_tt()`.
#' @param monospace `TRUE` reverts the effect of the `monospace` argument from `style_tt()`.
#' @param underline `TRUE` reverts the effect of the `underline` argument from `style_tt()`.
#' @param strikeout `TRUE` reverts the effect of the `strikeout` argument from `style_tt()`.
#' @param color `TRUE` reverts the effect of the `color` argument from `style_tt()`.
#' @param background `TRUE` reverts the effect of the `background` argument from `style_tt()`.
#' @param fontsize `TRUE` reverts the effect of the `fontsize` argument from `style_tt()`.
#' @param align `TRUE` reverts the effect of the `align` argument from `style_tt()`.
#' @param alignv `TRUE` reverts the effect of the `alignv` argument from `style_tt()`.
#' @param colspan `TRUE` reverts the effect of the `colspan` argument from `style_tt()`.
#' @param rowspan `TRUE` reverts the effect of the `rowspan` argument from `style_tt()`.
#' @param indent `TRUE` reverts the effect of the `indent` argument from `style_tt()`.
#' @param line `TRUE` reverts the effect of the `line` argument from `style_tt()`.
#' @param bootstrap_class `TRUE` reverts the effect of the `bootstrap_class` argument from `style_tt()`.
#' @param bootstrap_css `TRUE` reverts the effect of the `bootstrap_css` argument from `style_tt()`.
#' @param bootstrap_css_rule `TRUE` reverts the effect of the `bootstrap_css_rule` argument from `style_tt()`.
#' @param tabularray_inner `TRUE` reverts the effect of the `tabularray_inner` argument from `style_tt()`.
#' @param tabularray_outer `TRUE` reverts the effect of the `tabularray_outer` argument from `style_tt()`.
#' @return An object of class `tt` representing the table with stripped styling.
#' @export
strip_tt <- function(
  x,
  style = FALSE,
  format = FALSE,
  theme = FALSE,
  notes = FALSE,
  caption = FALSE,
  group = FALSE,
  bold = FALSE,
  italic = FALSE,
  monospace = FALSE,
  underline = FALSE,
  strikeout = FALSE,
  color = FALSE,
  background = FALSE,
  fontsize = FALSE,
  align = FALSE,
  alignv = FALSE,
  colspan = FALSE,
  rowspan = FALSE,
  indent = FALSE,
  line = FALSE,
  bootstrap_class = FALSE,
  bootstrap_css = FALSE,
  bootstrap_css_rule = FALSE,
  tabularray_inner = FALSE,
  tabularray_outer = FALSE,
  width = FALSE
) {
  out <- x

  # Reset style data.frame and lazy style
  if (style) {
    out@style <- data.frame()
    out@lazy_style <- list()
  }

  # Reset format settings
  if (format) {
    out@lazy_format <- list()
  }

  # Reset theme settings
  if (theme) {
    out@theme <- list("default")
    out@lazy_theme <- list()
  }

  # Reset notes and caption styling
  if (notes) out@style_notes <- list()
  if (caption) out@style_caption <- list()

  # Reset group styling
  if (group) {
    out@group_index_i <- numeric()
    out@group_n_i <- 0
    out@group_n_j <- 0
    out@lazy_group <- list()
    out@nrow <- nrow(out@data)
  }

  # Reset class-level styling
  if (bootstrap_class) out@bootstrap_class <- character()
  if (bootstrap_css_rule) out@bootstrap_css_rule <- character()

  # Reset individual style elements
  if (nrow(out@style) > 0) {
    if (bold) out@style$bold <- FALSE
    if (italic) out@style$italic <- FALSE
    if (monospace) out@style$monospace <- FALSE
    if (underline) out@style$underline <- FALSE
    if (strikeout) out@style$strikeout <- FALSE
    if (color) out@style$color <- NA
    if (background) out@style$background <- NA
    if (fontsize) out@style$fontsize <- NA
    if (align) out@style$align <- NA
    if (alignv) out@style$alignv <- NA
    if (colspan) out@style$colspan <- NA
    if (rowspan) out@style$rowspan <- NA
    if (indent) out@style$indent <- NA
    if (line) {
      out@style$line <- NA
      out@style$line_color <- NA
      out@style$line_width <- NA
    }
    if (bootstrap_css) out@style$bootstrap_css <- NA
    if (tabularray_inner) out@style$tabularray_inner <- NA
    if (tabularray_outer) out@style$tabularray_outer <- NA
    if (width) out@style$width <- NA
  }

  return(out)
}

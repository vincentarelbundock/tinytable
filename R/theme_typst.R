#' Typst-specific styles and options
#'
#' @param x A `tinytable` object.
#' @param multipage Logical. When `TRUE`, emits a Typst show rule that allows figures to break across pages. When `FALSE` (default), tables are kept on a single page. When `NULL`, no show rule is emitted.
#' @param figure Logical, whether to wrap the table in a Typst figure environment and block.
#' @param align_figure Character string indicating horizontal alignment: "l", "c", or "r".
#'   Defaults to `get_option("tinytable_theme_placement_horizontal", NULL)`. When NULL, uses default center alignment.
#' @param ... Additional arguments.
#'
#' @export
theme_typst <- function(x,
                        multipage = get_option("tinytable_typst_multipage", default = FALSE),
                        figure = get_option("tinytable_typst_figure", default = TRUE),
                        align_figure = get_option("tinytable_typst_align_figure", NULL), ...) {
  assert_flag(multipage, null.ok = TRUE)
  assert_flag(figure)
  assert_choice(align_figure, c("l", "c", "r"), null.ok = TRUE)

  fn <- function(table) {
    tab <- table@table_string
    if (isTRUE(multipage)) {
      tab <- sub("breakable: false", "breakable: true", tab, fixed = TRUE)
    } else if (isFALSE(multipage)) {
      tab <- sub("breakable: true", "breakable: false", tab, fixed = TRUE)
    } else {
      tab <- lines_drop(tab, regex = "show figure.*breakable")
    }
    table@table_string <- tab
    return(table)
  }
  x <- build_finalize(x, fn, output = "typst")

  if (!figure) {
    fn <- function(table) {
      tab <- table@table_string
      tab <- lines_drop(tab, regex = "table\\(", position = "before")
      tab <- lines_drop(tab, regex = "\\/\\/ end table", position = "after")
      table@table_string <- tab
      return(table)
    }
    x <- build_finalize(x, fn, output = "typst")
  }

  # Handle align_figure functionality
  if (!is.null(align_figure)) {
    fn <- function(table) {
      tab <- table@table_string
      if (align_figure == "l") {
        tab <- sub("#align(center,", "#align(left,", tab, fixed = TRUE)
      } else if (align_figure == "r") {
        tab <- sub("#align(center,", "#align(right,", tab, fixed = TRUE)
      }
      table@table_string <- tab
      return(table)
    }
    x <- build_finalize(x, fn, output = "typst")
  }

  return(x)
}

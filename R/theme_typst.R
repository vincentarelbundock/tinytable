#' Typst-specific styles and options
#'
#' @param x A `tinytable` object.
#' @param figure Logical, whether to wrap the table in a Typst figure environment and block.
#' @param align_figure Character string indicating horizontal alignment: "l", "c", or "r".
#'   Defaults to `get_option("tinytable_theme_placement_horizontal", NULL)`. When NULL, uses default center alignment.
#' @param ... Additional arguments.
#'
#' @export
theme_typst <- function(x,
                        figure = get_option("tinytable_typst_figure", default = TRUE),
                        align_figure = get_option("tinytable_typst_align_figure", NULL), ...) {
  assert_flag(figure)
  assert_choice(align_figure, c("l", "c", "r"), null.ok = TRUE)

  if (!figure) {
    fn <- function(table) {
      tab <- table@table_string
      # Remove first 5 lines (show figure + figure start)
      tab <- lines_drop(tab, "^#show figure:", position = "equal")
      tab <- lines_drop(tab, "^#figure\\(", position = "equal")
      tab <- lines_drop(tab, "^  \\$TINYTABLE_TYPST_CAPTION", position = "equal")
      tab <- lines_drop(tab, "^  kind:", position = "equal")
      tab <- lines_drop(tab, "^  supplement:", position = "equal")
      # Remove the last line (end figure)
      tab <- lines_drop(tab, "^\\) // end figure$", position = "equal")
      # Remove block wrapper lines but keep the content
      tab <- lines_drop(tab, "block\\[.*start block", position = "equal")
      tab <- lines_drop(tab, "\\].*end block", position = "equal")
      # Remove align wrapper lines but keep the content
      tab <- lines_drop(tab, "^  #align\\(center, \\[$", position = "equal")
      tab <- lines_drop(tab, "^  \\]\\) // end align$", position = "equal")
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

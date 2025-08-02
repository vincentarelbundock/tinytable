#' Default theme for TinyTable
#'
#' @param x A tinytable object.
#' @param ... Additional arguments are ignored.
#' @return A modified `tinytable` object.
#' @export
theme_default <- function(x, ...) {
  # Placement for LaTeX and Typst
  x <- theme_latex(x, placement = get_option("tinytable_latex_placement", default = NULL))
  x <- theme_typst(x, align_figure = get_option("tinytable_typst_align_figure", "c"))

  x@bootstrap_class <- "table table-borderless"

  fn <- function(x) {
    col <- if (x@output == "typst") "black" else "#d3d8dc"

    # top border
    if (x@output %in% c("html", "bootstrap") && length(x@names) == 0) {
      # For HTML with no column names, apply border to the first data row
      x <- style_tt(
        x,
        i = 1,
        line = "t",
        line_color = col,
        line_width = 0.1
      )
    } else {
      # For other cases, use the standard header position
      x <- style_tt(
        x,
        i = -x@nhead + 1,
        line = "t",
        line_color = col,
        line_width = 0.1
      )
    }
    # mid
    if (length(x@names) > 0) {
      x <- style_tt(
        x,
        i = 0,
        line = "b",
        line_color = col,
        line_width = 0.05
      )
    }
    # bottom
    x <- style_tt(
      x,
      i = nrow(x),
      line = "b",
      line_color = col,
      line_width = 0.1
    )
  }

  x <- build_prepare(x, fn, output = c("html", "bootstrap", "typst"))
  return(x)
}

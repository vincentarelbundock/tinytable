#' Default theme for TinyTable
#'
#' @param x A tinytable object.
#' @param ... Additional arguments are ignored.
#' @return A modified `tinytable` object.
#' @export
theme_default <- function(x, ...) {
  # Apply placement functionality for LaTeX and Typst
  placement_latex <- get_option("tinytable_latex_placement", default = NULL)
  placement_typst <- get_option("tinytable_typst_align_figure", "c")

  if (!is.null(placement_latex)) {
    x <- theme_latex(x, placement = placement_latex)
  }
  if (!is.null(placement_typst)) {
    x <- theme_typst(x, align_figure = placement_typst)
  }

  # bootstrap class
  bc <- if (length(x@bootstrap_class) == 0) {
    "table table-borderless"
  } else {
    x@bootstrap_class
  }
  fn <- function(x) theme_html(x, class = bc)
  x <- theme_html(x, class = bc)

  if (isTRUE(x@output %in% c("html", "bootstrap", "typst"))) {
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
      x <- theme_html(x, class = bc)
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

  return(x)
}

#' Default theme for TinyTable
#'
#' @param x A tinytable object.
#' @param ... Additional arguments are ignored.
#' @return A modified `tinytable` object.
#' @export
theme_default <- function(x, ...) {
  # run this after rbind_body_groupi()
  # Placement for LaTeX and Typst
  x <- theme_latex(x, placement = get_option("tinytable_latex_placement", default = NULL))
  x <- theme_typst(x, align_figure = get_option("tinytable_typst_align_figure", "c"))
  col <- "black"
  # top
  x <- style_tt(
    x,
    i = -x@nhead + 1,
    line = "t",
    line_color = col,
    line_width = 0.10
  )
  # middle
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
    line_width = 0.10
  )

  return(x)
}

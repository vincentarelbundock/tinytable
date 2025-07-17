theme_default <- function(x, ...) {
  fn <- theme_placement_factory(
    horizontal = get_option("tinytable_theme_default_horizontal", "c"),
    latex_float = get_option(
      "tinytable_theme_placement_latex_float",
      default = NULL
    )
  )
  x <- style_tt(x, finalize = fn)

  if (isTRUE(x@output %in% c("html", "typst"))) {
    col <- if (x@output == "typst") "black" else "#d3d8dc"
    bc <- if (length(x@bootstrap_class) == 0) {
      "table table-borderless"
    } else {
      x@bootstrap_class
    }
    # top
    x <- style_tt(
      x,
      bootstrap_class = bc,
      i = -x@nhead + 1,
      line = "t",
      line_color = col,
      line_width = 0.1
    )
    # mid
    if (length(x@names) > 0) {
      x <- style_tt(
        x,
        bootstrap_class = bc,
        i = 0,
        line = "b",
        line_color = col,
        line_width = 0.05
      )
    }
    # bottom
    x <- style_tt(
      x,
      bootstrap_class = bc,
      i = nrow(x),
      line = "b",
      line_color = col,
      line_width = 0.1
    )
  }

  return(x)
}

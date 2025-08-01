theme_striped <- function(x, ...) {
  fn <- theme_placement_factory(
    horizontal = get_option("tinytable_theme_default_horizontal", "c"),
    latex_float = get_option(
      "tinytable_theme_placement_latex_float",
      default = NULL
    )
  )
  x <- style_tt(x, finalize = fn)

  x <- style_tt(
    x,
    tabularray_inner = "row{even}={bg=black!5!white}",
    bootstrap_class = "table table-striped",
    output = "latex"
  )

  x <- style_tt(
    x,
    i = seq(1, nrow(x), by = 2),
    background = "#ededed",
    output = "typst"
  )

  # theme_default
  if (isTRUE(x@output %in% c("html", "bootstrap", "tabulator", "typst"))) {
    x <- style_tt(
      x,
      i = nrow(x),
      line = "b",
      line_color = "#d3d8dc",
      line_width = 0.1
    )
    x <- style_tt(
      x,
      i = 0,
      line = "bt",
      line_color = "#d3d8dc",
      line_width = 0.1
    )
  }
  return(x)
}

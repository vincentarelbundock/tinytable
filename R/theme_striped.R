theme_striped <- function(x, ...) {
  x <- theme_drop_default(x)

  # now: all formats
  x <- theme_bootstrap(x, class = "table table-striped")
  x <- theme_latex(x, inner = "row{even}={bg=black!5!white}")
  x <- style_tt(
    x,
    i = seq(1, nrow(x), by = 2),
    background = "#ededed")

  # prepare
  fn <- function(x) {
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
    return(x)
  }
  x <- build_prepare(x, fn, output = c("html", "bootstrap", "typst", "grid"))

  # finalize
  fn <- theme_placement_factory(
    horizontal = get_option("tinytable_theme_default_horizontal", "c"),
    latex_float = get_option("tinytable_theme_placement_latex_float", default = NULL)
  )
  x <- build_finalize(x, fn, output = "latex")

  return(x)
}

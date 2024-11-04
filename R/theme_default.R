theme_default <- function(x, ...) {

  fn <- theme_placement_factory(
    horizontal = get_option("tinytable_theme_default_horizontal", "c"),
    latex_float = get_option("tinytable_theme_placement_latex_float", default = NULL))
  x <- style_tt(x, finalize = fn)

  if (isTRUE(x@output %in% c("html", "typst"))) {
    bc <- if (length(x@bootstrap_class) == 0) "table table-borderless" else x@bootstrap_class
    x <- style_tt(x, 
      bootstrap_class = bc,
      i = nrow(x) + x@ngroupi, 
      line = "b", 
      line_color = "#d3d8dc", 
      line_width = 0.1)
    x <- style_tt(x, 
      bootstrap_class = bc,
      i = 0, 
      line = "bt", 
      line_color = "#d3d8dc", 
      line_width = 0.1)
  }
  return(x)
}

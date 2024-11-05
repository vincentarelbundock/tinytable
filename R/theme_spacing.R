theme_spacing <- function(x,
                          rowsep = get_option("tinytable_theme_spacing_rowsep", 0.1), 
                          colsep = get_option("tinytable_theme_spacing_colsep", 0.5), 
                          ...) {

  # placement
  fn <- theme_placement_factory(
    horizontal = get_option("tinytable_theme_default_horizontal", "c"),
    latex_float = get_option("tinytable_theme_placement_latex_float", default = NULL))
  x <- style_tt(x, finalize = fn)

  # rules
  if (isTRUE(x@output %in% c("html", "typst"))) {
    bc <- if (length(x@bootstrap_class) == 0) "table table-borderless" else x@bootstrap_class
    x <- style_tt(x, 
      bootstrap_class = bc,
      i = nrow(x), 
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

  # spacing
  x <- style_tt(x,
    tabularray_inner = sprintf("rowsep={%sem}, colsep = {%sem}", rowsep, colsep))
  x <- style_tt(x, 
    tabularray_inner = sprintf("rowsep={%sem}, colsep = {%sem}", rowsep, colsep))
  x <- style_tt(x, j = seq_len(ncol(x) - 1), 
    bootstrap_css = sprintf("padding-right: %sem;", colsep))
  x <- style_tt(x, i = -5:(nrow(x) - 1), 
    bootstrap_css = sprintf("padding-bottom: %sem;", rowsep))

  return(x)
}

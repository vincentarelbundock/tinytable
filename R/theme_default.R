theme_default <- function(x, ...) {
    if (isTRUE(x@output %in% c("html", "typst"))) {
      x <- style_tt(x, 
          bootstrap_class = "table table-borderless",
          i = nrow(x), 
          line = "b", 
          line_color = "#d3d8dc", 
          line_width = 0.1)
      x <- style_tt(x, 
          bootstrap_class = "table table-borderless",
          i = 0, 
          line = "bt", 
          line_color = "#d3d8dc", 
          line_width = 0.1)
    }
    return(x)
}

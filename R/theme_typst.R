#' Typst-specific styles and options
#'
#' @param x A `tinytable` object.
#' @param figure Logical, whether to wrap the table in a Typst figure environment and block.
#' @param ... Additional arguments.
#'
#' @export
theme_typst <- function(x, figure = TRUE, ...) {
  assert_flag(figure)
  
  if (!figure) {
    fn <- function(table) {
      tab <- table@table_string
      tab <- lines_drop(tab, regex = "table\\(", position = "before")
      tab <- lines_drop(tab, regex = "\\/\\/ end table", position = "after")
      table@table_string <- tab
      return(table)
    }
    x <- build_finalize(x, fn, output = "typst")
  }
  
  return(x)
}
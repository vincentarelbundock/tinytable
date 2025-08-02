#' HTML-specific styles and options
#'
#' @param x A `tinytable` object.
#' @param engine Character string specifying the HTML engine. If "raw", strips bootstrap styling and returns a simple table.
#' @param ... Additional arguments.
#'
#' @export
theme_html <- function(x, engine = "bootstrap", ...) {
  assert_choice(engine, c("bootstrap", "raw"))
  
  if (engine == "raw") {
    fn <- function(table) {
      tab <- table@table_string
      tab <- lines_drop(tab, regex = "<table class", position = "before")
      tab <- lines_drop(tab, regex = "<\\/table>", position = "after")
      table@table_string <- tab
      return(table)
    }
    x <- build_finalize(x, fn, output = c("html", "bootstrap"))
  }
  
  return(x)
}
#' @export
style_cell <- function(x, i, j, latex, html) {
  UseMethod("style_cell", x)
}

#' @export
style_row <- function(x, i, color, background, bold, italic, latex, html) {
  UseMethod("style_row", x)
}

#' @export
style_column <- function(x, i, align, color, background, bold, italic, latex, html) {
  UseMethod("style_column", x)
}

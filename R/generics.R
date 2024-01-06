#' @export
style_cell <- function(x, ...) {
  UseMethod("style_cell", x)
}

#' @export
style_row <- function(x, ...) {
  UseMethod("style_row", x)
}

#' @export
style_column <- function(x, ...) {
  UseMethod("style_column", x)
}

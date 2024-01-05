#' @export
spec_cell <- function(x, ...) {
  UseMethod("spec_cell", x)
}

#' @export
spec_row <- function(x, ...) {
  UseMethod("spec_row", x)
}

#' @export
spec_column <- function(x, ...) {
  UseMethod("spec_column", x)
}

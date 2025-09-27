#' Theme for a void table
#'
#' This function calls styles and formatting applied to a `tinytable` object up to that point in the pipeline.
#' @inheritParams theme_default
#' @export
theme_empty <- function(x, ...) {
  x@lazy_format <- list()
  x@lazy_style <- list()
  x@lazy_prepare <- list()
  x@lazy_finalize <- list()
  return(x)
}

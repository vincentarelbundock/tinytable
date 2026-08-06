#' Theme for a void table
#'
#' This function cancels all styles and formatting applied to a `tinytable`
#' object up to that point in the pipeline. Images and inline plots inserted
#' with `plot_tt()` are deliberately preserved: they are table content rather
#' than styling.
#' @inheritParams theme_tinytable
#' @export
theme_empty <- function(x, ...) {
  x@lazy_format <- list()
  x@lazy_style <- list()
  x@lazy_prepare <- list()
  x@lazy_finalize <- list()
  # @lazy_plot is intentionally NOT cleared: plot_tt() insertions are content
  # (images, inline plots), not styling, and must survive theme_empty().
  return(x)
}

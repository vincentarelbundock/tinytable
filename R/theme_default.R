#' Deprecated: Use theme_tinytable() instead
#'
#' @description
#' `theme_default()` has been renamed to `theme_tinytable()`. This function is
#' deprecated and will be removed in a future version.
#'
#' @inheritParams theme_tinytable
#' @return A modified `tinytable` object.
#' @export
theme_default <- function(x, ...) {
  warning(
    "`theme_default()` is deprecated. Please use `theme_tinytable()` instead.",
    call. = FALSE
  )
  theme_tinytable(x, ...)
}

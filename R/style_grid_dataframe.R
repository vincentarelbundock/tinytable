#' tinytable S4 method
#'
#' @keywords internal
setMethod(
  f = "style_eval",
  signature = "tinytable_dataframe",
  definition = style_eval_grid
)

#' tinytable S4 method
#'
#' @keywords internal
setMethod(
  f = "group_eval",
  signature = "tinytable_dataframe",
  definition = function(x, i = NULL, j = NULL, k = NULL, indent = 1, ...) {
    x <- group_eval_k(x, k)
    return(x)
  }
)

#' tinytable S4 method
#'
#' @keywords internal
setMethod(
  f = "finalize",
  signature = "tinytable_dataframe",
  definition = identity
)

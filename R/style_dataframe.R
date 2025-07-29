#' tinytable S4 method
#'
#' @keywords internal
setMethod(
  f = "style_eval",
  signature = "tinytable_dataframe",
  definition = identity
)

#' tinytable S4 method
#'
#' @keywords internal
setMethod(
  f = "group_eval_j",
  signature = "tinytable_dataframe",
  definition = identity
)

#' tinytable S4 method
#'
#' @keywords internal

setMethod(
  f = "finalize",
  signature = "tinytable_dataframe",
  definition = identity
)

#' tinytable S4 method
#'
#' @keywords internal
setMethod(
  f = "group_eval_j",
  signature = "tinytable_tabulator",
  definition = function(x, i = NULL, j = NULL, ...) {
    # Column group headers are not implemented for the tabulator engine
    warning(
      "group_tt(j = ...) column groups are not supported by the tabulator engine and will be ignored.",
      call. = FALSE
    )
    return(x)
  }
)
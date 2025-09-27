#' tinytable S4 method
#'
#' @keywords internal
setMethod(
  f = "group_eval_j",
  signature = "tinytable_typst",
  definition = function(x, i = NULL, j = NULL, indent = 0, ...) {
    # Column grouping now uses generic system - no special processing needed
    return(x)
  }
)

# All Typst-specific column grouping functions removed
# Column groups now handled by generic system in typst_tt.R

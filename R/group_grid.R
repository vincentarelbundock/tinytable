#' tinytable S4 method
#'
#' @keywords internal
setMethod(
  f = "group_eval_j",
  signature = "tinytable_grid",
  definition = function(x, i = NULL, j = NULL, ...) {
    # Only handle column grouping - row insertions now use matrix insertion
    x <- group_grid_col(x, j)
    return(x)
  }
)

group_grid_col <- function(x, j, ...) {
  if (is.null(j)) {
    return(x)
  }
  tab <- x@table_string
  header <- empty_cells(j)
  cw <- x@width_cols
  cw <- sapply(header, function(k) sum(cw[k]) + length(cw[k]) - 1)
  header <- t(matrix(names(cw)))
  header <- tt_eval(header, cw)
  header <- strsplit(header, split = "\\n")[[1]]
  header <- header[header != "\\n"]
  header <- header[!header %in% c("\\n", "")]
  header <- header[2]
  z <- strsplit(tab, split = "\\n")[[1]]
  z <- z[!z %in% c("\\n", "")]
  z <- c(z[1], header, z)

  # missing cell at the end
  nc <- nchar(z)
  idx <- nchar(z) < max(nc)
  z[idx] <- paste0(z[idx], strrep(" ", max(nc) - nchar(z[idx]) - 1), "|")

  tab <- paste(z, collapse = "\n")

  x@table_string <- tab

  return(x)
}



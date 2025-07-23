#' tinytable S4 method
#'
#' @keywords internal
setMethod(
  f = "group_eval_j",
  signature = "tinytable_bootstrap",
  definition = function(x, i = NULL, j = NULL, indent = 1, ...) {
    # Only handle column grouping - row insertions now use matrix insertion
    if (!is.null(j)) {
      x <- group_bootstrap_col(x, j = j, ...)
    }
    return(x)
  }
)

group_bootstrap_col <- function(x, j, ihead, ...) {
  out <- x@table_string

  out <- strsplit(out, "\\n")[[1]]
  header <- NULL

  miss <- as.list(setdiff(seq_len(ncol(x)), unlist(j)))
  miss <- stats::setNames(miss, rep(" ", length(miss)))
  j <- c(j, miss)

  max_col <- sapply(j, max)
  idx <- order(max_col)
  j <- j[idx]
  jstring <- lapply(seq_along(names(j)), function(k) {
    sprintf(
      '<th scope="col" align="center" colspan=%s data-row="%d" data-col="%d">%s</th>',
      max(j[[k]]) - min(j[[k]]) + 1,
      ihead,
      k - 1, # 0-based indexing for data-col
      names(j)[k]
    )
  })
  jstring <- paste(unlist(jstring), collapse = "\n")
  jstring <- sprintf("<tr>\n%s\n</tr>", jstring)

  idx <- grep("<thead>", out, fixed = TRUE)[1]
  out <- c(out[seq_len(idx)], jstring, out[(idx + 1):length(out)])

  out <- paste(out, collapse = "\n")

  x@table_string <- out

  # Only apply styling if there are actual column headers (not just the inserted matrix)
  if (x@nhead > 1) {
    x <- style_tt(x, i = ihead, align = "c")

    # midrule on numbered spans (not full columns of body)
    jnames <- names(j)
    jnames <- seq_along(jnames)[trimws(jnames) != ""]
    x <- style_tt(
      x,
      i = ihead,
      j = jnames,
      line = "b",
      line_width = 0.05,
      line_color = "#d3d8dc"
    )
  }

  return(x)
}



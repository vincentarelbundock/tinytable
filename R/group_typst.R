#' tinytable S4 method
#'
#' @keywords internal
setMethod(
  f = "group_eval_j",
  signature = "tinytable_typst",
  definition = function(x, i = NULL, j = NULL, indent = 0, ...) {
    # Only handle column grouping - row insertions now use matrix insertion
    if (!is.null(j)) {
      x <- group_typst_col(x, j, ...)
    }
    return(x)
  }
)


group_typst_col <- function(x, j, ihead, ...) {
  out <- x@table_string
  miss <- as.list(setdiff(seq_len(ncol(x)), unlist(j)))
  miss <- stats::setNames(miss, rep(" ", length(miss)))
  j <- c(j, miss)
  max_col <- sapply(j, max)
  idx <- order(max_col)
  j <- j[idx]
  lab <- names(j)
  len <- lengths(j)
  col <- ifelse(
    trimws(lab) == "",
    sprintf("[%s],", lab),
    sprintf(
      "table.cell(stroke: (bottom: .05em + black), colspan: %s, align: center)[%s],",
      len,
      lab
    )
  )
  col <- paste(col, collapse = "")
  out <- lines_insert(out, col, "repeat: true", "after")
  if (!any(grepl("column-gutter", out))) {
    out <- lines_insert(
      out,
      "    column-gutter: 5pt,",
      "// tinytable table start",
      "after"
    )
  }

  x@table_string <- out

  return(x)
}


group_typst <- function(x, i = NULL, j = NULL, ...) {
  out <- x

  # if (!is.null(i)) {
  #   out <- group_typst_row(out, i)
  # }

  if (!is.null(j)) {
    out <- group_typst_col(out, j, ...)
  }

  return(out)
}


group_typst_col <- function(x, j, ihead, ...) {
  out <- x@table_string
  miss <- as.list(setdiff(seq_len(ncol(x)), unlist(j)))
  miss <- stats::setNames(miss, rep(" ", length(miss)))
  j <- c(j, miss)
  max_col <- sapply(j, max)
  idx <- order(max_col)
  j <- j[idx]
  lab <- names(j)
  len <- sapply(j, length)
  col <- sprintf("colspanx(%s, align: center)[%s],", len, lab)
  col <- paste(col, collapse = "")
  out <- typst_insert(out, col, type = "body")

  x@table_string <- out

  # midrule
  jrule <- lapply(names(j), function(n) if (trimws(n) != "") j[[n]])
  jrule <- Filter(function(k) !is.null(k), jrule)
  for (jr in jrule) {
    x <- style_typst(x, i = ihead, j = jr, line = "b", line_width = .05, midrule = TRUE)
  }

  return(x)
}

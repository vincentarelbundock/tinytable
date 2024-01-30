group_typst <- function(x, i = NULL, j = NULL, ...) {
  out <- x

  # if (!is.null(i)) {
  #   out <- group_typst_row(out, i)
  # }

  if (!is.null(j)) {
    out <- group_typst_col(out, j)
  }

  return(out)
}


group_typst_col <- function(x, j, ...) {
  m <- meta(x)
  out <- x
  miss <- as.list(setdiff(seq_len(m$ncols), unlist(j)))
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
  return(out)
}


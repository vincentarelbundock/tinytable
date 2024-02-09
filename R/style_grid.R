style_grid <- function(x,
                       i = NULL,
                       j = NULL,
                       bold = FALSE,
                       italic = FALSE,
                       monospace = FALSE,
                       underline = FALSE,
                       strikeout = FALSE,
                       rowspan = NULL,
                       colspan = NULL,
                       ...) {

  if (meta(x, "output") != "markdown") return(x)

  out <- x

  ival <- if (is.null(i)) seq_len(meta(x, "nrows")) else i
  jval <- if (is.null(j)) seq_len(meta(x, "ncols")) else j

  for (col in seq_along(out)) {
    out[[col]] <- as.character(out[[col]])
  }

  for (row in ival) {
    for (col in jval) {
      if (isTRUE(bold)) {
        out[row, col] <- sprintf("**%s**", out[row, col])
      }
      if (isTRUE(italic)) {
        out[row, col] <- sprintf("*%s*", out[row, col])
      }
      if (isTRUE(strikeout)) {
        out[row, col] <- sprintf("~~%s~~", out[row, col])
      }
    }
  }

  if (!is.null(rowspan) || !is.null(colspan)) {
    idx_row <- if (isTRUE(rowspan > 1)) i + seq_len(rowspan) - 1 else i
    idx_col <- if (isTRUE(colspan > 1)) j + seq_len(colspan) - 1 else j
    backup <- out[i, j]
    for (w in idx_row) {
      for (z in idx_col) {
        if (z <= meta(x, "ncols")) {
          out[w, z] <- ""
        }
      }
    }
    out[i, j] <- backup
  }

  attr(out, "tinytable_meta") <- meta(x)
  class(out) <- class(x)
  return(out)
}  


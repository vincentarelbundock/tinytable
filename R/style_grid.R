


#' tinytable S4 method
#' 
#' @keywords internal
style_grid_internal <- function(x,
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

  out <- x@table_dataframe

  # i is a logical matrix mask
  if (is.matrix(i) && is.logical(i) && nrow(i) == nrow(x) && ncol(i) == ncol(x)) {
    assert_null(j)
    settings <- which(i == TRUE, arr.ind = TRUE)
    settings <- stats::setNames(data.frame(settings), c("i", "j"))
  } else {
    jval <- sanitize_j(j, x)
    ival <- sanitize_i(i, x)
    settings <- expand.grid(i = ival, j = jval)
  }

  # we only format the body, not headers
  settings <- settings[settings$i > 0,]

  # Unlike other formats, Markdown inserts `group_tt()` row labels after styling. This aligns the `i` index to the full columns.
  gr <- x@lazy_group
  gr <- Filter(function(k) !is.null(k$i), gr)
  # do not style spanning row labels
  lab_idx <- drop(unlist(lapply(gr, function(k) k$i)))
  lab_idx <- lab_idx + cumsum(rep(1, length(lab_idx))) - 1
  settings <- settings[!settings$i %in% lab_idx,]
  for (g in gr) {
    for (lab in g$i) {
      settings$i[settings$i > lab - 1] <- settings$i[settings$i > lab - 1] - 1
    }
    lab_idx <- c(lab_idx, g$i)
  }

  for (col in seq_along(out)) {
    out[[col]] <- as.character(out[[col]])
  }

  for (idx in seq_len(nrow(settings))) {
    row <- settings[idx, "i"]
    col <- settings[idx, "j"]
    if (isTRUE(bold)) {
      out[row, col] <- sprintf("**%s**", out[row, col])
    }
    if (isTRUE(italic)) {
      out[row, col] <- sprintf("*%s*", out[row, col])
    }
    if (isTRUE(strikeout)) {
      out[row, col] <- sprintf("~~%s~~", out[row, col])
    }
    if (!is.null(rowspan) || !is.null(colspan)) {
      idx_row <- if (isTRUE(rowspan > 1)) row + seq_len(rowspan) - 1 else row
      idx_col <- if (isTRUE(colspan > 1)) col + seq_len(colspan) - 1 else col
      backup <- out[row, col]
      for (w in idx_row) {
        for (z in idx_col) {
          if (z <= x@ncol) {
            out[w, z] <- ""
          }
        }
      }
      out[row, col] <- backup
    }
  }

  x@table_dataframe <- out
  return(x)
}




#' tinytable S4 method
#' 
#' @keywords internal
setMethod(
          f = "style_eval",
          signature = "tinytable_grid",
          definition = style_grid_internal
)

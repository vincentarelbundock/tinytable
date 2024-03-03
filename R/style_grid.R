setMethod(
  f = "style_eval",
  signature = "tinytable_grid",
  definition = function(x,
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
    if (x@output != "markdown") {
      return(x)
    }

    out <- x@table_dataframe

    ival <- if (is.null(i)) seq_len(nrow(x)) else i
    jval <- if (is.null(j)) seq_len(ncol(x)) else j

    # Unlike other formats, Markdown inserts `group_tt()` row labels after styling. This aligns the `i` index to the full columns.
    gr <- x@lazy_group
    gr <- Filter(function(k) !is.null(k$i), gr)
    # do not style spanning row labels
    lab_idx <- drop(unlist(lapply(gr, function(k) k$i)))
    lab_idx <- lab_idx + cumsum(rep(1, length(lab_idx))) - 1
    ival <- setdiff(ival, lab_idx)
    for (g in gr) {
      for (lab in g$i) {
        ival[ival > lab - 1] <- ival[ival > lab - 1] - 1
      }
      lab_idx <- c(lab_idx, g$i)
    }

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
    }

    x@table_dataframe <- out
    return(x)
  })


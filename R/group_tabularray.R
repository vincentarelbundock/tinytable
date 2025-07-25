#' tinytable S4 method
#'
#' @keywords internal
setMethod(
  f = "group_eval_j",
  signature = "tinytable_tabularray",
  definition = function(x, i = NULL, j = NULL, indent = 1, ...) {
    # Only handle column grouping - row insertions now use matrix insertion
    x <- group_tabularray_col(x, j, ...)
    return(x)
  })

group_tabularray_col <- function(x, j, ihead, ...) {
  # Process each group row (excluding the bottom row which is column names)
  if (nrow(x@data_group_j) > 1) {
    out <- strsplit(x@table_string, split = "\\n")[[1]]

    # Process each group row from top to bottom
    for (row_idx in 1:(nrow(x@data_group_j) - 1)) {
      group_row <- x@data_group_j[row_idx, ]

      # Build header row
      header <- rep("", ncol(x))
      cmidrules <- character(0)

      # Find consecutive spans of the same group label
      i <- 1
      while (i <= length(group_row)) {
        current_label <- group_row[i]
        span_start <- i

        # Find the end of this span
        while (i <= length(group_row) && group_row[i] == current_label) {
          i <- i + 1
        }
        span_end <- i - 1

        # Only add non-empty labels
        if (trimws(current_label) != "") {
          header[span_start] <- current_label
          # Add cmidrule for this span
          cmidrules <- c(cmidrules, sprintf("\\cmidrule[lr]{%s-%s}", span_start, span_end))
        }
      }

      header_line <- paste(header, collapse = " & ")
      header_line <- paste(header_line, "\\\\", paste(cmidrules, collapse = ""))

      # Insert the header line
      idx <- max(
        c(
          grep("% tabularray inner close", out),
          grep("\\toprule", out, fixed = TRUE)
        )
      )

      out <- c(
        out[1:idx],
        trimws(header_line),
        out[(idx + 1):length(out)]
      )
    }

    out <- paste(out, collapse = "\n")
    x@table_string <- out

    # Apply styling for each group row
    for (row_idx in 1:(nrow(x@data_group_j) - 1)) {
      group_row <- x@data_group_j[row_idx, ]

      # Find consecutive spans and apply styling
      i <- 1
      while (i <= length(group_row)) {
        current_label <- group_row[i]
        span_start <- i

        # Find the end of this span
        while (i <= length(group_row) && group_row[i] == current_label) {
          i <- i + 1
        }
        span_end <- i - 1
        span_length <- span_end - span_start + 1

        # Only style non-empty labels
        if (trimws(current_label) != "") {
          cs <- if (span_length == 1) NULL else span_length
          args <- list(
            tt_build_now = TRUE,
            x = x,
            i = ihead,
            j = span_start,
            align = "c",
            colspan = cs
          )
          x <- do.call(style_tt, args)
        }
      }
    }
  }

  return(x)
}

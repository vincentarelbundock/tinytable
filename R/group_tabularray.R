#' tinytable S4 method
#'
#' @keywords internal
setMethod(
  f = "group_eval_j",
  signature = "tinytable_tabularray",
  definition = function(x, i = NULL, j = NULL, ...) {
    # Only handle column grouping - row insertions now use matrix insertion
    x <- group_tabularray_col(x, j, ...)
    return(x)
  })

group_tabularray_col <- function(x, j, ihead, ...) {
  # Process column groups from @data_group_j
  if (nrow(x@data_group_j) > 0) {
    out <- strsplit(x@table_string, split = "\\n")[[1]]

    # Process each header row from bottom to top (reverse order) to match expected header order
    for (row_idx in nrow(x@data_group_j):1) {
      group_row <- as.character(x@data_group_j[row_idx, ])

      # Build header row
      header <- rep("", ncol(x))
      cmidrules <- character(0)

      # Find consecutive spans - treat "" as continuation, NA as ungrouped
      i <- 1
      while (i <= length(group_row)) {
        current_label <- group_row[i]
        span_start <- i

        # Skip NA (ungrouped) columns
        if (is.na(current_label)) {
          i <- i + 1
          next
        }

        # Find the end of this span
        # Only continue while we see empty strings - stop at any non-empty label (even if same)
        if (trimws(current_label) != "") {
          i <- i + 1 # Move past the current label
          # Continue only through empty strings
          while (i <= length(group_row) &&
            !is.na(group_row[i]) &&
            trimws(group_row[i]) == "") {
            i <- i + 1
          }
        } else {
          while (i <= length(group_row) &&
            !is.na(group_row[i]) &&
            trimws(group_row[i]) == "") {
            i <- i + 1
          }
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

    # Apply styling for each header row (reverse order to match header insertion)
    for (row_idx in nrow(x@data_group_j):1) {
      group_row <- as.character(x@data_group_j[row_idx, ])
      # Calculate the correct ihead for this specific row
      # The styling order should match the header insertion order
      header_position <- nrow(x@data_group_j) - row_idx + 1
      row_ihead <- ihead - (header_position - 1)

      # Find consecutive spans and apply styling
      i <- 1
      while (i <= length(group_row)) {
        current_label <- group_row[i]
        span_start <- i

        # Skip NA (ungrouped) columns
        if (is.na(current_label)) {
          i <- i + 1
          next
        }

        # Find the end of this span (same logic as header creation)
        if (trimws(current_label) != "") {
          i <- i + 1 # Move past the current label
          # Continue only through empty strings
          while (i <= length(group_row) &&
            !is.na(group_row[i]) &&
            trimws(group_row[i]) == "") {
            i <- i + 1
          }
        } else {
          while (i <= length(group_row) &&
            !is.na(group_row[i]) &&
            trimws(group_row[i]) == "") {
            i <- i + 1
          }
        }
        span_end <- i - 1
        span_length <- span_end - span_start + 1

        # Only style non-empty labels
        if (trimws(current_label) != "") {
          cs <- if (span_length == 1) NULL else span_length
          args <- list(
            tt_build_now = TRUE,
            x = x,
            i = row_ihead,
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

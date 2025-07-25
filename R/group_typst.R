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
  })


group_typst_col <- function(x, j, ihead, ...) {
  out <- x@table_string

  # Process column groups from @data_group_j
  if (nrow(x@data_group_j) > 0) {
    all_header_rows <- character(0)

    for (row_idx in 1:nrow(x@data_group_j)) {
      group_row <- as.character(x@data_group_j[row_idx, ])

      # Find consecutive spans of the same group label
      col_specs <- character(0)
      i <- 1
      while (i <= length(group_row)) {
        current_label <- group_row[i]
        span_start <- i

        # Skip NA (ungrouped) columns
        if (is.na(current_label)) {
          col_specs <- c(col_specs, sprintf("[ ],"))
          i <- i + 1
          next
        }

        # Find the end of this span
        # For non-empty labels, include following empty strings as continuation
        if (trimws(current_label) != "") {
          i <- i + 1 # Move past the current label
          # Continue through empty strings
          while (i <= length(group_row) &&
            !is.na(group_row[i]) &&
            trimws(group_row[i]) == "") {
            i <- i + 1
          }
        } else {
          # For empty labels, just move to next
          while (i <= length(group_row) &&
            !is.na(group_row[i]) &&
            trimws(group_row[i]) == "") {
            i <- i + 1
          }
        }
        span_end <- i - 1
        span_length <- span_end - span_start + 1

        if (trimws(current_label) == "") {
          col_specs <- c(col_specs, sprintf("[ ],"))
        } else {
          col_specs <- c(col_specs, sprintf(
            "table.cell(stroke: (bottom: .05em + black), colspan: %s, align: center)[%s],",
            span_length,
            current_label
          ))
        }
      }

      col <- paste(col_specs, collapse = "")
      all_header_rows <- c(all_header_rows, col)
    }

    # Insert all header rows at once
    if (length(all_header_rows) > 0) {
      all_headers <- paste(all_header_rows, collapse = "\n")
      out <- lines_insert(out, all_headers, "repeat: true", "after")
    }
  }

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

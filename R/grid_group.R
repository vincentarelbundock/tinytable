group_grid_col <- function(x, j, ...) {
  # Check if there are any column groups to process
  if (nrow(x@group_data_j) == 0) {
    return(x)
  }

  tab <- x@table_string
  cw <- x@width_cols

  # Process @group_data_j to create column group headers
  # Process all rows of column groups (not just the first)
  if (nrow(x@group_data_j) > 0) {
    all_header_lines <- list()

    # Process each row in @group_data_j (from last to first to maintain proper order)
    for (group_row_idx in nrow(x@group_data_j):1) {
      group_row <- as.character(x@group_data_j[group_row_idx, ])

      spans <- parse_group_spans(group_row)

      if (nrow(spans) > 0) {
        # Positional cells: labelled spans plus " "-labelled runs filling the
        # holes up to the last spanned column (duplicate labels preserved)
        runs <- Map(seq.int, spans$start, spans$end)
        labels <- spans$label
        missing_nums <- setdiff(seq_len(max(spans$end)), unlist(runs))
        if (length(missing_nums) > 0) {
          holes <- split(missing_nums, cumsum(c(1, diff(missing_nums) != 1)))
          runs <- c(runs, unname(holes))
          labels <- c(labels, rep(" ", length(holes)))
        }
        idx <- order(vapply(runs, min, numeric(1)))
        runs <- runs[idx]
        labels <- labels[idx]

        cw_grouped <- sapply(runs, function(k) sum(cw[k]) + length(cw[k]) - 1)
        header_matrix <- t(matrix(labels))
        header_formatted <- build_eval(header_matrix, cw_grouped)
        header_lines <- strsplit(header_formatted, split = "\\n")[[1]]
        header_lines <- header_lines[header_lines != ""]
        header_line <- header_lines[2]

        # Store header line for this group row
        all_header_lines[[group_row_idx]] <- header_line
      }
    }

    # Insert all header lines into the table
    if (length(all_header_lines) > 0) {
      z <- strsplit(tab, split = "\\n")[[1]]
      z <- z[z != ""]

      # Determine insertion position based on grid_hline setting
      header_lines_to_insert <- unlist(all_header_lines)

      if (isTRUE(x@grid_hline)) {
        # With hlines: insert after the first line (top border)
        z <- c(z[1], header_lines_to_insert, z[2:length(z)])
      } else {
        # Without hlines: insert at the very beginning (before column names)
        z <- c(header_lines_to_insert, z)
      }

      # missing cell at the end
      nc <- ansi_nchar(z)
      idx <- ansi_nchar(z) < max(nc)
      z[idx] <- paste0(
        z[idx],
        strrep(" ", max(nc) - ansi_nchar(z[idx]) - 1),
        "|"
      )

      tab <- paste(z, collapse = "\n")
      x@table_string <- tab
    }
  }

  return(x)
}


#' tinytable S4 method
#'
#' @keywords internal
setMethod(
  f = "group_eval_j",
  signature = "tinytable_dataframe",
  definition = function(x, i = NULL, j = NULL, ...) {
    # Only handle column grouping - row insertions now use matrix insertion
    x <- group_grid_col(x, j)
    return(x)
  }
)

#' tinytable S4 method
#'
#' @keywords internal
setMethod(
  f = "group_eval_j",
  signature = "tinytable_grid",
  definition = function(x, i = NULL, j = NULL, ...) {
    # Only handle column grouping - row insertions now use matrix insertion
    x <- group_grid_col(x, j)
    return(x)
  }
)

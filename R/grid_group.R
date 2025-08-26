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

      # Convert to the old format that empty_cells expects
      j_list <- list()
      i <- 1
      while (i <= length(group_row)) {
        current_label <- group_row[i]

        # Skip NA (ungrouped) columns
        if (is.na(current_label)) {
          i <- i + 1
          next
        }

        span_start <- i

        # Find the end of this span
        if (trimws(current_label) != "") {
          i <- i + 1 # Move past the current label
          # Continue through empty strings (continuation of span)
          while (
            i <= length(group_row) &&
              !is.na(group_row[i]) &&
              trimws(group_row[i]) == ""
          ) {
            i <- i + 1
          }
          span_end <- i - 1

          # Add to j_list if non-empty label
          j_list[[current_label]] <- span_start:span_end
        } else {
          i <- i + 1
        }
      }

      if (length(j_list) > 0) {
        header <- empty_cells(j_list)
        cw_grouped <- sapply(header, function(k) sum(cw[k]) + length(cw[k]) - 1)
        header_matrix <- t(matrix(names(cw_grouped)))
        header_formatted <- build_eval(header_matrix, cw_grouped)
        header_lines <- strsplit(header_formatted, split = "\\n")[[1]]
        header_lines <- header_lines[header_lines != "\\n"]
        header_lines <- header_lines[!header_lines %in% c("\\n", "")]
        header_line <- header_lines[2]

        # Store header line for this group row
        all_header_lines[[group_row_idx]] <- header_line
      }
    }

    # Insert all header lines into the table
    if (length(all_header_lines) > 0) {
      z <- strsplit(tab, split = "\\n")[[1]]
      z <- z[!z %in% c("\\n", "")]

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
      nc <- ansi_aware_nchar(z)
      idx <- ansi_aware_nchar(z) < max(nc)
      z[idx] <- paste0(z[idx], strrep(" ", max(nc) - ansi_aware_nchar(z[idx]) - 1), "|")

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
  })

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
  })

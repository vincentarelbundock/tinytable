#' tinytable S4 method
#'
#' @keywords internal
setMethod(
  f = "group_eval_j",
  signature = "tinytable_bootstrap",
  definition = function(x, i = NULL, j = NULL, indent = 1, ...) {
    # Only handle column grouping - row insertions now use matrix insertion
    if (!is.null(j)) {
      x <- group_bootstrap_col(x, j = j, ...)
    }
    return(x)
  }
)

group_bootstrap_col <- function(x, j, ihead, ...) {
  # Check if there are any column groups to process
  if (nrow(x@data_group_j) == 0) {
    return(x)
  }

  out <- x@table_string
  out <- strsplit(out, "\\n")[[1]]

  # Process column groups from @data_group_j (process all rows, not just the first)
  if (nrow(x@data_group_j) > 0) {
    all_group_rows <- list()
    all_styling_tasks <- list()
    
    # Process each row in @data_group_j separately (from last to first to maintain proper order)
    for (group_row_idx in nrow(x@data_group_j):1) {
      group_row <- as.character(x@data_group_j[group_row_idx, ])
      # Calculate the appropriate ihead for this group row: 
      # Row 1 in @data_group_j (most recent group_tt call) should get -1
      # Row 2 in @data_group_j (earlier group_tt call) should get -2, etc.
      current_ihead <- ihead - (nrow(x@data_group_j) - group_row_idx)
      
      # Convert to the old format that the bootstrap code expects
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
          i <- i + 1  # Move past the current label
          # Continue through empty strings (continuation of span)
          while (i <= length(group_row) && 
                 !is.na(group_row[i]) &&
                 trimws(group_row[i]) == "") {
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
        # Add missing columns as empty groups
        miss <- as.list(setdiff(seq_len(ncol(x)), unlist(j_list)))
        miss <- stats::setNames(miss, rep(" ", length(miss)))
        j_combined <- c(j_list, miss)

        max_col <- sapply(j_combined, max)
        idx <- order(max_col)
        j_combined <- j_combined[idx]
        
        jstring <- lapply(seq_along(names(j_combined)), function(k) {
          sprintf(
            '<th scope="col" align="center" colspan=%s data-row="%d" data-col="%d">%s</th>',
            max(j_combined[[k]]) - min(j_combined[[k]]) + 1,
            current_ihead,
            k - 1, # 0-based indexing for data-col
            names(j_combined)[k]
          )
        })
        jstring <- paste(unlist(jstring), collapse = "\n")
        jstring <- sprintf("<tr>\n%s\n</tr>", jstring)
        
        # Store the group row HTML for later insertion
        all_group_rows[[group_row_idx]] <- jstring

        # Store styling tasks for later application
        if (x@nhead > 1) {
          all_styling_tasks[[length(all_styling_tasks) + 1]] <- list(
            ihead = current_ihead,
            j_combined = j_combined
          )
        }
      }
    }
    
    # Insert all group rows at once
    if (length(all_group_rows) > 0) {
      all_jstrings <- paste(all_group_rows, collapse = "\n")
      idx <- grep("<thead>", out, fixed = TRUE)[1]
      out <- c(out[seq_len(idx)], all_jstrings, out[(idx + 1):length(out)])
      out <- paste(out, collapse = "\n")
      x@table_string <- out
      
      # Apply all styling
      for (task in all_styling_tasks) {
        x <- style_tt(x, i = task$ihead, align = "c")
        
        # midrule on numbered spans (not full columns of body)
        jnames <- names(task$j_combined)
        jnames <- seq_along(jnames)[trimws(jnames) != ""]
        if (length(jnames) > 0) {
          x <- style_tt(
            x,
            i = task$ihead,
            j = jnames,
            line = "b",
            line_width = 0.05,
            line_color = "#d3d8dc"
          )
        }
      }
    }
  }

  return(x)
}



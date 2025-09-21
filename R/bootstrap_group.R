#' tinytable S4 method
#'
#' @keywords internal
setMethod(
  f = "group_eval_j",
  signature = "tinytable_bootstrap",
  definition = function(x, i = NULL, j = NULL, ...) {
    # Only handle column grouping - row insertions now use matrix insertion
    if (!is.null(j)) {
      x <- bootstrap_groupj(x, j = j, ...)
    }
    return(x)
  })

bootstrap_groupj <- function(x, j, ihead, ...) {
  # Check if there are any column groups to process
  if (nrow(x@group_data_j) == 0) {
    return(x)
  }

  all_groupj_rows <- list()
  all_styling_tasks <- list()

  # Process each row in @group_data_j separately (from last to first to maintain proper order)
  for (groupj_idx in nrow(x@group_data_j):1) {
    groupj <- as.character(x@group_data_j[groupj_idx, ])

    # Calculate the appropriate ihead for this group row
    current_ihead <- ihead - (nrow(x@group_data_j) - groupj_idx)

    # Convert group row to column spans
    j_list <- bootstrap_groupj_span(groupj)

    if (length(j_list) > 0) {
      # Create HTML for this group row
      group_html <- bootstrap_groupj_html(x, j_list, current_ihead)
      all_groupj_rows[[groupj_idx]] <- group_html

      # Store styling tasks for later application (maintain original order)
      if (x@nhead > 1) {
        # Create j_combined for styling (same as in groupj_html_html)
        miss <- as.list(setdiff(seq_len(ncol(x)), unlist(j_list)))
        miss <- stats::setNames(miss, rep(" ", length(miss)))
        j_combined <- c(j_list, miss)
        max_col <- sapply(j_combined, max)
        idx <- order(max_col)
        j_combined <- j_combined[idx]

        all_styling_tasks[[length(all_styling_tasks) + 1]] <- list(
          ihead = current_ihead,
          j_combined = j_combined
        )
      }
    }
  }

  if (length(all_groupj_rows) > 0) {
    x <- bootstrap_groupj_insert(x, all_groupj_rows)
  }

  return(x)
}


# Helper function to parse a group row into column spans
bootstrap_groupj_span <- function(groupj) {
  j_list <- list()
  i <- 1

  while (i <= length(groupj)) {
    current_label <- groupj[i]

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
        i <= length(groupj) &&
          !is.na(groupj[i]) &&
          trimws(groupj[i]) == ""
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

  j_list
}

# Helper function to create HTML for a group row
bootstrap_groupj_html <- function(x, j_list, ihead) {
  # Add missing columns as empty groups
  miss <- as.list(setdiff(seq_len(ncol(x)), unlist(j_list)))
  miss <- stats::setNames(miss, rep(" ", length(miss)))
  j_combined <- c(j_list, miss)

  # Sort by column position
  max_col <- sapply(j_combined, max)
  idx <- order(max_col)
  j_combined <- j_combined[idx]

  # Generate HTML for each group
  jstring <- lapply(seq_along(names(j_combined)), function(k) {
    colspan_val <- max(j_combined[[k]]) - min(j_combined[[k]]) + 1

    # Calculate width style if x@width has multiple values (individual column widths)
    width_style <- ""
    if (length(x@width) > 1 && colspan_val > 1) {
      # Sum the widths of the columns this header spans
      spanned_cols <- j_combined[[k]]
      total_width <- sum(x@width[spanned_cols]) / sum(x@width) * 100
      width_style <- sprintf(' style="width: %s%%;"', round(total_width, 2))
    }

    sprintf(
      '<th scope="col" align="center" colspan=%s data-row="%d" data-col="%d"%s>%s</th>',
      colspan_val,
      ihead,
      k - 1, # 0-based indexing for data-col
      width_style,
      names(j_combined)[k]
    )
  })

  jstring <- paste(unlist(jstring), collapse = "\n")
  sprintf("<tr>\n%s\n</tr>", jstring)
}

# Helper function to insert group rows into the table
bootstrap_groupj_insert <- function(x, groupj_rows) {
  out <- strsplit(x@table_string, "\\n")[[1]]

  all_jstrings <- paste(groupj_rows, collapse = "\n")
  idx <- grep("<thead>", out, fixed = TRUE)[1]
  out <- c(out[seq_len(idx)], all_jstrings, out[(idx + 1):length(out)])
  out <- paste(out, collapse = "\n")

  x@table_string <- out
  x
}

# Helper function to apply styling to all groups
bootstrap_groupj_style <- function(x, styling_tasks) {
  # Apply styling in the same order as the original code
  for (task in styling_tasks) {
    x <- style_tt(x, i = task$ihead, align = "c")

    # Add midrule on numbered spans (not full columns of body)
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

  x
}

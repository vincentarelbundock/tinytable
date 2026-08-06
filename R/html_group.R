#' tinytable S4 method
#'
#' @keywords internal
setMethod(
  f = "group_eval_j",
  signature = "tinytable_html",
  definition = function(x, i = NULL, j = NULL, ihead = NULL, ...) {
    # Only handle column grouping - row insertions now use matrix insertion
    if (!is.null(j)) {
      x <- html_groupj(x, j = j, ihead = ihead, ...)
    }
    return(x)
  })

html_groupj <- function(x, j, ihead, ...) {
  # Check if there are any column groups to process
  if (nrow(x@group_data_j) == 0) {
    return(x)
  }

  all_groupj_rows <- list()

  # Process each row in @group_data_j separately (from last to first to maintain proper order)
  for (groupj_idx in nrow(x@group_data_j):1) {
    groupj <- as.character(x@group_data_j[groupj_idx, ])

    # Calculate the appropriate ihead for this group row
    current_ihead <- ihead - (nrow(x@group_data_j) - groupj_idx)

    # Convert group row to column spans
    spans <- parse_group_spans(groupj)

    if (nrow(spans) > 0) {
      # Create HTML for this group row
      group_html <- html_groupj_html(x, spans, current_ihead)
      all_groupj_rows[[groupj_idx]] <- group_html
    }
  }

  if (length(all_groupj_rows) > 0) {
    x <- html_groupj_insert(x, all_groupj_rows)
  }

  return(x)
}


# Helper function to create HTML for a group row
# `spans` is the data.frame returned by parse_group_spans(): label/start/end,
# positional so duplicate labels are preserved.
html_groupj_html <- function(x, spans, ihead) {
  # Add missing columns as empty single-column groups
  covered <- unlist(Map(seq.int, spans$start, spans$end))
  miss <- setdiff(seq_len(ncol(x)), covered)
  if (length(miss) > 0) {
    spans <- rbind(
      spans,
      data.frame(label = " ", start = miss, end = miss, stringsAsFactors = FALSE)
    )
  }

  # Sort by column position
  spans <- spans[order(spans$end), , drop = FALSE]

  # Generate HTML for each group
  jstring <- lapply(seq_len(nrow(spans)), function(k) {
    colspan_val <- spans$end[k] - spans$start[k] + 1

    # Calculate width style if x@width has multiple values (individual column widths)
    width_style <- ""
    if (length(x@width) > 1 && colspan_val > 1) {
      # Sum the widths of the columns this header spans
      spanned_cols <- spans$start[k]:spans$end[k]
      total_width <- sum(x@width[spanned_cols]) / sum(x@width) * 100
      width_style <- sprintf(' style="width: %s;"', format_markup_unit(round(total_width, 2), "%"))
    }

    sprintf(
      '<th scope="col" align="center" colspan=%s data-row="%d" data-col="%d"%s>%s</th>',
      colspan_val,
      ihead,
      spans$start[k], # Use the first column of the span, not the loop index
      width_style,
      spans$label[k]
    )
  })

  jstring <- paste(unlist(jstring), collapse = "\n")
  sprintf("<tr>\n%s\n</tr>", jstring)
}

# Helper function to insert group rows into the table
html_groupj_insert <- function(x, groupj_rows) {
  out <- strsplit(x@table_string, "\\n")[[1]]

  all_jstrings <- paste(groupj_rows, collapse = "\n")
  idx <- grep("<thead>", out, fixed = TRUE)[1]
  out <- c(out[seq_len(idx)], all_jstrings, out[(idx + 1):length(out)])
  out <- paste(out, collapse = "\n")

  x@table_string <- out
  x
}

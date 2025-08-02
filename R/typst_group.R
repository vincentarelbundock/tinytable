#' tinytable S4 method
#'
#' @keywords internal
setMethod(
  f = "group_eval_j",
  signature = "tinytable_typst",
  definition = function(x, i = NULL, j = NULL, indent = 0, ...) {
    # Only handle column grouping - row insertions now use matrix insertion
    if (!is.null(j)) {
      x <- typst_groupj(x, j, ...)
    }
    return(x)
  }
)

typst_groupj <- function(x, j, ihead, ...) {
  # Check if there are any column groups to process
  if (nrow(x@group_data_j) == 0) {
    return(x)
  }

  # Process all column groups and collect results
  header_rows <- typst_groupj_process(x)

  if (length(header_rows) > 0) {
    # Insert all header rows into the table
    x <- typst_groupj_insert(x, header_rows)
  }

  # Add column gutter if not present
  x <- typst_groupj_gutter(x)

  return(x)
}

# Helper function to process all column groups
typst_groupj_process <- function(x) {
  all_header_rows <- character(0)

  for (row_idx in 1:nrow(x@group_data_j)) {
    group_row <- as.character(x@group_data_j[row_idx, ])

    # Convert group row to column specifications
    col_specs <- typst_groupj_specs(group_row)

    if (length(col_specs) > 0) {
      col <- paste(col_specs, collapse = "")
      all_header_rows <- c(all_header_rows, col)
    }
  }

  all_header_rows
}

# Helper function to parse a group row into column specifications
typst_groupj_specs <- function(group_row) {
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
    span_end <- typst_groupj_span_end(group_row, i, current_label)
    span_length <- span_end - span_start + 1

    if (trimws(current_label) == "") {
      col_specs <- c(col_specs, sprintf("[ ],"))
    } else {
      col_specs <- c(
        col_specs,
        sprintf(
          "table.cell(stroke: (bottom: .05em + black), colspan: %s, align: center)[%s],",
          span_length,
          current_label
        )
      )
    }

    i <- span_end + 1
  }

  col_specs
}

# Helper function to find the end of a span
typst_groupj_span_end <- function(group_row, start_idx, current_label) {
  i <- start_idx

  # For non-empty labels, include following empty strings as continuation
  if (trimws(current_label) != "") {
    i <- i + 1 # Move past the current label
    # Continue through empty strings
    while (
      i <= length(group_row) &&
        !is.na(group_row[i]) &&
        trimws(group_row[i]) == ""
    ) {
      i <- i + 1
    }
  } else {
    # For empty labels, just move to next
    while (
      i <= length(group_row) &&
        !is.na(group_row[i]) &&
        trimws(group_row[i]) == ""
    ) {
      i <- i + 1
    }
  }

  i - 1
}

# Helper function to insert header rows into the table
typst_groupj_insert <- function(x, header_rows) {
  out <- x@table_string

  if (length(header_rows) > 0) {
    all_headers <- paste(header_rows, collapse = "\n")
    out <- lines_insert(out, all_headers, "repeat: true", "after")
  }

  x@table_string <- out
  x
}

# Helper function to add column gutter if not present
typst_groupj_gutter <- function(x) {
  out <- x@table_string

  if (!any(grepl("column-gutter", out))) {
    out <- lines_insert(
      out,
      "    column-gutter: 5pt,",
      "// tinytable table start",
      "after"
    )
  }

  x@table_string <- out
  x
}

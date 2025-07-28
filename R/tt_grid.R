# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

#' Create a grid line with specified character
#' @keywords internal
#' @noRd
create_grid_line <- function(width_cols, char = "-") {
  line_sep <- lapply(width_cols, function(k) strrep(char, k))
  line_sep <- paste(line_sep, collapse = "+")
  line_sep <- paste0("+", line_sep, "+")
  return(line_sep)
}

#' Calculate width of text content (with fansi support)
#' @keywords internal
#' @noRd
calculate_text_width <- function(text) {
  if (isTRUE(check_dependency("fansi"))) {
    nchar(as.character(fansi::strip_ctl(text)))
  } else {
    nchar(text)
  }
}

# =============================================================================
# DATA EXTRACTION FUNCTIONS
# =============================================================================

#' Extract colspan information from table styling
#' @keywords internal
#' @noRd
extract_colspan_info <- function(x) {
  if (!inherits(x, "tinytable") || nrow(x@style) == 0) {
    return(NULL)
  }

  sty <- x@style
  colspan_info <- sty[
    !is.na(sty$colspan) & sty$colspan > 1,
    c("i", "j", "colspan")
  ]

  if (nrow(colspan_info) == 0) {
    return(NULL)
  }

  return(colspan_info)
}

#' Check if a cell is part of a colspan
#' @keywords internal
#' @noRd
is_colspan_cell <- function(i, j, colspan_info, header) {
  if (is.null(colspan_info)) {
    return(FALSE)
  }

  data_row_idx <- if (header) i - 1 else i
  if (data_row_idx <= 0) {
    return(FALSE)
  }

  any(colspan_info$i == data_row_idx & colspan_info$j == j)
}

# =============================================================================
# WIDTH CALCULATION FUNCTIONS
# =============================================================================

#' Calculate basic column widths excluding colspan cells
#' @keywords internal
#' @noRd
calculate_basic_widths <- function(tab, x, header, width_cols) {
  colspan_info <- extract_colspan_info(x)

  for (j in seq_len(ncol(tab))) {
    # Collect non-colspan cell contents
    cell_widths <- character(0)
    for (i in seq_len(nrow(tab))) {
      if (!is_colspan_cell(i, j, colspan_info, header)) {
        cell_widths <- c(cell_widths, tab[i, j])
      }
    }

    # Calculate max width for this column
    if (length(cell_widths) > 0) {
      width_cols[j] <- max(calculate_text_width(cell_widths))
    } else {
      # Fallback if all cells in column have colspan
      width_cols[j] <- max(calculate_text_width(tab[, j]))
    }
  }

  return(list(width_cols = width_cols, colspan_info = colspan_info))
}

#' Adjust column widths for colspan cells
#' @keywords internal
#' @noRd
adjust_colspan_widths <- function(tab, width_cols, colspan_info, header) {
  if (is.null(colspan_info)) {
    return(width_cols)
  }

  for (idx in seq_len(nrow(colspan_info))) {
    i_row <- colspan_info[idx, "i"]
    j_col <- colspan_info[idx, "j"]
    colspan <- colspan_info[idx, "colspan"]

    # Get the actual row index in the tab matrix
    tab_row_idx <- if (header) i_row + 1 else i_row

    if (tab_row_idx <= nrow(tab) && j_col <= ncol(tab)) {
      # Get content width of the colspan cell
      cell_content <- tab[tab_row_idx, j_col]
      content_width <- calculate_text_width(cell_content)

      # Calculate current total width of spanned columns
      spanned_cols <- j_col:(j_col + colspan - 1)
      spanned_cols <- spanned_cols[spanned_cols <= length(width_cols)]
      current_total_width <- sum(width_cols[spanned_cols]) +
        length(spanned_cols) -
        1

      # Expand columns if content is longer than current width
      if (content_width > current_total_width) {
        extra_width <- content_width - current_total_width
        width_per_col <- extra_width / length(spanned_cols)

        for (col in spanned_cols) {
          width_cols[col] <- width_cols[col] + ceiling(width_per_col)
        }
      }
    }
  }

  return(width_cols)
}

#' Find the end of a span by looking for consecutive empty strings
#' @keywords internal
#' @noRd
find_span_end <- function(group_row, start_pos) {
  pos <- start_pos

  # Skip consecutive empty strings
  while (
    pos <= length(group_row) &&
      !is.na(group_row[pos]) &&
      trimws(group_row[pos]) == ""
  ) {
    pos <- pos + 1
  }

  return(pos - 1)
}

#' Find column spans in a group row
#' @keywords internal
#' @noRd
find_column_spans <- function(group_row) {
  spans <- list()
  current_pos <- 1

  # Process each position in the group row
  while (current_pos <= length(group_row)) {
    current_label <- group_row[current_pos]

    # Skip NA (ungrouped) columns
    if (is.na(current_label)) {
      current_pos <- current_pos + 1
      next
    }

    span_start <- current_pos

    # Find the end of this span
    if (trimws(current_label) != "") {
      # For non-empty labels, skip the label itself first
      current_pos <- current_pos + 1
    }

    # Find consecutive empty strings (common logic for both cases)
    span_end <- find_span_end(group_row, current_pos)
    current_pos <- span_end + 1

    # Only add non-empty labels
    if (trimws(current_label) != "") {
      spans[[length(spans) + 1]] <- list(
        label = current_label,
        start = span_start,
        end = span_end
      )
    }
  }

  return(spans)
}

#' Adjust column widths for group headers
#' @keywords internal
#' @noRd
adjust_group_widths <- function(x, width_cols) {
  # Handle column groups using @group_data_j
  if (nrow(x@group_data_j) > 0) {
    # Process each header row to extract column spans
    for (row_idx in seq_len(nrow(x@group_data_j))) {
      group_row <- as.character(x@group_data_j[row_idx, ])
      spans <- find_column_spans(group_row)

      # Adjust widths for each span
      for (span in spans) {
        group_cols <- span$start:span$end
        g_len <- nchar(span$label) + 2
        c_len <- sum(width_cols[group_cols])

        if (g_len > c_len) {
          width_cols[group_cols] <- width_cols[group_cols] +
            ceiling((g_len - c_len) / length(group_cols))
        }
      }
    }
  }

  # Handle row groups (they span entire table width)
  if (nrow(x@group_data_i) > 0) {
    # Check labels in first column of group data
    if (ncol(x@group_data_i) >= 1) {
      labels <- x@group_data_i[, 1]
      for (label in labels) {
        if (!is.na(label) && nchar(label) > 0) {
          g_len <- nchar(label) + 2
          # Total table width including separators
          c_len <- sum(width_cols) + length(width_cols) - 1
          if (g_len > c_len) {
            # Distribute extra width across all columns
            extra_width <- ceiling((g_len - c_len) / length(width_cols))
            width_cols <- width_cols + extra_width
          }
        }
      }
    }
  }

  return(width_cols)
}

#' Calculate all column widths including colspan and group adjustments
#' @keywords internal
#' @noRd
calculate_column_widths <- function(tab, x, header, width_cols) {
  # Calculate basic widths first
  result <- calculate_basic_widths(tab, x, header, width_cols)
  width_cols <- result$width_cols
  colspan_info <- result$colspan_info

  # Adjust for colspan cells
  width_cols <- adjust_colspan_widths(tab, width_cols, colspan_info, header)

  # Adjust for group headers
  if (inherits(x, "tinytable")) {
    width_cols <- adjust_group_widths(x, width_cols)
  }

  return(width_cols)
}

# =============================================================================
# TABLE BUILDING FUNCTIONS
# =============================================================================

#' Pad table cells to match column widths
#' @keywords internal
#' @noRd
pad_table_cells <- function(tab, width_cols) {
  for (j in seq_len(ncol(tab))) {
    if (isTRUE(check_dependency("fansi"))) {
      nc <- nchar(fansi::strip_ctl(tab[, j]))
    } else {
      nc <- nchar(tab[, j])
    }
    pad <- width_cols[j] - nc
    pad <- sapply(pad, function(k) strrep(" ", max(0, k)))
    tab[, j] <- paste0(tab[, j], pad)
  }
  return(tab)
}

#' Format table rows with proper separators
#' @keywords internal
#' @noRd
format_table_rows <- function(tab, x, header, width_cols) {
  body <- character(nrow(tab))

  # Get group row indices if available
  group_rows <- if (inherits(x, "tinytable")) x@group_index_i else integer(0)
  # Adjust for header if present
  if (header) {
    group_rows <- group_rows + 1 # Header adds one row at the top
  }

  for (row_idx in seq_len(nrow(tab))) {
    row_data <- tab[row_idx, ]
    # Check if this is a group row with colspan styling
    is_group_row <- row_idx %in% group_rows
    has_content <- nchar(trimws(row_data[1])) > 0
    others_empty <- all(trimws(row_data[-1]) == "")

    if (is_group_row && has_content && others_empty) {
      # This is a colspan row - create a single spanning cell
      total_width <- sum(width_cols) + length(width_cols) - 1
      content_width <- nchar(row_data[1])
      padding_needed <- total_width - content_width
      body[row_idx] <- paste0(
        "|",
        row_data[1],
        strrep(" ", padding_needed),
        "|"
      )
    } else {
      # Regular row - use column separators
      body[row_idx] <- paste0("|", paste(row_data, collapse = "|"), "|")
    }
  }

  return(body)
}

#' Assemble final table string with borders
#' @keywords internal
#' @noRd
assemble_table_string <- function(body, header, width_cols) {
  rule_head <- create_grid_line(width_cols, "=")
  rule_line <- create_grid_line(width_cols, "-")

  if (header) {
    tab <- c(
      "\n",
      rule_line,
      body[1],
      rule_head,
      body[2:length(body)],
      rule_line,
      "\n"
    )
  } else {
    tab <- c("\n", rule_line, body, rule_line, "\n")
  }

  return(paste(tab, collapse = "\n"))
}

# =============================================================================
# MAIN FUNCTION
# =============================================================================

tt_eval_grid <- function(x, width_cols = NULL, ...) {
  is_matrix <- is.matrix(x)
  if (is_matrix) {
    tab <- x
  } else {
    tab <- x@data_body
  }

  if (is.null(width_cols)) {
    width_cols <- x@width_cols
  }

  tthead <- inherits(x, "tinytable") && isTRUE(x@nhead > 0)
  if (length(colnames(x)) != 0 || tthead) {
    tab <- as.matrix(tab)
    tab <- base::rbind(colnames(x), tab)
    header <- TRUE
  } else {
    tab <- as.matrix(tab)
    header <- FALSE
  }

  # pad for readability
  padded <- sprintf(" %s ", tab)
  tab <- matrix(padded, ncol = ncol(tab))

  if (is.null(width_cols) || length(width_cols) == 0) {
    width_cols <- calculate_column_widths(tab, x, header, width_cols)
  }

  # Pad cells to match column widths
  tab <- pad_table_cells(tab, width_cols)

  # Format rows with proper separators
  body <- format_table_rows(tab, x, header, width_cols)

  # Assemble final table string
  out <- assemble_table_string(body, header, width_cols)

  if (is_matrix) {
    return(out)
  }

  # rebuild output
  x@width_cols <- width_cols
  x@table_string <- out

  # output
  return(x)
}

# =============================================================================
# AUXILIARY FUNCTIONS
# =============================================================================

empty_cells <- function(lst) {
  # Find the largest number in the list
  max_num <- max(unlist(lst))

  # Create the full range from 1 to the largest number
  full_range <- 1:max_num

  # Find missing numbers (holes)
  missing_nums <- setdiff(full_range, unlist(lst))

  if (length(missing_nums) == 0) {
    return(lst)
  }

  # Create new elements for missing numbers
  new_elements <- split(missing_nums, cumsum(c(1, diff(missing_nums) != 1)))

  # Name the new elements with empty strings and merge with original list
  names(new_elements) <- rep(" ", length(new_elements))
  filled_list <- c(lst, new_elements)

  # Sort the list by the minimum number in each element
  filled_list[order(sapply(filled_list, min))]
}

# insert horizontal rules everywhere (important for word)
grid_hlines <- function(x) {
  rule_line <- create_grid_line(x@width_cols, "-")
  out <- x@table_string
  lines <- strsplit(out, split = "\\n")[[1]]
  if (length(lines) > 1) {
    for (idlines in length(lines):2) {
      if (
        !startsWith(lines[idlines - 1], "+") &&
          !startsWith(lines[idlines], "+") &&
          lines[idlines] != ""
      ) {
        lines <- c(
          lines[1:(idlines - 1)],
          rule_line,
          lines[idlines:length(lines)]
        )
      }
    }
  }
  x@table_string <- paste(lines, collapse = "\n")
  return(x)
}

setMethod(
  f = "tt_eval",
  signature = "tinytable_grid",
  definition = tt_eval_grid
)

setMethod(
  f = "tt_eval",
  signature = "matrix",
  definition = tt_eval_grid
)

setMethod(
  f = "tt_eval",
  signature = "tinytable_dataframe",
  definition = tt_eval_grid
)

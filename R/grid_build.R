# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

#' Create a grid line with specified character
#' @keywords internal
#' @noRd
grid_create_line <- function(
    width_cols,
    char = "-",
    corner_char = "+",
    x = NULL) {
  # Always use "+" for corner characters
  corner_char <- "+"

  line_sep <- lapply(width_cols, function(k) strrep(char, k))
  line_sep <- paste(line_sep, collapse = corner_char)
  line_sep <- paste0(corner_char, line_sep, corner_char)
  return(line_sep)
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

    # Skip if row index is invalid (negative indices are used for group headers)
    if (tab_row_idx <= 0 || tab_row_idx > nrow(tab) || j_col <= 0 || j_col > ncol(tab)) {
      next
    }
      # Get content width of the colspan cell
      cell_content <- tab[tab_row_idx, j_col]
      content_width <- ansi_nchar(cell_content)

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

  return(width_cols)
}

#' Find column spans in a group row
#' @keywords internal
#' @noRd
find_column_spans <- function(group_row) {
  spans <- list()
  current_pos <- 1

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
      current_pos <- current_pos + 1
    }

    # Skip consecutive empty strings
    while (
      current_pos <= length(group_row) &&
        !is.na(group_row[current_pos]) &&
        trimws(group_row[current_pos]) == ""
    ) {
      current_pos <- current_pos + 1
    }

    span_end <- current_pos - 1

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
        g_len <- ansi_nchar(span$label) + 2
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
        if (!is.na(label) && ansi_nchar(label) > 0) {
          g_len <- ansi_nchar(label) + 2
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
  colspan_info <- extract_colspan_info(x)

  # Mark colspan cells once instead of rescanning colspan_info for every cell.
  # Rows in colspan_info use body indices; tab includes the header when present.
  excluded <- matrix(FALSE, nrow = nrow(tab), ncol = ncol(tab))
  if (!is.null(colspan_info)) {
    tab_i <- colspan_info$i + as.integer(header)
    tab_j <- colspan_info$j
    valid <- !is.na(tab_i) & !is.na(tab_j) &
      tab_i > 0L & tab_i <= nrow(tab) & tab_j > 0L & tab_j <= ncol(tab)
    if (any(valid)) {
      excluded[cbind(tab_i[valid], tab_j[valid])] <- TRUE
    }
  }

  # Calculate basic widths excluding colspan cells. ansi_nchar() is vectorized,
  # so this avoids growing cell_widths in the former nested row loop.
  for (j in seq_len(ncol(tab))) {
    keep <- !excluded[, j]
    if (any(keep)) {
      width_cols[j] <- max(ansi_nchar(tab[keep, j]))
    } else {
      width_cols[j] <- max(ansi_nchar(tab[, j]))
    }
  }

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
    nc <- ansi_nchar(tab[, j])
    pad <- strrep(" ", pmax.int(0L, width_cols[j] - nc))
    tab[, j] <- paste0(tab[, j], pad)
  }
  return(tab)
}

#' Check if row should be formatted as a spanning group row
#' @keywords internal
#' @noRd
is_spanning_group_row <- function(row_idx, row_data, group_rows) {
  is_group_row <- row_idx %in% group_rows
  has_content <- ansi_nchar(trimws(row_data[1])) > 0
  others_empty <- all(trimws(row_data[-1]) == "")
  return(is_group_row && has_content && others_empty)
}

#' Format table rows with proper separators
#' @keywords internal
#' @noRd
format_table_rows <- function(tab, x, header, width_cols) {
  body <- character(nrow(tab))

  # Get vertical line character - use "|" if enabled, space if disabled
  vline_char <- if (inherits(x, "tinytable") && isTRUE(x@grid_vline)) {
    "|"
  } else if (inherits(x, "tinytable") && !isTRUE(x@grid_vline)) {
    " " # Use space if vertical lines are disabled
  } else {
    "|" # Default fallback
  }

  # Use vertical line character for borders
  left_border <- vline_char
  right_border <- vline_char

  # Get group row indices if available
  group_rows <- if (inherits(x, "tinytable")) x@group_index_i else integer(0)
  if (header) {
    group_rows <- group_rows + 1 # Header adds one row at the top
  }

  spanning_rows <- logical(nrow(tab))
  candidates <- intersect(group_rows, seq_len(nrow(tab)))
  for (row_idx in candidates) {
    row_data <- tab[row_idx, ]
    spanning_rows[row_idx] <-
      ansi_nchar(trimws(row_data[1])) > 0 &&
      all(trimws(row_data[-1]) == "")
  }

  for (row_idx in seq_len(nrow(tab))) {
    row_data <- tab[row_idx, ]

    if (spanning_rows[row_idx]) {
      # This is a colspan row - create a single spanning cell
      total_width <- sum(width_cols) + length(width_cols) - 1
      content_width <- ansi_nchar(row_data[1])
      padding_needed <- total_width - content_width
      body[row_idx] <- paste0(
        left_border,
        row_data[1],
        strrep(" ", padding_needed),
        right_border
      )
    } else {
      # Regular row - use column separators
      body[row_idx] <- paste0(
        left_border,
        paste(row_data, collapse = vline_char),
        right_border
      )
    }
  }

  return(body)
}

#' Assemble final table string with borders
#' @keywords internal
#' @noRd
assemble_table_string <- function(body, header, width_cols, x = NULL) {
  # Get line characters - use hardcoded values
  if (!is.null(x) && inherits(x, "tinytable")) {
    hline_char <- if (isTRUE(x@grid_hline)) "-" else NULL
    hline_header_char <- if (isTRUE(x@grid_hline_header)) "=" else NULL
  } else {
    hline_char <- "-"
    hline_header_char <- "="
  }

  # Create rule lines only if enabled
  rule_head <- if (!is.null(hline_header_char)) {
    grid_create_line(width_cols, hline_header_char, x = x)
  } else {
    NULL
  }

  rule_line <- if (!is.null(hline_char)) {
    grid_create_line(width_cols, hline_char, x = x)
  } else {
    NULL
  }

  if (header) {
    tab_parts <- list("\n")

    # Top border
    if (!is.null(rule_line)) {
      tab_parts <- append(tab_parts, rule_line)
    }

    # Header row
    tab_parts <- append(tab_parts, body[1])

    # Header separator
    if (!is.null(rule_head)) {
      tab_parts <- append(tab_parts, rule_head)
    }

    # Body rows
    if (length(body) > 1) {
      tab_parts <- append(tab_parts, body[2:length(body)])
    }

    # Bottom border
    if (!is.null(rule_line)) {
      tab_parts <- append(tab_parts, rule_line)
    }

    tab_parts <- append(tab_parts, "\n")
    tab <- unlist(tab_parts)
  } else {
    tab_parts <- list("\n")

    # Top border
    if (!is.null(rule_line)) {
      tab_parts <- append(tab_parts, rule_line)
    }

    # Body rows
    tab_parts <- append(tab_parts, body)

    # Bottom border
    if (!is.null(rule_line)) {
      tab_parts <- append(tab_parts, rule_line)
    }

    tab_parts <- append(tab_parts, "\n")
    tab <- unlist(tab_parts)
  }

  return(paste(tab, collapse = "\n"))
}

# =============================================================================
# MAIN FUNCTION
# =============================================================================

build_eval_grid <- function(x, width_cols = NULL, ...) {
  is_matrix <- is.matrix(x)
  if (is_matrix) {
    tab <- x
  } else {
    tab <- x@data_body
  }

  if (is.null(width_cols)) {
    width_cols <- x@width_cols
  }

  # Apply text styling before padding (for non-matrix formats)
  if (!is_matrix && inherits(x, "tinytable") && nrow(x@style) > 0) {
    x <- style_grid_body(x)
    x <- style_grid_group(x)
    tab <- x@data_body
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

  # Apply background styling to padded cells (for grid formats only)
  if (!is_matrix && inherits(x, "tinytable") && nrow(x@style) > 0) {
    tab <- style_grid_body_background(tab, x, header)
  }

  if (inherits(x, "tinytable")) {
    x@data_body <- stats::setNames(as.data.frame(tab), names(x@data_body))
  }

  # Format rows with proper separators
  body <- format_table_rows(tab, x, header, width_cols)

  # Assemble final table string
  out <- assemble_table_string(body, header, width_cols, x)

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
  # Only add lines if horizontal lines are enabled
  if (!isTRUE(x@grid_hline)) {
    return(x)
  }

  rule_line <- grid_create_line(x@width_cols, "-", x = x)
  out <- x@table_string
  lines <- strsplit(out, split = "\\n")[[1]]
  if (length(lines) > 1) {
    corner_char <- "+"
    add_before <- c(
      FALSE,
      !startsWith(lines[-length(lines)], corner_char) &
        !startsWith(lines[-1L], corner_char) &
        lines[-1L] != ""
    )
    if (any(add_before)) {
      end_pos <- cumsum(1L + add_before)
      expanded <- character(length(lines) + sum(add_before))
      expanded[end_pos] <- lines
      expanded[end_pos[add_before] - 1L] <- rule_line
      lines <- expanded
    }
  }
  x@table_string <- paste(lines, collapse = "\n")
  return(x)
}

setMethod(
  f = "build_eval",
  signature = "tinytable_grid",
  definition = build_eval_grid
)

setMethod(
  f = "build_eval",
  signature = "matrix",
  definition = build_eval_grid
)

setMethod(
  f = "build_eval",
  signature = "tinytable_dataframe",
  definition = build_eval_grid
)

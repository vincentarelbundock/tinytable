grid_line <- function(width_cols, char = "-") {
  line_sep <- lapply(width_cols, function(k) strrep(char, k))
  line_sep <- paste(line_sep, collapse = "+")
  line_sep <- paste0("+", line_sep, "+")
  return(line_sep)
}

tt_eval_grid <- function(x, width_cols = NULL, ...) {
  is_matrix <- is.matrix(x)
  if (is_matrix) {
    tab <- x
  } else {
    tab <- x@data_processed
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
    # Get colspan information to exclude those cells from initial width calculation
    colspan_info <- NULL
    if (inherits(x, "tinytable") && nrow(x@style) > 0) {
      sty <- x@style
      colspan_info <- sty[!is.na(sty$colspan) & sty$colspan > 1, c("i", "j", "colspan")]
    }
    
    # First pass: calculate widths excluding colspan cells
    for (j in seq_len(ncol(tab))) {
      cell_widths <- character(0)
      for (i in seq_len(nrow(tab))) {
        # Check if this cell has colspan styling - if so, skip it for width calculation
        is_colspan_cell <- FALSE
        if (!is.null(colspan_info) && nrow(colspan_info) > 0) {
          # Adjust row index if header is present
          data_row_idx <- if (header) i - 1 else i
          if (data_row_idx > 0) {
            is_colspan_cell <- any(colspan_info$i == data_row_idx & colspan_info$j == j)
          }
        }
        
        if (!is_colspan_cell) {
          cell_widths <- c(cell_widths, tab[i, j])
        }
      }
      
      if (length(cell_widths) > 0) {
        if (isTRUE(check_dependency("fansi"))) {
          width_cols[j] <- max(nchar(as.character(fansi::strip_ctl(cell_widths))))
        } else {
          width_cols[j] <- max(nchar(cell_widths))
        }
      } else {
        # Fallback if all cells in column have colspan
        if (isTRUE(check_dependency("fansi"))) {
          width_cols[j] <- max(nchar(as.character(fansi::strip_ctl(tab[, j]))))
        } else {
          width_cols[j] <- max(nchar(tab[, j]))
        }
      }
    }
    
    # Second pass: adjust widths for colspan cells that are longer than spanned columns
    if (!is.null(colspan_info) && nrow(colspan_info) > 0) {
      for (idx in seq_len(nrow(colspan_info))) {
        i_row <- colspan_info[idx, "i"]
        j_col <- colspan_info[idx, "j"]
        colspan <- colspan_info[idx, "colspan"]
        
        # Get the actual row index in the tab matrix
        tab_row_idx <- if (header) i_row + 1 else i_row
        
        if (tab_row_idx <= nrow(tab) && j_col <= ncol(tab)) {
          # Get the content width of the colspan cell
          cell_content <- tab[tab_row_idx, j_col]
          if (isTRUE(check_dependency("fansi"))) {
            content_width <- nchar(as.character(fansi::strip_ctl(cell_content)))
          } else {
            content_width <- nchar(cell_content)
          }
          
          # Calculate current total width of spanned columns (including separators)
          spanned_cols <- j_col:(j_col + colspan - 1)
          spanned_cols <- spanned_cols[spanned_cols <= length(width_cols)]
          current_total_width <- sum(width_cols[spanned_cols]) + length(spanned_cols) - 1
          
          # If content is longer than current total width, expand columns proportionally
          if (content_width > current_total_width) {
            extra_width <- content_width - current_total_width
            width_per_col <- extra_width / length(spanned_cols)
            
            for (col in spanned_cols) {
              width_cols[col] <- width_cols[col] + ceiling(width_per_col)
            }
          }
        }
      }
    }
  }

  # groups are longer than col-widths
  if (inherits(x, "tinytable")) {
    # Handle column groups using @data_group_j
    if (nrow(x@data_group_j) > 0) {
      # Process each header row to extract column spans
      for (row_idx in seq_len(nrow(x@data_group_j))) {
        group_row <- as.character(x@data_group_j[row_idx, ])
        
        # Find consecutive spans - similar logic to other backends
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
          if (trimws(current_label) != "") {
            i <- i + 1  # Move past the current label
            # Continue through empty strings (continuation of span)
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
          
          # Only process non-empty labels
          if (trimws(current_label) != "") {
            group_cols <- span_start:span_end
            g_len <- nchar(current_label) + 2
            c_len <- sum(width_cols[group_cols])
            if (g_len > c_len) {
              width_cols[group_cols] <- width_cols[group_cols] +
                ceiling((g_len - c_len) / length(group_cols))
            }
          }
        }
      }
    }

    # Handle row groups (they span entire table width)
    if (nrow(x@data_group_i) > 0) {
      # Check labels in first column of group data
      if (ncol(x@data_group_i) >= 1) {
        labels <- x@data_group_i[, 1]
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
  }

  for (j in seq_len(ncol(x))) {
    if (isTRUE(check_dependency("fansi"))) {
      nc <- nchar(fansi::strip_ctl(tab[, j]))
    } else {
      nc <- nchar(tab[, j])
    }
    pad <- width_cols[j] - nc
    pad <- sapply(pad, function(k) strrep(" ", max(0, k)))
    tab[, j] <- paste0(tab[, j], pad)
  }

  rule_head <- grid_line(width_cols, "=")
  rule_line <- grid_line(width_cols, "-")

  # Handle colspan rows differently
  body <- character(nrow(tab))
  # Get group row indices if available
  group_rows <- if (inherits(x, "tinytable")) x@group_index_i else integer(0)
  # Adjust for header if present
  if (header) {
    group_rows <- group_rows + 1  # Header adds one row at the top
  }
  
  for (row_idx in seq_len(nrow(tab))) {
    row_data <- tab[row_idx, ]
    # Check if this is a group row with colspan styling
    is_group_row <- row_idx %in% group_rows
    has_content <- nchar(trimws(row_data[1])) > 0
    others_empty <- all(trimws(row_data[-1]) == "")
    # Temporary debug output
    if (is_group_row && has_content && others_empty) {
      # This is a colspan row - create a single spanning cell
      total_width <- sum(width_cols) + length(width_cols) - 1
      content_width <- nchar(row_data[1])
      padding_needed <- total_width - content_width
      body[row_idx] <- paste0("|", row_data[1], strrep(" ", padding_needed), "|")
    } else {
      # Regular row - use column separators
      body[row_idx] <- paste0("|", paste(row_data, collapse = "|"), "|")
    }
  }
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

  out <- paste(tab, collapse = "\n")

  if (is_matrix) {
    return(out)
  }

  # rebuild output
  x@width_cols <- width_cols
  x@table_string <- out

  # output
  return(x)
}

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
  rule_line <- grid_line(x@width_cols, "-")
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

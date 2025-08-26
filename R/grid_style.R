
style_grid_cell <- function(x, name = "italic", indent = NULL, color = NULL, background = NULL) {
  engine <- getOption("tinytable_grid_style_engine", default = "markdown")

  if (engine == "ansi") {
    # ANSI escape codes
    if (name == "italic") {
      out <- sprintf("\033[3m%s\033[23m", x)
    } else if (name == "bold") {
      out <- sprintf("\033[1m%s\033[22m", x)
    } else if (name == "strikeout") {
      out <- sprintf("\033[9m%s\033[29m", x)
    } else if (name == "underline") {
      out <- sprintf("\033[4m%s\033[24m", x)
    } else if (name == "indent" && !is.null(indent)) {
      out <- sprintf("%s%s", strrep(" ", indent), x)
    } else if (name == "color" && !is.null(color)) {
      ansi_color_code <- standardize_colors(color, format = "ansi")
      if (!is.na(ansi_color_code) && ansi_color_code != color) {
        out <- sprintf("\033[%sm%s\033[39m", ansi_color_code, x)
      } else {
        out <- x  # Unknown color, return unchanged
      }
    } else if (name == "background" && !is.null(background)) {
      ansi_bg_code <- standardize_colors(background, format = "ansi")
      if (!is.na(ansi_bg_code) && ansi_bg_code != background) {
        # Convert foreground ANSI code to background by replacing 38 with 48
        if (grepl("^38;2;", ansi_bg_code)) {
          ansi_bg_code <- sub("^38;2;", "48;2;", ansi_bg_code)
        } else if (grepl("^3[0-7]$", ansi_bg_code)) {
          # Convert standard foreground colors (30-37) to background (40-47)
          fg_code <- as.numeric(ansi_bg_code)
          ansi_bg_code <- as.character(fg_code + 10)
        } else if (grepl("^9[0-7]$", ansi_bg_code)) {
          # Convert bright foreground colors (90-97) to bright background (100-107)
          fg_code <- as.numeric(ansi_bg_code)
          ansi_bg_code <- as.character(fg_code + 10)
        }
        out <- sprintf("\033[%sm%s\033[49m", ansi_bg_code, x)
      } else {
        out <- x  # Unknown background color, return unchanged
      }
    } else {
      out <- x
    }
  } else {
    # Default markdown approach (colors not supported in markdown)
    if (name == "italic") {
      out <- sprintf("_%s_", x)
    } else if (name == "bold") {
      out <- sprintf("**%s**", x)
    } else if (name == "strikeout") {
      out <- sprintf("~~%s~~", x)
    } else if (name == "underline") {
      out <- sprintf("<u>%s</u>", x)
    } else if (name == "indent" && !is.null(indent)) {
      out <- sprintf("%s%s", strrep(" ", indent), x)
    } else {
      out <- x  # Colors ignored in markdown mode
    }
  }
  return(out)
}


style_eval_grid <- function(x) {
  out <- x@data_body
  sty <- x@style

  if (nrow(sty) == 0) {
    return(x)
  }


  sty <- prepare_grid_style(x)

  # styling
  for (idx in seq_len(nrow(sty))) {
    row <- sty[idx, "i"]
    col <- sty[idx, "j"]

    # Handle column names (i = 0)
    if (row == 0) {
      current_name <- colnames(x)[col]
      if (!identical(trimws(current_name), "")) {
        if (isTRUE(sty[idx, "bold"])) {
          colnames(x)[col] <- style_grid_cell(current_name, "bold")
        }
        if (isTRUE(sty[idx, "italic"])) {
          current_name <- colnames(x)[col]
          colnames(x)[col] <- style_grid_cell(current_name, "italic")
        }
        if (isTRUE(sty[idx, "strikeout"])) {
          current_name <- colnames(x)[col]
          colnames(x)[col] <- style_grid_cell(current_name, "strikeout")
        }
        if (isTRUE(sty[idx, "underline"])) {
          current_name <- colnames(x)[col]
          colnames(x)[col] <- style_grid_cell(current_name, "underline")
        }
        if (!is.na(sty[idx, "color"])) {
          current_name <- colnames(x)[col]
          colnames(x)[col] <- style_grid_cell(current_name, "color", color = sty[idx, "color"])
        }
        if (!is.na(sty[idx, "background"])) {
          current_name <- colnames(x)[col]
          colnames(x)[col] <- style_grid_cell(current_name, "background", background = sty[idx, "background"])
        }
      }
    }
    # Handle group headers (negative i)
    else if (row < 0) {
      if (nrow(x@group_data_j) > 0) {
        # Convert negative row index to positive index in group_data_j
        group_row <- abs(row)
        if (group_row <= nrow(x@group_data_j) && col <= ncol(x@group_data_j)) {
          current_value <- x@group_data_j[group_row, col]
          if (!is.na(current_value) && !identical(trimws(current_value), "")) {
            if (isTRUE(sty[idx, "bold"])) {
              x@group_data_j[group_row, col] <- style_grid_cell(current_value, "bold")
            }
            if (isTRUE(sty[idx, "italic"])) {
              current_value <- x@group_data_j[group_row, col]
              x@group_data_j[group_row, col] <- style_grid_cell(current_value, "italic")
            }
            if (isTRUE(sty[idx, "strikeout"])) {
              current_value <- x@group_data_j[group_row, col]
              x@group_data_j[group_row, col] <- style_grid_cell(current_value, "strikeout")
            }
            if (isTRUE(sty[idx, "underline"])) {
              current_value <- x@group_data_j[group_row, col]
              x@group_data_j[group_row, col] <- style_grid_cell(current_value, "underline")
            }
            if (!is.na(sty[idx, "color"])) {
              current_value <- x@group_data_j[group_row, col]
              x@group_data_j[group_row, col] <- style_grid_cell(current_value, "color", color = sty[idx, "color"])
            }
            if (!is.na(sty[idx, "background"])) {
              current_value <- x@group_data_j[group_row, col]
              x@group_data_j[group_row, col] <- style_grid_cell(current_value, "background", background = sty[idx, "background"])
            }
          }
        }
      }
    }
    # Handle main table body (positive i)
    else {
      current_value <- x@data_body[row, col]
      if (!identical(trimws(current_value), "")) {
        if (isTRUE(sty[idx, "bold"])) {
          x@data_body[row, col] <- style_grid_cell(current_value, "bold")
        }
        if (isTRUE(sty[idx, "italic"])) {
          current_value <- x@data_body[row, col]
          x@data_body[row, col] <- style_grid_cell(current_value, "italic")
        }
        if (isTRUE(sty[idx, "strikeout"])) {
          current_value <- x@data_body[row, col]
          x@data_body[row, col] <- style_grid_cell(current_value, "strikeout")
        }
        if (isTRUE(sty[idx, "underline"])) {
          current_value <- x@data_body[row, col]
          x@data_body[row, col] <- style_grid_cell(current_value, "underline")
        }
        if (!is.na(sty[idx, "indent"])) {
          indent <- sty[idx, "indent"]
          current_value <- x@data_body[row, col]
          x@data_body[row, col] <- style_grid_cell(current_value, "indent", indent)
        }
        if (!is.na(sty[idx, "color"])) {
          current_value <- x@data_body[row, col]
          x@data_body[row, col] <- style_grid_cell(current_value, "color", color = sty[idx, "color"])
        }
        if (!is.na(sty[idx, "background"])) {
          current_value <- x@data_body[row, col]
          x@data_body[row, col] <- style_grid_cell(current_value, "background", background = sty[idx, "background"])
        }
      }
    }

    # wipe adjacent cells
    rowspan <- sty[idx, "rowspan"]
    colspan <- sty[idx, "colspan"]
    rowspan <- if (is.na(rowspan)) 1 else rowspan
    colspan <- if (is.na(colspan)) 1 else colspan
    wipe <- expand.grid(
      i = row:(row + rowspan - 1),
      j = col:(col + colspan - 1)
    )
    wipe <- wipe[which(wipe$i != row | wipe$j != col), ]
    for (idx in seq_len(nrow(wipe))) {
      x@data_body[wipe$i[idx], wipe$j[idx]] <- ""
    }
  }

  return(x)
}


grid_colspan <- function(x) {
  sty <- prepare_grid_style(x)

  if (nrow(sty) == 0 || !is.data.frame(sty)) {
    return(x)
  }

  # Check if colspan column exists before accessing it
  if (!"colspan" %in% colnames(sty)) {
    return(x)
  }

  # Find rows with colspan > 1
  colspan_rows <- sty[!is.na(sty$colspan) & sty$colspan > 1, ]

  if (nrow(colspan_rows) == 0) {
    return(x)
  }

  # Get the table string and split into lines
  table_lines <- strsplit(x@table_string, "\\n")[[1]]

  # Find the header separator line (+=====+) as reference point
  header_sep_line <- which(grepl("^\\+={2,}", table_lines))
  if (length(header_sep_line) == 0) {
    # No header separator found, try to find any separator line starting with +
    header_sep_line <- which(grepl("^\\+-", table_lines))[1]
    if (is.na(header_sep_line)) {
      return(x) # Can't find reference point
    }
  } else {
    header_sep_line <- header_sep_line[1]
  }

  for (idx in seq_len(nrow(colspan_rows))) {
    row_idx <- colspan_rows[idx, "i"]
    col_idx <- colspan_rows[idx, "j"]
    colspan <- colspan_rows[idx, "colspan"]

    # Calculate target line: header_sep_line + row_idx (rows are consecutive after header separator)
    target_line <- header_sep_line + row_idx

    if (
      target_line <= length(table_lines) &&
        startsWith(table_lines[target_line], "|")
    ) {
      line <- table_lines[target_line]

      # Split the line by | to get cells
      cells <- strsplit(line, "\\|")[[1]]

      # Remove empty first and last elements
      if (length(cells) > 0 && cells[1] == "") {
        cells <- cells[-1]
      }
      if (length(cells) > 0 && cells[length(cells)] == "") {
        cells <- cells[-length(cells)]
      }

      # Remove | markers for the colspan range
      if (
        col_idx <= length(cells) && (col_idx + colspan - 1) <= length(cells)
      ) {
        # For colspan, use only the content from the first cell (the spanning cell)
        # and ignore content from subsequent cells that are being spanned over
        # Preserve leading space but trim trailing spaces
        spanned_content <- sub(" *$", "", cells[col_idx])

        # Calculate total width for the spanned cells
        if (
          !is.null(x@width_cols) &&
            length(x@width_cols) >= (col_idx + colspan - 1)
        ) {
          total_width <- sum(x@width_cols[col_idx:(col_idx + colspan - 1)]) +
            (colspan - 1)
          current_width <- nchar(spanned_content)
          if (current_width < total_width) {
            padding <- total_width - current_width
            spanned_content <- paste0(spanned_content, strrep(" ", padding))
          }
        }

        # Replace the cells with the spanned content
        new_cells <- cells
        new_cells[col_idx] <- spanned_content
        # Remove the subsequent cells that are part of the colspan
        if (colspan > 1) {
          indices_to_remove <- (col_idx + 1):(col_idx + colspan - 1)
          new_cells <- new_cells[-indices_to_remove]
        }

        # Reconstruct the line, preserving the original format
        table_lines[target_line] <- paste0(
          "|",
          paste(new_cells, collapse = "|"),
          "|"
        )
      }
    }
  }

  # Reconstruct the table string
  x@table_string <- paste(table_lines, collapse = "\n")

  return(x)
}

prepare_grid_style <- function(x) {
  sty <- x@style

  # Return early if no styles
  if (nrow(sty) == 0) {
    return(sty)
  }

  sty <- sty[, c(
    "i",
    "j",
    "bold",
    "italic",
    "strikeout",
    "underline",
    "indent",
    "color",
    "background",
    "colspan",
    "rowspan"
  )]
  styrows <- lapply(seq_len(nrow(sty)), function(i) sty[i, , drop = FALSE])

  for (idx in seq_along(styrows)) {
    sr <- styrows[[idx]]
    if (is.na(sr$i) && is.na(sr$j)) {
      cells <- expand.grid(i = seq_len(nrow(x)), j = seq_len(ncol(x)))
    } else if (is.na(sr$j)) {
      cells <- expand.grid(i = sr$i, j = seq_len(ncol(x)))
    } else if (is.na(sr$i)) {
      cells <- expand.grid(i = seq_len(nrow(x)), j = sr$j)
    } else {
      cells <- sr[, c("i", "j")]
    }
    sr$i <- sr$j <- NULL
    styrows[[idx]] <- merge(cells, sr)
  }

  grid_style_last <- function(k) {
    if (all(is.na(k))) {
      return(NA)
    }
    # NA are sometimes logical, so don't use } else if {
    if (is.logical(k)) {
      return(any(k))
    }
    return(unname(utils::tail(stats::na.omit(k), 1)))
  }

  sty <- do.call(rbind, styrows)
  sty <- split(sty, list(sty$i, sty$j), drop = TRUE)
  sty <- lapply(sty, function(z) data.frame(lapply(z, grid_style_last)))
  sty <- do.call(rbind, sty)

  return(sty)
}


###### MUST BE PLACED AFTER style_eval_grid definition
#' tinytable S4 method
#'
#' @keywords internal
setMethod(
  f = "style_eval",
  signature = "tinytable_grid",
  definition = style_eval_grid
)


#' tinytable S4 method
#'
#' @keywords internal
setMethod(
  f = "style_eval",
  signature = "tinytable_dataframe",
  definition = style_eval_grid
)


#' tinytable S4 method
#'
#' @keywords internal
setMethod(
  f = "finalize",
  signature = "tinytable_dataframe",
  definition = identity
)

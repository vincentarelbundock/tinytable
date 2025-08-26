style_grid_cell <- function(
  s,
  x,
  italic = FALSE,
  bold = FALSE,
  strikeout = FALSE,
  underline = FALSE,
  indent = NULL,
  color = NULL,
  background = NULL
) {
  # Use x@ansi if available, otherwise fall back to the global option
  use_ansi <- if (!is.null(x) && isTRUE(x@ansi)) {
    TRUE
  } else {
    getOption("tinytable_grid_style_engine", default = "markdown") == "ansi"
  }

  out <- s # Start with the original string

  if (use_ansi) {
    # ANSI escape codes - apply in sequence
    if (isTRUE(bold)) {
      out <- sprintf("\033[1m%s\033[22m", out)
    }
    if (isTRUE(italic)) {
      out <- sprintf("\033[3m%s\033[23m", out)
    }
    if (isTRUE(underline)) {
      out <- sprintf("\033[4m%s\033[24m", out)
    }
    if (isTRUE(strikeout)) {
      out <- sprintf("\033[9m%s\033[29m", out)
    }
    if (!is.null(color) && !is.na(color)) {
      ansi_color_code <- standardize_colors(color, format = "ansi")
      if (!is.na(ansi_color_code) && ansi_color_code != color) {
        out <- sprintf("\033[%sm%s\033[39m", ansi_color_code, out)
      }
    }
    if (!is.null(background) && !is.na(background)) {
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
        out <- sprintf("\033[%sm%s\033[49m", ansi_bg_code, out)
      }
    }
    if (!is.null(indent) && !is.na(indent)) {
      out <- sprintf("%s%s", strrep(" ", indent), out)
    }
  } else {
    # Default markdown approach - apply in sequence
    if (isTRUE(bold)) {
      out <- sprintf("**%s**", out)
    }
    if (isTRUE(italic)) {
      out <- sprintf("_%s_", out)
    }
    if (isTRUE(underline)) {
      out <- sprintf("<u>%s</u>", out)
    }
    if (isTRUE(strikeout)) {
      out <- sprintf("~~%s~~", out)
    }
    if (!is.null(indent) && !is.na(indent)) {
      out <- sprintf("%s%s", strrep(" ", indent), out)
    }
    # Colors ignored in markdown mode
  }

  return(out)
}


style_grid_group <- function(x) {
  # Determine the styling function to use based on output type and ANSI setting
  style_string_grid <- if (isTRUE(x@ansi)) {
    style_string_ansi
  } else {
    style_string_markdown
  }

  # Apply styling to row groups (group_data_i)
  if (nrow(x@group_data_i) > 0) {
    sty <- x@style

    # Find styling that applies to row groups (i = "groupi")
    group_styles <- sty[sty$i == "groupi" & is.na(sty$j), ]

    if (nrow(group_styles) > 0) {
      # Apply styling to all row group labels
      for (row_idx in seq_len(nrow(x@group_data_i))) {
        for (col_idx in seq_len(ncol(x@group_data_i))) {
          current_value <- x@group_data_i[row_idx, col_idx]
          if (!is.na(current_value) && !identical(trimws(current_value), "")) {
            # Apply the last (most recent) styling for each property
            for (style_idx in seq_len(nrow(group_styles))) {
              styles <- list(
                bold = if (!is.na(group_styles[style_idx, "bold"])) {
                  group_styles[style_idx, "bold"]
                } else {
                  FALSE
                },
                italic = if (!is.na(group_styles[style_idx, "italic"])) {
                  group_styles[style_idx, "italic"]
                } else {
                  FALSE
                },
                strikeout = if (!is.na(group_styles[style_idx, "strikeout"])) {
                  group_styles[style_idx, "strikeout"]
                } else {
                  FALSE
                },
                underline = if (!is.na(group_styles[style_idx, "underline"])) {
                  group_styles[style_idx, "underline"]
                } else {
                  FALSE
                },
                color = if (!is.na(group_styles[style_idx, "color"])) {
                  group_styles[style_idx, "color"]
                } else {
                  NULL
                },
                background = if (
                  !is.na(group_styles[style_idx, "background"])
                ) {
                  group_styles[style_idx, "background"]
                } else {
                  NULL
                }
              )
              x@group_data_i[row_idx, col_idx] <- style_string_grid(
                current_value,
                styles
              )
              current_value <- x@group_data_i[row_idx, col_idx]
            }
          }
        }
      }
    }
  }

  # Apply styling to column groups (group_data_j) - this is already handled in apply_grid_text_styling
  # but we can add explicit handling here if needed
  if (nrow(x@group_data_j) > 0) {
    sty <- x@style

    # Find styling that applies to column groups (i = "groupj")
    group_styles <- sty[sty$i == "groupj" & is.na(sty$j), ]

    if (nrow(group_styles) > 0) {
      # Apply styling to all column group labels
      for (row_idx in seq_len(nrow(x@group_data_j))) {
        for (col_idx in seq_len(ncol(x@group_data_j))) {
          current_value <- x@group_data_j[row_idx, col_idx]
          if (!is.na(current_value) && !identical(trimws(current_value), "")) {
            # Apply the last (most recent) styling for each property
            for (style_idx in seq_len(nrow(group_styles))) {
              styles <- list(
                bold = if (!is.na(group_styles[style_idx, "bold"])) {
                  group_styles[style_idx, "bold"]
                } else {
                  FALSE
                },
                italic = if (!is.na(group_styles[style_idx, "italic"])) {
                  group_styles[style_idx, "italic"]
                } else {
                  FALSE
                },
                strikeout = if (!is.na(group_styles[style_idx, "strikeout"])) {
                  group_styles[style_idx, "strikeout"]
                } else {
                  FALSE
                },
                underline = if (!is.na(group_styles[style_idx, "underline"])) {
                  group_styles[style_idx, "underline"]
                } else {
                  FALSE
                },
                color = if (!is.na(group_styles[style_idx, "color"])) {
                  group_styles[style_idx, "color"]
                } else {
                  NULL
                },
                background = if (
                  !is.na(group_styles[style_idx, "background"])
                ) {
                  group_styles[style_idx, "background"]
                } else {
                  NULL
                }
              )
              x@group_data_j[row_idx, col_idx] <- style_string_grid(
                current_value,
                styles
              )
              current_value <- x@group_data_j[row_idx, col_idx]
            }
          }
        }
      }
    }
  }

  return(x)
}

style_eval_grid <- function(x) {
  # For grid formats, styling is handled inside build_eval_grid
  # This allows proper ordering of text styles before padding and background after padding
  return(x)
}


grid_colspan <- function(x) {
  if (!x@output %in% c("markdown", "gfm", "dataframe")) {
    return(x)
  }

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
          current_width <- ansi_nchar(spanned_content)
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

style_eval_grid <- function(x) {
  # For grid formats, styling is handled inside build_eval_grid
  # This allows proper ordering of text styles before padding and background after padding
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
    
    # Check if table has column names (header row exists)
    has_header <- length(colnames(x)) > 0 && any(nzchar(colnames(x)))
    
    if (is.na(sr$i) && is.na(sr$j)) {
      if (has_header) {
        cells <- expand.grid(i = c(0, seq_len(nrow(x))), j = seq_len(ncol(x)))
      } else {
        cells <- expand.grid(i = seq_len(nrow(x)), j = seq_len(ncol(x)))
      }
    } else if (is.na(sr$j)) {
      cells <- expand.grid(i = sr$i, j = seq_len(ncol(x)))
    } else if (is.na(sr$i)) {
      if (has_header) {
        cells <- expand.grid(i = c(0, seq_len(nrow(x))), j = sr$j)
      } else {
        cells <- expand.grid(i = seq_len(nrow(x)), j = sr$j)
      }
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

  # Apply styling to column groups (group_data_j) - this is already handled in style_grid_body
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


#' Apply text styling to table data before padding
#' @keywords internal
#' @noRd
style_grid_body <- function(x) {
  sty <- prepare_grid_style(x)

  if (nrow(sty) == 0) {
    return(x)
  }

  # Determine the styling function to use based on output type and ANSI setting
  style_string_grid <- if (isTRUE(x@ansi)) {
    style_string_ansi
  } else {
    style_string_markdown
  }

  # Apply text styling to each cell (excluding background)
  for (idx in seq_len(nrow(sty))) {
    row <- sty[idx, "i"]
    col <- sty[idx, "j"]

    # Prepare styles list for the current cell
    styles <- list(
      bold = isTRUE(sty[idx, "bold"]),
      italic = isTRUE(sty[idx, "italic"]),
      strikeout = isTRUE(sty[idx, "strikeout"]),
      underline = isTRUE(sty[idx, "underline"]),
      color = if (!is.na(sty[idx, "color"])) sty[idx, "color"] else NULL,
      indent = if (!is.na(sty[idx, "indent"])) sty[idx, "indent"] else NULL
    )

    # Handle column names (i = 0)
    if (row == 0) {
      current_name <- colnames(x)[col]
      if (!identical(trimws(current_name), "")) {
        colnames(x)[col] <- style_string_grid(current_name, styles)
      }
    } else if (row < 0) {
      # Handle group headers (negative i)
      if (nrow(x@group_data_j) > 0) {
        # Convert negative row index to positive index in group_data_j
        group_row <- abs(row)
        if (group_row <= nrow(x@group_data_j) && col <= ncol(x@group_data_j)) {
          current_value <- x@group_data_j[group_row, col]
          if (!is.na(current_value) && !identical(trimws(current_value), "")) {
            x@group_data_j[group_row, col] <- style_string_grid(
              current_value,
              styles
            )
          }
        }
      }
    } else {
      # Handle main table body (positive i)
      current_value <- x@data_body[row, col]
      if (!identical(trimws(current_value), "")) {
        x@data_body[row, col] <- style_string_grid(current_value, styles)
      }
    }

    # wipe adjacent cells for colspan/rowspan
    rowspan <- sty[idx, "rowspan"]
    colspan <- sty[idx, "colspan"]
    rowspan <- if (is.na(rowspan)) 1 else rowspan
    colspan <- if (is.na(colspan)) 1 else colspan
    wipe <- expand.grid(
      i = row:(row + rowspan - 1),
      j = col:(col + colspan - 1)
    )
    wipe <- wipe[
      !(wipe$i == row & wipe$j == col) &
        wipe$i >= 1 &
        wipe$i <= nrow(x@data_body) &
        wipe$j >= 1 &
        wipe$j <= ncol(x@data_body),
    ]
    if (nrow(wipe) > 0) {
      for (idx_wipe in seq_len(nrow(wipe))) {
        x@data_body[wipe$i[idx_wipe], wipe$j[idx_wipe]] <- ""
      }
    }
  }

  return(x)
}


#' Apply background styling to padded table matrix
#' @keywords internal
#' @noRd
style_grid_body_background <- function(tab, x, header) {
  sty <- prepare_grid_style(x)

  if (nrow(sty) == 0) {
    return(tab)
  }

  # Determine the styling function to use based on output type and ANSI setting
  style_string_grid <- if (isTRUE(x@ansi)) {
    style_string_ansi
  } else {
    style_string_markdown
  }

  # Apply only background styling to each cell
  for (idx in seq_len(nrow(sty))) {
    # Skip if no background styling
    if (is.na(sty[idx, "background"])) {
      next
    }

    row <- sty[idx, "i"]
    col <- sty[idx, "j"]

    # Prepare styles list with only background
    styles <- list(background = sty[idx, "background"])

    # Handle column names (i = 0)
    if (row == 0 && header) {
      tab_row <- 1 # Header is first row in tab matrix
      if (tab_row <= nrow(tab) && col <= ncol(tab)) {
        current_content <- tab[tab_row, col]
        tab[tab_row, col] <- style_string_grid(current_content, styles)
      }
    } else if (row > 0) {
      # Handle main table body (positive i)
      tab_row <- if (header) row + 1 else row # Adjust for header row
      if (tab_row <= nrow(tab) && col <= ncol(tab)) {
        current_content <- tab[tab_row, col]
        tab[tab_row, col] <- style_string_grid(current_content, styles)
      }
    }
    # Handle group headers would go here if needed (negative i)
  }

  return(tab)
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

style_eval_grid <- function(x) {
  out <- x@table_dataframe
  sty <- x@style

  if (nrow(sty) == 0) {
    return(x)
  } else {
    sty <- prepare_grid_style(x)
  }

  for (col in seq_along(out)) {
    out[[col]] <- as.character(out[[col]])
  }

  for (idx in seq_len(nrow(sty))) {
    row <- sty[idx, "i"]
    col <- sty[idx, "j"]
    bold <- sty[which(sty$i == row & sty$j == col), "bold"]
    italic <- sty[which(sty$i == row & sty$j == col), "italic"]
    strikeout <- sty[which(sty$i == row & sty$j == col), "strikeout"]
    rowspan <- sty[which(sty$i == row & sty$j == col), "rowspan"]
    colspan <- sty[which(sty$i == row & sty$j == col), "colspan"]
    if (isTRUE(bold)) {
      out[row, col] <- sprintf("**%s**", out[row, col])
    }
    if (isTRUE(italic)) {
      out[row, col] <- sprintf("*%s*", out[row, col])
    }
    if (isTRUE(strikeout)) {
      out[row, col] <- sprintf("~~%s~~", out[row, col])
    }
    if (!is.null(rowspan) || !is.null(colspan)) {
      idx_row <- if (isTRUE(rowspan > 1)) row + seq_len(rowspan) - 1 else row
      idx_col <- if (isTRUE(colspan > 1)) col + seq_len(colspan) - 1 else col
      backup <- out[row, col]
      for (w in idx_row) {
        for (z in idx_col) {
          if (z <= x@ncol) {
            out[w, z] <- ""
          }
        }
      }
      out[row, col] <- backup
    }
  }

  x@table_dataframe <- out
  return(x)
}


#' tinytable S4 method
#'
#' @keywords internal
setMethod(
  f = "style_eval",
  signature = "tinytable_grid",
  definition = style_eval_grid
)


grid_colspan <- function(x) {
  sty <- prepare_grid_style(x)

  if (nrow(sty) == 0) {
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

    if (target_line <= length(table_lines) && startsWith(table_lines[target_line], "|")) {
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
      if (col_idx <= length(cells) && (col_idx + colspan - 1) <= length(cells)) {
        # Combine content from spanned cells
        spanned_content <- paste(trimws(cells[col_idx:(col_idx + colspan - 1)]), collapse = "")

        # Calculate total width for the spanned cells
        if (!is.null(x@width_cols) && length(x@width_cols) >= (col_idx + colspan - 1)) {
          total_width <- sum(x@width_cols[col_idx:(col_idx + colspan - 1)]) + (colspan - 1)
          padding <- total_width - nchar(spanned_content)
          spanned_content <- paste0(spanned_content, strrep(" ", max(0, padding)))
        }

        # Replace the cells with the spanned content
        new_cells <- cells
        new_cells[col_idx] <- spanned_content
        # Remove the subsequent cells that are part of the colspan
        if (colspan > 1) {
          indices_to_remove <- (col_idx + 1):(col_idx + colspan - 1)
          new_cells <- new_cells[-indices_to_remove]
        }

        # Reconstruct the line
        table_lines[target_line] <- paste0("|", paste(new_cells, collapse = "|"), "|")
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
  
  all_i <- seq_len(nrow(x))
  idx_g <- x@group_index_i
  idx_d <- setdiff(all_i, idx_g)

  # expand i to full rows
  if (any(is.na(sty$i))) {
    alli <- data.frame(i = seq_len(nrow(x)))
    alli <- merge(
      alli,
      sty[is.na(sty$i), colnames(sty) != "i"],
      all = TRUE,
      sort = FALSE
    )
    sty <- rbind(sty, alli)
    sty <- sty[!is.na(sty$i), ]
    sty <- sty[order(sty$i, sty$j), ]
  }

  last <- function(k) {
    if (all(is.na(k))) {
      return(NA)
    }
    if (is.logical(k)) {
      return(as.logical(max(k, na.rm = TRUE)))
    }
    return(utils::tail(stats::na.omit(k), 1))
  }
  sty <- do.call(
    rbind,
    by(sty, list(sty$i, sty$j), function(k) {
      data.frame(lapply(k, last))
    })
  )

  # TODO: style groups
  sty <- sty[which(!sty$i %in% idx_g), ]

  if (nrow(sty) == 0) {
    return(x)
  }

  # user-supplied indices are post-groups
  # adjust indices to match original data rows since we only operate on those
  for (g in rev(idx_g)) {
    sty[sty$i > g, "i"] <- sty[sty$i > g, "i"] - 1
  }
  return(sty)
}

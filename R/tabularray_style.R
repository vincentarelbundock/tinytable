# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Clean style strings
#' @keywords internal
#' @noRd
clean_style_strings <- function(k) {
  k <- gsub("\\s*", "", k)
  k <- gsub(",+", ",", k)
  k <- gsub("^,", "", k, perl = TRUE)

  # Remove duplicates from each style string
  k <- sapply(k, function(style_string) {
    if (is.na(style_string) || trimws(style_string) == "") {
      return(style_string)
    }

    # Split by comma and remove duplicates
    parts <- trimws(strsplit(style_string, ",")[[1]])
    parts <- parts[parts != ""] # Remove empty parts
    unique_parts <- unique(parts)

    # Rejoin with proper spacing
    paste(unique_parts, collapse = ", ")
  }, USE.NAMES = FALSE)

  k <- trimws(k)
  return(k)
}

#' Prepare d-column styling
#' @keywords internal
#' @noRd
prepare_dcolumn <- function(x, sty) {
  if (nrow(sty) > 0) {
    dcol_j <- sty[grepl("^d$", sty[["align"]]), "j"]
    dcol_j <- if (length(dcol_j) == 0) NULL else unique(dcol_j)
    for (idx_j in dcol_j) {
      spec <- calculate_dcolumn_spec(idx_j, x)
      spec <- sprintf("column{%s}={%s}\n", idx_j, spec)
      x@table_string <- insert_tabularray_content(
        x@table_string,
        content = spec,
        type = "inner"
      )
      for (idx_i in seq_len(x@nhead)) {
        spec <- paste(
          sprintf("cell{%s}{%s}={guard,halign=c,},", idx_i, idx_j),
          collapse = "\n"
        )
        x@table_string <- insert_tabularray_content(
          x@table_string,
          content = spec,
          type = "inner"
        )
      }
    }
  }
  return(x)
}

#' Generate tabularray column specifications
#' @keywords internal
#' @noRd
tabularray_columns <- function(x, rec) {
  # Complete columns (first because of d-column)
  cols <- unique(
    rec[
      (rec$span != "" | rec$set != "") & rec$complete_column,
      c("j", "set", "span"),
      drop = FALSE
    ]
  )

  spec <- by(cols, list(cols$set, cols$span), function(k) {
    sprintf("column{%s}={%s}{%s}", latex_range_string(k$j), k$span, k$set)
  })
  spec <- unique(as.vector(unlist(spec)))

  for (s in spec) {
    x@table_string <- insert_tabularray_content(
      x@table_string,
      content = s,
      type = "inner"
    )
  }

  return(x)
}

#' Generate tabularray row specifications
#' @keywords internal
#' @noRd
tabularray_rows <- function(x, rec) {
  # Complete rows
  rows <- unique(
    rec[
      (rec$span != "" | rec$set != "") &
        rec$complete_row &
        !rec$complete_column,
      c("i", "set", "span"),
      drop = FALSE
    ]
  )

  spec <- by(rows, list(rows$set, rows$span), function(k) {
    sprintf("row{%s}={%s}{%s}", paste(k$i, collapse = ","), k$span, k$set)
  })
  spec <- unique(as.vector(unlist(spec)))

  for (s in spec) {
    x@table_string <- insert_tabularray_content(
      x@table_string,
      content = s,
      type = "inner"
    )
  }

  return(x)
}

#' Generate tabularray cell specifications
#' @keywords internal
#' @noRd
tabularray_cells <- function(x, rec) {
  # Individual cells
  cells <- unique(
    rec[
      (rec$span != "" | rec$set != "") &
        !rec$complete_row &
        !rec$complete_column, ,
      drop = FALSE
    ]
  )

  if (nrow(cells) > 0) {
    # need to split by j otherwise we can end up with rectangular index that
    # cover cells that should not be styled
    cellsplit <- split(cells, list(cells$j, cells$set, cells$span))
    cellsplit <- Filter(function(k) nrow(k) > 0, cellsplit)
    spec <- sapply(cellsplit, function(cells) {
      sprintf(
        "cell{%s}{%s}={%s}{%s}",
        latex_range_string(cells$i),
        cells$j[1],
        cells$span[1],
        cells$set[1]
      )
    })
    spec <- sort(unique(as.vector(unlist(spec))))

    for (s in spec) {
      x@table_string <- insert_tabularray_content(
        x@table_string,
        content = s,
        type = "inner"
      )
    }
  }

  return(x)
}



#' Apply tabularray specifications
#' @keywords internal
#' @noRd
apply_tabularray_specs <- function(x) {
  for (spec in unique(stats::na.omit(x@tabularray_inner))) {
    x@table_string <- insert_tabularray_content(
      x@table_string,
      content = spec,
      type = "inner"
    )
  }

  for (spec in unique(stats::na.omit(x@tabularray_outer))) {
    x@table_string <- insert_tabularray_content(
      x@table_string,
      content = spec,
      type = "outer"
    )
  }

  return(x)
}

#' Process tabularray lines using expanded style data
#' @keywords internal
#' @noRd
process_tabularray_lines <- function(x, lines) {
  if (is.null(lines) || nrow(lines) == 0) {
    return(x)
  }

  # Adjust i values for header offset
  lines$i <- lines$i + x@nhead

  # Normalize colors once and define preambles
  line_color_map <- build_color_map(lines$line_color, "tabularray")
  for (col in line_color_map) {
    x <- define_color_preamble(x, col)
  }
  lines$line_color_mapped <- apply_color_map(lines$line_color, line_color_map)

  # Process horizontal lines
  hlines <- lines[!is.na(lines$line) & grepl("b|t", lines$line), ]
  if (nrow(hlines) > 0) {
    x <- process_horizontal_lines(x, hlines)
  }

  # Process vertical lines
  vlines <- lines[!is.na(lines$line) & grepl("l|r", lines$line), ]
  if (nrow(vlines) > 0) {
    x <- process_vertical_lines(x, vlines)
  }

  return(x)
}

#' Process tabularray other styles using expanded style data
#' @keywords internal
#' @noRd
process_tabularray_other_styles <- function(x, other) {
  if (is.null(other) || nrow(other) == 0) {
    # Apply tabularray specifications even if no other styles
    x <- apply_tabularray_specs(x)
    return(x)
  }

  # Adjust i values for header offset and map alignv values
  other$i <- other$i + x@nhead
  other$alignv <- map_alignv(other$alignv, "tabularray")

  # Create record grid
  rec <- expand.grid(
    i = c(seq_len(x@nrow + x@nhead)),
    j = seq_len(x@ncol)
  )

  set <- span <- rep("", nrow(rec))

  # Prepare d-columns (special case)
  x <- prepare_dcolumn(x, other)

  # Normalize color map once and define preambles
  color_map <- build_color_map(c(other$color, other$background), "tabularray")
  for (col in color_map) {
    x <- define_color_preamble(x, col)
  }

  # Build style strings directly from columns. `other` is the filtered
  # rectangular style grid, so each row already represents one concrete cell;
  # extracting one-row data frames and rescanning `rec` is unnecessary.
  yes <- function(z) !is.na(z) & z
  font_cmds <- paste0(
    ifelse(yes(other$bold), "\\bfseries", ""),
    ifelse(yes(other$italic), "\\itshape", ""),
    ifelse(yes(other$monospace), "\\ttfamily", ""),
    ifelse(yes(other$smallcap), "\\scshape", "")
  )
  cmd_strs <- paste0(
    ifelse(yes(other$underline), "\\tinytableTabularrayUnderline", ""),
    ifelse(yes(other$strikeout), "\\tinytableTabularrayStrikeout", "")
  )

  col_idx <- which(!is.na(other$color) & other$color %in% names(color_map))
  if (length(col_idx)) {
    mapped <- unname(color_map[other$color[col_idx]])
    mapped <- sub("^#", "c", mapped)
    cmd_strs[col_idx] <- sprintf("%s, fg=%s", cmd_strs[col_idx], mapped)
  }
  bg_idx <- which(!is.na(other$background) & other$background %in% names(color_map))
  if (length(bg_idx)) {
    mapped <- unname(color_map[other$background[bg_idx]])
    mapped <- sub("^#", "c", mapped)
    cmd_strs[bg_idx] <- sprintf("%s, bg=%s", cmd_strs[bg_idx], mapped)
  }

  font_sets <- rep("", nrow(other))
  fontsize <- suppressWarnings(as.numeric(other$fontsize))
  idx <- which(!is.na(fontsize))
  if (length(idx)) {
    font_cmds[idx] <- sprintf(
      "%s\\fontsize{%sem}{%sem}\\selectfont",
      font_cmds[idx],
      format_markup_num(fontsize[idx]),
      format_markup_num(fontsize[idx] + 0.3)
    )
  }
  idx <- which(!is.na(other$align) & !grepl("d", other$align))
  font_sets[idx] <- sprintf("%s, halign=%s,", font_sets[idx], other$align[idx])
  idx <- which(!is.na(other$alignv))
  font_sets[idx] <- sprintf("%s, valign=%s,", font_sets[idx], other$alignv[idx])
  idx <- which(!is.na(other$indent) & other$indent > 0)
  if (length(idx)) {
    font_sets[idx] <- sprintf(
      "%s preto={\\hspace{%sem}},",
      font_sets[idx],
      format_markup_num(other$indent[idx])
    )
  }

  span_strs <- rep("", nrow(other))
  idx <- which(!is.na(other$colspan))
  span_strs[idx] <- paste0(span_strs[idx], "c=", other$colspan[idx], ",")
  idx <- which(!is.na(other$rowspan))
  span_strs[idx] <- paste0(span_strs[idx], "r=", other$rowspan[idx], ",")
  if (length(x@width) == ncol(x)) {
    for (row in which(!is.na(other$colspan))) {
      cols <- other$j[row]:(other$j[row] + other$colspan[row] - 1)
      w <- sum(x@width[cols])
      font_sets[row] <- paste(font_sets[row], sprintf("wd=%s\\linewidth,", format_markup_num(w)))
    }
  }

  n_i <- x@nrow + x@nhead
  target_idx <- other$i + (other$j - 1L) * n_i
  valid_target <- !is.na(target_idx) & target_idx >= 1L & target_idx <= nrow(rec)

  # Preserve append semantics defensively if duplicate coordinates ever reach
  # this function, while avoiding a full-grid logical scan for every row.
  for (row in which(valid_target)) {
    idx <- target_idx[row]

    # Add font styling if present
    font_cmd <- font_cmds[row]
    if (trimws(font_cmd) != "") {
      set[idx] <- sprintf("%s font=%s, ", set[idx], font_cmd)
    }

    # Format command string for remaining cmd styles
    cmd <- cmd_strs[row]
    if (grepl("^,", cmd)) {
      tmp <- "%s, %s, "
    } else {
      tmp <- "%s, cmd=%s, "
    }
    if (trimws(cmd) != "") {
      set[idx] <- sprintf(tmp, set[idx], cmd)
    }

    # Add font settings
    set[idx] <- paste0(set[idx], font_sets[row])

    # Add spans
    span[idx] <- paste0(span[idx], span_strs[row])
  }

  # Only styled cells need the comparatively expensive split/dedup cleanup.
  used <- unique(target_idx[valid_target])
  set[used] <- clean_style_strings(set[used])
  span[used] <- clean_style_strings(span[used])
  rec$set <- set
  rec$span <- span

  # Mark complete rows and columns
  all_i <- seq_len(x@nrow + x@nhead)
  all_j <- seq_len(x@ncol)

  # rec contains one row per (i, j), so a style group covers a complete column
  # or row exactly when its group size equals the corresponding dimension.
  # This replaces two by()/transform()/rbind() cycles over the full grid.
  col_group <- interaction(rec$j, rec$set, rec$span, drop = TRUE)
  col_size <- tabulate(as.integer(col_group))
  rec$complete_column <- col_size[as.integer(col_group)] == length(all_i)

  row_group <- interaction(rec$i, rec$set, rec$span, drop = TRUE)
  row_size <- tabulate(as.integer(row_group))
  rec$complete_row <- row_size[as.integer(row_group)] == length(all_j)

  # Generate tabularray specifications
  x <- tabularray_columns(x, rec)
  x <- tabularray_rows(x, rec)
  x <- tabularray_cells(x, rec)

  # Apply tabularray specifications
  x <- apply_tabularray_specs(x)

  return(x)
}

#' Process horizontal lines from expanded style data
#' @keywords internal
#' @noRd
process_horizontal_lines <- function(x, hlines) {
  # Vectorize line specification building
  # Use precomputed line_color_mapped
  line_colors <- ifelse(
    grepl("^#", hlines$line_color_mapped),
    sub("^#", "c", hlines$line_color_mapped),
    hlines$line_color_mapped
  )

  line_widths <- ifelse(is.na(hlines$line_width), 0.1, hlines$line_width)
  line_specs <- sprintf("solid, %s, %sem", line_colors, format_markup_num(line_widths))

  # Add trimming vectorized
  has_trim <- !is.na(hlines$line_trim) & nzchar(hlines$line_trim)
  trim_l <- has_trim & grepl("l", hlines$line_trim)
  trim_r <- has_trim & grepl("r", hlines$line_trim)

  line_specs <- ifelse(trim_l, paste0(line_specs, ", l=-0.5"), line_specs)
  line_specs <- ifelse(trim_r, paste0(line_specs, ", r=-0.5"), line_specs)

  # Adjust row index based on line type (vectorized)
  rows <- ifelse(grepl("b", hlines$line), hlines$i + 1, hlines$i)

  # Build horizontal dataframe
  horizontal <- data.frame(
    i = rows,
    j = hlines$j,
    lin = line_specs,
    line = hlines$line,
    stringsAsFactors = FALSE
  )

  spec <- by(horizontal, list(horizontal$i, horizontal$lin), function(k) {
    ival <- latex_range_string(k$i)
    jval <- latex_range_string(k$j)
    lin_val <- k$lin[1]
    # Skip invalid line specifications
    if (is.na(lin_val) || lin_val == "" || ival == "" || jval == "") {
      return(NULL)
    }
    sprintf("hline{%s}={%s}{%s}", ival, jval, lin_val)
  })
  spec <- unique(as.vector(unlist(spec)))
  # Remove any NULL or NA entries
  spec <- spec[!is.na(spec) & spec != "NULL"]

  for (s in spec) {
    x@table_string <- insert_tabularray_content(
      x@table_string,
      content = s,
      type = "inner"
    )
  }

  return(x)
}

#' Process vertical lines from expanded style data
#' @keywords internal
#' @noRd
process_vertical_lines <- function(x, vlines) {
  # Vectorize line specification building
  # Use precomputed line_color_mapped
  line_colors <- ifelse(
    grepl("^#", vlines$line_color_mapped),
    sub("^#", "c", vlines$line_color_mapped),
    vlines$line_color_mapped
  )

  line_widths <- ifelse(is.na(vlines$line_width), 0.1, vlines$line_width)
  line_specs <- sprintf("solid, %s, %sem", line_colors, format_markup_num(line_widths))

  # Add trimming vectorized
  has_trim <- !is.na(vlines$line_trim) & nzchar(vlines$line_trim)
  trim_l <- has_trim & grepl("l", vlines$line_trim)
  trim_r <- has_trim & grepl("r", vlines$line_trim)

  line_specs <- ifelse(trim_l, paste0(line_specs, ", l=-0.5"), line_specs)
  line_specs <- ifelse(trim_r, paste0(line_specs, ", r=-0.5"), line_specs)

  # Adjust column index based on line type (vectorized)
  col_idx <- ifelse(grepl("r", vlines$line), vlines$j + 1, vlines$j)

  # Build vertical dataframe
  vertical <- data.frame(
    i = vlines$i,
    j = col_idx,
    lin = line_specs,
    line = vlines$line,
    stringsAsFactors = FALSE
  )

  spec <- by(vertical, list(vertical$lin), function(k) {
    ival <- latex_range_string(k$i)
    jval <- latex_range_string(k$j)
    lin_val <- k$lin[1]
    # Skip invalid line specifications
    if (is.na(lin_val) || lin_val == "" || ival == "" || jval == "") {
      return(NULL)
    }
    sprintf("vline{%s}={%s}{%s}", jval, ival, lin_val)
  })
  spec <- unique(as.vector(unlist(spec)))
  # Remove any NULL or NA entries
  spec <- spec[!is.na(spec) & spec != "NULL"]

  for (s in spec) {
    x@table_string <- insert_tabularray_content(
      x@table_string,
      content = s,
      type = "inner"
    )
  }

  return(x)
}

# =============================================================================
# MAIN METHOD
# =============================================================================

setMethod(
  f = "style_eval",
  signature = "tinytable_tabularray",
  definition = function(
      x,
      i = NULL,
      j = NULL,
      bold = FALSE,
      italic = FALSE,
      monospace = FALSE,
      underline = FALSE,
      strikeout = FALSE,
      color = NULL,
      background = NULL,
      fontsize = NULL,
      align = NULL,
      alignv = NULL,
      line = NULL,
      line_color = "black",
      line_width = 0.1,
      colspan = NULL,
      rowspan = NULL,
      indent = 0,
      ...) {
    # Use populated @style_other / @style_lines from build_tt()
    inputs <- style_backend_inputs(x, STYLE_PROPS_TABULARRAY)
    other <- inputs$other
    lines <- inputs$lines

    # Process lines using the expanded data
    x <- process_tabularray_lines(x, lines)

    # Process other styles using the expanded data
    x <- process_tabularray_other_styles(x, other)

    return(x)
  })

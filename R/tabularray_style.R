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
      spec <- sprintf("cell{%s}{%s}={guard,halign=c,},", seq_len(x@nhead), idx_j)
      x@table_string <- insert_tabularray_content(
        x@table_string,
        content = spec,
        type = "inner"
      )
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

  x@table_string <- insert_tabularray_content(
    x@table_string,
    content = spec,
    type = "inner"
  )

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
    sprintf("row{%s}={%s}{%s}", latex_range_string(k$i), k$span, k$set)
  })
  spec <- unique(as.vector(unlist(spec)))

  x@table_string <- insert_tabularray_content(
    x@table_string,
    content = spec,
    type = "inner"
  )

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

    x@table_string <- insert_tabularray_content(
      x@table_string,
      content = spec,
      type = "inner"
    )
  }

  return(x)
}



#' Apply tabularray specifications
#' @keywords internal
#' @noRd
apply_tabularray_specs <- function(x) {
  x@table_string <- insert_tabularray_content(
    x@table_string,
    content = unique(as.vector(stats::na.omit(x@tabularray_inner))),
    type = "inner"
  )

  x@table_string <- insert_tabularray_content(
    x@table_string,
    content = unique(as.vector(stats::na.omit(x@tabularray_outer))),
    type = "outer"
  )

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
    x <- process_tabularray_axis_lines(x, hlines, axis = "i")
  }

  # Process vertical lines
  vlines <- lines[!is.na(lines$line) & grepl("l|r", lines$line), ]
  if (nrow(vlines) > 0) {
    x <- process_tabularray_axis_lines(x, vlines, axis = "j")
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

#' Process horizontal or vertical lines from expanded style data
#'
#' Shared implementation for hlines (`axis = "i"`) and vlines (`axis = "j"`).
#' Lines are grouped by (position, line spec) so that disjoint requests
#' sharing the same style never merge into a rectangular cross-product.
#' @keywords internal
#' @noRd
process_tabularray_axis_lines <- function(x, lines, axis) {
  # Vectorize line specification building
  # Use precomputed line_color_mapped
  line_colors <- ifelse(
    grepl("^#", lines$line_color_mapped),
    sub("^#", "c", lines$line_color_mapped),
    lines$line_color_mapped
  )

  line_widths <- ifelse(is.na(lines$line_width), 0.1, lines$line_width)
  line_types <- if ("line_type" %in% names(lines)) lines$line_type else NA_character_
  line_types <- ifelse(is.na(line_types), "solid", line_types)
  line_specs <- sprintf(
    "%s, %s, %sem", line_types, line_colors, format_markup_num(line_widths)
  )

  # Add trimming vectorized
  has_trim <- !is.na(lines$line_trim) & nzchar(lines$line_trim)
  trim_l <- has_trim & grepl("l", lines$line_trim)
  trim_r <- has_trim & grepl("r", lines$line_trim)

  line_specs <- ifelse(trim_l, paste0(line_specs, ", l=-0.5"), line_specs)
  line_specs <- ifelse(trim_r, paste0(line_specs, ", r=-0.5"), line_specs)

  if (axis == "i") {
    # "b" draws below the row: shift to the next hline slot (vectorized)
    pos <- ifelse(grepl("b", lines$line), lines$i + 1, lines$i)
    extent <- lines$j
    template <- "hline{%s}={%s}{%s}"
  } else {
    # "r" draws right of the column: shift to the next vline slot (vectorized)
    pos <- ifelse(grepl("r", lines$line), lines$j + 1, lines$j)
    extent <- lines$i
    template <- "vline{%s}={%s}{%s}"
  }

  dat <- data.frame(
    pos = pos,
    extent = extent,
    lin = line_specs,
    stringsAsFactors = FALSE
  )

  spec <- by(dat, list(dat$pos, dat$lin), function(k) {
    pos_val <- latex_range_string(k$pos)
    extent_val <- latex_range_string(k$extent)
    lin_val <- k$lin[1]
    # Skip invalid line specifications
    if (is.na(lin_val) || lin_val == "" || pos_val == "" || extent_val == "") {
      return(NULL)
    }
    sprintf(template, pos_val, extent_val, lin_val)
  })
  spec <- unique(as.vector(unlist(spec)))
  # Remove any NULL or NA entries
  spec <- spec[!is.na(spec) & spec != "NULL"]

  x@table_string <- insert_tabularray_content(
    x@table_string,
    content = spec,
    type = "inner"
  )

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

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Map alignv values to tabularray format
#' @keywords internal
#' @noRd
map_alignv_values <- function(sty) {
  sty$alignv[which(sty$alignv == "b")] <- "f"
  sty$alignv[which(sty$alignv == "t")] <- "h"
  sty$alignv[which(sty$alignv == "m")] <- "m"
  return(sty)
}

#' Build LaTeX style commands for text style
#' @keywords internal
#' @noRd
style_text <- function(sty_row) {
  font <- ""
  cmd <- ""
  
  # Use descriptive font commands for tabularray font key
  if (isTRUE(sty_row$bold)) {
    font <- paste0(font, "\\bfseries")
  }
  if (isTRUE(sty_row$italic)) {
    font <- paste0(font, "\\itshape")
  }
  if (isTRUE(sty_row$monospace)) {
    font <- paste0(font, "\\ttfamily")
  }
  if (isTRUE(sty_row$smallcap)) {
    font <- paste0(font, "\\scshape")
  }
  
  # Keep underline and strikeout as cmd since they need special macros
  if (isTRUE(sty_row$underline)) {
    cmd <- paste0(cmd, "\\tinytableTabularrayUnderline")
  }
  if (isTRUE(sty_row$strikeout)) {
    cmd <- paste0(cmd, "\\tinytableTabularrayStrikeout")
  }
  
  return(list(font = font, cmd = cmd))
}

#' Style colors for tabularray
#' @keywords internal
#' @noRd
style_colors <- function(cmd, sty_row, x) {
  # Text color
  col <- sty_row$color
  if (!is.na(col)) {
    col <- standardize_colors(col, format = "tabularray")
    x <- define_color_preamble(x, col)
    if (grepl("^#", col)) {
      col <- sub("^#", "c", col)
    }
    cmd <- sprintf("%s, fg=%s", cmd, col)
  }

  # Background color
  bg <- sty_row$background
  if (!is.na(bg)) {
    bg <- standardize_colors(bg, format = "tabularray")
    x <- define_color_preamble(x, bg)
    if (grepl("^#", bg)) {
      bg <- sub("^#", "c", bg)
    }
    cmd <- sprintf("%s, bg=%s", cmd, bg)
  }

  return(list(cmd = cmd, x = x))
}

#' Style fonts for tabularray
#' @keywords internal
#' @noRd
style_fonts <- function(set, sty_row) {
  # Font size
  fontsize <- sty_row$fontsize
  if (!is.na(as.numeric(fontsize))) {
    set <- sprintf(
      "%s font=\\fontsize{%sem}{%sem}\\selectfont,",
      set,
      fontsize,
      fontsize + 0.3
    )
  }

  # Horizontal alignment
  halign <- sty_row$align
  if (!is.na(halign) && !grepl("d", halign)) {
    set <- sprintf("%s, halign=%s,", set, halign)
  }

  # Vertical alignment
  alignv <- sty_row$alignv
  if (!is.na(alignv)) {
    set <- sprintf("%s, valign=%s,", set, alignv)
  }

  # Indentation
  indent <- sty_row$indent
  if (isTRUE(indent > 0)) {
    set <- sprintf("%s preto={\\hspace{%sem}},", set, indent)
  }

  return(set)
}

#' Style spans for tabularray
#' @keywords internal
#' @noRd
style_spans <- function(span, sty_row) {
  if ("colspan" %in% names(sty_row) && !is.na(sty_row$colspan)) {
    span <- paste0(span, "c=", sty_row$colspan, ",")
  }
  if ("rowspan" %in% names(sty_row) && !is.na(sty_row$rowspan)) {
    span <- paste0(span, "r=", sty_row$rowspan, ",")
  }
  return(span)
}

# style_lines function removed - line processing now handled directly in tabularray_hlines

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
    parts <- parts[parts != ""]  # Remove empty parts
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
  all_i <- seq_len(x@nrow + x@nhead)
  all_j <- seq_len(x@ncol)

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
  all_j <- seq_len(x@ncol)

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
  all_j <- seq_len(x@ncol)

  # Individual cells
  cells <- unique(
    rec[
      (rec$span != "" | rec$set != "") &
        !rec$complete_row &
        !rec$complete_column,
      ,
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
apply_tabularray_specs <- function(x, sty) {
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
  unique_line_colors <- unique(lines$line_color[!is.na(lines$line_color)])
  if (length(unique_line_colors) > 0) {
    line_color_map <- stats::setNames(
      sapply(unique_line_colors, standardize_colors, format = "tabularray", USE.NAMES = FALSE),
      unique_line_colors
    )
    # Define color preambles once
    for (col in line_color_map) {
      x <- define_color_preamble(x, col)
    }
    # Map colors in lines dataframe
    lines$line_color_mapped <- ifelse(
      !is.na(lines$line_color) & lines$line_color %in% names(line_color_map),
      line_color_map[lines$line_color],
      "black"
    )
  } else {
    lines$line_color_mapped <- "black"
  }

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
    x <- apply_tabularray_specs(x, NULL)
    return(x)
  }

  # Adjust i values for header offset and map alignv values
  other$i <- other$i + x@nhead
  other <- map_alignv_values(other)

  # Create record grid
  rec <- expand.grid(
    i = c(seq_len(x@nrow + x@nhead)),
    j = seq_len(x@ncol)
  )

  set <- span <- rep("", nrow(rec))

  # Prepare d-columns (special case)
  x <- prepare_dcolumn(x, other)

  # Normalize color map once
  unique_colors <- c(
    unique(other$color[!is.na(other$color)]),
    unique(other$background[!is.na(other$background)])
  )
  unique_colors <- unique(unique_colors)
  if (length(unique_colors) > 0) {
    color_map <- stats::setNames(
      sapply(unique_colors, standardize_colors, format = "tabularray", USE.NAMES = FALSE),
      unique_colors
    )
    # Define color preambles once
    for (col in color_map) {
      x <- define_color_preamble(x, col)
    }
  } else {
    color_map <- character(0)
  }

  # Vectorize style building per row
  font_cmds <- character(nrow(other))
  cmd_strs <- character(nrow(other))
  font_sets <- character(nrow(other))
  span_strs <- character(nrow(other))

  for (row in seq_len(nrow(other))) {
    # Build style commands
    text_style <- style_text(other[row, ])
    font_cmds[row] <- text_style$font
    cmd_strs[row] <- text_style$cmd

    # Style colors using precomputed map
    cmd <- cmd_strs[row]
    col <- other$color[row]
    if (!is.na(col) && col %in% names(color_map)) {
      col_mapped <- color_map[col]
      if (grepl("^#", col_mapped)) {
        col_mapped <- sub("^#", "c", col_mapped)
      }
      cmd <- sprintf("%s, fg=%s", cmd, col_mapped)
    }

    bg <- other$background[row]
    if (!is.na(bg) && bg %in% names(color_map)) {
      bg_mapped <- color_map[bg]
      if (grepl("^#", bg_mapped)) {
        bg_mapped <- sub("^#", "c", bg_mapped)
      }
      cmd <- sprintf("%s, bg=%s", cmd, bg_mapped)
    }
    cmd_strs[row] <- cmd

    # Style fonts
    font_sets[row] <- style_fonts("", other[row, ])

    # Style spans
    span_strs[row] <- style_spans("", other[row, ])
  }

  # Apply styling to record grid (still need loop for NA expansion)
  for (row in seq_len(nrow(other))) {
    # Find matching cells in record grid
    idx_i <- other$i[row]
    if (is.na(idx_i)) {
      idx_i <- unique(rec$i)
    }
    idx_j <- other$j[row]
    if (is.na(idx_j)) {
      idx_j <- unique(rec$j)
    }
    idx <- rec$i %in% idx_i & rec$j %in% idx_j

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

  # Clean style strings
  rec$set <- clean_style_strings(set)
  rec$span <- clean_style_strings(span)

  # Mark complete rows and columns
  all_i <- seq_len(x@nrow + x@nhead)
  all_j <- seq_len(x@ncol)

  rec <- do.call(
    rbind,
    by(rec, list(rec$j, rec$set, rec$span), function(k) {
      transform(k, complete_column = all(all_i %in% k$i))
    })
  )
  rec <- do.call(
    rbind,
    by(rec, list(rec$i, rec$set, rec$span), function(k) {
      transform(k, complete_row = all(all_j %in% k$j))
    })
  )

  # Generate tabularray specifications
  x <- tabularray_columns(x, rec)
  x <- tabularray_rows(x, rec)
  x <- tabularray_cells(x, rec)

  # Apply tabularray specifications
  x <- apply_tabularray_specs(x, other)

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
  line_specs <- sprintf("solid, %s, %sem", line_colors, line_widths)

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
  line_specs <- sprintf("solid, %s, %sem", line_colors, line_widths)

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
    ...
  ) {
    # Use populated @style_other from build_tt()
    other <- x@style_other

    # Filter to only cells that have actual styles
    if (nrow(other) > 0) {
      has_style <- rowSums(!is.na(other[, c("bold", "italic", "underline", "strikeout",
                                             "monospace", "smallcap", "align", "alignv",
                                             "color", "background", "fontsize", "indent",
                                             "colspan", "rowspan"), drop = FALSE])) > 0
      other <- other[has_style, , drop = FALSE]
    }

    # Use populated @style_lines from build_tt()
    lines <- x@style_lines
    if (nrow(lines) == 0) {
      lines <- NULL
    }

    # Process lines using the expanded data
    x <- process_tabularray_lines(x, lines)

    # Process other styles using the expanded data
    x <- process_tabularray_other_styles(x, other)

    return(x)
  }
)

insert_tabularray_content <- function(x, content = NULL, type = "body") {
  out <- x

  out <- strsplit(out, "\n")[[1]]
  comment <- switch(
    type,
    "body" = "% tabularray inner close",
    "outer" = "% tabularray outer close",
    "inner" = "% tabularray inner close"
  )
  idx <- grep(comment, out)

  if (length(content) > 0) {
    content <- trimws(content)
    if (!grepl(",$", content) && type != "body") {
      content <- paste0(content, ",")
    }
    if (type == "body") {
      out <- c(out[1:idx], content, out[(idx + 1):length(out)])
    } else {
      out <- c(out[1:(idx - 1)], content, out[idx:length(out)])
    }
  }

  out <- paste(out, collapse = "\n")

  return(out)
}

define_color_preamble <- function(x, col) {
  if (grepl("^#", col)) {
    # hex color need to be defined in LaTeX
    col <- sub("^#", "c", col)
    regex <- sprintf("DefineColor.*%s", col)
    if (!grepl(regex, x@table_string)) {
      b <- sprintf(
        "\\tinytableDefineColor{%s}{HTML}{%s}",
        col,
        sub("^c", "", col)
      )
      x@table_string <- insert_tabularray_content(
        x@table_string,
        content = b,
        type = "body"
      )
    }
  }
  return(x)
}

calculate_dcolumn_spec <- function(j, x) {
  siunitx <- get_option(
    "tinytable_siunitx_table_format",
    default = "table-format=-%s.%s,table-align-text-before=false,table-align-text-after=false,input-symbols={-,\\*+()}"
  )
  num <- unlist(x@data_body[, j])

  # empty cells
  num <- sapply(num, trimws)
  num <- num[sapply(num, nchar) > 0]

  num <- strsplit(num, "\\.")
  num <- lapply(num, function(k) if (length(k) == 1) c(k, " ") else k)

  left <- sapply(num, function(k) k[[1]])
  right <- sapply(num, function(k) k[[2]])
  left <- max(nchar(gsub("\\D", "", left)))
  right <- max(nchar(gsub("\\D", "", right)))
  out <- sprintf(siunitx, left, right)
  out <- sprintf("si={%s},", out)
  return(out)
}

latex_range_string <- function(x) {
  if (length(x) == 0) {
    return("")
  }
  x <- sort(unique(x))
  start <- x[c(TRUE, diff(x) != 1)]
  end <- x[c(diff(x) != 1, TRUE)]
  parts <- ifelse(start == end, start, paste0(start, "-", end))
  paste(parts, collapse = ",")
}

## not longer used, but took a while to collect and might be useful in the future
# out <- list(
#   rows_keys = c("halign", "valign", "ht", "bg", "fg", "font", "mode", "cmd", "abovesep", "belowsep", "rowsep", "preto", "appto", "indent"),
#   columns_keys = c("halign", "valign", "wd", "co", "bg", "fg", "font", "mode", "cmd", "leftsep", "rightsep", "colsep", "preto", "appto", "indent"),
#   hborders_keys = c("pagebreak", "abovespace", "belowspace"),
#   vborders_keys = c("leftspace", "rightspace"),
#   cells_keys = c("halign", "valign", "wd", "bg", "fg", "font", "mode", "cmd", "preto", "appto"),
#   outer_specs_keys = c("baseline", "long", "tall", "expand"),
#   inner_specs_keys = c("rulesep", "hlines", "vline", "hline", "vlines", "stretch", "abovesep", "belowsep", "rowsep", "leftsep", "rightsep", "colsep", "hspan", "vspan", "baseline"),
#   span = c("r", "c")
# )

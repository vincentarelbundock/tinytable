#' Map alignment values to Typst equivalents
#' @keywords internal
#' @noRd
typst_map_alignments <- function(sty) {
  sty$align[which(sty$align == "l")] <- "left"
  sty$align[which(sty$align == "c")] <- "center"
  sty$align[which(sty$align == "d")] <- "center"
  sty$align[which(sty$align == "r")] <- "right"

  sty$alignv[which(sty$alignv == "t")] <- "top"
  sty$alignv[which(sty$alignv == "m")] <- "horizon"
  sty$alignv[which(sty$alignv == "b")] <- "bottom"

  return(sty)
}

#' Insert field into Typst style string
#' @keywords internal
#' @noRd
typst_insert_field <- function(x, name = "bold", value = "true") {
  old <- sprintf("%s: [^,]*,", name)
  new <- sprintf("%s: %s,", name, value)
  out <- ifelse(grepl(old, x), sub(old, new, x), sprintf("%s, %s", x, new))
  return(out)
}

#' Clean Typst style string
#' @keywords internal
#' @noRd
typst_clean_css <- function(css) {
  css <- gsub(" +", " ", trimws(css))
  css <- sub("^,", "", trimws(css))
  css <- gsub(",+", ",", trimws(css))
  return(css)
}

#' Apply Typst styles and generate style-dict/array
#' @keywords internal
#' @noRd
typst_apply_styles <- function(x, rec) {
  # Filter out empty styles
  idx <- rec$css != ""
  rec <- rec[idx, , drop = FALSE]
  if (nrow(rec) == 0) {
    return(x)
  }

  # Get unique styles
  uni <- split(rec, rec$css)

  # Create style-array (unique styles)
  style_array_entries <- sapply(uni, function(x) {
    style_str <- x$css[1]
    sprintf("(%s),", style_str)
  })

  # Create style-dict (cell positions -> style array indices)
  style_dict_entries <- character(0)
  for (style_idx in seq_along(uni)) {
    cells <- uni[[style_idx]]
    entry <- sprintf('"%s_%s": %s', cells$i, cells$j, style_idx - 1)
    style_dict_entries <- c(style_dict_entries, entry)
  }

  # Remove duplicate keys (keep last occurrence for each coordinate)
  if (length(style_dict_entries) > 0) {
    keys <- gsub('"([^"]+)":\\s*\\d+', '\\1', style_dict_entries)
    style_dict_entries <- style_dict_entries[!duplicated(keys, fromLast = TRUE)]
  }

  # Insert style-dict entries as single line
  if (length(style_dict_entries) > 0) {
    combined_dict <- paste0(
      "    ",
      paste(style_dict_entries, collapse = ", ")
    )
    x@table_string <- lines_insert(
      x@table_string,
      combined_dict,
      "tinytable style-dict after",
      "after"
    )
  }

  # Insert style-array entries
  for (entry in rev(style_array_entries)) {
    x@table_string <- lines_insert(
      x@table_string,
      paste0("    ", entry),
      "tinytable cell style after",
      "after"
    )
  }

  return(x)
}




typst_split_chunks <- function(x) {
  x <- sort(x)
  breaks <- c(0, which(diff(x) != 1), length(x))
  result <- list()
  for (i in seq_along(breaks)[-length(breaks)]) {
    chunk <- x[(breaks[i] + 1):breaks[i + 1]]
    result[[i]] <- c(min = min(chunk), max = max(chunk))
  }
  out <- data.frame(do.call(rbind, result))
  out$max <- out$max + 1
  return(out)
}

typst_hlines <- function(x, lin) {
  if (nrow(lin) == 0) {
    return(x)
  }

  # Normalize colors once before splitting
  unique_line_colors <- unique(lin$line_color[!is.na(lin$line_color)])
  if (length(unique_line_colors) > 0) {
    line_color_map <- setNames(
      sapply(unique_line_colors, standardize_colors, format = "typst", USE.NAMES = FALSE),
      unique_line_colors
    )
    lin$line_color_mapped <- ifelse(
      !is.na(lin$line_color) & lin$line_color %in% names(line_color_map),
      line_color_map[lin$line_color],
      "black"
    )
  } else {
    lin$line_color_mapped <- "black"
  }

  tmp <- split(lin, list(lin$i, lin$line, lin$line_color_mapped, lin$line_width))
  tmp <- Filter(function(x) nrow(x) > 0, tmp)
  tmp <- lapply(tmp, function(k) {
    xmin <- typst_split_chunks(k$j)$min
    xmax <- typst_split_chunks(k$j)$max
    ymin <- k$i[1]
    ymax <- k$i[1] + 1
    line <- k$line[1]
    color <- k$line_color_mapped[1]
    width <- if (is.na(k$line_width[1])) 0.1 else k$line_width[1]
    width <- sprintf("%sem", width)
    out <- ""
    if (grepl("t", line)) {
      tmp <- "table.hline(y: %s, start: %s, end: %s, stroke: %s + %s),"
      tmp <- sprintf(tmp, ymin, xmin, xmax, width, color)
      out <- paste(out, tmp)
    }
    if (grepl("b", line)) {
      tmp <- "table.hline(y: %s, start: %s, end: %s, stroke: %s + %s),"
      tmp <- sprintf(tmp, ymax, xmin, xmax, width, color)
      out <- paste(out, tmp)
    }
    return(out)
  })
  for (l in tmp) {
    x@table_string <- lines_insert(
      x@table_string,
      l,
      "tinytable lines before",
      "before"
    )
  }
  return(x)
}

# =============================================================================
# NEW SIMPLIFIED FUNCTIONS USING expand_style()
# =============================================================================

#' Process Typst line styles using expanded data
#' @keywords internal
#' @noRd
process_typst_lines <- function(x, lines) {
  if (is.null(lines) || nrow(lines) == 0) {
    return(x)
  }

  # Convert tinytable indexing to Typst 0-based indexing
  # tinytable: 0=colnames, -1=group1, -2=group2, ..., 1,2,3=data
  # Typst: group headers come first, then colnames, then data
  # Need to account for the fact that group headers are inserted at the top

  # Convert tinytable indexing to Typst 0-based indexing
  # tinytable: 0=colnames, -1=group1, -2=group2, ..., 1,2,3=data
  # Typst: group headers come first (0,1,...), then colnames, then data
  if (x@nhead > 0) {
    # Case with headers/colnames: normal conversion
    lines$i <- ifelse(lines$i < 0,
                     x@nhead + lines$i - 1,  # Headers: -1 becomes nhead-2, -2 becomes nhead-3
                     lines$i + x@nhead - 1)  # Column names (0) and data: 0 becomes nhead-1, 1 becomes nhead
  } else {
    # Case with no headers (nhead = 0): filter out i=0 (non-existent colnames), then convert
    lines <- lines[lines$i > 0, , drop = FALSE]  # Remove i=0 entries
    if (nrow(lines) > 0) {
      lines$i <- lines$i - 1  # Data rows: 1 becomes 0, 2 becomes 1, etc.
    }
  }
  lines$j <- lines$j - 1

  # Process horizontal and vertical lines
  x <- typst_hlines(x, lines)
  x <- typst_vlines(x, lines)

  return(x)
}

#' Process Typst other styles using expanded data
#' @keywords internal
#' @noRd
process_typst_other_styles <- function(x, other) {
  if (is.null(other) || nrow(other) == 0) {
    return(x)
  }

  # Map alignments to Typst format
  other <- typst_map_alignments(other)

  # Normalize colors once
  unique_colors <- c(
    unique(other$color[!is.na(other$color)]),
    unique(other$background[!is.na(other$background)])
  )
  unique_colors <- unique(unique_colors)
  if (length(unique_colors) > 0) {
    color_map <- setNames(
      sapply(unique_colors, standardize_colors, format = "typst", USE.NAMES = FALSE),
      unique_colors
    )
  } else {
    color_map <- character(0)
  }

  # Generate CSS for each cell - vectorize where possible
  css <- rep("", nrow(other))

  # Process boolean styles (vectorized)
  is_bold <- !is.na(other$bold) & other$bold
  is_italic <- !is.na(other$italic) & other$italic
  is_underline <- !is.na(other$underline) & other$underline
  is_strikeout <- !is.na(other$strikeout) & other$strikeout
  is_monospace <- !is.na(other$monospace) & other$monospace

  for (row in seq_len(nrow(other))) {
    if (is_bold[row]) {
      css[row] <- typst_insert_field(css[row], "bold", "true")
    }
    if (is_italic[row]) {
      css[row] <- typst_insert_field(css[row], "italic", "true")
    }
    if (is_underline[row]) {
      css[row] <- typst_insert_field(css[row], "underline", "true")
    }
    if (is_strikeout[row]) {
      css[row] <- typst_insert_field(css[row], "strikeout", "true")
    }
    if (is_monospace[row]) {
      css[row] <- typst_insert_field(css[row], "mono", "true")
    }
    if (!is.na(other[row, "color"])) {
      color_value <- color_map[other[row, "color"]]
      css[row] <- typst_insert_field(css[row], "color", color_value)
    }
    if (!is.na(other[row, "background"])) {
      bg_value <- color_map[other[row, "background"]]
      css[row] <- typst_insert_field(css[row], "background", bg_value)
    }
    if (!is.na(other[row, "fontsize"])) {
      css[row] <- typst_insert_field(css[row], "fontsize", paste0(other[row, "fontsize"], "em"))
    }
    # Handle alignment (combining horizontal and vertical if both present)
    align_h <- other[row, "align"]
    align_v <- other[row, "alignv"]
    if (!is.na(align_h) || !is.na(align_v)) {
      align_parts <- character(0)
      if (!is.na(align_h)) align_parts <- c(align_parts, align_h)
      if (!is.na(align_v)) align_parts <- c(align_parts, align_v)
      combined_align <- paste(align_parts, collapse = " + ")
      css[row] <- typst_insert_field(css[row], "align", combined_align)
    }
    if (!is.na(other[row, "indent"]) && other[row, "indent"] > 0) {
      css[row] <- typst_insert_field(css[row], "indent", paste0(other[row, "indent"], "em"))
    }
  }

  # Clean CSS and add to data frame
  other$css <- sapply(css, typst_clean_css)

  # Convert tinytable indexing to Typst 0-based indexing
  # tinytable: 0=colnames, -1=group1, -2=group2, ..., 1,2,3=data
  # Typst: group headers come first, then colnames, then data
  # Need to account for the fact that group headers are inserted at the top

  # Convert tinytable indexing to Typst 0-based indexing
  # tinytable: 0=colnames, -1=group1, -2=group2, ..., 1,2,3=data
  # Typst: group headers come first (0,1,...), then colnames, then data
  if (x@nhead > 0) {
    # Case with headers/colnames: normal conversion
    other$i <- ifelse(other$i < 0,
                     x@nhead + other$i - 1,  # Headers: -1 becomes nhead-2, -2 becomes nhead-3
                     other$i + x@nhead - 1)  # Column names (0) and data: 0 becomes nhead-1, 1 becomes nhead
  } else {
    # Case with no headers (nhead = 0): filter out i=0 (non-existent colnames), then convert
    other <- other[other$i > 0, , drop = FALSE]  # Remove i=0 entries
    if (nrow(other) > 0) {
      other$i <- other$i - 1  # Data rows: 1 becomes 0, 2 becomes 1, etc.
    }
  }
  other$j <- other$j - 1

  # Generate style-dict and style-array for optimized lookup
  x <- typst_apply_styles(x, other)

  return(x)
}

typst_vlines <- function(x, lin) {
  lin <- lin[grepl("l|r", lin$line), , drop = FALSE]
  if (nrow(lin) == 0) {
    return(x)
  }

  # Normalize colors once before splitting
  unique_line_colors <- unique(lin$line_color[!is.na(lin$line_color)])
  if (length(unique_line_colors) > 0) {
    line_color_map <- setNames(
      sapply(unique_line_colors, standardize_colors, format = "typst", USE.NAMES = FALSE),
      unique_line_colors
    )
    lin$line_color_mapped <- ifelse(
      !is.na(lin$line_color) & lin$line_color %in% names(line_color_map),
      line_color_map[lin$line_color],
      "black"
    )
  } else {
    lin$line_color_mapped <- "black"
  }

  lin <- split(lin, list(lin$j, lin$line, lin$line_color_mapped, lin$line_width))
  lin <- Filter(function(x) nrow(x) > 0, lin)
  lin <- lapply(lin, function(k) {
    ymin <- typst_split_chunks(k$i)$min
    ymax <- typst_split_chunks(k$i)$max
    xmin <- k$j[1]
    xmax <- xmin + 1
    line <- k$line[1]
    color <- k$line_color_mapped[1]
    width <- if (is.na(k$line_width[1])) 0.1 else k$line_width[1]
    width <- sprintf("%sem", width)
    out <- ""
    if (grepl("l", line)) {
      tmp <- "table.vline(x: %s, start: %s, end: %s, stroke: %s + %s),"
      tmp <- sprintf(tmp, xmin, ymin, ymax, width, color)
      out <- paste(out, tmp)
    }
    if (grepl("r", line)) {
      tmp <- "table.vline(x: %s, start: %s, end: %s, stroke: %s + %s),"
      tmp <- sprintf(tmp, xmax, ymin, ymax, width, color)
      out <- paste(out, tmp)
    }
    return(out)
  })
  for (l in lin) {
    x@table_string <- lines_insert(
      x@table_string,
      l,
      "tinytable lines before",
      "before"
    )
  }
  return(x)
}


#' Internal styling function
#'
#' @inheritParams style_tt
#' @keywords internal
#' @noRd
setMethod(
  f = "style_eval",
  signature = "tinytable_typst",
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
    indent = 0,
    midrule = FALSE, # undocumented, only used by `group_tt()`
    ...
  ) {
    # Use populated @style_other from build_tt()
    other <- x@style_other

    # Filter to only cells that have actual styles
    if (nrow(other) > 0) {
      has_style <- rowSums(!is.na(other[, c("bold", "italic", "underline", "strikeout",
                                             "monospace", "smallcap", "align", "alignv",
                                             "color", "background", "fontsize", "indent"), drop = FALSE])) > 0
      other <- other[has_style, , drop = FALSE]
    }

    # Use populated @style_lines from build_tt()
    lines <- x@style_lines
    if (nrow(lines) == 0) {
      lines <- NULL
    }

    # gutters are used for group_tt(j) but look ugly with cell fill
    if (!is.null(other) && !all(is.na(other$background))) {
      x@table_string <- lines_drop(
        x@table_string,
        "column-gutter:",
        fixed = TRUE
      )
    }

    # Process lines using the expanded data
    x <- process_typst_lines(x, lines)

    # Process other styles using the expanded data
    x <- process_typst_other_styles(x, other)

    return(x)
  }
)

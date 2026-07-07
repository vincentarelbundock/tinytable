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

#' Vectorized CSS builder for the typst style-array
#'
#' Replaces the per-cell for-loop that called typst_insert_field() (which does
#' regex sub() on a growing string per property) with a single pass that builds
#' each cell's "key: value, key: value, ..." string directly. Output is
#' byte-identical to the concatenation of sequential typst_insert_field()
#' calls followed by typst_clean_css().
#'
#' @param other Filtered @style_other data frame (only rows with at least one style)
#' @param color_map Named character vector mapping original color names to typst rgb() strings
#' @return Character vector of css strings (one per row of `other`), un-trimmed
#' @keywords internal
#' @noRd
.typst_build_css_vectorized <- function(other, color_map) {
  n <- nrow(other)
  if (n == 0) return(character(0))

  bold        <- !is.na(other$bold)       & other$bold
  italic      <- !is.na(other$italic)     & other$italic
  underline   <- !is.na(other$underline)  & other$underline
  strikeout   <- !is.na(other$strikeout)  & other$strikeout
  monospace   <- !is.na(other$monospace)  & other$monospace
  color_set   <- !is.na(other$color)
  bg_set      <- !is.na(other$background)
  fs_set      <- !is.na(other$fontsize)
  indent_set  <- !is.na(other$indent)     & other$indent > 0
  align_h_set <- !is.na(other$align)
  align_v_set <- !is.na(other$alignv)

  parts <- vector("list", n)
  for (k in seq_len(n)) parts[[k]] <- character(0)

  # scalar emission: same string for every matching cell
  add_scalar <- function(mask, key, val) {
    if (!any(mask)) return()
    fragment <- sprintf("%s: %s", key, val)
    for (k in which(mask)) parts[[k]] <<- c(parts[[k]], fragment)
  }
  # vector emission: val is parallel to `other`; only used where mask is TRUE
  add_vec <- function(mask, key, val) {
    if (!any(mask)) return()
    fragment <- sprintf("%s: %s", key, val)
    for (k in which(mask)) parts[[k]] <<- c(parts[[k]], fragment[k])
  }

  add_scalar(bold, "bold", "true")
  add_scalar(italic, "italic", "true")
  add_scalar(underline, "underline", "true")
  add_scalar(strikeout, "strikeout", "true")
  add_scalar(monospace, "mono", "true")

  if (any(color_set)) {
    cv <- ifelse(color_set, color_map[other$color], "")
    add_vec(color_set, "color", cv)
  }
  if (any(bg_set)) {
    bv <- ifelse(bg_set, color_map[other$background], "")
    add_vec(bg_set, "background", bv)
  }
  if (any(fs_set)) {
    fv <- ifelse(fs_set,
                 vapply(other$fontsize,
                        function(x) if (is.na(x)) "" else format_markup_unit(x, "em"),
                        character(1)),
                 "")
    add_vec(fs_set, "fontsize", fv)
  }

  if (any(align_h_set) || any(align_v_set)) {
    ah <- ifelse(is.na(other$align),  "", other$align)
    av <- ifelse(is.na(other$alignv), "", other$alignv)
    combined <- ifelse(nchar(ah) > 0 & nchar(av) > 0,
                       paste(ah, av, sep = " + "),
                       paste0(ah, av))
    mask <- align_h_set | align_v_set
    add_vec(mask, "align", combined)
  }

  if (any(indent_set)) {
    iv <- ifelse(indent_set,
                 vapply(other$indent,
                        function(x) if (is.na(x)) "" else format_markup_unit(x, "em"),
                        character(1)),
                 "")
    add_vec(indent_set, "indent", iv)
  }

  # Each cell becomes a comma-separated list with a trailing comma, matching
  # the original typst_insert_field() + typst_clean_css() output format.
  vapply(parts, function(p) {
    s <- paste(p, collapse = ", ")
    if (nchar(s) > 0) paste0(s, ",") else s
  }, character(1))
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

  # Create style-dict (cell positions -> style array indices) using vectorized
  # sprintf instead of growing a vector inside a for loop. Each unique style
  # group can produce many cell-position keys, so we pre-allocate a list and
  # unlist() once.
  style_dict_list <- vector("list", length(uni))
  for (style_idx in seq_along(uni)) {
    cells <- uni[[style_idx]]
    style_dict_list[[style_idx]] <- sprintf('"%s_%s": %s', cells$i, cells$j, style_idx - 1)
  }
  style_dict_entries <- unlist(style_dict_list, use.names = FALSE)

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
    line_color_map <- stats::setNames(
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
    # Split chunks based on consecutive columns AND line_trim boundaries
    # line_trim marks group boundaries - split chunks at these points
    chunks <- typst_split_chunks(k$j)

    # Further split chunks based on line_trim markers
    # line_trim="l" or "lr" means start of a new group
    # line_trim="r" or "lr" means end of current group
    final_chunks <- list()
    for (chunk_idx in seq_len(nrow(chunks))) {
      chunk_start <- chunks$min[chunk_idx]
      chunk_end <- chunks$max[chunk_idx]
      chunk_cols <- chunk_start:(chunk_end - 1)  # -1 because max is exclusive

      # Find trim boundaries within this chunk
      chunk_rows <- k[k$j %in% chunk_cols, , drop = FALSE]
      chunk_rows <- chunk_rows[order(chunk_rows$j), ]

      # Split on line_trim boundaries
      current_start <- chunk_start
      for (i in seq_len(nrow(chunk_rows))) {
        trim_val <- chunk_rows$line_trim[i]
        col_val <- chunk_rows$j[i]

        # Check if this is a boundary
        if (!is.na(trim_val)) {
          if (grepl("r", trim_val)) {
            # End of current segment
            final_chunks <- c(final_chunks, list(c(min = current_start, max = col_val + 1)))
            # Next segment starts after this column
            if (i < nrow(chunk_rows)) {
              current_start <- col_val + 1
            }
          } else if (grepl("l", trim_val) && col_val > current_start) {
            # Start of new segment (close previous if any)
            if (col_val > current_start) {
              final_chunks <- c(final_chunks, list(c(min = current_start, max = col_val)))
            }
            current_start <- col_val
          }
        }

        # If last column in chunk, close the segment
        if (i == nrow(chunk_rows) && col_val + 1 >= chunk_end) {
          if (current_start < chunk_end) {
            final_chunks <- c(final_chunks, list(c(min = current_start, max = chunk_end)))
          }
        }
      }
    }

    # If no trim markers found, use original chunks
    if (length(final_chunks) == 0) {
      final_chunks <- lapply(seq_len(nrow(chunks)), function(i) c(min = chunks$min[i], max = chunks$max[i]))
    }

    ymin <- k$i[1]
    ymax <- k$i[1] + 1
    line <- k$line[1]
    color <- k$line_color_mapped[1]
    width <- if (is.na(k$line_width[1])) 0.1 else k$line_width[1]
    width <- format_markup_unit(width, "em")
    out <- ""

    # Generate hlines for each chunk
    for (chunk in final_chunks) {
      if (grepl("t", line)) {
        tmp <- "table.hline(y: %s, start: %s, end: %s, stroke: %s + %s),"
        tmp <- sprintf(tmp, ymin, chunk["min"], chunk["max"], width, color)
        out <- paste(out, tmp)
      }
      if (grepl("b", line)) {
        tmp <- "table.hline(y: %s, start: %s, end: %s, stroke: %s + %s),"
        tmp <- sprintf(tmp, ymax, chunk["min"], chunk["max"], width, color)
        out <- paste(out, tmp)
      }
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
    color_map <- stats::setNames(
      sapply(unique_colors, standardize_colors, format = "typst", USE.NAMES = FALSE),
      unique_colors
    )
  } else {
    color_map <- character(0)
  }

  # Generate CSS for each cell. The reference implementation builds one cell
  # at a time by repeatedly calling typst_insert_field(), which uses regex
  # sub() on a growing string. The vectorized version below produces an
  # identical result but is much faster on tables with many styled cells
  # (heat-map backgrounds, per-cell alignment, etc.). The output strings are
  # joined with ", " and given a trailing comma to match the original
  # typst_insert_field() format exactly (typst_clean_css() collapses runs
  # of commas and trims whitespace, so byte-identical output is achievable).
  css <- .typst_build_css_vectorized(other, color_map)

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
    line_color_map <- stats::setNames(
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
    width <- format_markup_unit(width, "em")
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

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


#' Process Typst styles and generate CSS and line data
#' @keywords internal
#' @noRd
typst_process_styles <- function(sty, rec) {
  css <- rep("", nrow(rec))
  lin <- data.frame()

  for (row in seq_len(nrow(sty))) {
    idx_i <- sty$i[row]
    if (is.na(idx_i)) {
      idx_i <- unique(rec$i)
    }
    idx_j <- sty$j[row]
    if (is.na(idx_j)) {
      idx_j <- unique(rec$j)
    }
    idx <- rec$i == idx_i & rec$j == idx_j

    if (isTRUE(sty[row, "bold"])) {
      css[idx] <- typst_insert_field(css[idx], "bold", "true")
    }
    if (isTRUE(sty[row, "italic"])) {
      css[idx] <- typst_insert_field(css[idx], "italic", "true")
    }
    if (isTRUE(sty[row, "underline"])) {
      css[idx] <- typst_insert_field(css[idx], "underline", "true")
    }
    if (isTRUE(sty[row, "strikeout"])) {
      css[idx] <- typst_insert_field(css[idx], "strikeout", "true")
    }
    if (isTRUE(sty[row, "monospace"])) {
      css[idx] <- typst_insert_field(css[idx], "monospace", "true")
    }
    if (isTRUE(sty[row, "smallcap"])) {
      css[idx] <- typst_insert_field(css[idx], "smallcaps", "true")
    }

    # Combine horizontal and vertical alignment
    align_h <- sty[row, "align"]
    align_v <- sty[row, "alignv"]

    if (!is.na(align_h) || !is.na(align_v)) {
      combined_align <- character(0)
      if (!is.na(align_h)) {
        combined_align <- c(combined_align, align_h)
      }
      if (!is.na(align_v)) {
        combined_align <- c(combined_align, align_v)
      }
      final_align <- paste(combined_align, collapse = " + ")
      css[idx] <- typst_insert_field(css[idx], "align", final_align)
    }

    fs <- sty[row, "indent"]
    if (!is.na(fs)) {
      css[idx] <- typst_insert_field(css[idx], "indent", sprintf("%sem", fs))
    }

    fs <- sty[row, "fontsize"]
    if (!is.na(fs)) {
      css[idx] <- typst_insert_field(
        css[idx],
        "fontsize",
        sprintf("%sem", fs)
      )
    }

    col <- standardize_colors(sty[row, "color"], format = "typst")
    if (!is.na(col)) {
      css[idx] <- typst_insert_field(css[idx], "color", col)
    }

    bg <- standardize_colors(sty[row, "background"], format = "typst")
    if (!is.na(bg)) {
      css[idx] <- typst_insert_field(css[idx], "background", bg)
    }

    lin <- typst_add_line_data(lin, sty, row, rec, idx)
  }

  css <- typst_clean_css(css)

  return(list(css = css, lin = lin))
}

#' Add line data to Typst line data frame
#' @keywords internal
#' @noRd
typst_add_line_data <- function(lin, sty, row, rec, idx) {
  line <- sty[row, "line"]
  if (is.na(line)) {
    return(lin)
  }

  # Check if idx matches any cells
  if (!any(idx)) {
    return(lin)
  }

  line_color <- standardize_colors(
    sty[row, "line_color"],
    format = "typst"
  )
  line_color <- ifelse(is.na(line_color), "black", line_color)
  line_width <- sty[row, "line_width"]
  if (is.na(line_width)) {
    line_width <- 0.1
  }

  tmp <- data.frame(
    i = rec$i[idx],
    j = rec$j[idx],
    line = unname(line),
    line_color = unname(line_color),
    line_width = unname(line_width)
  )
  return(rbind(lin, tmp))
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

  tmp <- split(lin, list(lin$i, lin$line, lin$line_color, lin$line_width))
  tmp <- Filter(function(x) nrow(x) > 0, tmp)
  tmp <- lapply(tmp, function(k) {
    xmin <- typst_split_chunks(k$j)$min
    xmax <- typst_split_chunks(k$j)$max
    ymin <- k$i[1]
    ymax <- k$i[1] + 1
    line <- k$line[1]
    color <- if (is.na(k$line_color[1])) {
      "black"
    } else {
      standardize_colors(k$line_color[1], format = "typst")
    }
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

typst_vlines <- function(x, lin) {
  lin <- lin[grepl("l|r", lin$line), , drop = FALSE]
  if (nrow(lin) == 0) {
    return(x)
  }

  lin <- split(lin, list(lin$j, lin$line, lin$line_color, lin$line_width))
  lin <- Filter(function(x) nrow(x) > 0, lin)
  lin <- lapply(lin, function(k) {
    ymin <- typst_split_chunks(k$i)$min
    ymax <- typst_split_chunks(k$i)$max
    xmin <- k$j[1]
    xmax <- xmin + 1
    line <- k$line[1]
    color <- if (is.na(k$line_color[1])) "black" else k$line_color[1]
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
    sty <- x@style

    # gutters are used for group_tt(j) but look ugly with cell fill
    if (!all(is.na(sty$background))) {
      x@table_string <- lines_drop(
        x@table_string,
        "column-gutter:",
        fixed = TRUE
      )
    }

    sty <- typst_map_alignments(sty)

    # sty & rec use the same 1-based indices as tinytable::tt()
    rec <- expand.grid(
      i = c(-(seq_len(x@nhead) - 1), seq_len(x@nrow)),
      j = seq_len(x@ncol)
    )
    css <- rep("", nrow(rec))

    result <- typst_process_styles(sty, rec)
    css <- result$css
    lin <- result$lin

    rec$css <- css

    # 0-based indexing
    lin$i <- lin$i + x@nhead - 1
    lin$j <- lin$j - 1
    rec$i <- rec$i + x@nhead - 1
    rec$j <- rec$j - 1

    # Generate style-dict and style-array for optimized lookup
    x <- typst_apply_styles(x, rec)

    x <- typst_hlines(x, lin)

    x <- typst_vlines(x, lin)

    return(x)
  }
)

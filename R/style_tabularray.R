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
  cmd <- ""
  if (isTRUE(sty_row$bold)) {
    cmd <- paste0(cmd, "\\bfseries")
  }
  if (isTRUE(sty_row$italic)) {
    cmd <- paste0(cmd, "\\textit")
  }
  if (isTRUE(sty_row$underline)) {
    cmd <- paste0(cmd, "\\tinytableTabularrayUnderline")
  }
  if (isTRUE(sty_row$strikeout)) {
    cmd <- paste0(cmd, "\\tinytableTabularrayStrikeout")
  }
  if (isTRUE(sty_row$monospace)) {
    cmd <- paste0(cmd, "\\texttt")
  }
  return(cmd)
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
  if (!is.na(sty_row$colspan)) {
    span <- paste0(span, "c=", sty_row$colspan, ",")
  }
  if (!is.na(sty_row$rowspan)) {
    span <- paste0(span, "r=", sty_row$rowspan, ",")
  }
  return(span)
}

#' Style lines for tabularray
#' @keywords internal
#' @noRd
style_lines <- function(rec, sty_row, x) {
  # Handle NA values for i and j (apply to all rows/columns)
  idx_i <- sty_row$i
  if (is.na(idx_i)) {
    idx_i <- unique(rec$i)
  }
  idx_j <- sty_row$j
  if (is.na(idx_j)) {
    idx_j <- unique(rec$j)
  }

  # Create index for matching cells
  idx <- rec$i %in% idx_i & rec$j %in% idx_j

  # Line type
  if (!is.na(sty_row$line)) {
    rec$line[idx] <- sty_row$line
  }

  # Line color
  lcol <- sty_row$line_color
  if (!is.na(lcol)) {
    lcol <- standardize_colors(lcol, format = "tabularray")
    x <- define_color_preamble(x, lcol)
    if (grepl("^#", lcol)) {
      lcol <- sub("^#", "c", lcol)
    }
    rec$line_color[idx] <- lcol
  }

  # Line width
  if (!is.na(sty_row$line_width)) {
    rec$line_width[idx] <- sty_row$line_width
  }

  return(list(rec = rec, x = x))
}

#' Clean style strings
#' @keywords internal
#' @noRd
clean_style_strings <- function(k) {
  k <- gsub("\\s*", "", k)
  k <- gsub(",+", ",", k)
  k <- gsub("^,", "", k, perl = TRUE)
  k <- gsub(",", ", ", k)
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

#' Generate tabularray horizontal line specifications
#' @keywords internal
#' @noRd
tabularray_hlines <- function(x, rec) {
  # Horizontal lines
  horizontal <- rec[
    grepl("b|t", rec$line),
    c("i", "j", "lin", "line"),
    drop = FALSE
  ]
  horizontal_bottom <- horizontal[grepl("b", horizontal$line), , drop = FALSE]
  horizontal_bottom$i <- horizontal_bottom$i + 1
  horizontal <- rbind(
    horizontal[grepl("t", horizontal$line), , drop = FALSE],
    horizontal_bottom
  )

  spec <- by(horizontal, list(horizontal$lin), function(k) {
    ival <- latex_range_string(k$i)
    jval <- latex_range_string(k$j)
    sprintf("hline{%s}={%s}{%s}", ival, jval, k$lin[1])
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

#' Generate tabularray vertical line specifications
#' @keywords internal
#' @noRd
tabularray_vlines <- function(x, rec) {
  # Vertical lines
  vertical <- rec[
    grepl("l|r", rec$line),
    c("i", "j", "lin", "line"),
    drop = FALSE
  ]
  vertical_right <- vertical[grepl("r", vertical$line), , drop = FALSE]
  vertical_right$j <- vertical_right$j + 1
  vertical <- rbind(
    vertical[grepl("l", vertical$line), , drop = FALSE],
    vertical_right
  )

  spec <- by(vertical, list(vertical$lin), function(k) {
    ival <- latex_range_string(k$i)
    jval <- latex_range_string(k$j)
    sprintf("vline{%s}={%s}{%s}", jval, ival, k$lin[1])
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

#' Apply tabularray specifications
#' @keywords internal
#' @noRd
apply_tabularray_specs <- function(x, sty) {
  for (spec in unique(stats::na.omit(x@latex_inner))) {
    x@table_string <- insert_tabularray_content(
      x@table_string,
      content = spec,
      type = "inner"
    )
  }

  for (spec in unique(stats::na.omit(x@latex_outer))) {
    x@table_string <- insert_tabularray_content(
      x@table_string,
      content = spec,
      type = "outer"
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
    # Prepare style data
    sty <- x@style
    sty$i <- sty$i + x@nhead
    sty <- map_alignv_values(sty)

    # Create record grid
    rec <- expand.grid(
      i = c(seq_len(x@nrow + x@nhead)),
      j = seq_len(x@ncol),
      line = NA,
      line_color = NA,
      line_width = NA
    )

    set <- span <- rep("", nrow(rec))

    # Prepare d-columns (special case)
    x <- prepare_dcolumn(x, sty)

    # Apply styling to each row
    for (row in seq_len(nrow(sty))) {
      # index: sty vs rec
      idx_i <- sty$i[row]
      if (is.na(idx_i)) {
        idx_i <- unique(rec$i)
      }
      idx_j <- sty$j[row]
      if (is.na(idx_j)) {
        idx_j <- unique(rec$j)
      }
      idx <- rec$i == idx_i & rec$j == idx_j

      # Build style commands
      cmd <- style_text(sty[row, ])

      # Style colors
      color_result <- style_colors(cmd, sty[row, ], x)
      cmd <- color_result$cmd
      x <- color_result$x

      # Format command string
      if (grepl("^,", cmd)) {
        tmp <- "%s, %s, "
      } else {
        tmp <- "%s, cmd=%s, "
      }
      if (trimws(cmd) != "") {
        set[idx] <- sprintf(tmp, set[idx], cmd)
      }

      # Style fonts
      set[idx] <- style_fonts(set[idx], sty[row, ])

      # Style spans
      span[idx] <- style_spans(span[idx], sty[row, ])

      # Style lines
      line_result <- style_lines(rec, sty[row, ], x)
      rec <- line_result$rec
      x <- line_result$x
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

    # Prepare line specifications
    rec$lin <- "solid, "
    rec$lin <- ifelse(
      !is.na(rec$line_color),
      paste0(rec$lin, rec$line_color),
      rec$lin
    )
    rec$lin <- ifelse(
      !is.na(rec$line_width),
      paste0(rec$lin, sprintf(", %sem", rec$line_width)),
      rec$lin
    )
    rec$lin[is.na(rec$line)] <- NA

    # Generate line specifications
    x <- tabularray_hlines(x, rec)
    x <- tabularray_vlines(x, rec)

    # Apply tabularray specifications
    x <- apply_tabularray_specs(x, sty)

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

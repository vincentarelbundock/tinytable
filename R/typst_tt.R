apply_typst_spans <- function(body, sty) {
  # spans must be replaced before concatenating strings
  # Only process spans for positive row indices (data body), not headers (negative indices)
  spans <- sty[
    which(
      ((!is.na(sty$colspan) & sty$colspan > 1) |
        (!is.na(sty$rowspan) & sty$rowspan > 1)) &
        (!is.na(sty$i) & sty$i > 0)
    ),
    ,
    drop = FALSE
  ]
  if (nrow(spans) > 0) {
    # Deduplicate spans by i, j, colspan, and rowspan to avoid applying the same span multiple times
    # This can happen when line styles are expanded (e.g., "lt" becomes "l" and "t" entries)
    spans <- spans[!duplicated(spans[, c("i", "j", "colspan", "rowspan")]), , drop = FALSE]
    table_nrows <- nrow(body)
    table_ncols <- ncol(body)

    for (idx in seq_len(nrow(spans))) {
      rowspan <- spans[idx, "rowspan"]
      colspan <- spans[idx, "colspan"]
      row_idx <- spans[idx, "i"]
      col_idx <- spans[idx, "j"]

      # Sanity checks for span dimensions
      if (!is.na(colspan) && (col_idx + colspan - 1) > table_ncols) {
        stop(sprintf(
          "colspan of %d at column %d exceeds table width of %d columns",
          colspan,
          col_idx,
          table_ncols
        ))
      }
      if (!is.na(rowspan) && (row_idx + rowspan - 1) > table_nrows) {
        stop(sprintf(
          "rowspan of %d at row %d exceeds table height of %d rows",
          rowspan,
          row_idx,
          table_nrows
        ))
      }

      # Build table.cell() arguments
      cell_args <- character(0)
      if (!is.na(colspan) && colspan > 1) {
        cell_args <- c(cell_args, sprintf("colspan: %s", colspan))
      }
      if (!is.na(rowspan) && rowspan > 1) {
        cell_args <- c(cell_args, sprintf("rowspan: %s", rowspan))
      }

      # spanning cell
      body[row_idx, col_idx] <- sprintf(
        "table.cell(%s)%s",
        paste(cell_args, collapse = ", "),
        body[row_idx, col_idx]
      )

      # empty cells
      row_span <- if (!is.na(rowspan)) rowspan else 1
      col_span <- if (!is.na(colspan)) colspan else 1
      row_idx_empty <- seq(row_idx, row_idx + row_span - 1)
      col_idx_empty <- seq(col_idx, col_idx + col_span - 1)
      empty <- expand.grid(i = row_idx_empty, j = col_idx_empty)
      empty <- empty[empty$i != row_idx | empty$j != col_idx, , drop = FALSE]
      for (k in seq_len(nrow(empty))) {
        body[empty[k, "i"], empty[k, "j"]] <- NA
      }
    }
  }
  return(body)
}

setMethod(
  f = "build_eval",
  signature = "tinytable_typst",
  definition = function(x, ...) {
    out <- typst_template()
    out <- typst_body(x, out)
    out <- typst_header(x, out)
    out <- typst_widths(x, out)
    out <- typst_notes(x, out)
    out <- typst_add_gutter(x, out)
    x@table_string <- out
    return(x)
  }
)

# Helper function to load the Typst template
typst_template <- function() {
  out <- readLines(system.file("templates/typst.typ", package = "tinytable"))
  paste(out, collapse = "\n")
}

# Helper function to process table body
typst_body <- function(x, out) {
  # Prepare body data
  body <- apply(x@data_body, 2, function(k) paste0("[", k, "]"), simplify = FALSE)
  body <- do.call(cbind, body)

  # Apply colspan and rowspan transformations
  body <- apply_typst_spans(body, x@style)

  if (nrow(x@data_body) && is.null(dim(body))) {
    body <- matrix(body)
  }

  # Convert body to Typst format
  body <- apply(body, 1, function(k) {
    clean_k <- stats::na.omit(k)
    if (length(clean_k) > 0) {
      paste(clean_k, collapse = ", ")
    } else {
      NA_character_ # Mark completely empty rows for removal
    }
  })

  # Keep only non-empty rows
  body <- stats::na.omit(body)
  body <- paste(body, collapse = ",\n")
  body <- paste0(body, ",\n")

  lines_insert(out, body, "tinytable cell content after", "after")
}

# Identify the column-group underlines created by `group_tt(j = ...)`
#
# Those rules carry a `line_trim` marker, which every other backend uses to
# leave a gap between the rules of two adjacent groups. Typst's
# `table.hline()` snaps to column boundaries and has no trim option, so two
# adjacent group rules merge into a single long line and the reader cannot
# tell where one group ends and the next begins. We therefore draw them
# inside the group header cell with `place()`, which is bounded by the cell's
# inner width and so leaves the gap.
#
# Returns the rules to draw (one row per group span) and a logical vector
# flagging the rows of `x@style_lines` they replace, so `style_eval()` can
# skip them instead of drawing the same rule twice.
#' @keywords internal
#' @noRd
typst_group_line_rules <- function(x) {
  lines <- x@style_lines
  n_lines <- if (is.null(lines)) 0L else nrow(lines)
  out <- list(rules = NULL, consumed = rep(FALSE, n_lines))
  if (n_lines == 0 || nrow(x@group_data_j) == 0) {
    return(out)
  }

  eq <- function(a, b) !is.na(a) & !is.na(b) & a == b

  rules <- list()
  n_head <- nrow(x@group_data_j)
  for (row_idx in seq_len(n_head)) {
    # The bottom group row is at i = -1, the one above it at i = -2, etc.
    i_style <- -(n_head - row_idx + 1)
    spans <- parse_group_spans(as.character(x@group_data_j[row_idx, ]))
    for (span_idx in seq_len(nrow(spans))) {
      cols <- spans$start[span_idx]:spans$end[span_idx]
      bottom <- lines$i == i_style & lines$line == "b" & lines$j %in% cols
      trimmed <- which(bottom & !is.na(lines$line_trim))
      if (length(trimmed) == 0) {
        next
      }
      width <- lines$line_width[trimmed[1]]
      color <- lines$line_color[trimmed[1]]
      sel <- which(
        bottom & eq(lines$line_width, width) & eq(lines$line_color, color)
      )
      # Only replace a rule that covers the whole span: a partial rule would
      # be drawn in the wrong place by a cell-wide `place()`.
      if (!all(cols %in% lines$j[sel])) {
        next
      }
      out$consumed[sel] <- TRUE
      rules[[length(rules) + 1L]] <- data.frame(
        header_row = row_idx,
        start = spans$start[span_idx],
        end = spans$end[span_idx],
        line_width = width,
        line_color = color,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rules) > 0) {
    out$rules <- do.call(rbind, rules)
  }
  out
}

# Helper function to process header
typst_header <- function(x, out) {
  # Collect all header lines in correct visual order (top to bottom)
  all_headers <- character(0)

  # Add group headers (first call first = top of table)
  if (nrow(x@group_data_j) > 0) {
    rules <- typst_group_line_rules(x)$rules
    for (row_idx in 1:nrow(x@group_data_j)) {
      group_row <- as.character(x@group_data_j[row_idx, ])
      row_rules <- if (is.null(rules)) {
        NULL
      } else {
        rules[rules$header_row == row_idx, , drop = FALSE]
      }
      header_line <- typst_build_group_header(group_row, rules = row_rules)
      if (!is.null(header_line)) {
        all_headers <- c(all_headers, header_line)
      }
    }
  }

  # Add regular column headers (closest to data = bottom of header)
  header <- !is.null(colnames(x)) && length(colnames(x)) > 0
  if (header) {
    header <- paste(paste0("[", colnames(x), "]"), collapse = ", ")
    header <- paste0(header, ",")
    all_headers <- c(all_headers, header)
  }

  # Insert all headers at once
  if (length(all_headers) > 0) {
    all_headers_text <- paste(all_headers, collapse = "\n")
    out <- lines_insert(out, all_headers_text, "repeat: true", "after")
  }

  out
}

# Helper function to process column widths
#
# Three regimes:
# 1. Explicit `width` (scalar or vector): fixed percentage columns, unchanged.
#    A scalar is split equally across columns; a vector gives each column its
#    own share with a total of `sum(width)`.
# 2. Auto width with notes: proportional columns. Typst sizes `auto` columns
#    to fit every cell, including the `table.footer` note cell that spans the
#    full table, so a long note stretches the table to the page width and
#    distorts the column proportions (#669). Instead we measure each column's
#    natural width and convert the measurements to `fr` units, with the table
#    total capped at the natural/page width. The measurement ignores the
#    footer, so notes wrap at the table width instead of dictating it.
# 3. Auto width, no notes: plain `auto` columns, unchanged.
typst_widths <- function(x, out) {
  if (length(x@width) == 1) {
    width <- rep(sprintf("%.2f%%", x@width / ncol(x) * 100), ncol(x))
    width <- sprintf("    columns: (%s),", paste(width, collapse = ", "))
    return(lines_insert(out, width, "tinytable table start", "after"))
  }
  if (length(x@width) > 1) {
    width <- sprintf("%.2f%%", x@width * 100)
    width <- sprintf("    columns: (%s),", paste(width, collapse = ", "))
    return(lines_insert(out, width, "tinytable table start", "after"))
  }

  if (length(x@notes) == 0) {
    width <- paste(rep("auto", ncol(x)), collapse = ", ")
    width <- sprintf("    columns: (%s),", width)
    return(lines_insert(out, width, "tinytable table start", "after"))
  }

  # Proportional columns: measure natural column widths in Typst itself.
  # Group headers and spanning cells are excluded from the measurement; they
  # span several columns, so they do not pin down any single column's width.
  coldata <- apply(x@data_body, 2, function(k) {
    paste0("[", k, "]", collapse = ", ")
  })
  if (!is.null(colnames(x)) && length(colnames(x)) > 0) {
    coldata <- paste0("[", colnames(x), "], ", coldata)
  }
  coldata <- sprintf("    (%s),", coldata)
  coldata <- paste(
    c("  #let tinytable-coldata = (", coldata, "  )", ""),
    collapse = "\n"
  )

  # `grid` with the same inset as `table` measures the same natural width,
  # but is immune to the `show table.cell` styling rule, whose styles are
  # keyed by cell position and would misapply in a single-column layout.
  total <- "calc.min(tinytable-naturals.sum(), size.width)"
  wrapper_open <- paste(
    c(
      coldata,
      "  #context layout(size => {",
      "    let tinytable-naturals = tinytable-coldata.map(col => measure(grid(columns: 1, inset: 5pt, ..col)).width)",
      sprintf("    let tinytable-total = %s", total),
      "    block(width: tinytable-total)[",
      ""
    ),
    collapse = "\n"
  )

  out <- lines_insert(out, wrapper_open, "tinytable align-figure before", "after")
  out <- lines_insert(
    out,
    "    columns: tinytable-naturals.map(w => w.pt() * 1fr),",
    "tinytable table start",
    "after"
  )
  out <- lines_insert(out, "\n  ]\n  })", "end table", "after")
  out
}

# Helper function to process notes
typst_notes <- function(x, out) {
  if (length(x@notes) == 0) {
    return(out)
  }

  # Add footer structure
  ft <- "
    table.footer(
      repeat: false,
      // tinytable notes after
    ),
    "
  out <- lines_insert(out, ft, "tinytable footer after", "after")

  # Process each note
  notes <- rev(x@notes)
  if (is.null(names(notes))) {
    lab <- rep("", length(notes))
  } else {
    lab <- names(notes)
  }

  notes <- sapply(notes, function(n) if (is.list(n)) n$text else n)

  note_text <- vapply(
    seq_along(notes),
    function(k) typst_note(notes[k], lab[k], ncol(x)),
    character(1)
  )
  if (length(note_text) > 0) {
    # Repeated insertion after one marker reverses the input, so reverse the
    # batch to preserve the existing byte-for-byte output order.
    out <- lines_insert(
      out,
      paste(rev(note_text), collapse = "\n"),
      "tinytable notes after",
      "after"
    )
  }

  out
}

# Helper function to format a single note
typst_note <- function(note, label, ncols) {
  if (label == "") {
    sprintf(
      "    table.cell(align: left, colspan: %s, %s),",
      ncols,
      note
    )
  } else {
    l <- sprintf("[#super[%s] ", label)
    n <- sub("[", l, note, fixed = TRUE)
    tmp <- sprintf(
      "    table.cell(align: left, colspan: %s, %s),",
      ncols,
      n
    )
    sub("text(, ", "text(", tmp, fixed = TRUE)
  }
}

# Helper function to build Typst group header from group row data
#
# `rules` (see typst_group_line_rules()) holds the underlines to draw inside
# the span cells rather than as table-wide `table.hline()` calls.
typst_build_group_header <- function(group_row, rules = NULL) {
  spans <- parse_group_spans(group_row)

  header_parts <- character(0)
  pos <- 1

  emit_empty_until <- function(parts, from, to) {
    if (to >= from) {
      parts <- c(parts, rep("[ ]", to - from + 1))
    }
    parts
  }

  # `place()` is bounded by the cell's inner width, so the rule stops short of
  # the column edge on both sides: adjacent group rules stay separated.
  span_rule <- function(start, end) {
    if (is.null(rules) || nrow(rules) == 0) {
      return("")
    }
    hit <- which(rules$start == start & rules$end == end)
    if (length(hit) == 0) {
      return("")
    }
    width <- rules$line_width[hit[1]]
    width <- format_markup_unit(if (is.na(width)) 0.1 else width, "em")
    color <- normalize_colors(rules$line_color[hit[1]], "typst")
    sprintf(
      " #place(bottom, dy: 0.4em, line(length: 100%%, stroke: %s + %s))",
      width,
      color
    )
  }

  for (span_idx in seq_len(nrow(spans))) {
    # Empty cells for uncovered columns before this span
    header_parts <- emit_empty_until(header_parts, pos, spans$start[span_idx] - 1)

    span_length <- spans$end[span_idx] - spans$start[span_idx] + 1
    rule <- span_rule(spans$start[span_idx], spans$end[span_idx])
    if (span_length > 1) {
      # Multi-column span - use table.cell with colspan
      header_parts <- c(header_parts, sprintf(
        "table.cell(colspan: %s, align: center)[%s%s]",
        span_length,
        spans$label[span_idx],
        rule
      ))
    } else {
      # Single column - just centered content
      header_parts <- c(
        header_parts,
        sprintf("[%s%s]", spans$label[span_idx], rule)
      )
    }
    pos <- spans$end[span_idx] + 1
  }

  # Trailing uncovered columns
  header_parts <- emit_empty_until(header_parts, pos, length(group_row))

  if (length(header_parts) > 0) {
    paste0(paste(header_parts, collapse = ", "), ",")
  } else {
    NULL
  }
}

# Helper function to add column gutter if needed
typst_add_gutter <- function(x, out) {
  # Add column gutter if there are column groups and it's not already present
  if (nrow(x@group_data_j) > 0 && !any(grepl("column-gutter", out))) {
    out <- lines_insert(
      out,
      "    column-gutter: 5pt,",
      "// tinytable table start",
      "after"
    )
  }
  out
}

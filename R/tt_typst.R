apply_typst_spans <- function(body, sty) {
  # spans must be replaced before concatenating strings
  spans <- sty[
    which(
      (!is.na(sty$colspan) & sty$colspan > 1) |
        (!is.na(sty$rowspan) & sty$rowspan > 1)
    ),
    ,
    drop = FALSE
  ]
  if (nrow(spans) > 0) {
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
  f = "tt_eval",
  signature = "tinytable_typst",
  definition = function(x, ...) {
    out <- typst_template()
    out <- typst_body(x, out)
    out <- typst_header(x, out)
    out <- typst_widths(x, out)
    out <- typst_notes(x, out)
    out <- typst_alignment(x, out)
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
  body <- apply(x@data_body, 2, function(k) paste0("[", k, "]"))

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

  typst_insert(out, body, type = "body")
}

# Helper function to process header
typst_header <- function(x, out) {
  header <- !is.null(colnames(x)) && length(colnames(x)) > 0
  if (header) {
    header <- paste(paste0("[", colnames(x), "]"), collapse = ", ")
    header <- paste0(header, ",")
    out <- lines_insert(out, header, "repeat: true", "after")
  }
  out
}

# Helper function to process column widths
typst_widths <- function(x, out) {
  if (length(x@width) == 0) {
    width <- rep("auto", ncol(x))
  } else if (length(x@width) == 1) {
    width <- rep(sprintf("%.2f%%", x@width / ncol(x) * 100), ncol(x))
  } else {
    width <- sprintf("%.2f%%", x@width * 100)
  }
  width <- sprintf("    columns: (%s),", paste(width, collapse = ", "))
  lines_insert(out, width, "tinytable table start", "after")
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

  for (k in seq_along(notes)) {
    note_text <- typst_note(notes[k], lab[k], ncol(x))
    out <- lines_insert(out, note_text, "tinytable notes after", "after")
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

# Helper function to process default alignment
typst_alignment <- function(x, out) {
  align_default <- sprintf(
    "  #let align-default-array = ( %s, ) // tinytable align-default-array here",
    paste(rep("left", ncol(x)), collapse = ", ")
  )
  lines_insert(
    out,
    align_default,
    "// tinytable align-default-array before",
    "after"
  )
}

typst_insert <- function(x, content = NULL, type = "body") {
  if (is.null(content)) {
    return(x)
  }

  out <- strsplit(x, "\n")[[1]]
  comment <- switch(
    type,
    "lines" = "tinytable lines before",
    "style" = "tinytable cell style before",
    "body" = "tinytable cell content after"
  )
  idx <- grep(comment, out)

  if (type == "body") {
    out <- c(out[1:idx], content, out[(idx + 1):length(out)])
  } else {
    out <- c(out[1:(idx - 1)], content, out[idx:length(out)])
  }

  out <- paste(out, collapse = "\n")
  return(out)
}

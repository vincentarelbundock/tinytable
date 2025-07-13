apply_typst_spans <- function(body, sty) {
  # spans must be replaced before concatenating strings
  spans <- sty[which((!is.na(sty$colspan) & sty$colspan > 1) | (!is.na(sty$rowspan) & sty$rowspan > 1)), , drop = FALSE]
  if (nrow(spans) > 0) {
    for (idx in seq_len(nrow(spans))) {
      rowspan <- spans[idx, "rowspan"]
      colspan <- spans[idx, "colspan"]
      row_idx <- spans[idx, "i"]
      col_idx <- spans[idx, "j"]

      # Build table.cell() arguments
      cell_args <- character(0)
      if (!is.na(colspan) && colspan > 1) cell_args <- c(cell_args, sprintf("colspan: %s", colspan))
      if (!is.na(rowspan) && rowspan > 1) cell_args <- c(cell_args, sprintf("rowspan: %s", rowspan))

      # spanning cell
      body[row_idx, col_idx] <- sprintf(
        "table.cell(%s)%s",
        paste(cell_args, collapse = ", "),
        body[row_idx, col_idx])

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
    out <- readLines(system.file("templates/typst.typ", package = "tinytable"))
    out <- paste(out, collapse = "\n")

    # body
    body <- apply(x@table_dataframe, 2, function(k) paste0("[", k, "]"))

    # get style information
    sty <- x@style

    # Apply colspan and rowspan transformations
    body <- apply_typst_spans(body, sty)

    if (nrow(x@table_dataframe) && is.null(dim(body))) {
      body <- matrix(body)
    }

    header <- !is.null(colnames(x)) && length(colnames(x)) > 0
    if (header) {
      header <- paste(paste0("[", colnames(x), "]"), collapse = ", ")
      header <- paste0(header, ",")
      out <- lines_insert(out, header, "repeat: true", "after")
    }
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
    out <- typst_insert(out, body, type = "body")

    if (length(x@width) == 0) {
      width <- rep("auto", ncol(x))
    } else if (length(x@width) == 1) {
      width <- rep(sprintf("%.2f%%", x@width / ncol(x) * 100), ncol(x))
    } else {
      width <- sprintf("%.2f%%", x@width * 100)
    }
    width <- sprintf("    columns: (%s),", paste(width, collapse = ", "))
    out <- lines_insert(out, width, "tinytable table start", "after")

    # notes
    if (length(x@notes) > 0) {
      ft <- "
    table.footer(
      repeat: false,
      // tinytable notes after
    ),
    "
      out <- lines_insert(out, ft, "tinytable footer after", "after")
      notes <- rev(x@notes)
      # otherwise an empty caption is created automatically
      if (is.null(names(notes))) {
        lab <- rep("", length(notes))
      } else {
        lab <- names(notes)
      }
      notes <- sapply(notes, function(n) if (is.list(n)) n$text else n)
      for (k in seq_along(notes)) {
        if (lab[k] == "") {
          tmp <- sprintf(
            "    table.cell(align: left, colspan: %s, %s),",
            ncol(x),
            notes[k]
          )
        } else {
          n <- notes[k]
          l <- sprintf("[#super[%s] ", lab[k])
          n <- sub("[", l, n, fixed = TRUE)
          tmp <- sprintf(
            "    table.cell(align: left, colspan: %s, %s),",
            ncol(x),
            n
          )
        }
        tmp <- sub("text(, ", "text(", tmp, fixed = TRUE)
        out <- lines_insert(out, tmp, "tinytable notes after", "after")
      }
    }

    # default alignment
    align_default <- sprintf(
      "  #let align-default-array = ( %s, ) // tinytable align-default-array here",
      paste(rep("left", ncol(x)), collapse = ", ")
    )
    out <- lines_insert(
      out,
      align_default,
      "// tinytable align-default-array before",
      "after"
    )

    x@table_string <- out

    return(x)
  })

typst_insert <- function(x, content = NULL, type = "body") {
  if (is.null(content)) {
    return(x)
  }

  out <- strsplit(x, "\n")[[1]]
  comment <- switch(type,
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

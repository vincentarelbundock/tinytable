setMethod(
  f = "tt_eval",
  signature = "tinytable_bootstrap",
  definition = function(x, ...) {
    template <- readLines(
      system.file("templates/bootstrap.html", package = "tinytable")
    )

    mathjax <- get_option("tinytable_html_mathjax", default = FALSE)
    assert_flag(mathjax, name = "tinytable_html_mathjax")
    if (isFALSE(mathjax)) {
      template <- paste(template, collapse = "\n")
      sta <- "    <!-- tinytable mathjax start -->"
      end <- "    <!-- tinytable mathjax end -->"
      template <- lines_drop_between(template, sta, end, fixed = TRUE)
      template <- strsplit(template, "\n")[[1]]
    }

    quartoprocessing <- get_option(
      "tinytable_quarto_disable_processing",
      default = TRUE
    )
    assert_flag(quartoprocessing, name = "tinytable_quarto_disable_processing")
    if (isFALSE(quartoprocessing)) {
      template <- sub(
        "data-quarto-disable-processing='true'",
        "data-quarto-disable-processing='false'",
        template,
        fixed = TRUE
      )
    }

    # caption
    if (length(x@caption) != 1) {
      template <- sub(
        "$tinytable_BOOTSTRAP_CAPTION",
        "",
        template,
        fixed = TRUE
      )
    } else {
      template <- sub(
        "$tinytable_BOOTSTRAP_CAPTION",
        sprintf("<caption>%s</caption>", x@caption),
        template,
        fixed = TRUE
      )
    }

    # note
    if (length(x@notes) == 0) {
      template <- sub(
        "$tinytable_BOOTSTRAP_NOTE",
        "",
        template,
        fixed = TRUE
      )
    } else {
      notes_tmp <- NULL
      for (k in seq_along(x@notes)) {
        if (!is.null(names(x@notes))) {
          if (is.list(x@notes[[k]])) {
            tmp <- sprintf(
              "<tr><td colspan='%s'><sup>%s</sup> %s</td></tr>",
              ncol(x),
              names(x@notes)[k],
              x@notes[[k]]$text
            )
            # note is a string
          } else {
            tmp <- sprintf(
              "<tr><td colspan='%s'><sup>%s</sup> %s</td></tr>",
              ncol(x),
              names(x@notes)[k],
              x@notes[k]
            )
          }
        } else {
          tmp <- sprintf(
            "<tr><td colspan='%s'>%s</td></tr>",
            ncol(x),
            x@notes[[k]]
          )
        }
        notes_tmp <- c(notes_tmp, tmp)
      }
      notes <- paste(notes_tmp, collapse = "\n")
      notes <- paste0("<tfoot>", notes, "</tfoot>")
      template <- sub(
        "$tinytable_BOOTSTRAP_NOTE",
        notes,
        template,
        fixed = TRUE
      )
      for (ii in seq_along(notes)) {
        x <- style_tt(x, i = nrow(x) + ii, align = "l")
      }
    }

    # width
    if (length(x@width) == 1) {
      template <- sub(
        "width: auto;",
        sprintf(
          "table-layout: fixed; width: %s%% !important;",
          round(x@width * 100)
        ),
        template,
        fixed = TRUE
      )
    } else if (length(x@width) > 1) {
      template <- sub(
        "width: auto;",
        sprintf(
          "table-layout: fixed; width: %s%% !important;",
          round(sum(x@width) * 100)
        ),
        template,
        fixed = TRUE
      )
    }

    # (pseudo-)unique table IDs
    id <- get_id("")
    x@id <- id

    # table and styling function in JS must have different names when there is more than one table on a page.
    template <- gsub(
      "styleCell",
      paste0("styleCell_", id),
      template,
      fixed = TRUE
    )
    template <- gsub(
      "spanCell",
      paste0("spanCell_", id),
      template,
      fixed = TRUE
    )
    template <- gsub(
      "$tinytable_TABLE_ID",
      paste0("tinytable_", id),
      template,
      fixed = TRUE
    )

    # header
    idx <- grep("$tinytable_BOOTSTRAP_HEADER", template, fixed = TRUE)

    if (length(colnames(x)) > 0) {
      # Generate all header cells at once
      col_indices <- seq_along(colnames(x)) - 1
      header_cells <- sprintf(
        '    <th scope="col" data-row="0" data-col="%d">%s</th>',
        col_indices,
        colnames(x)
      )
      header <- c("  <tr>", header_cells, "  </tr>")
      header <- paste(strrep(" ", 11), header)
    } else {
      header <- NULL
    }
    template <- c(
      template[1:(idx - 1)],
      header,
      template[(idx + 1):length(template)]
    )
    # body
    body <- NULL

    # Calculate row indices with vectorized operations
    # All rows in table_dataframe should get consecutive indices starting from 1
    # Group rows have already been inserted by rbind_**()
    # The user's row indices (i parameter) should match the HTML data-row values
    i_idx <- seq_len(nrow(x@data_body))

    # Generate all cells at once using matrix operations
    row_indices <- rep(i_idx, each = ncol(x@data_body))
    col_indices <- rep(
      seq_len(ncol(x@data_body)) - 1,
      times = nrow(x@data_body)
    )
    cell_values <- as.vector(t(x@data_body))

    # Create all cells in one operation
    cells <- sprintf(
      '    <td data-row="%d" data-col="%d">%s</td>',
      row_indices,
      col_indices,
      cell_values
    )

    # Reshape into rows
    cells_matrix <- matrix(cells, ncol = ncol(x@data_body), byrow = TRUE)
    rows <- apply(cells_matrix, 1, function(row) {
      c("  <tr>", row, "  </tr>")
    })

    body <- unlist(rows)

    idx <- grep("$tinytable_BOOTSTRAP_BODY", template, fixed = TRUE)
    template <- c(
      template[1:(idx - 1)],
      paste(strrep(" ", 13), body),
      template[(idx + 1):length(template)]
    )

    out <- paste(template, collapse = "\n")

    # before style_eval()
    x@table_string <- out

    if (length(x@width) > 1) {
      for (j in seq_len(ncol(x))) {
        css <- sprintf("width: %s%%;", x@width[j] / sum(x@width) * 100)
        x <- theme_html(x, engine = "bootstrap", j = j, css = css)
      }
    }

    if (length(x@bootstrap_class) == 0) {
      if (
        length(x@theme) == 0 ||
        is.null(x@theme[[1]]) ||
          is.function(x@theme[[1]]) ||
          isTRUE("default" %in% x@theme[[1]])
      ) {
        x@lazy_theme <- c(x@lazy_theme, list(theme_default))
      }
    }


    return(x)
  })

bootstrap_setting <- function(x, new, component = "row") {
  att <- attributes(x)
  out <- strsplit(x, "\n")[[1]]
  if (component == "row") {
    idx <- grep("tinytable rows before this", out)
  } else if (component == "column") {
    idx <- grep("tinytable columns before this", out)
  } else if (component == "cell") {
    idx <- utils::tail(grep("</script>", out, fixed = TRUE), 1)
    # idx <- grep("tinytable cells before this", out)
  } else if (component == "css") {
    idx <- grep("</style>", out, fixed = TRUE)
  } else if (component == "newrows") {
    idx <- grep("tinytable new rows before this", out)
  }
  out <- c(
    out[1:(idx - 1)],
    new,
    out[idx:length(out)]
  )
  out <- paste(out, collapse = "\n")
  attributes(out) <- att
  class(out) <- class(x)
  return(out)
}

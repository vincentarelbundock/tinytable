# internal function
# style_tt() stores style calls and we only want to evaluate them at the end because
# some rows may be added, which changes how the style is applied
#
# THE ORDER MATTERS A LOT!

#' Build group header and body parts with position calculations
#' @keywords internal
#' @noRd
rbind_body_groupi <- function(x) {
  # Reconstruct the final table by combining formatted data_body and group_data_i parts
  if (nrow(x@group_data_i) == 0) {
    # No groups - @data_body already contains the final formatted data
    return(x)
  }

  # Calculate total final table size
  total_rows <- nrow(x@data_body) + nrow(x@group_data_i)
  final_ncol <- ncol(x@data_body)

  # Create final data frame with proper structure
  final_df <- data.frame(matrix(
    NA_character_,
    nrow = total_rows,
    ncol = final_ncol
  ))
  colnames(final_df) <- colnames(x@data_body)

  # Insert body data at index_body positions
  if (nrow(x@data_body) > 0 && length(x@index_body) > 0) {
    for (i in seq_along(x@index_body)) {
      row_idx <- x@index_body[i]
      if (!is.na(row_idx) && row_idx > 0 && row_idx <= total_rows) {
        final_df[row_idx, ] <- x@data_body[i, ]
      }
    }
  }

  # Insert group i data at group_index_i positions
  if (nrow(x@group_data_i) > 0 && length(x@group_index_i) > 0) {
    for (i in seq_len(nrow(x@group_data_i))) {
      row_idx <- x@group_index_i[i]
      if (!is.na(row_idx) && row_idx > 0 && row_idx <= total_rows) {
        final_df[row_idx, ] <- x@group_data_i[i, ]
      }
    }
  }

  x@data_body <- final_df

  return(x)
}


build_tt <- function(x, output = NULL) {
  output <- sanitize_output(output)

  x <- switch(output,
    html = swap_class(x, "tinytable_bootstrap"),
    bootstrap = swap_class(x, "tinytable_bootstrap"),
    latex = swap_class(x, "tinytable_tabularray"),
    markdown = swap_class(x, "tinytable_grid"),
    gfm = swap_class(x, "tinytable_grid"),
    typst = swap_class(x, "tinytable_typst"),
    dataframe = swap_class(x, "tinytable_dataframe"),
    tabulator = swap_class(x, "tinytable_tabulator"),
  )

  x@output <- output

  # before format_tt() because we need the indices
  x@nrow <- nrow(x@data) + nrow(x@group_data_i)

  # pre-process: theme_*() calls that need formatting conditional on @output
  for (p in x@lazy_prepare) {
    x <- p(x)
  }

  # apply the style_notes
  x <- style_notes(x)
  x <- style_caption(x)

  for (th in x@lazy_theme) {
    fn <- th[[1]]
    args <- th[[2]]
    args[["x"]] <- x
    x <- do.call(fn, args)
  }

  x <- render_fansi(x)

  # Calculate which positions are body vs group
  if (nrow(x@group_data_i) == 0) {
    x@index_body <- seq_len(nrow(x))
  } else {
    x@index_body <- setdiff(seq_len(nrow(x)), x@group_index_i)
  }

  # markdown styles are applied via special format_tt() calls, so they need to
  # happen before evaluating x@lazy_format
  if (x@output %in% c("markdown", "gfm", "dataframe")) {
    x <- style_eval(x)
  }

  # format each component individually, including groups before inserting them into the body
  for (l in x@lazy_format) {
    l[["x"]] <- x

    # For tabulator output, skip formatting for numeric/logical/date columns
    # as these will be handled by tabulator formatters
    if (x@output == "tabulator" && !is.null(l$j)) {
      j_clean <- sanitize_j(l$j, x)
      skip_format <- FALSE

      for (col_idx in j_clean) {
        col_data <- x@data[[col_idx]]
        if (inherits(col_data, c("integer", "numeric", "double", "logical", "Date", "POSIXct", "POSIXlt"))) {
          skip_format <- TRUE
          break
        }
      }

      if (!skip_format) {
        x <- eval(l)
      }
    } else {
      x <- eval(l)
    }
  }

  # insert group rows into body
  x <- rbind_body_groupi(x)

  # plots and images
  for (l in x@lazy_plot) {
    l[["x"]] <- x
    x <- eval(l)
  }

  # add footnote markers just after formatting, otherwise appending converts to string
  x <- footnote_markers(x)

  # data frame we trim strings, pre-padded for markdown
  if (x@output == "dataframe") {
    tmp <- x@data_body
    for (i in seq_along(tmp)) {
      tmp[[i]] <- trimws(tmp[[i]])
    }
    x@data_body <- tmp
  }

  # draw the table
  x <- tt_eval(x)

  # groups require the table to be drawn first, expecially group_tabularray_col() and friends
  # Handle column groups from @group_data_j
  if (nrow(x@group_data_j) > 0) {
    # Calculate ihead for the group headers - start from -1 for the top header row
    ihead <- -1
    # Apply group_eval_j once with all groups
    x <- group_eval_j(x, j = seq_len(ncol(x)), ihead = ihead)
  }

  # style is separate from content in formats that allow it
  if (!x@output %in% c("markdown", "gfm", "dataframe")) {
    for (l in x@lazy_style) {
      l[["x"]] <- x
      # output-specific styling
      if (is.null(l$output) || isTRUE(x@output == l$output)) {
        x <- eval(l)
      }
    }
  }

  # markdown styles are applied earlier
  if (!x@output %in% c("markdown", "gfm", "dataframe")) {
    x <- style_eval(x)
  } else {
    x <- grid_colspan(x)
  }

  x <- finalize(x)

  x@table_string <- lines_drop_consecutive_empty(x@table_string)
  if (output == "gfm") {
    assert_dependency("pandoc")
    x@table_string <- paste(
      pandoc::pandoc_convert(text = x@table_string, to = "gfm"),
      collapse = "\n"
    )
  }

  return(x)
}

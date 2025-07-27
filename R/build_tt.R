# internal function
# style_tt() stores style calls and we only want to evaluate them at the end because
# some rows may be added, which changes how the style is applied
#
# THE ORDER MATTERS A LOT!

#' Build group header and body parts with position calculations
#' @keywords internal
#' @noRd
rbind_body_groupi <- function(x) {
  # Reconstruct the final table by combining formatted data_body and data_group_i parts
  if (nrow(x@data_group_i) == 0) {
    # No groups - @data_body already contains the final formatted data
    return(x)
  }

  # Calculate total final table size
  total_rows <- nrow(x@data_body) + nrow(x@data_group_i)
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

  # Insert group i data at index_group_i positions
  if (nrow(x@data_group_i) > 0 && length(x@index_group_i) > 0) {
    for (i in seq_len(nrow(x@data_group_i))) {
      row_idx <- x@index_group_i[i]
      if (!is.na(row_idx) && row_idx > 0 && row_idx <= total_rows) {
        final_df[row_idx, ] <- x@data_group_i[i, ]
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
    latex = swap_class(x, "tinytable_tabularray"),
    markdown = swap_class(x, "tinytable_grid"),
    gfm = swap_class(x, "tinytable_grid"),
    typst = swap_class(x, "tinytable_typst"),
    dataframe = swap_class(x, "tinytable_dataframe"),
  )

  x@output <- output

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

  # separate group parts for individual formatting
  if (nrow(x@data_group_i) == 0) {
    # No row group insertions - set index for full table as body
    x@index_body <- seq_len(nrow(x@data))
  } else {
    # Calculate which positions are body vs group
    all_positions <- seq_len(nrow(x@data) + nrow(x@data_group_i))
    group_positions <- x@index_group_i
    body_positions <- setdiff(all_positions, group_positions)

    x@index_body <- body_positions
  }

  # before format_tt() because we need the indices
  x@nrow <- nrow(x@data) + nrow(x@data_group_i)

  # format each component individually
  for (l in x@lazy_format) {
    l[["x"]] <- x
    x <- eval(l)
  }

  # insert group rows into body
  x <- rbind_body_groupi(x)

  # add footnote markers just after formatting, otherwise appending converts to string
  x <- footnote_markers(x)

  # plots and images
  for (l in x@lazy_plot) {
    l[["x"]] <- x
    x <- eval(l)
  }

  # data frame we trim strings, pre-padded for markdown
  if (x@output == "dataframe") {
    tmp <- x@data_body
    for (i in seq_along(tmp)) {
      tmp[[i]] <- trimws(tmp[[i]])
    }
    x@data_body <- tmp
  }

  # markdown styles need to be applied before creating the table but after `format_tt()`, otherwise there's annoying parsing, etc.
  if (x@output %in% c("markdown", "gfm", "dataframe")) {
    x <- style_eval(x)
  }

  # draw the table
  x <- tt_eval(x)

  # groups require the table to be drawn first, expecially group_tabularray_col() and friends
  # For Typst and LaTeX, handle all column groups at once from @data_group_j
  if (x@output %in% c("typst", "latex") && nrow(x@data_group_j) > 0) {
    # Calculate ihead for the group headers - start from -1 for the top header row
    ihead <- -1
    # Apply group_eval_j once with all groups
    x <- group_eval_j(x, j = seq_len(ncol(x@data)), ihead = ihead)
  } else {
    # For other formats (HTML), handle column groups from @data_group_j
    if (nrow(x@data_group_j) > 0) {
      # Calculate ihead for the group headers - start from -1 for the top header row
      ihead <- -1
      # Apply group_eval_j once with all groups
      x <- group_eval_j(x, j = seq_len(ncol(x@data)), ihead = ihead)
    }
  }

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

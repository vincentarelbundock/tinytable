# internal function
# style_tt() stores style calls and we only want to evaluate them at the end because
# some rows may be added, which changes how the style is applied
#
# THE ORDER MATTERS A LOT!


#' Build group header and body parts with position calculations
#' @keywords internal
#' @noRd
build_group_parts <- function(x) {
  if (length(x@lazy_group_i) == 0) {
    # No group insertions - set empty header and full table as body
    x@data_header <- data.frame()
    x@data_body <- x@table_dataframe
    x@header_indices <- numeric(0)
    x@body_indices <- seq_len(nrow(x@table_dataframe))
    return(x)
  }

  # Create a temporary copy to process group insertions
  x_temp <- x

  # Apply all group insertions using the existing logic
  for (k in x@lazy_group_i) {
    x_temp <- group_eval_i(x_temp, k)
  }

  # Now we have the full table with group rows inserted
  final_table <- x_temp@table_dataframe

  # Identify which rows are group headers vs body based on group_index_i
  group_indices <- x_temp@group_index_i

  if (length(group_indices) == 0) {
    # No group indices found - set empty header and full table as body
    x@data_header <- data.frame()
    x@data_body <- final_table
    x@header_indices <- numeric(0)
    x@body_indices <- seq_len(nrow(final_table))
    x@group_index_i <- group_indices
    return(x)
  }

  # Split into header and body
  all_indices <- seq_len(nrow(final_table))
  header_indices <- intersect(group_indices, all_indices)
  body_indices <- setdiff(all_indices, header_indices)

  if (length(header_indices) > 0) {
    x@data_header <- final_table[header_indices, , drop = FALSE]
    rownames(x@data_header) <- NULL
  } else {
    x@data_header <- final_table[0, , drop = FALSE]
  }

  if (length(body_indices) > 0) {
    x@data_body <- final_table[body_indices, , drop = FALSE]
    rownames(x@data_body) <- NULL
  } else {
    # Create empty data frame with same structure
    x@data_body <- final_table[0, , drop = FALSE]
  }

  x@header_indices <- header_indices
  x@body_indices <- body_indices
  x@group_index_i <- group_indices
  
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

  x <- clean_fansi(x)

  # Handle groups vs no groups differently
  if (length(x@lazy_group_i) > 0) {
    # Groups case: separate, format, then recombine
    # Step 1: Build group_header and group_body data frames
    x <- build_group_parts(x)
    
    # Step 2: Apply formatting to the separated parts
    # Create temporary table for header formatting
    if (nrow(x@data_header) > 0) {
      x_header <- x
      x_header@table_dataframe <- x@data_header
      for (l in x@lazy_format) {
        l[["x"]] <- x_header
        x_header <- eval(l)
      }
      x@data_header <- x_header@table_dataframe
    }
    
    # Create temporary table for body formatting
    if (nrow(x@data_body) > 0) {
      x_body <- x
      x_body@table_dataframe <- x@data_body
      for (l in x@lazy_format) {
        l[["x"]] <- x_body
        x_body <- eval(l)
      }
      x@data_body <- x_body@table_dataframe
    }
    
    # Step 3: Reconstruct the final table
    # Get the original final order by recreating the full table
    x_temp <- x
    for (k in x@lazy_group_i) {
      x_temp <- group_eval_i(x_temp, k)
    }

    # Now rebuild using our formatted pieces
    final_df <- x_temp@table_dataframe

    # Replace group rows with formatted data_header and body rows with formatted data_body
    if (nrow(x@data_header) > 0 && length(x@header_indices) > 0) {
      for (i in seq_along(x@header_indices)) {
        row_idx <- x@header_indices[i]
        if (row_idx <= nrow(final_df)) {
          final_df[row_idx, ] <- x@data_header[i, ]
        }
      }
    }

    if (nrow(x@data_body) > 0 && length(x@body_indices) > 0) {
      for (i in seq_along(x@body_indices)) {
        row_idx <- x@body_indices[i]
        if (row_idx <= nrow(final_df)) {
          final_df[row_idx, ] <- x@data_body[i, ]
        }
      }
    }

    x@table_dataframe <- final_df
    x@data <- final_df
    x@nrow <- nrow(final_df)
  } else {
    # No groups case: apply formatting directly
    for (l in x@lazy_format) {
      l[["x"]] <- x
      x <- eval(l)
    }
  }

  # add footnote markers just after formatting, otherwise appending converts to string
  x <- footnote_markers(x)

  # plots and images
  for (l in x@lazy_plot) {
    l[["x"]] <- x
    x <- eval(l)
  }

  # data frame we trim strings, pre-padded for markdown
  if (x@output == "dataframe") {
    tmp <- x@table_dataframe
    for (i in seq_along(tmp)) {
      tmp[[i]] <- trimws(tmp[[i]])
    }
    x@table_dataframe <- tmp
  }

  # markdown styles need to be applied before creating the table but after `format_tt()`, otherwise there's annoying parsing, etc.
  if (x@output %in% c("markdown", "gfm", "dataframe")) {
    x <- style_eval(x)
  }

  # draw the table
  x <- tt_eval(x)

  # groups require the table to be drawn first, expecially group_tabularray_col() and friends
  # For Typst and LaTeX, handle all column groups at once from @data_group_j
  if (x@output %in% c("typst", "latex") && length(x@lazy_group_j) > 0) {
    # Calculate ihead for the group headers - start from -1 for the top header row
    ihead <- -1
    # Apply group_eval_j once with all groups
    x <- group_eval_j(x, j = seq_len(ncol(x@data_group_j)), ihead = ihead)
  } else {
    # For other formats, evaluate each group individually
    ihead <- 0
    for (idx in seq_along(x@lazy_group_j)) {
      l <- x@lazy_group_j[[idx]]
      l[["x"]] <- x
      if (length(l[["j"]]) > 0) {
        ihead <- ihead - 1
        l[["ihead"]] <- ihead
      }
      x <- eval(l)
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

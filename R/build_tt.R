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
    return(list(
      group_header = NULL,
      group_body = x@table_dataframe,
      header_indices = NULL,
      body_indices = seq_len(nrow(x@table_dataframe))
    ))
  }
  
  # For this simplified implementation, let's simulate the group_eval_i process
  # but separate the results into header and body parts
  
  # Create a temporary copy to process group insertions
  x_temp <- x
  
  # Apply all group insertions using the existing logic
  for (l in x@lazy_group_i) {
    if (l$fn == "group_eval_i") {
      x_temp <- group_eval_i(x_temp, l$k)
    }
  }
  
  # Now we have the full table with group rows inserted
  final_table <- x_temp@table_dataframe
  original_nrows <- nrow(x@table_dataframe)
  
  # Identify which rows are group headers vs body based on group_index_i
  group_indices <- x_temp@group_index_i
  
  if (length(group_indices) == 0) {
    return(list(
      group_header = NULL,
      group_body = final_table,
      header_indices = NULL,
      body_indices = seq_len(nrow(final_table))
    ))
  }
  
  # Split into header and body
  all_indices <- seq_len(nrow(final_table))
  header_indices <- intersect(group_indices, all_indices)
  body_indices <- setdiff(all_indices, header_indices)
  
  group_header <- NULL
  group_body <- NULL
  
  if (length(header_indices) > 0) {
    group_header <- final_table[header_indices, , drop = FALSE]
    rownames(group_header) <- NULL
  }
  
  if (length(body_indices) > 0) {
    group_body <- final_table[body_indices, , drop = FALSE]
    rownames(group_body) <- NULL
  } else {
    # Create empty data frame with same structure
    group_body <- final_table[0, , drop = FALSE]
  }
  
  return(list(
    group_header = group_header,
    group_body = group_body,
    header_indices = header_indices,
    body_indices = body_indices
  ))
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

  tab <- x@table_dataframe

  # strip ANSI from `tibble`/`pillar`; keep for markdown
  if (isTRUE(check_dependency("fansi"))) {
    for (col in seq_along(tab)) {
      if (isTRUE(x@output == "html")) {
        tab[[col]] <- as.character(fansi::to_html(tab[[col]], warn = FALSE))
      } else if (isTRUE(!x@output %in% c("markdown", "dataframe"))) {
        tab[[col]] <- as.character(fansi::strip_ctl(tab[[col]]))
      }
    }
  }
  x@table_dataframe <- tab

  # Two-step group_tt processing: create group_header and group_body, then apply formatting
  
  # Step 1: Create group_header and group_body data frames with position calculations
  group_parts <- build_group_parts(x)
  group_header <- group_parts$group_header
  group_body <- group_parts$group_body
  header_indices <- group_parts$header_indices
  body_indices <- group_parts$body_indices
  
  # Step 2: Apply lazy_format to each part based on calculated indices
  if (!is.null(group_header)) {
    # Create temporary table object for group_header formatting
    x_header <- x
    x_header@table_dataframe <- group_header
    for (l in x@lazy_format) {
      # Apply formatting to header rows
      should_apply <- FALSE
      l_header <- l
      
      if (is.null(l$i)) {
        # Apply to all rows in header
        should_apply <- TRUE
        # Keep l_header$i as NULL
      } else if (any(l$i %in% header_indices)) {
        # Apply only to specified header indices
        matching_indices <- intersect(l$i, header_indices)
        if (length(matching_indices) > 0) {
          should_apply <- TRUE
          # Adjust indices to match the group_header data frame
          l_header$i <- match(matching_indices, header_indices)
        }
      }
      
      if (should_apply) {
        l_header[["x"]] <- x_header
        x_header <- eval(l_header)
      }
    }
    group_header <- x_header@table_dataframe
  }
  
  if (!is.null(group_body)) {
    # Create temporary table object for group_body formatting  
    x_body <- x
    x_body@table_dataframe <- group_body
    for (l in x@lazy_format) {
      # Apply formatting to body rows
      should_apply <- FALSE
      l_body <- l
      
      if (is.null(l$i)) {
        # Apply to all rows in body
        should_apply <- TRUE
        # Keep l_body$i as NULL
      } else if (any(l$i %in% body_indices)) {
        # Apply only to specified body indices
        matching_indices <- intersect(l$i, body_indices)
        if (length(matching_indices) > 0) {
          should_apply <- TRUE
          # Adjust indices to match the group_body data frame
          l_body$i <- match(matching_indices, body_indices)
        }
      }
      
      if (should_apply) {
        l_body[["x"]] <- x_body
        x_body <- eval(l_body)
      }
    }
    group_body <- x_body@table_dataframe
  }
  
  # Step 3: Combine formatted results into single character data frame
  if (!is.null(group_header) || !is.null(group_body)) {
    # Reassemble the table in the correct order
    final_table <- data.frame()
    
    # Get the original final order by recreating the full table
    x_temp <- x
    for (l in x@lazy_group_i) {
      if (l$fn == "group_eval_i") {
        x_temp <- group_eval_i(x_temp, l$k)
      }
    }
    
    # Now rebuild using our formatted pieces
    final_df <- x_temp@table_dataframe
    group_indices <- x_temp@group_index_i
    
    # Replace group rows with formatted group_header and body rows with formatted group_body
    if (!is.null(group_header) && length(header_indices) > 0) {
      for (i in seq_along(header_indices)) {
        row_idx <- header_indices[i]
        if (row_idx <= nrow(final_df)) {
          final_df[row_idx, ] <- group_header[i, ]
        }
      }
    }
    
    if (!is.null(group_body) && length(body_indices) > 0) {
      for (i in seq_along(body_indices)) {
        row_idx <- body_indices[i]
        if (row_idx <= nrow(final_df)) {
          final_df[row_idx, ] <- group_body[i, ]
        }
      }
    }
    
    x@table_dataframe <- final_df
    x@data <- final_df
    x@nrow <- nrow(final_df)
    x@group_index_i <- x_temp@group_index_i
  } else {
    # No group processing needed, apply standard formatting
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

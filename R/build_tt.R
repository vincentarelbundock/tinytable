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

  # before format_tt() because we need the indices
  x@nrow <- nrow(x@data) + nrow(x@group_data_i)

  return(x)
}


build_tt <- function(x, output = NULL) {
  x <- sanitize_output(x, output)
  output <- infer_output(x)

  x <- switch(output,
    html = {
      if (identical(x@html_engine, "tabulator")) {
        swap_class(x, "tinytable_tabulator")
      } else {
        swap_class(x, "tinytable_html")
      }
    },
    latex = swap_class(x, "tinytable_tabularray"),
    markdown = swap_class(x, "tinytable_grid"),
    typst = swap_class(x, "tinytable_typst"),
    dataframe = swap_class(x, "tinytable_dataframe"),
    stop("Unsupported output format: '", output, "'. Supported formats are: html, latex, markdown, typst, dataframe", call. = FALSE)
  )

  x@output <- output


  # Calculate which positions are body vs group
  if (nrow(x@group_data_i) == 0) {
    x@index_body <- seq_len(nrow(x))
  } else {
    x@index_body <- setdiff(seq_len(nrow(x)), x@group_index_i)
  }

  # format each component individually, including groups before inserting them into the body
  for (l in x@lazy_format) {
    l[["x"]] <- x
    x <- eval(l)
  }

  # apply lazy subset operations before inserting group rows
  x <- subset_lazy(x)

  # insert group rows into body
  x <- rbind_body_groupi(x)

  # pre-process: theme_*() calls that need formatting conditional on @output
  # this is useful after rbind() because we now have the final indices and headers
  for (p in x@lazy_prepare) {
    o <- attr(p, "output")
    if (is.null(o) || x@output %in% o) {
      x <- p(x)
    }
  }

  # plots and images
  for (l in x@lazy_plot) {
    l[["x"]] <- x
    x <- eval(l)
  }

  # add footnote markers just after formatting, otherwise appending converts to string
  x <- footnote_markers(x)

  # Create rectangular style dataframe with one row per cell
  # This happens after row group operations so we have the final row structure
  iseq <- seq_len(nrow(x))
  iseq <- c(-1 * 0:(x@nhead - 1), iseq)  # include headers
  jseq <- seq_len(ncol(x))
  rect <- expand.grid(i = iseq, j = jseq)

  # Initialize all style columns with NA
  style_cols <- c(
    "bold", "italic", "underline", "strikeout", "monospace", "smallcap",
    "align", "alignv", "color", "background", "fontsize", "indent",
    "html_css", "colspan", "rowspan"
  )

  for (col in style_cols) {
    rect[[col]] <- NA
  }

  x@style_other <- rect

  # apply style_tt() and theme_*() after all group operations
  lazy_style <- x@lazy_style
  x@lazy_style <- list()

  for (p in lazy_style) {
    p[["x"]] <- x
    x <- eval(p)
  }

  # Fix colspan that exceeds column count after lazy styles are evaluated
  if (nrow(x@style) > 0) {
    end <- x@style$j + x@style$colspan - 1
    x@style$colspan <- ifelse(
      !is.na(end) & end > x@ncol,
      x@style$colspan - (end - x@ncol),
      x@style$colspan)
  }

  # apply styling AFTER formatting/escaping to avoid escaping the style brackets
  x <- style_notes(x)
  x <- style_caption(x)

  # Populate @style_other by applying each entry from @style sequentially
  # This must happen before build_eval() for backends (like Grid) that apply styles during build
  for (idx in seq_len(nrow(x@style))) {
    style_row <- x@style[idx, , drop = FALSE]
    x@style_other <- apply_style_to_rect(x@style_other, style_row)
  }

  # draw the table
  x <- build_eval(x)

  # groups require the table to be drawn first, expecially group_tabularray_col() and friends
  # Handle column groups from @group_data_j
  if (nrow(x@group_data_j) > 0) {
    # Calculate ihead for the group headers - start from -1 for the top header row
    ihead <- -1
    # Apply group_eval_j once with all groups
    x <- group_eval_j(x, j = seq_len(ncol(x)), ihead = ihead)
  }

  x <- style_eval(x)

  # no-op: only modifies markdown
  x <- grid_colspan(x)

  x <- finalize(x)

  # post-process: theme_*() calls that need formatting conditional on @output and table drawn
  for (p in x@lazy_finalize) {
    o <- attr(p, "output")
    if (is.null(o) || x@output %in% o) {
      x <- p(x)
    }
  }

  x@table_string <- lines_drop_consecutive_empty(x@table_string)
  if (x@markdown_style == "gfm") {
    assert_dependency("pandoc")
    x@table_string <- paste(
      pandoc::pandoc_convert(text = x@table_string, to = "gfm"),
      collapse = "\n"
    )
  }

  return(x)
}


build_prepare <- function(x, fn, output = NULL) {
  attr(fn, "output") <- output
  x@lazy_prepare <- c(x@lazy_prepare, list(fn))
  return(x)
}


build_finalize <- function(x, fn, output = NULL) {
  attr(fn, "output") <- output
  x@lazy_finalize <- c(x@lazy_finalize, list(fn))
  return(x)
}

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Handle deprecated arguments
#' @keywords internal
#' @noRd
handle_deprecated_args <- function(...) {
  if ("indent" %in% ...names()) {
    warning(
      'Argument `indent` is deprecated and will be removed in a future version. Use `style_tt("~group", indent = 1)` instead.',
      call. = FALSE
    )
  }
}

#' Process matrix insertion for row grouping
#' @keywords internal
#' @noRd
progress_group_matrix <- function(x, i, j) {
  k <- group_tt_ij_k(x, i, j)
  converted_from_list <- k[[3]]

  # Calculate indices and update table
  positions <- k[[1]]
  idx <- positions + cumsum(rep(1, length(positions))) - 1
  x@nrow <- x@nrow + length(positions)

  # Create group data frame
  group_matrix <- k[[2]]
  group_df <- as.data.frame(group_matrix, stringsAsFactors = FALSE)

  # Handle row duplication for multiple positions with single matrix row
  if (length(positions) > 1 && nrow(group_df) == 1) {
    group_df <- group_df[rep(1, length(positions)), , drop = FALSE]
  }

  # Set column names to match the table
  if (ncol(group_df) == ncol(x@data)) {
    colnames(group_df) <- colnames(x@data)
  }

  # Add to existing group data or create new
  if (nrow(x@group_data_i) == 0) {
    x@group_data_i <- group_df
    x@group_index_i <- idx
  } else {
    msg <- "Only one group row insertion is allowed at a time with `group_tt(i=...)`."
    stop(msg, call. = FALSE)
  }

  # Apply styling for matrix insertion
  if (converted_from_list) {
    x <- style_tt(x, i = idx, j = 1, colspan = ncol(x))
  }

  return(x)
}

#' Process row grouping with list input
#' @keywords internal
#' @noRd
process_group_i <- function(x, i, j) {
  # Convert list to matrix insertion format for row grouping
  k <- group_tt_ij_k(x, i, NULL) # Pass NULL for j to trigger list conversion
  converted_from_list <- k[[3]]

  # Calculate indices and update table
  positions <- k[[1]]
  idx <- positions + cumsum(rep(1, length(positions))) - 1
  x@nrow <- x@nrow + length(positions)

  # Create group data frame
  group_matrix <- k[[2]]
  group_df <- as.data.frame(group_matrix, stringsAsFactors = FALSE)

  # Set column names to match the table
  if (ncol(group_df) == ncol(x@data)) {
    colnames(group_df) <- colnames(x@data)
  }

  # Add to existing group data or create new
  if (nrow(x@group_data_i) == 0) {
    x@group_data_i <- group_df
  } else {
    x@group_data_i <- rbind_nocol(x@group_data_i, group_df)
  }

  # Add indices to track final positions
  x@group_index_i <- c(x@group_index_i, idx)

  # Apply styling for list-converted group headers
  if (converted_from_list) {
    x <- style_tt(x, i = idx, j = 1, colspan = ncol(x))
  }

  return(x)
}

#' Process column grouping
#' @keywords internal
#' @noRd
process_group_j <- function(x, j) {
  j <- sanitize_group_index(j, hi = ncol(x), orientation = "column")
  x@nhead <- x@nhead + 1

  # Create group row
  new_row <- rep(NA_character_, ncol(x))

  # Set group labels in appropriate columns
  for (i in seq_along(j)) {
    group_name <- names(j)[i]
    group_cols <- j[[i]]
    # Set the label in the first column of the span
    new_row[group_cols[1]] <- group_name
    # For spans > 1: set continuation columns based on output format
    # HTML needs empty strings, Typst needs repeated text
    if (length(group_cols) > 1) {
      new_row[group_cols[-1]] <- ""
    }
  }

  # Add to existing group data or create new
  if (nrow(x@group_data_j) == 0) {
    x@group_data_j <- data.frame(matrix(
      NA_character_,
      nrow = 1,
      ncol = ncol(x@data)
    ))
    colnames(x@group_data_j) <- colnames(x@data)
    x@group_data_j[1, ] <- new_row
  } else {
    new_header_row <- data.frame(matrix(
      NA_character_,
      nrow = 1,
      ncol = ncol(x@data)
    ))
    colnames(new_header_row) <- colnames(x@data)
    new_header_row[1, ] <- new_row
    x@group_data_j <- rbind_nocol(new_header_row, x@group_data_j)
  }

  # Add colspan styling for each group span
  # The group header row index depends on how many group rows exist
  # Most recent group is at -1, previous groups at -2, -3, etc.
  header_row_i <- -nrow(x@group_data_j)
  for (i in seq_along(j)) {
    group_cols <- j[[i]]
    if (length(group_cols) > 1) {
      # Add center alignment and colspan for groups that span multiple columns
      x <- style_tt(x, i = header_row_i, j = group_cols[1], align = "c", colspan = length(group_cols))
    } else {
      # Just center alignment for single-column groups
      x <- style_tt(x, i = header_row_i, j = group_cols[1], align = "c")
    }
  }

  return(x)
}

#' Add styling lines for the newly created column group using user-provided columns
#' @keywords internal
#' @noRd
add_group_line_styling_simple <- function(x, j) {
  # Sanitize j to get proper indices (same as what was passed to group_tt)
  j <- sanitize_group_index(j, hi = ncol(x), orientation = "column")

  # The newly added group will be at row -1 in the final HTML structure
  # (since bootstrap processes groups from last to first, the last group ends up at -1)
  group_row_i <- -1

  # Clear existing styling for all header rows since the row positions will shift
  if (nrow(x@style) > 0) {
    existing_header_mask <- x@style$i < 0 & !is.na(x@style$i)
    if (any(existing_header_mask, na.rm = TRUE)) {
      x@style <- x@style[!existing_header_mask, ]
    }
  }

  # Re-style all group rows (both existing and new) based on final HTML structure
  # Most recent group (last in @group_data_j) goes to row -1, previous groups go to -2, -3, etc.
  for (group_idx in nrow(x@group_data_j):1) {
    table_row_i <- -(nrow(x@group_data_j) - group_idx + 1)

    # Add center alignment
    x <- style_tt(x, i = table_row_i, align = "c")

    # All groups (including the newly added one) are now stored in @group_data_j
    # So we can reconstruct all of them from stored data
    group_row_data <- as.character(x@group_data_j[group_idx, ])
    current_j <- parse_group_row_spans(group_row_data)

    # Add lines for each named group
    for (idx in seq_along(current_j)) {
      group_name <- names(current_j)[idx]
      group_cols <- current_j[[idx]]

      # Only add lines for non-empty group names
      if (!is.null(group_name) && trimws(group_name) != "") {
        # Determine trimming based on column positions
        trim_left <- min(group_cols) > 1
        trim_right <- max(group_cols) < ncol(x)

        # Build trim specification
        line_trim_spec <- ""
        if (trim_left && trim_right) {
          line_trim_spec <- "lr"
        } else if (trim_left) {
          line_trim_spec <- "l"
        } else if (trim_right) {
          line_trim_spec <- "r"
        } else {
          line_trim_spec <- NULL  # No trimming
        }

        x <- style_tt(
          x,
          i = table_row_i,
          j = group_cols,
          line = "b",
          line_width = 0.05,
          line_color = "black",
          line_trim = line_trim_spec
        )
      }
    }
  }

  return(x)
}


#' Parse a group row into column spans (similar to bootstrap_groupj_span)
#' @keywords internal
#' @noRd
parse_group_row_spans <- function(group_row) {
  j_list <- list()
  i <- 1

  while (i <= length(group_row)) {
    current_label <- group_row[i]

    # Skip NA (ungrouped) columns
    if (is.na(current_label)) {
      i <- i + 1
      next
    }

    span_start <- i

    # Find the end of this span
    if (trimws(current_label) != "") {
      i <- i + 1 # Move past the current label
      # Continue through empty strings (continuation of span)
      while (
        i <= length(group_row) &&
          !is.na(group_row[i]) &&
          trimws(group_row[i]) == ""
      ) {
        i <- i + 1
      }
      span_end <- i - 1

      # Add to j_list if non-empty label
      j_list[[current_label]] <- span_start:span_end
    } else {
      i <- i + 1
    }
  }

  j_list
}

#' Process delimiter-based column grouping
#' @keywords internal
#' @noRd
process_delimiter_grouping <- function(x, j) {
  if (any(grepl(j, x@names, fixed = TRUE))) {
    j_delim <- j_delim_to_named_list(x = x, j = j)
    x@names <- j_delim$colnames

    # Apply multiple levels of grouping if they exist (in reverse order)
    if (length(j_delim$groupnames) > 0) {
      for (level_groups in rev(j_delim$groupnames)) {
        x <- process_group_j(x, level_groups)
        # Add line styling for each group level
        x <- add_group_line_styling_simple(x, level_groups)
      }
    }
    j <- NULL # Set to NULL since we've already applied the groupings
  } else {
    j <- NULL
  }
  return(list(x = x, j = j))
}

# =============================================================================
# MAIN FUNCTION
# =============================================================================

#' Spanning labels to identify groups of rows or columns
#'
#' @export
#' @inheritParams tt
#' @inheritParams style_tt
#' @param i Character vector, named list, or integer vector
#' + A character vector of labels with length equal to the number of rows in `x`
#' + A named list of row indices to group. The names of the list will be used as labels. The indices represent the position where labels should be inserted in the original table. For example,
#'   - `i=list("Hello"=5)`: insert the "Hello" label after the 4th row in the original table.
#'   - `i=list("Hello"=2, "World"=2)`: insert the two labels consecutively after the 1st row in the original table.
#'   - `i=list("Foo Bar"=0)`: insert the label in the first row after the header.
#' + Vector of positive integers: For matrix insertion: `i` specifies row positions and `j` must be a character matrix to insert in the table (see below for details).
#' @param j String, named list, or character matrix
#' - Named list of column indices to group, ex: `j=list("A"=1:2,"B"=3:6)`. The names of the list will be used as labels. See below for more examples. Note: empty labels must be a space: " ".
#' - A single string when column names include the group name as a prefix, ex: group1_column1, group1_column2, etc.
#' - Character matrix for inserting rows at positions specified by `i`. The matrix must have the same number of columns as the table, or be a single column with a number of elements that is a multiple of the table's column count (which will be automatically reshaped). Each row of the matrix matches an element
#' - Unquoted expression: Non-standard evaluation is supported. When supplying an unquoted expression, it is first evaluated in the calling environment, then in the data frame passed to `tt()`.
#' @param ... Other arguments are ignored.
#' @return An object of class `tt` representing the table.
#' @template limitations_word_markdown
#' @details
#' Warning: The `style_tt()` can normally be used to style the group headers, as expected, but that feature is not available for Markdown and Word tables.
#' @examples
#'
#' # vector of row labels
#' dat <- data.frame(
#'   label = c("a", "a", "a", "b", "b", "c", "a", "a"),
#'   x1 = rnorm(8),
#'   x2 = rnorm(8)
#' )
#' tt(dat[, 2:3]) |> group_tt(i = dat$label)
#'
#' # named lists of labels
#' tt(mtcars[1:10, 1:5]) |>
#'   group_tt(
#'     i = list(
#'       "Hello" = 3,
#'       "World" = 8
#'     ),
#'     j = list(
#'       "Foo" = 2:3,
#'       "Bar" = 4:5
#'     )
#'   )
#'
#' dat <- mtcars[1:9, 1:8]
#' tt(dat) |>
#'   group_tt(i = list(
#'     "I like (fake) hamburgers" = 3,
#'     "She prefers halloumi" = 4,
#'     "They love tofu" = 7
#'   ))
#'
#' tt(dat) |>
#'   group_tt(
#'     j = list(
#'       "Hamburgers" = 1:3,
#'       "Halloumi" = 4:5,
#'       "Tofu" = 7
#'     )
#'   )
#'
#' x <- mtcars[1:5, 1:6]
#' tt(x) |>
#'   group_tt(j = list("Hello" = 1:2, "World" = 3:4, "Hello" = 5:6)) |>
#'   group_tt(j = list("Foo" = 1:3, "Bar" = 4:6))
#'
#' # column names with delimiters
#' dat <- data.frame(
#'   A_id = 1,
#'   A_a1 = 2,
#'   A_a2 = "3",
#'   B_b1 = 4,
#'   B_b2 = 5,
#'   B_C = 6
#' )
#' tt(dat) |> group_tt(j = "_")
#'
#' # matrix insertion
#' rowmat <- matrix(colnames(iris))
#' tt(head(iris, 7)) |>
#'   group_tt(i = c(2, 5), j = rowmat)
#'
#' rowmat <- matrix(c(
#'   "a", "b", "c", "d", "e",
#'   1, 2, 3, 4, 5))
#' tt(head(iris, 7)) |>
#'   group_tt(i = 2, j = rowmat) |>
#'   style_tt(i = "groupi", background = "pink")
group_tt <- function(
    x,
    i = getOption("tinytable_group_i", default = NULL),
    j = getOption("tinytable_group_j", default = NULL),
    ...) {
  handle_deprecated_args(...)
  if (!inherits(x, "tinytable")) {
    stop("`x` must be generated by `tinytable::tt()`.", call. = FALSE)
  }

  # non-standard evaluation before anything else
  tmp <- nse_i_j(x,
    i_expr = substitute(i),
    j_expr = substitute(j),
    pf = parent.frame())
  i <- tmp$i

  if (is.null(i) && is.null(j)) {
    stop("At least one of `i` or `j` must be specified.", call. = FALSE)
  }

  if (isTRUE(check_atomic_vector(i)) && !is.list(i) && length(i) > 1 && is.null(j)) {
    i <- sanitize_group_vec2list(i)
  }

  # matrix insertion case
  if ((isTRUE(check_integerish(i)) && isTRUE(check_matrix(j))) ||
    (is.list(i) && is.null(j))) {
    return(progress_group_matrix(x, i, j))
  }

  # row grouping when i is a list (but j is also provided)
  if (is.list(i) && !is.null(j)) {
    x <- process_group_i(x, i, j)
  }

  # delimiter-based column grouping
  if (isTRUE(check_string(j))) {
    result <- process_delimiter_grouping(x, j)
    x <- result$x
    j <- result$j
  }

  # column grouping
  if (!is.null(j)) {
    x <- process_group_j(x, j)
    x <- add_group_line_styling_simple(x, j)
  }

  return(x)
}

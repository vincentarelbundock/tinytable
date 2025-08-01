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
process_matrix_insertion <- function(x, i, j) {
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
process_row_grouping <- function(x, i, j) {
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
process_column_grouping <- function(x, j) {
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
    # Set empty string in continuation columns (if span > 1)
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

  return(x)
}

#' Process delimiter-based column grouping
#' @keywords internal
#' @noRd
process_delimiter_grouping <- function(x, j) {
  if (any(grepl(j, x@names, fixed = TRUE))) {
    j_delim <- j_delim_to_named_list(x = x, j = j)
    x@names <- j_delim$colnames
    j <- j_delim$groupnames
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
  ...
) {
  handle_deprecated_args(...)
  if (!inherits(x, "tinytable")) {
    stop("`x` must be generated by `tinytable::tt()`.", call. = FALSE)
  }
  if (is.null(i) && is.null(j)) {
    stop("At least one of `i` or `j` must be specified.", call. = FALSE)
  }

  if (is.vector(i) && !is.list(i) && length(i) > 1 && is.null(j)) {
    i <- sanitize_group_vec2list(i)
  }

  # matrix insertion case
  if (
    (isTRUE(check_integerish(i)) && isTRUE(check_matrix(j))) ||
      (is.list(i) && is.null(j))
  ) {
    return(process_matrix_insertion(x, i, j))
  }

  # row grouping when i is a list (but j is also provided)
  if (is.list(i) && !is.null(j)) {
    x <- process_row_grouping(x, i, j)
  }

  # delimiter-based column grouping
  if (isTRUE(check_string(j))) {
    result <- process_delimiter_grouping(x, j)
    x <- result$x
    j <- result$j
  }

  # column grouping
  if (!is.null(j)) {
    x <- process_column_grouping(x, j)
  }

  return(x)
}

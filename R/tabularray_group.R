#' tinytable S4 method
#'
#' @keywords internal
setMethod(
  f = "group_eval_j",
  signature = "tinytable_tabularray",
  definition = function(x, i = NULL, j = NULL, ...) {
    # Only handle column grouping - row insertions now use matrix insertion
    x <- group_tabularray_col(x, j, ...)
    return(x)
  })

group_tabularray_col <- function(x, j, ihead, ...) {
  # Process column groups from @group_data_j
  if (nrow(x@group_data_j) > 0) {
    # Process each header row from bottom to top (reverse order) to match expected header order
    for (row_idx in nrow(x@group_data_j):1) {
      group_row <- as.character(x@group_data_j[row_idx, ])

      # Build and insert header row
      header_line <- build_tabularray_header(group_row, ncol(x))
      x <- insert_tabularray_header(x, header_line)
    }

    # Apply styling for each header row (reverse order to match header insertion)
    for (row_idx in nrow(x@group_data_j):1) {
      group_row <- as.character(x@group_data_j[row_idx, ])
      # Calculate the correct ihead for this specific row
      # The styling order should match the header insertion order
      header_position <- nrow(x@group_data_j) - row_idx + 1
      row_ihead <- ihead - (header_position - 1)

      # Apply styling to spans
      x <- style_tabularray_header_spans(x, group_row, row_ihead)
    }
  }

  return(x)
}

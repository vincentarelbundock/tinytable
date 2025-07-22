#' @keywords internal
#' @noRd
group_insert_matrix <- function(x, k) {
  positions <- k[[1]]
  matrix_data <- k[[2]]

  if (!is.character(matrix_data)) {
    stop("Matrix `j` must be a character matrix", call. = FALSE)
  }

  # Always convert to data frame
  matrix_df <- as.data.frame(matrix_data, stringsAsFactors = FALSE)

  # Check column count assertion
  if (ncol(matrix_df) != ncol(x@table_dataframe)) {
    stop("Matrix must have the same number of columns as the table", call. = FALSE)
  }

  # Set column names to match the table
  names(matrix_df) <- names(x@table_dataframe)

  # Simplify logic by standardizing positions and matrix rows
  matrix_rows <- nrow(matrix_df)

  # If single position but multiple matrix rows, replicate the position
  if (length(positions) == 1 && matrix_rows > 1) {
    positions <- rep(positions[1], matrix_rows)
  }

  # If multiple positions but single matrix row, replicate the matrix row
  if (length(positions) > 1 && matrix_rows == 1) {
    matrix_df <- matrix_df[rep(1, length(positions)), , drop = FALSE]
    matrix_rows <- nrow(matrix_df)
  }

  # Validate that positions and matrix rows match
  if (length(positions) != matrix_rows) {
    stop("Length of positions must equal number of matrix rows after standardization", call. = FALSE)
  }

  # Process each matrix row at its corresponding position
  # Sort by position in descending order to avoid index shifting issues
  pos_order <- order(positions, decreasing = TRUE)

  for (i in seq_along(pos_order)) {
    idx <- pos_order[i]
    pos <- positions[idx]
    matrix_row <- matrix_df[idx, , drop = FALSE]

    # Insert the matrix row at the specified position
    # Position logic: 1 means at the top, 2 means after first row, etc.
    if (pos > nrow(x@table_dataframe)) {
      # Insert at the end
      x@table_dataframe <- rbind(x@table_dataframe, matrix_row)
    } else if (pos == 1) {
      # Insert at the very top
      colnames(matrix_row) <- colnames(x@table_dataframe)
      x@table_dataframe <- rbind(matrix_row, x@table_dataframe)
    } else {
      # Insert after the (pos-1)th row
      before_rows <- x@table_dataframe[1:(pos - 1), , drop = FALSE]
      after_rows <- x@table_dataframe[pos:nrow(x@table_dataframe), , drop = FALSE]

      # guard against malformed column names when tt(colnames=FALSE)
      colnames(matrix_row) <-
        colnames(before_rows) <-
        colnames(after_rows) <-
        seq_len(ncol(matrix_row))

      # empty rows
      rows <- list(before_rows, matrix_row, after_rows)
      rows <- Filter(function(x) nrow(x) > 0, rows)

      # Combine the rows
      x@table_dataframe <- do.call(rbind, rows)
    }
  }

  # Update all slots that need to be modified
  x@data <- x@table_dataframe

  return(x)
}

#' @keywords internal
#' @noRd
group_insert_matrix <- function(x, k) {
  positions <- k[[1]]
  matrix_data <- k[[2]]

  if (!is.character(matrix_data)) {
    stop("Matrix `j` must be a character matrix", call. = FALSE)
  }

  # Convert to data frame
  matrix_df <- as.data.frame(matrix_data, stringsAsFactors = FALSE)

  # Check column count
  if (ncol(matrix_df) != ncol(x@table_dataframe)) {
    stop(
      "Matrix must have the same number of columns as the table",
      call. = FALSE
    )
  }

  # Set column names to match the table
  names(matrix_df) <- names(x@table_dataframe)

  # Standardize: if single position with multiple matrix rows, replicate position
  if (length(positions) == 1 && nrow(matrix_df) > 1) {
    positions <- rep(positions[1], nrow(matrix_df))
  }
  # If multiple positions with single matrix row, replicate the row
  if (length(positions) > 1 && nrow(matrix_df) == 1) {
    matrix_df <- matrix_df[rep(1, length(positions)), , drop = FALSE]
  }

  # Insert rows in reverse order to avoid index shifting
  insertion_order <- order(positions, decreasing = TRUE)
  
  for (i in insertion_order) {
    pos <- positions[i]
    row_to_insert <- matrix_df[i, , drop = FALSE]
    
    # Insert row at position (1 = top, 2 = after first row, etc.)
    if (pos <= 1) {
      x@table_dataframe <- rbind(row_to_insert, x@table_dataframe)
    } else if (pos > nrow(x@table_dataframe)) {
      x@table_dataframe <- rbind(x@table_dataframe, row_to_insert)
    } else {
      before <- x@table_dataframe[1:(pos - 1), , drop = FALSE]
      after <- x@table_dataframe[pos:nrow(x@table_dataframe), , drop = FALSE]
      x@table_dataframe <- rbind(before, row_to_insert, after)
    }
  }

  # Update tracking
  x@data <- x@table_dataframe
  x@nrow <- nrow(x@table_dataframe)
  
  # Track matrix positions for styling
  if (length(positions) > 0) {
    unique_positions <- sort(unique(positions))
    final_positions <- numeric(0)
    for (pos in unique_positions) {
      count <- sum(positions == pos)
      final_positions <- c(final_positions, seq(pos, pos + count - 1))
    }
    x@group_index_i_matrix <- c(x@group_index_i_matrix, final_positions)
    x@group_index_i_matrix <- sort(unique(x@group_index_i_matrix))
  }

  return(x)
}
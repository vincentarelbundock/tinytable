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
    stop(
      "Matrix must have the same number of columns as the table",
      call. = FALSE
    )
  }

  # Set column names to match the table
  names(matrix_df) <- names(x@table_dataframe)

  # Simplify logic by standardizing positions and matrix rows
  matrix_rows <- nrow(matrix_df)

  # Check if positions are sequential
  are_sequential <- function(pos) {
    if (length(pos) <= 1) return(FALSE)
    sorted_pos <- sort(pos)
    all(diff(sorted_pos) == 1)
  }

  # Handle the different cases:
  if (length(positions) == 1 && matrix_rows > 1) {
    # Single position, multiple matrix rows: replicate the position
    positions <- rep(positions[1], matrix_rows)
  } else if (length(positions) > 1 && matrix_rows == 1) {
    # Multiple positions, single matrix row
    if (are_sequential(positions)) {
      # Sequential positions: insert all rows consecutively at the first position
      positions <- rep(min(positions), length(positions))
      matrix_df <- matrix_df[rep(1, length(positions)), , drop = FALSE]
      matrix_rows <- nrow(matrix_df)
    } else {
      # Non-sequential positions: replicate the matrix row for each position
      matrix_df <- matrix_df[rep(1, length(positions)), , drop = FALSE]
      matrix_rows <- nrow(matrix_df)
    }
  }

  # Validate that positions and matrix rows match
  if (length(positions) != matrix_rows) {
    stop(
      "Length of positions must equal number of matrix rows after standardization",
      call. = FALSE
    )
  }

  # Process each matrix row at its corresponding position
  # Sort by position in descending order to avoid index shifting issues
  pos_order <- order(positions, decreasing = TRUE)
  
  # Track actual final positions of inserted rows for group_index_i
  final_positions <- numeric(length(positions))

  for (i in seq_along(pos_order)) {
    idx <- pos_order[i]
    pos <- positions[idx]
    matrix_row <- matrix_df[idx, , drop = FALSE]

    # Insert the matrix row at the specified position
    # Position logic: 1 means at the top, 2 means after first row, etc.
    if (pos > nrow(x@table_dataframe)) {
      # Insert at the end
      final_positions[idx] <- nrow(x@table_dataframe) + 1
      x@table_dataframe <- rbind(x@table_dataframe, matrix_row)
    } else if (pos == 1) {
      # Insert at the very top
      final_positions[idx] <- 1
      colnames(matrix_row) <- colnames(x@table_dataframe)
      x@table_dataframe <- rbind(matrix_row, x@table_dataframe)
    } else {
      # Insert after the (pos-1)th row
      final_positions[idx] <- pos
      before_rows <- x@table_dataframe[1:(pos - 1), , drop = FALSE]
      after_rows <- x@table_dataframe[
        pos:nrow(x@table_dataframe),
        ,
        drop = FALSE
      ]

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
  
  # Update nrow to reflect actual table size after insertions
  x@nrow <- nrow(x@table_dataframe)
  
  # Track matrix insertion positions separately from regular group rows
  # The positions that were added to group_index_i for this matrix insertion
  # should be tracked as matrix positions
  # For a matrix insertion at position i with n rows, the positions are i, i+1, ..., i+n-1
  if (length(positions) > 0) {
    # Calculate the actual positions that were added to group_index_i
    base_position <- min(positions)
    num_rows <- length(positions)
    if (num_rows == 1) {
      num_rows <- nrow(matrix_df)
    }
    
    actual_positions <- seq(base_position, base_position + num_rows - 1)
    x@group_index_i_matrix <- c(x@group_index_i_matrix, actual_positions)
    x@group_index_i_matrix <- sort(unique(x@group_index_i_matrix))
  }

  return(x)
}

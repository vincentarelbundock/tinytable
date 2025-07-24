#' Helper functions for row insertion in group_tt
#'
#' @keywords internal
#' @noRd
group_tt_ij_k <- function(x, i, j) {
  # Convert list input to matrix format for unified processing
  converted_from_list <- FALSE
  if (is.list(i) && is.null(j)) {
    # Convert list like list("Blah blah" = 2) to matrix insertion format
    positions <- unlist(i)
    labels <- names(i)

    if (is.null(labels) || any(labels == "")) {
      stop("All list entries must have names for group labels.", call. = FALSE)
    }

    # Create matrix with labels in first column, empty in others
    matrix_data <- matrix("", nrow = length(labels), ncol = ncol(x))
    matrix_data[, 1] <- labels

    # Convert to matrix insertion format
    i <- positions
    j <- matrix_data
    converted_from_list <- TRUE
  }

  # Ensure j is a character matrix (coerce if needed)
  if (!is.character(j)) {
    j <- as.character(j)
  }

  # If x has more than 1 column and j is a 1-column matrix, try to reshape j
  if (is.matrix(j) && ncol(x) > 1 && ncol(j) == 1) {
    total_elements <- nrow(j) * ncol(j)
    if (total_elements %% ncol(x) == 0) {
      # Reshape j to have the same number of columns as x
      j <- matrix(
        j,
        nrow = total_elements / ncol(x),
        ncol = ncol(x),
        byrow = TRUE
      )
    }
  }

  # Check that matrix width matches table width
  if (is.matrix(j) && ncol(j) != ncol(x)) {
    stop(
      sprintf(
        "Matrix must have the same number of columns as the table (%d columns)",
        ncol(x)
      ),
      call. = FALSE
    )
  }

  # Validate row insertion positions against table size
  if (any(i > nrow(x) + 1)) {
    stop(sprintf("`i` should be smaller than %s", nrow(x) + 1), call. = FALSE)
  }

  # If single position but multiple matrix rows, replicate the position
  matrix_rows <- nrow(j)
  if (length(i) == 1 && matrix_rows > 1) {
    i <- rep(i, matrix_rows)
  }

  list(i, j, converted_from_list)
}

#' Perform row insertion for group_tt matrix operations
#'
#' @keywords internal
#' @noRd
group_eval_i <- function(x, k) {
  positions <- k[[1]]
  matrix_data <- k[[2]]

  # Convert to data frame
  matrix_df <- as.data.frame(matrix_data, stringsAsFactors = FALSE)

  # Set column names to match the table
  table_names <- names(x@table_dataframe)
  if (is.null(table_names)) {
    table_names <- names(x@data)
  }
  
  # Handle column name assignment more gracefully
  if (length(table_names) == ncol(matrix_df)) {
    names(matrix_df) <- table_names
  } else if (length(table_names) > ncol(matrix_df)) {
    # Use only the first n table names if table has more columns
    names(matrix_df) <- table_names[1:ncol(matrix_df)]
  } else {
    # If matrix has more columns than table, use table names and pad with defaults
    new_names <- c(table_names, paste0("V", (length(table_names) + 1):ncol(matrix_df)))
    names(matrix_df) <- new_names
  }

  # Standardize: if single position with multiple matrix rows, replicate position
  if (length(positions) == 1 && nrow(matrix_df) > 1) {
    positions <- rep(positions[1], nrow(matrix_df))
  }
  # If multiple positions with single matrix row, replicate the row
  if (length(positions) > 1 && nrow(matrix_df) == 1) {
    matrix_df <- matrix_df[rep(1, length(positions)), , drop = FALSE]
  }

  # Insert rows in reverse order to avoid index shifting
  # When positions are equal, we want to insert in reverse order of appearance
  # so they end up in the correct final order
  insertion_order <- order(as.numeric(positions), seq_along(positions), decreasing = c(TRUE, TRUE))

  for (i in insertion_order) {
    pos <- positions[i]
    row_to_insert <- matrix_df[i, , drop = FALSE]

    # Insert row at position (1 = top, 2 = after first row, etc.)
    if (pos <= 1) {
      colnames(row_to_insert) <- colnames(x@table_dataframe)
      x@table_dataframe <- rbind(row_to_insert, x@table_dataframe)
    } else if (pos > nrow(x@table_dataframe)) {
      colnames(row_to_insert) <- colnames(x@table_dataframe)
      x@table_dataframe <- rbind(x@table_dataframe, row_to_insert)
    } else {
      before <- x@table_dataframe[1:(pos - 1), , drop = FALSE]
      after <- x@table_dataframe[pos:nrow(x@table_dataframe), , drop = FALSE]
      colnames(before) <- colnames(after) <- colnames(row_to_insert)
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

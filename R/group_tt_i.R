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
  # For vector-based grouping (consecutive groups), the indices represent group boundaries
  # and can be larger than the table size since they represent insertion points
  if (any(i > nrow(x) + 1)) {
    # Check if this is a vector-based grouping case (consecutive groups)
    # In this case, the indices represent where groups should be inserted
    # and can be larger than the current table size
    if (length(i) > 1 && all(diff(i) >= 0)) {
      # This looks like consecutive grouping, allow it
    } else {
      stop(sprintf("`i` should be smaller than %s", nrow(x) + 2), call. = FALSE)
    }
  }

  # If single position but multiple matrix rows, replicate the position
  matrix_rows <- nrow(j)
  if (length(i) == 1 && matrix_rows > 1) {
    i <- rep(i, matrix_rows)
  }

  list(i, j, converted_from_list)
}

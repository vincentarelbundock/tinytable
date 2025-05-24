insert_values <- function(vec, values, positions) {
    if (length(values) != length(positions)) {
        stop("The length of values and positions must be the same")
    }

    # Sort the positions in decreasing order along with their corresponding values
    ord <- order(positions, decreasing = TRUE)
    values <- values[ord]
    positions <- positions[ord]

    # Create a vector of indices for the original vector
    original_indices <- seq_along(vec)

    # Insert values and update indices
    for (i in seq_along(values)) {
        vec <- append(vec, values[i], after = positions[i] - 1)
        original_indices <- append(
            original_indices,
            NA,
            after = positions[i] - 1
        )
    }

    # Return the extended vector and the original indices vector
    return(data.frame(vec = vec, old = original_indices, new = seq_along(vec)))
}

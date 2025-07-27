format_vector_replace <- function(vec, vec_original = NULL, replace = NULL, ...) {
  if (is.null(replace) || isFALSE(replace) || length(replace) == 0) {
    return(vec)
  }

  result <- vec

  for (z in seq_along(replace)) {
    new <- names(replace)[z]
    old_vals <- replace[[z]]

    for (old in old_vals) {
      match_idx <- vapply(
        seq_along(result),
        function(i) {
          x <- result[[i]]
          x0 <- if (!is.null(vec_original)) vec_original[[i]] else x

          any(c(
            is.na(old) && is.na(x),
            is.na(old) && is.na(x0),
            is.nan(old) && is.nan(x),
            is.nan(old) && is.nan(x0),
            is.infinite(old) && is.infinite(x) && typeof(x) == "double" && sign(x) == sign(old),
            is.infinite(old) && is.infinite(x0) && typeof(x0) == "double" && sign(x0) == sign(old),
            identical(old, x),
            identical(old, x0)
          ))
        },
        logical(1)
      )

      result[match_idx] <- new
    }
  }

  return(result)
}

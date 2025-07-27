format_vector_replace <- function(vec, replace = NULL, ...) {
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

          if (is.na(old) && is.na(x)) {
            TRUE
          } else if (is.nan(old) && is.nan(x)) {
            TRUE
          } else if (
            is.infinite(old) &&
              is.infinite(x) &&
              typeof(x) == "double" &&
              typeof(old) == "double" &&
              sign(x) == sign(old)
          ) {
            TRUE
          } else {
            identical(old, x)
          }
        },
        logical(1)
      )

      result[match_idx] <- new
    }
  }

  return(result)
}

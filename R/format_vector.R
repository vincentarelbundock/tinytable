format_vector_sprintf <- function(vec, sprintf_pattern = NULL) {
  if (is.null(sprintf_pattern)) {
    return(NULL)
  }
  base::sprintf(sprintf_pattern, vec)
}

format_vector_logical <- function(vec, bool_fn = NULL) {
  if (!is.logical(vec) || is.null(bool_fn)) {
    return(NULL)
  }
  bool_fn(vec)
}

format_vector_date <- function(vec, date_format = NULL) {
  if (!inherits(vec, "Date") || is.null(date_format)) {
    return(NULL)
  }
  format(vec, date_format)
}

format_vector_other <- function(vec, other_fn = NULL) {
  if (!is.function(other_fn)) {
    return(NULL)
  }
  other_fn(vec)
}

format_vector_custom <- function(vec, fn = NULL) {
  if (!is.function(fn)) {
    return(NULL)
  }
  fn(vec)
}

format_vector_math <- function(vec, math = FALSE) {
  if (!isTRUE(math)) {
    return(NULL)
  }
  sprintf("$%s$", vec)
}

format_vector_replace <- function(vec, replace = NULL) {
  if (is.null(replace) || isFALSE(replace) || length(replace) == 0) {
    return(vec)
  }
  result <- vec
  for (w in seq_along(replace)) {
    for (z in seq_along(result)) {
      new <- replace[[w]]
      old <- names(replace)[[w]]
      if (identical(old, result[z])) {
        result[z] <- new
      }
    }
  }
  return(result)
}


format_vector_markdown <- function(vec, output_format) {
  if (is.null(output_format)) {
    return(NULL)
  }

  if (output_format == "html") {
    vapply(
      vec,
      function(k) {
        k <- litedown::mark(I(k), "html")
        k <- sub("<p>", "", k, fixed = TRUE)
        k <- sub("</p>", "", k, fixed = TRUE)
        return(k)
      },
      character(1)
    )
  } else if (output_format == "latex") {
    vapply(
      vec,
      function(k) {
        litedown::mark(I(k), "latex")
      },
      character(1)
    )
  } else {
    vec
  }
}

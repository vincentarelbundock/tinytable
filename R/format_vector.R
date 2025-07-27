format_vector_sprintf <- function(vec, sprintf_pattern = NULL, ...) {
  if (is.null(sprintf_pattern)) {
    return(NULL)
  }
  base::sprintf(sprintf_pattern, vec)
}

format_vector_logical <- function(vec, bool_fn = NULL, ...) {
  if (!is.logical(vec) || is.null(bool_fn)) {
    return(NULL)
  }
  bool_fn(vec)
}

format_vector_date <- function(vec, date_format = NULL, ...) {
  if (!inherits(vec, "Date") || is.null(date_format)) {
    return(NULL)
  }
  format(vec, date_format)
}

format_vector_other <- function(vec, other_fn = NULL, ...) {
  if (!is.function(other_fn)) {
    return(NULL)
  }
  other_fn(vec)
}

format_vector_custom <- function(vec, fn = NULL, ...) {
  if (!is.function(fn)) {
    return(NULL)
  }
  fn(vec)
}

format_vector_math <- function(vec, math = FALSE, ...) {
  if (!isTRUE(math)) {
    return(NULL)
  }
  sprintf("$%s$", vec)
}

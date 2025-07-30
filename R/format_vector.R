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

format_vector_quarto <- function(i, col, x, ...) {
  out <- x@data_body
  if (isTRUE(x@output %in% c("html", "bootstrap", "tabulator"))) {
    fun <- function(z) {
      z@table_string <- sub(
        "data-quarto-disable-processing='true'",
        "data-quarto-disable-processing='false'",
        z@table_string,
        fixed = TRUE
      )
      return(z)
    }
    x <- style_tt(x, finalize = fun)
    out[i, col] <- sprintf(
      '<span data-qmd="%s"></span>',
      out[i, col, drop = TRUE]
    )
  } else if (isTRUE(x@output == "latex")) {
    assert_dependency("base64enc")
    tmp <- sapply(
      out[i, col, drop = TRUE],
      function(z) base64enc::base64encode(charToRaw(z))
    )
    out[i, col] <- sprintf("\\QuartoMarkdownBase64{%s}", tmp)
  }

  x@data_body <- out
  return(x)
}

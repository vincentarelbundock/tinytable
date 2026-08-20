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

format_vector_linebreak <- function(vec, linebreak = NULL, output = NULL, ...) {
  if (is.null(linebreak)) {
    return(NULL)
  }

  # Determine the appropriate line break sequence based on output format
  if (is.null(output) || output == "markdown") {
    return(NULL) # No line break replacement for markdown
  }

  if (output %in% c("html", "bootstrap", "tabulator")) {
    lb <- "<br>"
  } else if (output %in% c("latex", "pdf")) {
    lb <- "\\\\"
    # tabularray wrapper for line breaks
    if (any(grepl(linebreak, vec, fixed = TRUE))) {
      vec <- sprintf("{%s}", vec)
    }
  } else if (output == "typst") {
    # needs a space in typst
    lb <- " \\ "
  } else {
    return(NULL) # Unknown output format
  }

  gsub(linebreak, lb, vec, fixed = TRUE)
}

format_vector_quarto <- function(vec, output_format, ...) {
  if (is.null(output_format)) {
    return(NULL)
  }

  if (output_format %in% c("html", "bootstrap", "tabulator")) {
    sprintf('<span data-qmd="%s"></span>', vec)
  } else if (output_format == "latex") {
    assert_dependency("base64enc")
    vapply(
      vec,
      function(z) {
        sprintf(
          "\\QuartoMarkdownBase64{%s}",
          base64enc::base64encode(charToRaw(z))
        )
      },
      character(1)
    )
  } else {
    vec
  }
}

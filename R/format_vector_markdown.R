format_vector_markdown <- function(vec, output_format, ...) {
  if (is.null(output_format)) {
    return(NULL)
  }

  if (output_format %in% c("html", "bootstrap", "tabulator")) {
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

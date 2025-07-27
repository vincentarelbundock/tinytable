format_vector_quarto <- function(i, col, x, ...) {
  out <- x@data_body
  if (isTRUE(x@output == "html")) {
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

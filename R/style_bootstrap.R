build_bootstrap_css <- function(css_vector, id, type = "cell") {
  if (type == "row") {
    out <- sprintf(".table tr.%s td {", id)
  } else if (type == "cell") {
    out <- sprintf(".table td.%s {", id)
  }
  out <- c(out, paste0(css_vector, ";"))
  out <- paste(out, collapse = " ")
  out <- paste(out, "}")
  return(out)
}


#' @export
style_bootstrap <- function(x,
                            i,
                            j,
                            css,
                            colspan) {

  if (missing(i)) i <- seq_len(attr(x, "nrow"))
  if (missing(j)) j <- seq_len(attr(x, "ncol"))
  assert_integerish(i, lower = 1, null.ok = TRUE)
  assert_integerish(j, lower = 1, null.ok = TRUE)

  # JS 0-indexing,
  j <- j - 1
  i <- i - 1 + attr(x, "nhead")

  out <- x

  id <- get_id(stem = "tinytable_css_")

  # css style
  css <- build_bootstrap_css(css_vector = css, id = id, type = "cell")
  out <- bootstrap_setting(out, css, component = "css")

  # listener applies the styling to columns
  for (row in i) {
    for (col in j) {
      listener <- sprintf("window.addEventListener('load', function () { styleCell(%s, %s, '%s') })", row, col, id)
      out <- bootstrap_setting(out, listener, component = "cell")
    }
  }

  class(out) <- class(x)

  return(out)
}


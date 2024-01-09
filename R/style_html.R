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
ibStyle.IttyBittyTable_html <- function(x,
                                        i,
                                        j,
                                        options,
                                        ...) {

  if (missing(i)) i <- NULL
  if (missing(j)) j <- NULL
  if (missing(options)) options <- do.call(ibOptions, list(...))
  assert_integerish(i, lower = 1, null.ok = TRUE)
  assert_integerish(j, lower = 1, null.ok = TRUE)

  if (inherits(options, "ibOptions")) {
    options <- options$bootstrap
  }

  loop <- "cell"

  # all cells
  if (is.null(i) && is.null(j)){
    i <- seq_len(attr(x, "nrow"))
    j <- seq_len(attr(x, "ncol"))

  # columns
  # we don't need a separate column block because we need to
  # apply styles at the cell level in HTML anyway. 
  } else if (is.null(i)) {
    i <- seq_len(attr(x, "nrow"))

  # rows
  # css can be applied to whole rows.
  } else if (is.null(j)) {
    loop <- "row"

  }

  out <- x

  id <- get_id(stem = "ibStyle_")

  if (loop == "cell") {
    css <- build_bootstrap_css(css_vector = options$css, id = id, type = "cell")
    out <- bootstrap_setting(out, css, component = "css")
    for (row in i) {
      for (col in j) {
        # 0-indexing in JS
        new <- sprintf("table.rows[%s].cells[%s].classList.add('%s');", row, col, id)
        out <- bootstrap_setting(out, new, component = "row")
      }
    }
  } else if (loop == "row") {
    css <- build_bootstrap_css(css_vector = options$css, id = id, type = "row")
    out <- bootstrap_setting(out, css, component = "css")
    for (row in i) {
      # 0-indexing in JS
      new <- sprintf("table.rows[%s].classList.add('%s');", row, id)
      out <- bootstrap_setting(out, new, component = "row")
    }
  }

  return(out)
}


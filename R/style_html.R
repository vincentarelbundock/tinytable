build_bootstrap_css <- function(css_vector, id, type) {
  if (type == "cell") {
    out <- sprintf(".table td.%s {", id)
  } else if (type == "row") {
    out <- sprintf(".table tr.%s td {", id)
  }
  out <- c(out, paste0(css_vector, ";"))
  out <- paste(out, collapse = " ")
  out <- paste(out, "}")
  return(out)
}

 
#' @export
ibStyle.IttyBittyTable_html <- function(x,
                                 i = NULL,
                                 j = NULL,
                                 color = NULL,
                                 background = NULL,
                                 bold = FALSE,
                                 italic = FALSE,
                                 options = tabularrayOptions()) {

  assert_integerish(i, lower = 1, null.ok = TRUE)
  assert_integerish(j, lower = 1, null.ok = TRUE)
  assert_string(color, null.ok = TRUE)
  assert_string(background, null.ok = TRUE)
  assert_flag(bold)
  assert_flag(italic)

  if (inherits(options, "ibOptions")) {
    options <- options$bootstrap
  }

  if (is.null(i)) {
    out <- style_col_IttyBittyTable_html(x = x,
                                       j = j,
                                       color = color,
                                       background = background,
                                       bold = bold,
                                       italic = italic,
                                       options = options)
    return(out)
  } else if (is.null(j)) {
    out <- style_row_IttyBittyTable_html(x = x,
                                    i = i,
                                    color = color,
                                    background = background,
                                    bold = bold,
                                    italic = italic,
                                    options = options)
    return(out)
    # all cells
  } else if (is.null(i) && is.null(j)){
    i <- seq_len(attr(x, "nrow"))
    j <- seq_len(attr(x, "ncol"))
  }

  out <- x

  cellid <- get_id(stem = "style_row_")

  css <- build_bootstrap_css(css_vector = options$css, id = cellid, type = "cell")
  out <- bootstrap_setting(out, css, component = "css")

  for (row in i) {
    for (col in j) {
      # 0-indexing in JS
      new <- sprintf("table.rows[%s].cells[%s].classList.add('%s');", row, col - 1, cellid)
      out <- bootstrap_setting(out, new, component = "row")
    }
  }

  return(out)
}



style_row_IttyBittyTable_html <- function(x,
                                     i = NULL,
                                     color = NULL,
                                     background = NULL,
                                     bold = FALSE,
                                     italic = FALSE,
                                     options = bootstrapOptions(),
                                     ...) {

  assert_integerish(i, lower = 1, null.ok = TRUE)
  assert_string(color, null.ok = TRUE)
  assert_string(background, null.ok = TRUE)
  assert_flag(bold)
  assert_flag(italic)

  # all rows
  if (is.null(i)) i <- seq_len(attr(x, "nrow"))

  out <- x

  rowid <- get_id(stem = "style_row_")

  css <- sprintf("%s .table tr.%s td {", strrep(" ", 8), rowid)
  css <- c(css, paste(options$css, collapse = ";"))

  out <- bootstrap_setting(out, css, component = "css")

  for (row in i) {
    new <- sprintf("table.rows[%s].classList.add('%s');", row, rowid)
    out <- bootstrap_setting(out, new, component = "row")
  }

  return(out)
}




style_col_IttyBittyTable_html <- function(x,
                                        j = NULL,
                                        color = NULL,
                                        background = NULL,
                                        bold = FALSE,
                                        italic = FALSE,
                                        options = bootstrapOptions(),
                                        ...) {

  assert_integerish(j, lower = 1, null.ok = TRUE)
  assert_string(color, null.ok = TRUE)
  assert_string(background, null.ok = TRUE)
  assert_flag(bold)
  assert_flag(italic)

  # all columns
  if (is.null(j)) j <- seq_len(attr(x, "ncol"))

  out <- x

  columnid <- get_id(stem = "style_col_")

  css <- sprintf("%s .table td.%s {", strrep(" ", 8), columnid)
  if (!is.null(color)) {
    tmp <- sprintf("%s color: %s", strrep(" ", 10), color)
    css <- c(css, tmp)
  }
  if (!is.null(background)) {
    tmp <- sprintf("%s background-color: %s", strrep(" ", 10), background)
    css <- c(css, tmp)
  }
  if (isTRUE(bold)) {
    tmp <- sprintf("%s font-weight: bold", strrep(" ", 10))
    css <- c(css, tmp)
  }
  if (isTRUE(italic)) {
    tmp <- sprintf("%s font-style: italic", strrep(" ", 10))
    css <- c(css, tmp)
  }
  tmp <- sprintf("%s}", strrep(" ", 8))
  css <- c(css, tmp)
  css <- trimws(css)
  css <- paste(css, collapse = "; ")

  out <- bootstrap_setting(out, css, component = "css")

  for (column in j) {
    # 0-indexing in JS
    new <- sprintf("table.rows[i].cells[%s].classList.add('%s');", column - 1, columnid)
    out <- bootstrap_setting(out, new, component = "column")
  }

  return(out)
}

#' @export
style_cell.tinytable_html <- function(x,
                                      i = NULL,
                                      j = NULL,
                                      color = NULL,
                                      background = NULL,
                                      bold = FALSE,
                                      italic = FALSE,
                                      html = htmlOptions(),
                                      ...) {

  assert_integerish(i, lower = 1, null.ok = TRUE)
  assert_integerish(j, lower = 1, null.ok = TRUE)
  assert_string(color, null.ok = TRUE)
  assert_string(background, null.ok = TRUE)
  assert_flag(bold)
  assert_flag(italic)

  # all cells
  if (is.null(i)) i <- seq_len(attr(x, "nrow"))
  if (is.null(j)) j <- seq_len(attr(x, "ncol"))

  out <- x

  cellid <- get_id(stem = "style__row_")

  css <- sprintf("%s .table td.%s {", strrep(" ", 8), cellid)
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

  for (row in i) {
    for (col in j) {
      # 0-indexing in JS
      new <- sprintf("table.rows[%s].cells[%s].classList.add('%s');", row, col - 1, cellid)
      out <- bootstrap_setting(out, new, component = "row")
    }
  }

  return(out)
}

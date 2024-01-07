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

  for (row in i) {
    for (col in j) {

      if (!is.null(color)) {
        new <- sprintf('table.rows[%s].cells[%s].style.backgroundColor = "%s";', row, col, color)
        out <- bootstrap_setting(out, new, component = "cell")
      }

      if (!is.null(background)) {
        new <- sprintf('table.rows[%s].cells[%s].style.backgroundColor = "%s";', row, col, background)
        out <- bootstrap_setting(out, new, component = "cell")
      }

    }
  }

  return(out)
}

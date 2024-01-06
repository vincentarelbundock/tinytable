#' @export
style_cell.tinytable_html <- function(x,
                                      i = NULL,
                                      j = NULL,
                                      halign = NULL,
                                      valign = NULL,
                                      bg = NULL,
                                      fg = NULL,
                                      font = NULL,
                                      ...) {

  assert_integerish(i, lower = 1, null.ok = TRUE)
  assert_integerish(j, lower = 1, null.ok = TRUE)

  if (is.null(i)) i <- seq_len(attr(x, "nrow"))
  if (is.null(j)) j <- seq_len(attr(x, "ncol"))

  out <- x

  for (row in i) {

    for (col in j) {

      assert_choice(halign, choice = c("c", "l", "r"), null.ok = TRUE)
      if (!is.null(halign)) {
        tmp <- switch(halign,
        c = "center",
        l = "left",
        r = "right")
        new <- sprintf('table.rows[%s].cells[%s].style.textAlign = "%s";', row, col, halign)
        out <- bootstrap_setting(out, new, component = "cell")
      }

      assert_choice(valign, choice = c("t", "m", "b"), null.ok = TRUE)
      if (!is.null(valign)) {
        tmp <- switch(valign,
        t = "top",
        m = "middle",
        b = "bottom")
        new <- sprintf('table.rows[%s].cells[%s].style.verticalAlign = "%s";', row, col, valign)
        out <- bootstrap_setting(out, new, component = "cell")
      }

      assert_string(bg, null.ok = TRUE)
      if (!is.null(bg)) {
        new <- sprintf('table.rows[%s].cells[%s].style.backgroundColor = "%s";', row, col, bg)
        out <- bootstrap_setting(out, new, component = "cell")
      }

      assert_string(fg, null.ok = TRUE)
      if (!is.null(fg)) {
        new <- sprintf('table.rows[%s].cells[%s].style.color = "%s";', row, col, fg)
        out <- bootstrap_setting(out, new, component = "cell")
      }

      assert_string(font, null.ok = TRUE)
      if (!is.null(font)) {
        new <- sprintf("table.rows[%s].cells[%s].style.fontFamily = '%s';", row, col, font)
        out <- bootstrap_setting(out, new, component = "cell")
      }
    }

  }

  return(out)
}

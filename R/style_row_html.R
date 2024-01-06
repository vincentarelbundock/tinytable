#' @export
style_row.tinytable_html <- function(x,
                                     i = NULL,
                                     halign = NULL,
                                     valign = NULL,
                                     ht = NULL,
                                     bg = NULL,
                                     fg = NULL,
                                     font = NULL,
                                     mode = NULL,
                                     ...) {

  assert_integerish(i, lower = 1, null.ok = TRUE)

  if (is.null(i)) i <- seq_len(attr(x, "nrow"))
  out <- x

  for (row in i) {

    assert_choice(halign, choice = c("c", "l", "r"), null.ok = TRUE)
    if (!is.null(halign)) {
      tmp <- switch(halign,
      c = "center",
      l = "left",
      r = "right")
      new <- sprintf("table.rows[%s].style.textAlign = '%s';", row, tmp)
      out <- bootstrap_setting(out, new, component = "row")
    }

    assert_choice(valign, choice = c("t", "m", "b"), null.ok = TRUE)
    if (!is.null(valign)) {
      tmp <- switch(valign,
      t = "top",
      m = "middle",
      b = "bottom")
      new <- sprintf("table.rows[%s].style.verticalAlign = '%s';", row, tmp)
      out <- bootstrap_setting(out, new, component = "row")
    }

    assert_string(bg, null.ok = TRUE)
    if (!is.null(bg)) {
      new <- sprintf("table.rows[%s].style.backgroundColor = '%s';", row, bg)
      out <- bootstrap_setting(out, new, component = "row")
    }

    assert_string(fg, null.ok = TRUE)
    if (!is.null(fg)) {
      new <- sprintf("table.rows[%s].style.color = '%s';", row, fg)
      out <- bootstrap_setting(out, new, component = "row")
    }

    assert_string(ht, null.ok = TRUE)
    if (!is.null(ht)) {
      new <- sprintf("table.rows[%s].style.height = '%s';", row, ht)
      out <- bootstrap_setting(out, new, component = "row")
    }

    assert_string(font, null.ok = TRUE)
    if (!is.null(font)) {
      new <- sprintf("table.rows[%s].style.fontFamily = '%s';", row, font)
      out <- bootstrap_setting(out, new, component = "row")
    }

  }

  return(out)
}

#' @export
style_column.tinytable_html <- function(x,
                                        j = NULL,
                                        halign = NULL,
                                        valign = NULL,
                                        bg = NULL,
                                        fg = NULL,
                                        font = NULL,
                                        ...) {

  assert_integerish(j, lower = 1, null.ok = TRUE)

  if (is.null(j)) j <- seq_len(attr(x, "ncol"))

  out <- x

  for (col in j) {

    assert_choice(halign, choice = c("c", "l", "r"), null.ok = TRUE)
    if (!is.null(halign)) {
      tmp <- switch(halign,
      c = "center",
      l = "left",
      r = "right")
      new <- sprintf('table.rows[i].cells[%s].style.textAlign = "%s";', col, halign)
      out <- bootstrap_setting(out, new, component = "column")
    }

    assert_choice(valign, choice = c("t", "m", "b"), null.ok = TRUE)
    if (!is.null(valign)) {
      tmp <- switch(valign,
      t = "top",
      m = "middle",
      b = "bottom")
      new <- sprintf('table.rows[i].cells[%s].style.verticalAlign = "%s";', col, valign)
      out <- bootstrap_setting(out, new, component = "column")
    }

    assert_string(bg, null.ok = TRUE)
    if (!is.null(bg)) {
      new <- sprintf('table.rows[i].cells[%s].style.backgroundColor = "%s";', col, bg)
      out <- bootstrap_setting(out, new, component = "column")
    }

    assert_string(fg, null.ok = TRUE)
    if (!is.null(fg)) {
      new <- sprintf('table.rows[i].cells[%s].style.color = "%s";', col, fg)
      out <- bootstrap_setting(out, new, component = "column")
    }

    assert_string(font, null.ok = TRUE)
    if (!is.null(font)) {
      new <- sprintf("table.rows[%s].style.fontFamily = '%s';", col, font)
      out <- bootstrap_setting(out, new, component = "column")
    }

  }

  return(out)
}


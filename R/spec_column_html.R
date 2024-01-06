#' @export
spec_column.tinytable_html <- function(x,

                                       j = NULL,
                                       halign = NULL,
                                       valign = NULL,
                                       ht = NULL,
                                       bg = NULL,
                                       fg = NULL,
                                       font = NULL,
                                       mode = NULL,
                                       ...) {


  assert_integerish(j, lower = 1, null.ok = TRUE)

  if (is.null(j)) j <- seq_len(attr(x, "ncol"))
  out <- x

  for (col in j) {

    assert_string(bg, null.ok = TRUE)
    if (!is.null(bg)) {
      new <- sprintf('table.rows[i].cells[%s].style.backgroundColor = "%s";', col, bg)
      out <- bootstrap_setting(out, new, component = "column")
    }

  }

  return(out)
}


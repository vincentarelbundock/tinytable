#' @export
style_row.tinytable_html <- function(x,
                                     i = NULL,
                                     j = NULL,
                                     color = NULL,
                                     background = NULL,
                                     bold = FALSE,
                                     italic = FALSE,
                                     html = htmlOptions(),
                                     ...) {

  assert_integerish(i, lower = 1, null.ok = TRUE)
  assert_string(color, null.ok = TRUE)
  assert_string(background, null.ok = TRUE)
  assert_flag(bold)
  assert_flag(italic)

  # all rows
  if (is.null(i)) i <- seq_len(attr(x, "nrow"))

  out <- x

  rowid <- get_id(stem = "tr_")
  css <- sub("$TINYTABLE_ID",
    rowid,
    ".table tr.$TINYTABLE_ID {
      background-color: blue;
      color: white;
    }",
    fixed = TRUE)
  out <- bootstrap_setting(out, css, component = "css")

  for (row in i) {

    new <- sprintf("table.rows[%s].classList.add('%s');", row, rowid)
    new <- paste(strrep(" ", 13), new)
    out <- bootstrap_setting(out, new, component = "row")
  
    if (!is.null(background)) {
      new <- sprintf("table.rows[%s].style.backgroundColor = '%s';", row, background)
      new <- paste(strrep(" ", 13), new)
      out <- bootstrap_setting(out, new, component = "row")
    }
    
    if (isTRUE(bold)) {
      new <- sprintf("table.rows[%s].style.fontWeight = 'bold';", row)
      new <- paste(strrep(" ", 13), new)
      out <- bootstrap_setting(out, new, component = "row")
    }

    if (isTRUE(italic)) {
      new <- sprintf("table.rows[%s].style.fontStyle = 'italic';", row)
      new <- paste(strrep(" ", 13), new)
      out <- bootstrap_setting(out, new, component = "row")
    }

  }

  return(out)
}

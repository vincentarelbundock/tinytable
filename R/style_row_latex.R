style_row_tinytable_latex <- function(x,
                                      i = NULL,
                                      color = NULL,
                                      background = NULL,
                                      bold = FALSE,
                                      italic = FALSE,
                                      latex = latexOptions(),
                                      ...) {

  assert_integerish(i, lower = 1, null.ok = TRUE)
  assert_string(color, null.ok = TRUE)
  assert_string(background, null.ok = TRUE)
  assert_flag(bold)
  assert_flag(italic)

  keys <- latex$rows_keys

  # all rows
  if (is.null(i)) i <- seq_len(attr(x, "nrow"))

  # color, background, italic, and bold
  if (!is.null(color)) {
    keys <- paste0(keys, ",fg=", color)
  }
  if (!is.null(background)) {
    keys <- paste0(keys, ",bg=", background)
  }
  if (isTRUE(italic)) {
    if (grepl("cmd=", keys)) {
      keys <- sub("cmd=", "cmd=\\textit", keys, fixed = TRUE)
    } else {
      keys <- paste0(keys, ",cmd=\\textit")
    }
  }
  if (isTRUE(bold)) {
    if (grepl("cmd=", keys)) {
      keys <- sub("cmd=", "cmd=\\bfseries", keys, fixed = TRUE)
    } else {
      keys <- paste0(keys, ",cmd=\\bfseries")
    }
  }

  # no keys no change
  if (keys == "") return(x)

  # build keys
  new <- sprintf(
    "row{%s}={%s},",
    paste(i, collapse = ","),
    keys) 

  # insert keys
  out <- tabularray_setting(x, new, inner = TRUE)

  return(out)
}

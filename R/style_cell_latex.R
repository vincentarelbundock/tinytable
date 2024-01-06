#' @export
style_cell.tinytable_latex <- function(x,
                                      i = NULL,
                                      j = NULL,
                                      halign = NULL,
                                      valign = NULL,
                                      wd = NULL,
                                      bg = NULL,
                                      fg = NULL,
                                      font = NULL,
                                      mode = NULL,
                                      cmd = NULL,
                                      preto = NULL,
                                      appto = NULL,
                                      r = NULL,
                                      c = NULL,
                                      ...) {

  content <- ""
  span <- ""

  assert_integerish(i, lower = 1, null.ok = TRUE)
  if (is.null(i)) i <- seq_len(attr(x, "nrow"))

  assert_integerish(j, lower = 1, null.ok = TRUE)
  if (is.null(j)) j <- seq_len(attr(x, "ncol"))

  # DRY might be nice but less readable
  assert_choice(halign, choice = c("l", "c", "r", "j"), null.ok = TRUE)
  content <- paste0(content, ",halign=", halign)

  assert_choice(valign, choice = c("t", "m", "b", "h", "f"), null.ok = TRUE)
  if (!is.null(valign)) content <- paste0(content, ",valign=", valign)

  assert_string(wd, null.ok = TRUE)
  if (!is.null(wd)) content <- paste0(content, ",wd=", wd)

  assert_string(bg, null.ok = TRUE)
  if (!is.null(bg)) content <- paste0(content, ",bg=", bg)

  assert_string(fg, null.ok = TRUE)
  if (!is.null(fg)) content <- paste0(content, ",fg=", fg)

  assert_string(font, null.ok = TRUE)
  if (!is.null(font)) content <- paste0(content, ",font=", font)

  assert_string(mode, null.ok = TRUE)
  if (!is.null(mode)) content <- paste0(content, ",mode=", mode)

  assert_string(cmd, null.ok = TRUE)
  if (!is.null(cmd)) content <- paste0(content, ",cmd=", cmd)

  assert_string(preto, null.ok = TRUE)
  if (!is.null(preto)) content <- paste0(content, ",preto=", preto)

  assert_string(appto, null.ok = TRUE)
  if (!is.null(appto)) content <- paste0(content, ",appto=", appto)

  assert_integerish(r, lower = 1, null.ok = TRUE)
  if (!is.null(r)) span <- paste0(span, ",r=", r)

  assert_integerish(c, lower = 1, null.ok = TRUE)
  if (!is.null(c)) span <- paste0(span, ",c=", c)

  content <- gsub(",+", ",", content)
  span <- gsub(",+", ",", span)

  spec <- sprintf(
    "cell{%s}{%s}={%s}{%s},",
    paste(i, collapse = ","),
    paste(j, collapse = ","),
    span,
    content
  ) 
  spec <- gsub("\\{,*", "\\{", spec)

  out <- tabularray_setting(x, spec, inner = TRUE)

  return(out)
}

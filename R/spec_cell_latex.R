#' @export
spec_cell.tinytable_latex <- function(x,
  i, j,
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
  c = NULL) {

  checkmate::assert_integerish(i, lower = 1, upper = attr(x, "nrow"), null.ok = FALSE)
  checkmate::assert_integerish(j, lower = 1, upper = attr(x, "ncol"), null.ok = FALSE)

  content <- ""
  span <- ""

  # DRY might be nice but less readable
  checkmate::assert_choice(halign, choice = c("l", "c", "r", "j"), null.ok = TRUE)
  content <- paste0(content, ",halign=", halign)

  checkmate::assert_choice(valign, choice = c("t", "m", "b", "h", "f"), null.ok = TRUE)
  if (!is.null(valign)) content <- paste0(content, ",valign=", valign)

  checkmate::assert_string(wd, null.ok = TRUE)
  if (!is.null(wd)) content <- paste0(content, ",wd=", wd)

  checkmate::assert_string(bg, null.ok = TRUE)
  if (!is.null(bg)) content <- paste0(content, ",bg=", bg)

  checkmate::assert_string(fg, null.ok = TRUE)
  if (!is.null(fg)) content <- paste0(content, ",fg=", fg)

  checkmate::assert_string(font, null.ok = TRUE)
  if (!is.null(font)) content <- paste0(content, ",font=", font)

  checkmate::assert_string(mode, null.ok = TRUE)
  if (!is.null(mode)) content <- paste0(content, ",mode=", mode)

  checkmate::assert_string(cmd, null.ok = TRUE)
  if (!is.null(cmd)) content <- paste0(content, ",cmd=", cmd)

  checkmate::assert_string(preto, null.ok = TRUE)
  if (!is.null(preto)) content <- paste0(content, ",preto=", preto)

  checkmate::assert_string(appto, null.ok = TRUE)
  if (!is.null(appto)) content <- paste0(content, ",appto=", appto)

  checkmate::assert_integerish(r, lower = 1, null.ok = TRUE)
  if (!is.null(r)) span <- paste0(span, ",r=", r)

  checkmate::assert_integerish(c, lower = 1, null.ok = TRUE)
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

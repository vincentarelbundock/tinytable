#' @export
spec_row.tinytable_latex <- function(x,
                                     i,
                                     halign = NULL,
                                     valign = NULL,
                                     ht = NULL,
                                     bg = NULL,
                                     fg = NULL,
                                     font = NULL,
                                     mode = NULL,
                                     cmd = NULL,
                                     preto = NULL,
                                     appto = NULL,
                                     tabularray = NULL) {

  checkmate::assert_integerish(i, lower = 1, null.ok = FALSE)

  content <- ""

  checkmate::assert_choice(halign, choice = c("l", "c", "r", "j"), null.ok = TRUE)
  if (!is.null(halign)) content <- paste0(content, ",halign=", halign)

  checkmate::assert_choice(valign, choice = c("t", "m", "b", "h", "f"), null.ok = TRUE)
  if (!is.null(valign)) content <- paste0(content, ",valign=", valign)

  checkmate::assert_string(ht, null.ok = TRUE)
  if (!is.null(ht)) content <- paste0(content, ",ht=", ht)

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

  checkmate::assert_string(tabularray, null.ok = TRUE)
  if (!is.null(tabularray)) content <- paste0(content, ",", tabularray)

  content <- gsub(",+", ",", content)

  new <- sprintf(
    "row{%s}={%s},",
    paste(i, collapse = ","),
    content) 

  out <- tabularray_setting(x, new, inner = TRUE)

  return(out)
}

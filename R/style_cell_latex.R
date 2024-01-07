#' @export
style_cell.tinytable_latex <- function(x,
                                       i,
                                       j = NULL,
                                       color = NULL,
                                       background = NULL,
                                       bold = FALSE,
                                       italic = FALSE,
                                       latex = latexOptions(),
                                       ...) {

  out <- x

  assert_integerish(i, lower = 1, null.ok = TRUE)
  assert_integerish(j, lower = 1, null.ok = TRUE)
  assert_string(color, null.ok = TRUE)
  assert_string(background, null.ok = TRUE)
  assert_flag(bold)
  assert_flag(italic)

  keys <- latex$cells_keys
  span <- latex$span

  # all cells
  if (is.null(i)) i <- seq_len(attr(x, "nrow"))
  if (is.null(j)) j <- seq_len(attr(x, "ncol"))


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

  # build and insert keys
  if (keys != "") {
    new <- sprintf(
      "cell{%s}{%s}={%s}{%s},",
      paste(i, collapse = ","),
      paste(j, collapse = ","),
      span,
      keys
    ) 
    out <- tabularray_setting(out, new, inner = TRUE)
  }

  return(out)
}  

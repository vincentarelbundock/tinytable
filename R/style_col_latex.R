style_col_IttyBittyTable_latex <- function(x,
                                         j = NULL,
                                         align = NULL,
                                         color = NULL,
                                         background = NULL,
                                         bold = FALSE,
                                         italic = FALSE,
                                         latex = latexOptions(),
                                         ...) {

  out <- x

  assert_integerish(j, lower = 1, null.ok = TRUE)
  assert_string(color, null.ok = TRUE)
  assert_string(background, null.ok = TRUE)
  assert_flag(bold)
  assert_flag(italic)
  assert_string(align, null.ok = TRUE)

  keys <- latex$columns_keys

  # all columns
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
      "column{%s}={%s},",
      paste(j, collapse = ","),
      keys) 
    out <- tabularray_setting(out, new, inner = TRUE)
  }

  # align: we work one column at a time because they could have different alignments
  if (is.character(align)) {
    align <- strsplit(align, "")[[1]]
    if (any(!align %in% c("j", "c", "r", "l")) || length(align) != length(j)) {
      msg <- sprintf(
        "`align` must be a string of length %s with character in 'j', 'c', 'r', 'l'.",
        length(j))
      stop(msg, call. = FALSE)
    }
    for (i in seq_along(align)) {
      out <- tabularray_setting(out, sprintf("column{%s}={halign=%s},", j[i], align[i]))
    } 
  }

  return(out)
}  

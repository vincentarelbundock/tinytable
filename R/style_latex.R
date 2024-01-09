#' @export
ibStyle.IttyBittyTable_latex <- function(x,
                                         i = NULL,
                                         j = NULL,
                                         options = NULL,
                                         ...) {

  if (inherits(options, "ibOptions")) {
    options <- options$tabularray
  }

  out <- x

  assert_integerish(i, lower = 1, null.ok = TRUE)
  assert_integerish(j, lower = 1, null.ok = TRUE)
  assert_string(color, null.ok = TRUE)
  assert_string(background, null.ok = TRUE)
  assert_flag(bold)
  assert_flag(italic)

  keys <- options$cells_keys
  span <- options$span

  if (is.null(i)) {
    out <- style_col_IttyBittyTable_latex(x = x,
      j = j,
      color = color,
      background = background,
      bold = bold,
      italic = italic,
      options = options
    )
    return(out)
  } else if (is.null(j)) {
    out <- style_row_IttyBittyTable_latex(x = x,
      i = i,
      color = color,
      background = background,
      bold = bold,
      italic = italic,
      options = options)
    return(out)
    # all cells
  } else if (is.null(i) && is.null(j)){
    i <- seq_len(attr(x, "nrow"))
    j <- seq_len(attr(x, "ncol"))
  }

  # do not color headers unless there are negative numbers
  # needed to match indexing behavior of Bootstrap and JS
  i <- i + attr(x, "nhead")

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
  if (keys != "" || span != "") {
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




style_row_IttyBittyTable_latex <- function(x,
                                           i = NULL,
                                           color = NULL,
                                           background = NULL,
                                           bold = FALSE,
                                           italic = FALSE,
                                           options = tabularrayOptions(),
                                           ...) {

  assert_integerish(i, lower = 1, null.ok = TRUE)
  assert_string(color, null.ok = TRUE)
  assert_string(background, null.ok = TRUE)
  assert_flag(bold)
  assert_flag(italic)

  keys <- options$rows_keys

  # all rows
  if (is.null(i)) i <- seq_len(attr(x, "nrow"))

  # do not color headers unless there are negative numbers
  # needed to match indexing behavior of Bootstrap and JS
  i <- i + attr(x, "nhead")

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




style_col_IttyBittyTable_latex <- function(x,
                                           j = NULL,
                                           align = NULL,
                                           color = NULL,
                                           background = NULL,
                                           bold = FALSE,
                                           italic = FALSE,
                                           options = tabularrayOptions(),
                                           ...) {

  out <- x

  assert_integerish(j, lower = 1, null.ok = TRUE)
  assert_string(color, null.ok = TRUE)
  assert_string(background, null.ok = TRUE)
  assert_flag(bold)
  assert_flag(italic)
  assert_string(align, null.ok = TRUE)

  keys <- options$columns_keys

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

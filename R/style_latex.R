#' @export
ibStyle.IttyBittyTable_latex <- function(x,
                                         i,
                                         j,
                                         options,
                                         ...) {

  if (inherits(options, "ibOptions")) {
    options <- options$tabularray
  } else if (!inherits(options, "tabularrayOptions")) {
    msg <- "`options` must be a call to `ibOptions()` or `tabularrayOptions()`."
    stop(msg, call. = FALSE)
  }

  if (missing(i)) i <- NULL
  if (missing(j)) j <- NULL
  if (missing(options)) options <- do.call(ibOptions, list(...))

  out <- x

  assert_integerish(i, lower = 1, null.ok = TRUE)
  assert_integerish(j, lower = 1, null.ok = TRUE)
  assert_string(color, null.ok = TRUE)
  assert_string(background, null.ok = TRUE)
  assert_flag(bold)
  assert_flag(italic)

  if (is.null(i)) {
    out <- style_col_IttyBittyTable_latex(x = x,
      j = j,
      options = options
    )
    return(out)
  } else if (is.null(j)) {
    out <- style_row_IttyBittyTable_latex(x = x,
      i = i,
      options = options)
    return(out)
    # all cells
  } else if (is.null(i) && is.null(j)){
    i <- seq_len(attr(x, "nrow"))
    j <- seq_len(attr(x, "ncol"))
  }

  keys <- options$cells_keys
  span <- options$span

  # do not color headers unless there are negative numbers
  # needed to match indexing behavior of Bootstrap and JS
  i <- i + attr(x, "nhead")

  # cells
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

  # rows
  new <- sprintf(
    "row{%s}={%s},",
    paste(i, collapse = ","),
    keys) 

  # columns
  if (keys != "") {
    new <- sprintf(
      "column{%s}={%s},",
      paste(j, collapse = ","),
      keys) 
    out <- tabularray_setting(out, new, inner = TRUE)
  }

  return(out)
}  


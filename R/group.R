#' @export
ibGroup <- function(x,
                      i = NULL,
                      j = NULL,
                      color = NULL,
                      background = NULL,
                      italic = FALSE,
                      bold = FALSE,
                      rule = TRUE) {

  assert_integerish(i, null.ok = TRUE)
  assert_integerish(j, null.ok = TRUE)
  assert_flag(rule)

  if (is.null(i) && is.null(j)) {
    stop("At least one of `i` or `j` must be specified.", call. = FALSE)
  }

  if (inherits(x, "IttyBittyTable_latex") && is.null(j)) {
    out <- group_row_latex(x,
                           i = i,
                           color = color,
                           background = background,
                           italic = italic,
                           bold = bold,
                           rule = rule)
  }

  return(out)
}


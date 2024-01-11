#' @export
group_tt <- function(x, i, j, italic = FALSE, rule = TRUE) {

  assert_flag(rule)
  if (!missing(i)) assert_integerish(i)
  if (!missing(j)) assert_integerish(j)
  if (missing(i) && missing(j)) {
    stop("At least one of `i` or `j` must be specified.", call. = FALSE)
  }

  if (inherits(x, "tinytable_tabularray") && missing(j)) {
    out <- group_row_latex(x, i = i, italic = italic, rule = rule)
  }

  return(out)
}


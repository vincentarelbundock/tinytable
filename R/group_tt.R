#' Spanning labels to identify groups of rows or columns
#'
#' @export
#' @inheritParams tt
#' @inheritParams style_tt
#' @param indent integer number of `pt` to use when indenting the non-labelled rows.
#' @param ... All additional arguments (ex: `italic`, `bold`, `color`) are automatically passed to the `style_tt()` function to style the labels.
group_tt <- function(x, i, j, indent = 1, ...) {

  out <- x
  if (inherits(out, "tinytable_tabularray") && inherits(out, "tinytable_bootstrap")) {
    return(x)
  }
  assert_integerish(indent, lower = 1)
  if (!missing(i)) assert_integerish(i)
  if (!missing(j)) assert_integerish(j)
  if ((missing(i) && missing(j)) || (!missing(i) && !missing(j))) {
    stop("One and only one of `i` or `j` must be specified.", call. = FALSE)
  }

  if (inherits(out, "tinytable_tabularray") && missing(j)) {
    out <- group_tabularray(out, i = i, indent = indent, ...)
  } else if (inherits(out, "tinytable_bootstrap") && missing(j)) {
    out <- group_bootstrap(out, i = i, indent = indent, ...)
  }

  attr(out, "nrow") <- attr(out, "nrow") + length(i)

  return(out)
}


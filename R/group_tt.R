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
  if ((missing(i) && missing(j)) || (!missing(i) && !missing(j))) {
    stop("One and only one of `i` or `j` must be specified.", call. = FALSE)
  }

  if (missing(i)) i <- NULL
  if (missing(j)) j <- NULL

  i <- sanitize_group_index(i, hi = attr(x, "nrow") + 1)
  j <- sanitize_group_index(j, hi = attr(x, "ncol"))

  # we don't need this as a list, and we use some sorting later
  i <- unlist(i)

  if (inherits(out, "tinytable_tabularray")) {
    out <- group_tabularray(out, i = i, j = j, indent = indent, ...)
  } else if (inherits(out, "tinytable_bootstrap")) {
    out <- group_bootstrap(out, i = i, j = j, indent = indent, ...)
  }

  attr(out, "nrow") <- attr(out, "nrow") + length(i)
  if (is.list(j)) attr(out, "nhead") <- attr(out, "nhead") + 1

  return(out)
}



sanitize_group_index <- function(idx, hi) {
  if (is.null(idx)) return(idx)
  assert_named_list(idx)
  for (n in names(idx)) {
    assert_integerish(idx[[n]], lower = 1, upper = hi, name = n)
  }
  if (anyDuplicated(unlist(idx)) > 0) stop("Duplicate group indices.", call. = FALSE)
  out <- lapply(idx, function(x) min(x):max(x))
  return(out)
}

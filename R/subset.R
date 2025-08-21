#' Subsetting a `tinytable` object
#'
#' Return subsets `tinytable` which meet conditions.
#' @inheritParams base::subset.data.frame
#' @export
subset.tinytable <- function(x, subset, select, drop = FALSE, ...) {
  chkDots(...)
  r <- if (missing(subset)) {
    rep_len(TRUE, nrow(x@data_body))
  } else {
    e <- substitute(subset)
    r <- eval(e, x@data_body, parent.frame())
    if (!is.logical(r)) {
      stop("'subset' must be logical")
    }
    r & !is.na(r)
  }
  vars <- if (missing(select)) {
    rep_len(TRUE, ncol(x@data_body))
  } else {
    nl <- as.list(seq_along(x@data_body))
    names(nl) <- names(x@data_body)
    eval(substitute(select), nl, parent.frame())
  }
  tmp <- x@data_body[r, vars, drop = drop]
  x@data_body <- tmp
  return(x)
}

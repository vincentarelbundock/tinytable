nse_i_j <- function(x, i_expr, j_expr, pf) {
  if (!inherits(x, "tinytable")) {
    return(list(i = eval(i_expr, parent.frame()), j = eval(j_expr, parent.frame())))
  }

  i <- i_expr
  j <- j_expr

  i <- tryCatch(eval(i, x@data_body),
    error = function(e) eval(i, pf))

  if (is.logical(i) && length(i) == nrow(x@data_body)) {
    i <- which(i)
  }

  # if j is a symbol matching a column in x@data_body
  if (is.symbol(j) && as.character(j) %in% colnames(x@data_body)) {
    j <- as.character(j)

    # if j is a call to c() of column names
  } else if (is.call(j) && identical(j[[1L]], as.name("c"))) {
    syms <- as.list(j[-1L])
    if (all(vapply(syms, function(s) is.symbol(s) && as.character(s) %in% colnames(x@data_body), logical(1)))) {
      j <- vapply(syms, as.character, character(1))
    } else {
      j <- eval(j, pf)
    }

    # otherwise: evaluate normally
  } else {
    j <- eval(j, pf)
  }
  return(list(i = i, j = j))
}

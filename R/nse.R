nse_i_j <- function(x, i_expr, j_expr, pf) {
  if (!inherits(x, "tinytable")) {
    # `pf` is the frame of the user's call (e.g., format_tt() on a plain data
    # frame). Evaluating in parent.frame() here would look up variables in
    # nse_i_j()'s caller instead, where user-defined objects do not exist.
    return(list(i = eval(i_expr, pf), j = eval(j_expr, pf)))
  }

  i <- i_expr
  j <- j_expr

  tmpenv <- new.env()
  # Evaluate NSE expressions against the *original* typed columns in @data.
  # @data_body is character-coerced by tt(), so a predicate like `x > 30`
  # would otherwise compare strings lexicographically ("5" > "30" is TRUE)
  # and silently select the wrong rows. Columns that only exist in the
  # working copy fall back to their @data_body version.
  val <- as.list(x@data_body)
  if (nrow(x@data) == nrow(x@data_body)) {
    for (nm in intersect(names(val), colnames(x@data))) {
      val[[nm]] <- x@data[[nm]]
    }
  }
  val <- c(val, list(groupi = x@group_index_i))
  val <- val[names(val) != ""]
  list2env(val, tmpenv)
  i <- tryCatch(eval(i, pf), error = function(e) eval(i, tmpenv))

  if (is.logical(i) && length(i) == nrow(x@data_body)) {
    i <- which(i)
    # A logical predicate is evaluated against the data rows of @data_body,
    # which does not (yet) include the label rows inserted by group_tt():
    # those are only merged into @data_body by rbind_body_groupi() at build
    # time. Numeric `i`, however, is interpreted in final-table coordinates
    # when the lazy style/format/plot calls are resolved. Mark
    # predicate-derived indices so lazy consumers can remap them through
    # @index_body (see nse_remap_i()).
    attr(i, "tt_nse_data_rows") <- TRUE
  }

  j <- tryCatch(eval(j_expr, pf), error = function(e) NULL)

  # if j is a symbol matching a column in x@data_body
  if (is.null(j)) {
    j <- j_expr
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
  }

  return(list(i = i, j = j))
}


# Remap row indices produced by an NSE predicate in nse_i_j() from data-row
# coordinates (rows of the pre-group @data_body) to final-table coordinates,
# using @index_body. @index_body is computed by build_tt() before any lazy
# call is resolved, so this must only be called at lazy-evaluation time
# (style_tt_lazy(), format_tt_lazy(), plot_tt_lazy()). This is correct
# regardless of whether group_tt() was called before or after the styling
# call: group_tt() never modifies @data_body eagerly, so predicates always
# see data rows only. When the table has no group rows, @index_body is the
# identity mapping and this is a no-op. Unmarked `i` is returned unchanged.
nse_remap_i <- function(i, x) {
  if (isTRUE(attr(i, "tt_nse_data_rows")) && length(x@index_body) > 0) {
    i <- x@index_body[as.vector(i, mode = "integer")]
  }
  return(i)
}

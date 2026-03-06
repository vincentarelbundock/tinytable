#' Subsetting a `tinytable` object
#'
#' Return subsets `tinytable` which meet conditions.
#' @inheritParams base::subset.data.frame
#' @export
subset.tinytable <- function(x, subset, select, drop = FALSE, ...) {
  chkDots(...)

  # Evaluate subset condition
  r <- rep_len(TRUE, nrow(x@data_body))
  if (!missing(subset)) {
    warning("The `subset` argument of the `subset()` function is not supported for `tinytable` objects. Filter the rows before supplying the data frame to `tt()`.", call. = FALSE)
  }

  # Evaluate select condition
  old_ncol <- ncol(x@data_body)
  vars <- if (missing(select)) {
    rep_len(TRUE, old_ncol)
  } else {
    nl <- as.list(seq_along(x@data_body))
    names(nl) <- names(x@data_body)
    eval(substitute(select), nl, parent.frame())
  }

  # Build a column remapping vector: col_remap[old_col] = new_col (0 = removed).
  kept <- if (is.logical(vars)) which(vars) else seq_len(old_ncol)[vars]
  col_remap <- integer(old_ncol)
  col_remap[kept] <- seq_along(kept)
  new_ncol <- length(kept)

  # Apply subsetting to data
  x@data_body <- x@data_body[r, vars, drop = drop]
  x@data <- x@data[r, vars, drop = drop]

  # Subset row group data
  if (nrow(x@group_data_i) > 0) {
    x@group_data_i <- x@group_data_i[, vars, drop = drop]
  }

  # Truncate column group data to the new column count. Group spans are
  # positional: they refer to whatever columns end up at those positions
  # in the final table.
  if (nrow(x@group_data_j) > 0) {
    x@group_data_j <- x@group_data_j[, seq_len(new_ncol), drop = FALSE]
    colnames(x@group_data_j) <- colnames(x@data_body)
  }

  # Update dimensions and names
  x@nrow <- nrow(x@data_body) + nrow(x@group_data_i)
  x@ncol <- new_ncol
  x@names <- names(x@data_body)

  # Update width_cols to match new column count
  if (length(x@width_cols) > 0) {
    x@width_cols <- x@width_cols[kept]
  }

  # Remap numeric j-indices in lazy calls recorded before subset
  x@lazy_format <- lapply(x@lazy_format, remap_j, col_remap = col_remap, old_ncol = old_ncol)
  x@lazy_plot <- lapply(x@lazy_plot, remap_j, col_remap = col_remap, old_ncol = old_ncol)

  # Avoid colspan that exceeds the new number of columns
  if (nrow(x@style) > 0) {
    end <- x@style$j + x@style$colspan - 1
    x@style$colspan <- ifelse(
      !is.na(end) & end > x@ncol,
      x@style$colspan - (end - x@ncol),
      x@style$colspan)
  }

  return(x)
}


#' Remap j-indices in a lazy call after column subsetting
#' @keywords internal
#' @noRd
remap_j <- function(call_args, col_remap, old_ncol) {
  if (is.call(call_args)) {
    j <- call_args[["j"]]
  } else if (is.list(call_args)) {
    j <- call_args$j
  } else {
    return(call_args)
  }

  if (is.numeric(j) && length(j) > 0 && all(j > 0)) {
    valid <- j >= 1L & j <= old_ncol
    new_j <- integer(length(j))
    new_j[valid] <- col_remap[j[valid]]
    new_j <- new_j[new_j > 0L]
    if (is.call(call_args)) {
      call_args[["j"]] <- if (length(new_j) > 0L) new_j else NULL
    } else {
      call_args$j <- if (length(new_j) > 0L) new_j else NULL
    }
  }

  call_args
}

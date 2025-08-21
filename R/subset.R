#' Subsetting a `tinytable` object
#'
#' Return subsets `tinytable` which meet conditions.
#' @inheritParams base::subset.data.frame
#' @export
subset.tinytable <- function(x, subset, select, drop = FALSE, ...) {
  chkDots(...)

  # Store subset arguments in lazy_subset slot for later execution
  subset_args <- list(
    subset = if (missing(subset)) NULL else substitute(subset),
    select = if (missing(select)) NULL else substitute(select),
    drop = drop,
    environment = parent.frame()
  )

  x@lazy_subset <- subset_args
  return(x)
}

#' Apply lazy subset operations
#' @keywords internal
#' @noRd
subset_lazy <- function(x) {
  if (is.null(x@lazy_subset)) {
    return(x)
  }

  args <- x@lazy_subset

  # Evaluate subset condition
  r <- rep_len(TRUE, nrow(x@data_body))
  if (!is.null(args$subset)) {
    warning("The `subset` argument of the `subset()` function is not supported for `tinytable` objects. Filter the rows before supplying the data frame to `tt()`.", call. = FALSE)
  }

  # Evaluate select condition
  vars <- if (is.null(args$select)) {
    rep_len(TRUE, ncol(x@data_body))
  } else {
    nl <- as.list(seq_along(x@data_body))
    names(nl) <- names(x@data_body)
    eval(args$select, nl, args$environment)
  }

  # Apply subsetting
  x@data_body <- x@data_body[r, vars, drop = args$drop]
  x@data <- x@data[r, vars, drop = args$drop]

  # Subset group data
  if (nrow(x@group_data_i) > 0) {
    x@group_data_i <- x@group_data_i[, vars, drop = args$drop]
  }
  if (nrow(x@group_data_j) > 0) {
    x@group_data_j <- x@group_data_j[, vars, drop = args$drop]
  }

  # Update dimensions and names
  x@nrow <- nrow(x@data_body)
  x@ncol <- ncol(x@data_body)
  x@names <- names(x@data_body)

  # Update width_cols to match new column count
  if (length(x@width_cols) > 0) {
    if (is.logical(vars)) {
      x@width_cols <- x@width_cols[vars]
    } else {
      # vars is a numeric vector of column indices
      x@width_cols <- x@width_cols[vars]
    }
  }

  # avoid colspan that exceeds the new number of columns
  if (nrow(x@style) > 0) {
    end <- x@style$j + x@style$colspan - 1
    x@style$colspan <- ifelse(
      !is.na(end) & end > x@ncol,
      x@style$colspan - (end - x@ncol),
      x@style$colspan)
  }

  # Clear the lazy subset after applying
  x@lazy_subset <- NULL

  return(x)
}

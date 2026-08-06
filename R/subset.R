#' Subsetting a `tinytable` object
#'
#' Return subsets `tinytable` which meet conditions.
#' @inheritParams base::subset.data.frame
#' @export
subset.tinytable <- function(x, subset, select, drop = FALSE, ...) {
  chkDots(...)

  # `drop = TRUE` would return vectors, which cannot be assigned to the
  # data.frame S4 slots. Warn and ignore.
  if (!isFALSE(drop)) {
    warning(
      "`drop = TRUE` is not supported for tinytable objects; ignoring.",
      call. = FALSE
    )
  }

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

  # Resolve `vars` to positive column positions. Character selections must be
  # matched by name: `seq_len(old_ncol)[vars]` would silently yield NA for
  # character input, corrupting the column remapping below.
  kept <- if (is.logical(vars)) {
    which(vars)
  } else if (is.character(vars)) {
    match(vars, names(x@data_body))
  } else {
    seq_len(old_ncol)[vars]
  }
  if (anyNA(kept)) {
    bad <- if (is.character(vars)) {
      vars[!vars %in% names(x@data_body)]
    } else {
      vars[is.na(seq_len(old_ncol)[vars])]
    }
    stop(
      sprintf(
        "Invalid `select`: cannot match column(s): %s",
        paste(bad, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Build a column remapping vector: col_remap[old_col] = new_col (0 = removed).
  col_remap <- integer(old_ncol)
  col_remap[kept] <- seq_along(kept)
  new_ncol <- length(kept)

  # Apply subsetting to data
  x@data_body <- x@data_body[r, kept, drop = FALSE]
  x@data <- x@data[r, kept, drop = FALSE]

  # Subset row group data
  if (nrow(x@group_data_i) > 0) {
    x@group_data_i <- x@group_data_i[, kept, drop = FALSE]
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
  x@lazy_format <- remap_lazy_list(x@lazy_format, col_remap, old_ncol)
  x@lazy_plot <- remap_lazy_list(x@lazy_plot, col_remap, old_ncol)
  x@lazy_style <- remap_lazy_list(x@lazy_style, col_remap, old_ncol)

  # Avoid colspan that exceeds the new number of columns
  x@style <- clamp_colspan(x@style, x@ncol)

  return(x)
}


#' Remap j-indices in a list of lazy calls after column subsetting
#'
#' Entries whose target columns were all removed are dropped entirely:
#' setting their `j` to NULL instead would make them apply to every
#' remaining column.
#' @keywords internal
#' @noRd
remap_lazy_list <- function(lst, col_remap, old_ncol) {
  out <- lapply(lst, remap_j, col_remap = col_remap, old_ncol = old_ncol)
  out[!vapply(out, is.null, logical(1))]
}


#' Remap j-indices in a lazy call after column subsetting
#'
#' Returns the (possibly modified) lazy entry, or NULL if every column the
#' entry targeted was removed by the subset. Only plain numeric `j` values
#' are remapped; anything else (NULL, character column names, NSE markers,
#' ...) is deliberately left untouched so we never mangle entries we cannot
#' interpret.
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

  if (!is.numeric(j) || length(j) == 0L || anyNA(j)) {
    return(call_args)
  }

  if (all(j < 0)) {
    # Negative j is a selection against the pre-subset table: convert it to
    # the equivalent positive positions against the OLD column count, then
    # remap those like any other stored indices.
    if (any(abs(j) > old_ncol)) {
      return(call_args)
    }
    j <- seq_len(old_ncol)[j]
  } else if (any(j <= 0)) {
    # mixed signs or zeros: not a valid selection; leave untouched
    return(call_args)
  }

  valid <- j >= 1 & j <= old_ncol
  new_j <- integer(length(j))
  new_j[valid] <- col_remap[j[valid]]
  new_j <- new_j[new_j > 0L]

  # All targeted columns were removed.
  if (length(new_j) == 0L) {
    colspan <- if (is.call(call_args)) {
      call_args[["colspan"]]
    } else {
      call_args$colspan
    }
    if (!is.null(colspan)) {
      # The entry anchors a column span (e.g. the full-width row-group labels
      # created by group_tt(i = ...)). Re-anchor at the first surviving column
      # to the right of the original anchor so the span still covers the same
      # region; colspan is clamped to the new width at build time.
      right <- col_remap[seq(min(j), old_ncol)]
      right <- right[right > 0L]
      if (length(right) == 0L) {
        return(NULL)
      }
      new_j <- min(right)
    } else {
      # Plain cell style/format: drop the lazy entry. Setting j to NULL
      # instead would make it apply to *every* remaining column.
      return(NULL)
    }
  }

  if (is.call(call_args)) {
    call_args[["j"]] <- new_j
  } else {
    call_args$j <- new_j
  }
  call_args
}

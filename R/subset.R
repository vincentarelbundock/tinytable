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
  # character input, corrupting the column selection below.
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

  new_ncol <- length(kept)

  # Apply subsetting to data
  x@data_body <- x@data_body[r, kept, drop = FALSE]
  x@data <- x@data[r, kept, drop = FALSE]

  # Subset row group data
  if (nrow(x@group_data_i) > 0) {
    x@group_data_i <- x@group_data_i[, kept, drop = FALSE]
  }

  # Truncate the column group labels. Like `i` in `style_tt()`, span positions
  # refer to the rendered table rather than to the input data, so a span stays
  # where it was and is only clipped against the new right edge. Spans that
  # start beyond the last surviving column disappear.
  if (nrow(x@group_data_j) > 0) {
    gdj <- x@group_data_j
    new_gdj <- data.frame(
      matrix(NA_character_, nrow = nrow(gdj), ncol = new_ncol),
      stringsAsFactors = FALSE
    )
    for (row_idx in seq_len(nrow(gdj))) {
      spans <- parse_group_spans(as.character(gdj[row_idx, ]))
      new_row <- rep(NA_character_, new_ncol)
      for (span_idx in seq_len(nrow(spans))) {
        start <- spans$start[span_idx]
        if (start > new_ncol) {
          next
        }
        end <- min(spans$end[span_idx], new_ncol)
        new_row[start] <- spans$label[span_idx]
        if (end > start) {
          new_row[seq.int(start + 1L, end)] <- ""
        }
      }
      new_gdj[row_idx, ] <- new_row
    }
    colnames(new_gdj) <- colnames(x@data_body)
    x@group_data_j <- new_gdj
  }

  # Update dimensions and names
  x@nrow <- nrow(x@data_body) + nrow(x@group_data_i)
  x@ncol <- new_ncol
  x@names <- names(x@data_body)

  # Update width_cols to match new column count
  if (length(x@width_cols) > 0) {
    x@width_cols <- x@width_cols[kept]
  }

  # Numeric `j` in a lazy call refers to a position in the rendered table, the
  # same convention `i` follows, so removing a column does not shift the stored
  # indices. Entries only need to be clipped to the narrower table.
  x@lazy_format <- clip_lazy_list(x@lazy_format, new_ncol)
  x@lazy_plot <- clip_lazy_list(x@lazy_plot, new_ncol)
  x@lazy_style <- clip_lazy_list(x@lazy_style, new_ncol)

  # Column group styles are derived from @group_data_j, which was just
  # truncated: rebuild them rather than clip them.
  x <- style_group_j(x)

  # Avoid colspan that exceeds the new number of columns
  x@style <- clamp_colspan(x@style, x@ncol)

  return(x)
}


#' Clip j-indices in a list of lazy calls after column subsetting
#'
#' Entries that no longer target any column are dropped entirely: setting
#' their `j` to NULL instead would make them apply to every remaining column.
#' @keywords internal
#' @noRd
clip_lazy_list <- function(lst, new_ncol) {
  out <- lapply(lst, clip_j, new_ncol = new_ncol)
  out[!vapply(out, is.null, logical(1))]
}


#' Clip the j-indices of a lazy call to the width of a subsetted table
#'
#' Returns the (possibly modified) lazy entry, or NULL if every column the
#' entry targeted now falls outside the table. Only plain positive numeric `j`
#' values are clipped; anything else (NULL, negative selections, character
#' column names, NSE markers, ...) is deliberately left untouched so we never
#' mangle entries we cannot interpret.
#' @keywords internal
#' @noRd
clip_j <- function(call_args, new_ncol) {
  if (is.call(call_args)) {
    j <- call_args[["j"]]
  } else if (is.list(call_args)) {
    j <- call_args$j
  } else {
    return(call_args)
  }

  if (!is.numeric(j) || length(j) == 0L || anyNA(j) || any(j <= 0)) {
    return(call_args)
  }

  new_j <- j[j <= new_ncol]
  if (length(new_j) == 0L) {
    return(NULL)
  }

  colspan <- if (is.call(call_args)) {
    call_args[["colspan"]]
  } else {
    call_args$colspan
  }

  if (is.numeric(colspan) && length(colspan) == 1L && !is.na(colspan)) {
    # Shrink the span so it stops at the last surviving column. `colspan` must
    # be >= 2: a span reduced to a single column is a plain cell, so drop the
    # argument instead of setting it to 1.
    new_colspan <- min(colspan, new_ncol - min(new_j) + 1L)
    if (new_colspan < 2L) {
      new_colspan <- NULL
    }
    if (is.call(call_args)) {
      call_args[["colspan"]] <- new_colspan
    } else {
      call_args$colspan <- new_colspan
    }
  }

  if (is.call(call_args)) {
    call_args[["j"]] <- new_j
  } else {
    call_args$j <- new_j
  }
  call_args
}

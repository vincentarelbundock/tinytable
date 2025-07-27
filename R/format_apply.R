apply_caption <- function(x, format_fn, source = "original", ...) {
  if (!inherits(x, "tinytable")) {
    return(x)
  }
  x@caption <- format_fn(x@caption, ...)
  return(x)
}

apply_notes <- function(x, format_fn, ...) {
  if (!inherits(x, "tinytable")) {
    return(x)
  }

  for (idx in seq_along(x@notes)) {
    n <- x@notes[[idx]]
    if (is.character(n) && length(n) == 1) {
      x@notes[[idx]] <- format_fn(n, ...)
    } else if (is.list(n) && "text" %in% names(n)) {
      n$text <- format_fn(n$text, ...)
      x@notes[[idx]] <- n
    }
  }
  return(x)
}

apply_group_j <- function(x, format_fn, ...) {
  if (!inherits(x, "tinytable")) {
    return(x)
  }
  data_slot <- x@data_group_j
  if (nrow(data_slot) > 0) {
    for (row_idx in seq_len(nrow(data_slot))) {
      for (col_idx in seq_len(ncol(data_slot))) {
        current_value <- data_slot[row_idx, col_idx]
        if (!is.na(current_value) && trimws(current_value) != "") {
          formatted_value <- format_fn(current_value, ...)
          if (!is.null(formatted_value)) {
            data_slot[row_idx, col_idx] <- formatted_value
          }
        }
      }
    }
  }
  x@data_group_j <- data_slot
  return(x)
}

apply_colnames <- function(x, format_fn, ...) {
  colnames(x) <- format_fn(colnames(x), ...)
  return(x)
}

# Global dispatcher function for applying formatting functions
apply_format <- function(
    x,
    i,
    j,
    format_fn,
    components = NULL,
    inherit_class = NULL,
    original_data = TRUE,
    ...) {
  if (is.character(components)) {
    if ("all" %in% components) {
      components <- c(
        "colnames",
        "caption",
        "notes",
        "groupi",
        "groupj",
        "cells"
      )
    }
  }

  if (identical(components, "groupi")) {
    i <- x@index_group_i
  }

  if (inherits(x, "tinytable")) {
    out <- x@data_body
    ori <- x@data
  } else {
    out <- ori <- x
  }

  # Apply formatting to specified components only
  if ("colnames" %in% components) {
    x <- apply_colnames(x, format_fn, ...)
  }
  if ("caption" %in% components) {
    x <- apply_caption(x, format_fn, ...)
  }
  if ("notes" %in% components) {
    x <- apply_notes(x, format_fn, ...)
  }
  if ("groupj" %in% components) {
    x <- apply_group_j(x, format_fn, ...)
  }

  if (is.null(components) || any(c("cells", "groupi") %in% components)) {
    # Apply to specific cells
    # Filter columns based on inherits argument and original data
    j_filtered <- j

    if (inherits(x, "tinytable")) {
      classref <- x@data
    } else {
      classref <- out
    }

    if (is.character(inherit_class)) {
      j_filtered <- j[sapply(j, function(col) {
        inherits(classref[[col]], inherit_class)
      })]
    } else if (is.function(inherit_class)) {
      j_filtered <- j[sapply(j, function(col) inherit_class(classref[[col]]))]
    }

    # Default behavior: use original values, fall back to out if ori is not available
    for (col in j_filtered) {
      idx_body <- if (inherits(x, "tinytable")) {
        x@index_body
      } else {
        seq_len(nrow(out))
      }

      # original data rows
      if (original_data) {
        idx_out <- i

        idx_out <- intersect(i, idx_body)
        idx_ori <- seq_len(nrow(ori))[idx_body %in% i]
        tmp <- tryCatch(
          format_fn(ori[idx_ori, col, drop = TRUE], ...),
          error = function(e) NULL
        )
        if (length(tmp) > 0) {
          out[idx_out, col] <- tmp
        }

        # row groups
        idx_out <- setdiff(i, idx_body)
        tmp <- tryCatch(
          format_fn(out[idx_out, col, drop = TRUE], ...),
          error = function(e) NULL
        )
        if (length(tmp) > 0) out[idx_out, col] <- tmp
      } else {
        tmp <- tryCatch(
          format_fn(out[i, col, drop = TRUE], ...),
          error = function(e) NULL
        )
        if (length(tmp) > 0) out[i, col] <- tmp
      }
    }

    if (inherits(x, "tinytable")) {
      x@data_body <- out
    } else {
      x <- out
    }
  }

  return(x)
}

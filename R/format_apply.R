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

apply_group_j <- function(x, i, j, format_fn, components, ...) {
  if (!inherits(x, "tinytable")) {
    return(x)
  }

  if (!"groupj" %in% components && !any(i < 0)) {
    return(x)
  }

  data_slot <- x@group_data_j

  if ("groupj" %in% components) {
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
  } else if (any(i < 0)) {
    i_idx <- i[i < 0]
    i_idx <- i_idx - min(i_idx) + 1
    for (row in i_idx) {
      for (col in j) {
        current_value <- data_slot[row, col]
        if (!is.na(current_value) && trimws(current_value) != "") {
          formatted_value <- format_fn(current_value, ...)
          if (!is.null(formatted_value)) {
            data_slot[row, col] <- formatted_value
          }
        }
      }
    }
  }

  x@group_data_j <- data_slot
  return(x)
}

apply_colnames <- function(x, i, j, format_fn, components, ...) {
  if ("colnames" %in% components) {
    colnames(x) <- format_fn(colnames(x), ...)
  } else if (0 %in% i) {
    formatted <- format_fn(colnames(x)[j], ...)
    if (length(formatted) == length(j)) {
      colnames(x)[j] <- formatted
    }
  }
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
  ...
) {
  if (is.character(components)) {
    if ("all" %in% components) {
      components <- c(
        "colnames",
        "caption",
        "notes",
        "~groupi",
        "groupi",
        "groupj",
        "cells"
      )
    }
  }

  if (identical(components, "groupi")) {
    i <- x@group_index_i
  }

  if (inherits(x, "tinytable")) {
    out <- x@data_body
    ori <- x@data
  } else {
    out <- ori <- x
  }

  # Apply formatting to specified components only
  x <- apply_colnames(x, i, j, format_fn, components = components, ...)
  x <- apply_group_j(x, i, j, format_fn, components = components, ...)

  if ("caption" %in% components) {
    x <- apply_caption(x, format_fn, ...)
  }
  if ("notes" %in% components) {
    x <- apply_notes(x, format_fn, ...)
  }

  if (
    is.null(components) || any(c("cells", "groupi", "~groupi") %in% components)
  ) {
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

    i <- sort(unique(i))

    # group i
    if (inherits(x, "tinytable") && length(x@group_index_i) > 0) {
      for (col in j_filtered) {
        if (!inherits(x, "tinytable")) {
          next
        }
        idx <- x@group_index_i %in% i
        tmp <- tryCatch(
          format_fn(x@group_data_i[idx, col], ...),
          error = function(e) NULL
        )
        if (length(tmp) > 0) {
          x@group_data_i[idx, col] <- tmp
        }
      }
    }

    # body
    for (col in j_filtered) {
      # index: we are only formatting the body rows
      # ori & out currently have the same dimensions
      if (inherits(x, "tinytable")) {
        idx <- x@index_body %in% i
      } else {
        idx <- seq_len(nrow(out)) %in% i
      }

      # original data rows
      if (original_data) {
        vec <- vec_original <- ori[idx, col, drop = TRUE]
        formatted <- tryCatch(
          format_fn(
            vec = vec,
            vec_original = vec_original,
            ...
          ),
          error = function(e) NULL
        )
      } else {
        vec <- out[idx, col, drop = TRUE]
        vec_original <- ori[idx, col, drop = TRUE]
        formatted <- tryCatch(
          format_fn(
            vec = vec,
            vec_original = vec_original,
            ...
          ),
          error = function(e) NULL
        )
      }
      if (length(formatted) > 0) {
        out[idx, col] <- formatted
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

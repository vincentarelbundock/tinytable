#' Apply a single style entry to the rectangular style dataframe
#' @param style_other Rectangular dataframe with all cells
#' @param style_row Single row from @style dataframe
#' @return Modified style_other dataframe
#' @keywords internal
#' @noRd
apply_style_to_rect <- function(style_other, style_row) {
  # Determine which cells this entry applies to
  if (is.na(style_row$i)) {
    # Apply to all rows
    i_vals <- unique(style_other$i)
  } else {
    i_vals <- style_row$i
  }

  if (is.na(style_row$j)) {
    # Apply to all columns
    j_vals <- unique(style_other$j)
  } else {
    j_vals <- style_row$j
  }

  # Find matching rows in style_other
  mask <- style_other$i %in% i_vals & style_other$j %in% j_vals

  # Overwrite non-NA values from style_row
  style_props <- c("bold", "italic", "underline", "strikeout",
                   "monospace", "smallcap", "align", "alignv",
                   "color", "background", "fontsize", "indent",
                   "html_css", "colspan", "rowspan")

  for (prop in style_props) {
    if (prop %in% names(style_row) && !is.na(style_row[[prop]])) {
      style_other[[prop]][mask] <- style_row[[prop]]
    }
  }

  return(style_other)
}


#' Append line style entry to the style_lines dataframe
#' @param style_lines Dataframe accumulating line entries
#' @param style_row Single row from @style dataframe
#' @param rect Full rectangular grid of (i,j) cells
#' @return Modified style_lines dataframe with new entries appended
#' @keywords internal
#' @noRd
append_lines_to_rect <- function(style_lines, style_row, rect) {
  # Skip if no line property
  if (is.na(style_row$line)) {
    return(style_lines)
  }

  # Determine which cells this line entry applies to
  if (is.na(style_row$i) && is.na(style_row$j)) {
    # Apply to all cells
    i_vals <- unique(rect$i)
    j_vals <- unique(rect$j)
  } else if (is.na(style_row$i)) {
    # Apply to all rows in these columns
    i_vals <- unique(rect$i)
    j_vals <- style_row$j
  } else if (is.na(style_row$j)) {
    # Apply to all columns in these rows
    i_vals <- style_row$i
    j_vals <- unique(rect$j)
  } else {
    # Specific cells
    i_vals <- style_row$i
    j_vals <- style_row$j
  }

  # Create new entries for all matching (i,j) combinations
  new_entries <- expand.grid(i = i_vals, j = j_vals, stringsAsFactors = FALSE)

  # Add line properties
  new_entries$line <- style_row$line
  new_entries$line_color <- if ("line_color" %in% names(style_row)) style_row$line_color else NA
  new_entries$line_width <- if ("line_width" %in% names(style_row)) style_row$line_width else NA
  new_entries$line_trim <- if ("line_trim" %in% names(style_row)) style_row$line_trim else NA

  # Append to style_lines
  if (nrow(style_lines) == 0) {
    return(new_entries)
  } else {
    return(rbind(style_lines, new_entries))
  }
}


#' Resolve all entries of @style into the rectangular @style_other grid and
#' the line list @style_lines in a single batched pass.
#'
#' This is functionally equivalent to (and produces value-identical output as)
#' the per-row loop:
#'   for (idx in seq_len(nrow(x@style))) {
#'     style_other <- apply_style_to_rect(style_other, x@style[idx,])
#'     style_lines <- append_lines_to_rect(style_lines, x@style[idx,], rect)
#'   }
#' but is O(N + cells) instead of O(N * cells * props) by avoiding the per-row
#' full-rect mask scan. The naive loop becomes the dominant bottleneck once a
#' table accumulates more than a few hundred style_tt() entries (which is
#' common when styling individual cells for heat-map-like effects, alternating
#' row backgrounds, or per-column alignment).
#'
#' @param style_other Rectangular dataframe of (i, j, <props>) initialized with NAs
#' @param style_df @style data frame (one row per style_tt setting entry)
#' @return list(other=, lines=) with the populated data frames
#' @keywords internal
#' @noRd
resolve_styles_batch <- function(style_other, style_df) {
  n <- nrow(style_df)
  empty_lines <- data.frame(
    i = integer(0), j = integer(0),
    line = character(0),
    line_color = character(0),
    line_width = numeric(0),
    line_trim = character(0),
    stringsAsFactors = FALSE
  )
  if (n == 0) {
    return(list(
      other = style_other,
      lines = empty_lines,
      has_style = rep(FALSE, nrow(style_other))
    ))
  }

  style_props <- c("bold", "italic", "underline", "strikeout",
                   "monospace", "smallcap", "align", "alignv",
                   "color", "background", "fontsize", "indent",
                   "html_css", "colspan", "rowspan")

  # Unique i and j present in style_other (used when i or j is NA meaning "all")
  so_i <- style_other$i
  so_j <- style_other$j
  uniq_i <- unique(so_i)
  uniq_j <- unique(so_j)

  idx_matrix <- matrix(
    NA_integer_,
    nrow = length(uniq_i),
    ncol = length(uniq_j),
    dimnames = list(as.character(uniq_i), as.character(uniq_j))
  )
  idx_matrix[cbind(match(so_i, uniq_i), match(so_j, uniq_j))] <- seq_along(so_i)

  sd_i <- style_df$i
  sd_j <- style_df$j
  i_na <- is.na(sd_i)
  j_na <- is.na(sd_j)

  target_i <- vector("list", n)
  target_j <- vector("list", n)
  target_idx <- vector("list", n)
  for (k in seq_len(n)) {
    target_i[[k]] <- if (i_na[k]) uniq_i else sd_i[k]
    target_j[[k]] <- if (j_na[k]) uniq_j else sd_j[k]

    i_pos <- if (i_na[k]) seq_along(uniq_i) else match(sd_i[k], uniq_i)
    j_pos <- if (j_na[k]) seq_along(uniq_j) else match(sd_j[k], uniq_j)
    if (anyNA(i_pos) || anyNA(j_pos)) {
      target_idx[[k]] <- integer(0)
    } else {
      idx <- as.vector(idx_matrix[i_pos, j_pos, drop = FALSE])
      target_idx[[k]] <- idx[!is.na(idx)]
    }
  }

  has_style <- rep(FALSE, nrow(style_other))

  # For each property, walk style rows that have a non-NA value, in order,
  # and write directly into style_other at the matching linear indices.
  # "Last write wins" semantics is preserved by iterating in source order.
  for (prop in style_props) {
    if (!prop %in% names(style_df)) next
    vals <- style_df[[prop]]
    has <- which(!is.na(vals))
    if (length(has) == 0L) next
    for (k in has) {
      idx <- target_idx[[k]]
      if (length(idx)) {
        style_other[[prop]][idx] <- vals[[k]]
        has_style[idx] <- TRUE
      }
    }
  }

  # Lines are additive: expand every style_df row that has a non-NA `line`
  # into one entry per (i, j) cell, then rbind() once at the end.
  has_line <- which(!is.na(style_df$line))
  if (length(has_line)) {
    line_color_col <- if ("line_color" %in% names(style_df)) style_df$line_color else rep(NA_character_, n)
    line_width_col <- if ("line_width" %in% names(style_df)) style_df$line_width else rep(NA_real_, n)
    line_trim_col  <- if ("line_trim"  %in% names(style_df)) style_df$line_trim  else rep(NA_character_, n)

    line_entries <- vector("list", length(has_line))
    for (kp in seq_along(has_line)) {
      k <- has_line[kp]
      # Issue #664: unname targets before creating data.frame() to avoid
      # "row names contain missing values" when grouping order leaves NA names.
      ri <- as.integer(unname(target_i[[k]]))
      rj <- as.integer(unname(target_j[[k]]))
      # cartesian product (preserves append_lines_to_rect ordering)
      line_entries[[kp]] <- data.frame(
        i = rep(ri, times = length(rj)),
        j = rep(rj, each  = length(ri)),
        line = style_df$line[k],
        line_color = line_color_col[k],
        line_width = line_width_col[k],
        line_trim  = line_trim_col[k],
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    }
    style_lines <- do.call(rbind, line_entries)
  } else {
    style_lines <- empty_lines
  }

  list(other = style_other, lines = style_lines, has_style = has_style)
}


#' Filter a rectangular @style_other grid to rows relevant for a backend
#' @keywords internal
#' @noRd
filter_style_other <- function(style_other, style_cols) {
  if (is.null(style_other) || nrow(style_other) == 0) {
    return(style_other)
  }

  style_cols <- intersect(style_cols, names(style_other))
  if (length(style_cols) == 0L) {
    return(style_other[0, , drop = FALSE])
  }

  has_style <- attr(style_other, "has_style", exact = TRUE)
  if (!is.null(has_style) && length(has_style) == nrow(style_other)) {
    style_other <- style_other[has_style, , drop = FALSE]
  }

  if (nrow(style_other) == 0) {
    return(style_other)
  }

  has_backend_style <- rowSums(!is.na(style_other[, style_cols, drop = FALSE])) > 0
  style_other[has_backend_style, , drop = FALSE]
}


# NOTE: expand_lines() and expand_style() have been removed.
# Line styles are now handled via @style_lines in build_tt()
# Other styles are handled via @style_other in build_tt()

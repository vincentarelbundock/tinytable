last_df = function(x, bycols = c("i", "j")) {
  # build key once
  key = paste(x[[bycols[1]]], x[[bycols[2]]], sep = "_")
  if (!anyDuplicated(key)) return(x)

  # split into unique vs duplicated groups
  dup_mask = duplicated(key) | duplicated(key, fromLast = TRUE)
  x_unique = x[!dup_mask, , drop = FALSE]
  x_dup    = x[ dup_mask, , drop = FALSE]

  # group only the duplicated rows
  bycols_list = as.list(x_dup[, bycols, drop = FALSE])
  groups = split(seq_len(nrow(x_dup)), bycols_list)

  # cache column info
  col_names = names(x_dup)
  is_grouping = col_names %in% bycols

  # aggregate only duplicated groups
  result_list = vector("list", length(groups))
  for (g_idx in seq_along(groups)) {
    idx = groups[[g_idx]]

    if (length(idx) == 1L) {
      # safety: copy as-is (should be rare in dup-only subset)
      result_list[[g_idx]] = x_dup[idx, , drop = FALSE]
    } else {
      group_data = x_dup[idx, , drop = FALSE]
      result_row = group_data[1, , drop = FALSE]  # template keeps classes/levels

      # aggregate non-grouping columns
      for (col_idx in which(!is_grouping)) {
        col = col_names[col_idx]
        values = group_data[[col]]

        if (length(values) == 0L) next

        if (is.logical(values)) {
          result_row[[col]] = any(values, na.rm = TRUE)
        } else if (is.numeric(values)) {
          mx = suppressWarnings(max(values, na.rm = TRUE))
          # if all NA, keep last value (NA) to avoid -Inf
          if (is.infinite(mx) && mx < 0) {
            result_row[[col]] = values[length(values)]
          } else {
            result_row[[col]] = mx
          }
        } else {
          # character/factor/list: last non-NA if available, else last
          non_na = values[!is.na(values)]
          result_row[[col]] = if (length(non_na) > 0L) non_na[length(non_na)] else values[length(values)]
        }
      }

      result_list[[g_idx]] = result_row
    }
  }

  x_dup_agg = do.call(rbind, result_list)

  # recombine untouched uniques with processed duplicates
  out = rbind(x_unique, x_dup_agg)
  rownames(out) = NULL
  out
}


expand_lines <- function(x, rect, styles) {
  # Extract line-related properties
  line_props <- names(styles)[grepl("^line", names(styles))]
  if (length(line_props) == 0) {
    return(NULL)
  }
  idx <- grepl("^line|^i$|^j$", names(styles))
  lines <- styles[!is.na(styles$line), idx, drop = FALSE]

  if (nrow(lines) == 0) {
    return(NULL)
  }

  # Expand NA i/j for lines, similar to expand_other
  line_list <- list()

  for (r in seq_len(nrow(lines))) {
    cols <- names(lines)
    if (is.na(lines[r, "i"]) && is.na(lines[r, "j"])) {
      cols_expand <- setdiff(cols, c("i", "j"))
    } else if (is.na(lines[r, "i"])) {
      cols_expand <- setdiff(cols, "i")
    } else if (is.na(lines[r, "j"])) {
      cols_expand <- setdiff(cols, "j")
    } else {
      cols_expand <- cols
    }
    rect_line <- merge(rect, lines[r, cols_expand, drop = FALSE], all = TRUE, sort = FALSE)
    # Only remove rows where i or j are NA (essential columns), not other columns like line_trim
    rect_line <- rect_line[!is.na(rect_line$i) & !is.na(rect_line$j), , drop = FALSE]
    if (nrow(rect_line) > 0) {
      line_list <- c(line_list, list(rect_line))
    }
  }

  if (length(line_list) > 0) {
    out <- do.call(rbind, line_list)
    return(out)
  } else {
    return(NULL)
  }
}

expand_other <- function(x, rect, styles) {
  # Extract non-line properties
  other_props <- names(styles)[!grepl("^line", names(styles)) & !names(styles) %in% c("i", "j")]
  if (length(other_props) == 0) {
    return(NULL)
  }

  style_list <- list()

  for (p in other_props) {
    # Keep only relevant columns, unique rows, and rows with a concrete value for this property
    cols <- c("i", "j", p)
    cols <- unique(intersect(cols, names(styles)))
    sub <- unique(styles[, cols, drop = FALSE])
    sub <- sub[!is.na(sub[[p]]), , drop = FALSE]

    if (nrow(sub) == 0) next

    # Expand NA i/j
    row_style <- list()

    for (r in seq_len(nrow(sub))) {
      if (is.na(sub[r, "i"]) && is.na(sub[r, "j"])) {
        cols_expand <- setdiff(cols, c("i", "j"))
      } else if (is.na(sub[r, "i"])) {
        cols_expand <- setdiff(cols, "i")
      } else if (is.na(sub[r, "j"])) {
        cols_expand <- setdiff(cols, "j")
      } else {
        cols_expand <- cols
      }
      rect_p <- merge(rect, sub[r, cols_expand, drop = FALSE], all = TRUE, sort = FALSE)
      # Don't use na.omit - it removes column-specific styles (i=NA, j=specific)
      # Only remove rows where both i and j are NA, which shouldn't happen
      rect_p <- rect_p[!(is.na(rect_p$i) & is.na(rect_p$j)), , drop = FALSE]
      row_style <- c(row_style, list(rect_p))
    }
    out <- do.call(rbind, row_style)

    # Last style wins for non-line properties
    out <- last_df(unique(out))
    style_list[[p]] <- out
  }

  # Merge all other styles efficiently
  if (length(style_list) > 0) {
    # Instead of using Reduce(merge), collect all unique (i,j) pairs first
    # Then add columns from each style_list element
    all_cells <- unique(do.call(rbind, lapply(style_list, function(x) x[, c("i", "j"), drop = FALSE])))

    # Start with the (i,j) base
    style_other <- all_cells

    # Add each style property column
    for (p in names(style_list)) {
      prop_data <- style_list[[p]]
      # Create a merge key
      style_other_key <- paste(style_other$i, style_other$j, sep = "_")
      prop_key <- paste(prop_data$i, prop_data$j, sep = "_")

      # Match and assign values
      match_idx <- match(style_other_key, prop_key)
      style_other[[p]] <- prop_data[[p]][match_idx]
    }

    # Ensure all expected style columns exist in style_other
    # Include ALL columns that style_tt_lazy creates to ensure rbind compatibility
    expected_cols <- c(
      "bold", "italic", "underline", "strikeout", "monospace", "smallcap",
      "align", "alignv", "color", "background", "fontsize", "indent", "html_css",
      "colspan", "rowspan", "line", "line_color", "line_width", "line_trim")
    missing_cols <- setdiff(expected_cols, names(style_other))
    if (length(missing_cols) > 0) {
      for (col in missing_cols) {
        if (col %in% c("bold", "italic", "underline", "strikeout", "monospace", "smallcap")) {
          style_other[[col]] <- FALSE
        } else {
          style_other[[col]] <- NA
        }
      }
    }

    return(style_other)
  } else {
    NULL
  }
}

expand_style <- function(x) {
  # 1) Full rectangle of cells
  iseq <- seq_len(nrow(x))
  iseq <- c(-1 * 0:(x@nhead - 1), iseq)
  jseq <- seq_len(ncol(x))
  rect <- expand.grid(i = iseq, j = jseq)

  styles <- x@style
  if (is.null(styles) || !nrow(styles)) {
    return(list(lines = NULL, other = NULL))
  }


  # Use separate functions for lines and other properties
  style_lines <- expand_lines(x, rect, styles)
  style_other <- expand_other(x, rect, styles)

  list(lines = style_lines, other = style_other)
}

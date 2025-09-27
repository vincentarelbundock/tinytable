last_df <- function(x, bycols = c("i", "j")) {
  bycols <- as.list(x[, bycols, drop = FALSE])
  out <- split(x, bycols)
  out <- Filter(function(k) nrow(k) > 0, out)

  out <- lapply(out, function(group) {
    if (nrow(group) == 1) {
      return(group)
    }

    # Aggregate each column based on its type
    result <- group[1, , drop = FALSE]  # Start with first row as template

    for (col in names(group)) {
      if (col %in% bycols) {
        # Keep grouping columns as-is
        next
      }

      values <- group[[col]]

      if (is.logical(values)) {
        # For logical: return any(TRUE)
        result[[col]] <- any(values, na.rm = TRUE)
      } else if (is.numeric(values)) {
        # For numeric: return max()
        result[[col]] <- max(values, na.rm = TRUE)
      } else {
        # For other types: return last non-NA value, or just last if all are NA
        non_na_values <- values[!is.na(values)]
        if (length(non_na_values) > 0) {
          result[[col]] <- utils::tail(non_na_values, 1)
        } else {
          result[[col]] <- utils::tail(values, 1)
        }
      }
    }

    return(result)
  })

  do.call(rbind, out)
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
      rect_p <- stats::na.omit(rect_p)
      row_style <- c(row_style, list(rect_p))
    }
    out <- do.call(rbind, row_style)

    # Last style wins for non-line properties
    out <- last_df(unique(out))
    style_list[[p]] <- out
  }

  # Merge all other styles
  if (length(style_list) > 0) {
    style_other <- Reduce(function(d1, d2) merge(d1, d2, all = TRUE, sort = FALSE), style_list)

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

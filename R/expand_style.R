#' Apply a single style entry to the rectangular style dataframe
#' @param style_other Rectangular dataframe with all cells
#' @param style_row Single row from @style dataframe
#' @return Modified style_other dataframe
#' @keywords internal
#' @noRd
apply_style_to_rect = function(style_other, style_row) {
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

expand_style <- function(x) {
  # NOTE: This function now only handles line styles.
  # Other styles (bold, italic, color, etc.) are handled via @style_other in build_tt()

  # 1) Full rectangle of cells
  iseq <- seq_len(nrow(x))
  iseq <- c(-1 * 0:(x@nhead - 1), iseq)
  jseq <- seq_len(ncol(x))
  rect <- expand.grid(i = iseq, j = jseq)

  styles <- x@style
  if (is.null(styles) || !nrow(styles)) {
    return(list(lines = NULL, other = NULL))
  }

  # Only expand line styles
  style_lines <- expand_lines(x, rect, styles)

  list(lines = style_lines, other = NULL)
}

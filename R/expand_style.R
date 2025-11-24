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


# NOTE: expand_lines() and expand_style() have been removed.
# Line styles are now handled via @style_lines in build_tt()
# Other styles are handled via @style_other in build_tt()

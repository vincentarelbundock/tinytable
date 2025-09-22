
# =============================================================================
# CSS Utility Layer
# =============================================================================

#' Parse CSS rule into named character vector
#' @keywords internal
#' @noRd
css_parse <- function(rule) {
  if (is.null(rule) || !nzchar(trimws(rule))) return(character())
  parts <- strsplit(rule, ";", fixed = TRUE)[[1]]
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  out <- character()
  for (p in parts) {
    kv <- strsplit(p, ":", fixed = TRUE)[[1]]
    if (length(kv) >= 2) {
      k <- trimws(kv[1])
      v <- trimws(paste(kv[-1], collapse = ":"))
      out[k] <- v
    }
  }
  out
}

#' Render named character vector back to CSS rule
#' @keywords internal
#' @noRd
css_render <- function(x) {
  if (!length(x)) return("")
  paste0(names(x), ": ", unname(x), collapse = "; ")
}

#' Pick first matching key from CSS map with default
#' @keywords internal
#' @noRd
css_pick <- function(named_chr, keys, default = NULL) {
  for (key in keys) {
    if (key %in% names(named_chr)) {
      return(named_chr[[key]])
    }
  }
  default
}

#' Merge flag values (take max across duplicates)
#' @keywords internal
#' @noRd
css_merge_flags <- function(named_chr, flag_keys) {
  for (flag_key in flag_keys) {
    matching_keys <- names(named_chr)[names(named_chr) == flag_key]
    if (length(matching_keys) > 1) {
      values <- as.numeric(named_chr[matching_keys])
      named_chr[[flag_key]] <- as.character(max(values, na.rm = TRUE))
      # Remove duplicates
      named_chr <- named_chr[!names(named_chr) %in% matching_keys[-1]]
    }
  }
  named_chr
}

#' Parse border declaration into components
#' @keywords internal
#' @noRd
border_parse <- function(x) {
  if (is.na(x) || !nzchar(x)) return(list(style = NA, width = NA, color = NA))
  toks <- strsplit(trimws(x), "\\s+")[[1]]
  styles <- c("none", "hidden", "dotted", "dashed", "solid", "double",
             "groove", "ridge", "inset", "outset")
  style <- toks[toks %in% styles][1]
  width <- toks[grepl("^(\\d+(\\.\\d+)?)(px|em|rem|pt|%)$", toks)][1]
  color <- setdiff(toks, c(style, width))[1]  # crude but works with named color/hex
  list(style = if (is.na(style)) NA else style,
       width = if (is.na(width)) NA else width,
       color = if (is.na(color)) NA else color)
}

#' Initialize default pseudo-element CSS variables
#' @keywords internal
#' @noRd
pseudo_vars_init <- function() {
  c("--border-left" = "0", "--border-right" = "0", "--border-top" = "0", "--border-bottom" = "0",
    "--line-width-left" = "0.1em", "--line-width-right" = "0.1em", "--line-width-top" = "0.1em",
    "--line-width-bottom" = "0.1em", "--trim-top-left" = "0%", "--trim-top-right" = "0%",
    "--trim-bottom-left" = "0%", "--trim-bottom-right" = "0%", "--trim-left-top" = "0%",
    "--trim-left-bottom" = "0%", "--trim-right-top" = "0%", "--trim-right-bottom" = "0%")
}

#' Generate pseudo-element CSS template
#' @keywords internal
#' @noRd
pseudo_css_templates <- function(id, base_rule) {
  paste0(
    "      .tinytable td.", id, ", .tinytable th.", id, " { ", base_rule, " }\n",
    "      .tinytable td.", id, "::before, .tinytable th.", id, "::before { content: ''; position: absolute; ",
      "top: var(--trim-top-left, var(--trim-top-right, 0)); left: var(--trim-left-top, 0); right: var(--trim-right-top, 0); bottom: 0; ",
      "pointer-events: none; z-index: 1; ",
      "border-left: calc(var(--border-left) * var(--line-width-left, var(--line-width, 0.1em))) solid var(--line-color-left, var(--line-color, black)); ",
      "border-right: calc(var(--border-right) * var(--line-width-right, var(--line-width, 0.1em))) solid var(--line-color-right, var(--line-color, black)); ",
      "border-top: calc(var(--border-top) * var(--line-width-top, var(--line-width, 0.1em))) solid var(--line-color-top, var(--line-color, black)); ",
    "}\n",
    "      .tinytable td.", id, "::after, .tinytable th.", id, "::after { content: ''; position: absolute; ",
      "left: var(--trim-bottom-left, 0); right: var(--trim-bottom-right, 0); bottom: var(--trim-left-bottom, var(--trim-right-bottom, 0)); ",
      "height: calc(var(--border-bottom) * var(--line-width-bottom, var(--line-width, 0.1em))); ",
      "background: var(--line-color-bottom, var(--line-color, black)); pointer-events: none; z-index: 2; }"
  )
}

#' Build and cache header column mappings
#' @keywords internal
#' @noRd
header_col_mapping_cache <- function(x) {
  cache <- list()
  if (nrow(x@group_data_j) > 0) {
    for (group_row_idx in seq_len(nrow(x@group_data_j))) {
      target_i <- -(nrow(x@group_data_j) - group_row_idx + 1)
      cache[[as.character(target_i)]] <- html_get_datacol_mapping(x, target_i)
    }
  }
  cache
}

# =============================================================================
# Refactored Functions
# =============================================================================

html_convert_to_pseudo_elements <- function(css_rule) {
  parse <- css_parse(css_rule)
  vars <- pseudo_vars_init()

  # Default values
  line_color <- "black"
  line_width <- "0.1em"

  # Process border declarations
  for (key in names(parse)) {
    val <- parse[[key]]

    if (key == "border" && grepl("solid", val)) {
      # All borders
      vars[["--border-left"]] <- "1"
      vars[["--border-right"]] <- "1"
      vars[["--border-top"]] <- "1"
      vars[["--border-bottom"]] <- "1"
      border_parts <- border_parse(val)
      if (!is.na(border_parts$color)) line_color <- border_parts$color
      if (!is.na(border_parts$width)) line_width <- border_parts$width
    } else if (key %in% c("border-left", "border-right", "border-top", "border-bottom") && grepl("solid", val)) {
      # Individual sides
      side_var <- paste0("--", key)
      vars[[side_var]] <- "1"
      border_parts <- border_parse(val)
      if (!is.na(border_parts$color)) line_color <- border_parts$color
      if (!is.na(border_parts$width)) line_width <- border_parts$width
    }
  }

  # Allow user overrides from existing CSS variables
  for (var_name in names(vars)) {
    if (var_name %in% names(parse)) {
      vars[[var_name]] <- parse[[var_name]]
    }
  }

  # Extract line properties with user overrides
  line_color <- css_pick(parse, "--line-color", line_color)
  line_width <- css_pick(parse, "--line-width", line_width)

  # Remove all border-related keys from regular CSS
  border_keys <- c("border", "border-left", "border-right", "border-top", "border-bottom",
                  "--border-left", "--border-right", "--border-top", "--border-bottom",
                  "--line-color", "--line-width", "--line-width-left", "--line-width-right",
                  "--line-width-top", "--line-width-bottom", "--trim-top-left", "--trim-top-right",
                  "--trim-bottom-left", "--trim-bottom-right", "--trim-left-top", "--trim-left-bottom",
                  "--trim-right-top", "--trim-right-bottom")
  non_border <- parse[!names(parse) %in% border_keys]

  # Add position: relative if missing
  if (!"position" %in% names(non_border)) {
    non_border[["position"]] <- "relative"
  }

  # Combine everything
  all_css <- c(non_border,
               c("--line-color" = line_color, "--line-width" = line_width),
               vars)

  css_render(all_css)
}

html_clean_css_rule <- function(css_rule) {
  # Parse the (potentially concatenated) CSS rule
  m <- css_parse(css_rule)

  # For border flags, we need to OR them together by taking max values
  # This handles the case where concatenated CSS has overwritten border values
  # We need to reconstruct the intended borders from all the CSS parts

  # Split the original CSS by semicolon and look for all border values
  parts <- strsplit(css_rule, ";", fixed = TRUE)[[1]]
  parts <- trimws(parts[nzchar(parts)])

  border_flags <- c("--border-left", "--border-right", "--border-top", "--border-bottom")
  color_flags <- c("--line-color-left", "--line-color-right", "--line-color-top", "--line-color-bottom")

  # Extract all border flag values and take maximum
  for (flag in border_flags) {
    flag_parts <- parts[grepl(paste0("^", flag, ":"), parts)]
    if (length(flag_parts) > 0) {
      values <- sapply(flag_parts, function(p) {
        val <- trimws(strsplit(p, ":", fixed = TRUE)[[1]][2])
        as.numeric(val)
      })
      m[[flag]] <- as.character(max(values, na.rm = TRUE))
    }
  }

  # For color flags, prioritize non-black colors
  for (flag in color_flags) {
    flag_parts <- parts[grepl(paste0("^", flag, ":"), parts)]
    if (length(flag_parts) > 0) {
      # Extract all color values for this flag
      color_values <- sapply(flag_parts, function(p) {
        trimws(strsplit(p, ":", fixed = TRUE)[[1]][2])
      })

      # Prioritize first non-black color, otherwise use last color
      non_black_colors <- color_values[color_values != "black"]
      if (length(non_black_colors) > 0) {
        m[[flag]] <- non_black_colors[1]
      } else {
        m[[flag]] <- color_values[length(color_values)]
      }
    }
  }

  css_render(m)
}

html_consolidate_css_vars <- function(rec) {
  # Group by cell position (i, j)
  cell_groups <- split(rec, paste(rec$i, rec$j))


  consolidated_rec <- data.frame()
  for (group in cell_groups) {
    if (nrow(group) == 1) {
      # Single rule - clean it in case it has concatenated CSS
      group$css_arguments <- html_clean_css_rule(group$css_arguments)
      consolidated_rec <- rbind(consolidated_rec, group)
    } else {
      # Multiple rules for the same cell - consolidate
      # First clean each CSS string to handle concatenated CSS properly
      cleaned_css <- lapply(group$css_arguments, html_clean_css_rule)
      maps <- lapply(cleaned_css, css_parse)

      # Smart consolidation that handles column mapping issues
      m <- character()
      border_flags <- c("--border-left", "--border-right", "--border-top", "--border-bottom")
      color_flags <- c("--line-color-left", "--line-color-right", "--line-color-top", "--line-color-bottom")
      trim_flags <- c("--trim-top-left", "--trim-top-right", "--trim-bottom-left", "--trim-bottom-right",
                      "--trim-left-top", "--trim-left-bottom", "--trim-right-top", "--trim-right-bottom")

      for (map in maps) {
        for (key in names(map)) {
          if (key %in% border_flags) {
            # For border flags, take maximum (OR logic)
            if (key %in% names(m)) {
              m[[key]] <- as.character(max(as.numeric(m[[key]]), as.numeric(map[[key]]), na.rm = TRUE))
            } else {
              m[[key]] <- map[[key]]
            }
          } else if (key %in% color_flags) {
            # For directional color flags, prioritize non-black colors
            if (key %in% names(m)) {
              current_color <- m[[key]]
              new_color <- map[[key]]
              # If current is black and new is not, use new
              # If current is not black, keep current (first non-black wins)
              if (current_color == "black" && new_color != "black") {
                m[[key]] <- new_color
              }
            } else {
              m[[key]] <- map[[key]]
            }
          } else if (key %in% trim_flags) {
            # For trim flags, take maximum (if any border needs trimming)
            if (key %in% names(m)) {
              # Convert percentage strings to numeric, take max, convert back
              current_val <- as.numeric(gsub("%", "", m[[key]]))
              new_val <- as.numeric(gsub("%", "", map[[key]]))
              max_val <- max(current_val, new_val, na.rm = TRUE)
              m[[key]] <- paste0(max_val, "%")
            } else {
              m[[key]] <- map[[key]]
            }
          } else {
            # For non-border properties, last-write-wins
            m[[key]] <- map[[key]]
          }
        }
      }

      # No need for css_merge_flags since we handled border flags above

      final_css <- css_render(m)

      # Create consolidated row (use first row as template)
      new_row <- group[1, ]
      new_row$css_arguments <- final_css
      consolidated_rec <- rbind(consolidated_rec, new_row)
    }
  }

  return(consolidated_rec)
}

# Helper function to adjust column indices using cached mappings
html_adjust_column_indices_cached <- function(user_j, user_i, mapping_cache) {
  if (length(mapping_cache) == 0) {
    return(user_j)
  }

  adjusted_j <- user_j

  # Handle each row separately since column groups affect data-col numbering per row
  for (unique_i in unique(user_i)) {
    row_mask <- user_i == unique_i
    if (is.na(unique_i) || !any(row_mask, na.rm = TRUE)) next

    # Only adjust for header rows affected by column grouping
    if (unique_i < 0) {
      cache_key <- as.character(unique_i)
      if (cache_key %in% names(mapping_cache)) {
        col_mapping <- mapping_cache[[cache_key]]

        # Vectorized mapping application
        orig_cols <- user_j[row_mask]
        valid_mask <- orig_cols >= 1 & orig_cols <= length(col_mapping)
        adjusted_j[row_mask][valid_mask] <- col_mapping[orig_cols[valid_mask]]
      }
    }
    # For non-header rows (unique_i >= 0), no adjustment needed
  }

  return(adjusted_j)
}

# Legacy function for backward compatibility
html_adjust_column_indices <- function(x, user_j, user_i) {
  cache <- header_col_mapping_cache(x)
  html_adjust_column_indices_cached(user_j, user_i, cache)
}

# Get mapping from user column index to HTML data-col value for header rows with column groups
html_get_datacol_mapping <- function(x, target_i) {
  # Initialize with 1:1 mapping (1-based, will be converted to 0-based later)
  col_mapping <- seq_len(x@ncol)

  # Only handle column groups for header rows
  if (nrow(x@group_data_j) > 0 && target_i < 0) {
    # Group rows are processed in reverse order, so map target_i to correct row
    # target_i = -1 corresponds to the first group (nrow), target_i = -2 to second group (nrow-1), etc.
    group_row_idx <- nrow(x@group_data_j) - (abs(target_i) - 1)
    if (group_row_idx >= 1 && group_row_idx <= nrow(x@group_data_j)) {
      groupj <- as.character(x@group_data_j[group_row_idx, ])
      j_list <- html_groupj_span(groupj)

      if (length(j_list) > 0) {
        # Recreate the j_combined logic from html_groupj_html
        miss <- as.list(setdiff(seq_len(x@ncol), unlist(j_list)))
        miss <- stats::setNames(miss, rep(" ", length(miss)))
        j_combined <- c(j_list, miss)

        # Sort by column position
        max_col <- sapply(j_combined, max)
        idx <- order(max_col)
        j_combined <- j_combined[idx]

        # Create mapping: each user column maps to its HTML data-col position
        # The HTML data-col position corresponds to the index in j_combined (k-1 for 0-based)
        for (k in seq_along(j_combined)) {
          original_cols <- j_combined[[k]]
          # All columns in this span map to the same data-col position (k)
          # Since we return 1-based and later subtract 1, we use k here
          for (orig_col in original_cols) {
            col_mapping[orig_col] <- k
          }
        }
      }
    }
  }

  return(col_mapping)
}

#' Internal styling function
#'
#' @inheritParams style_tt
#' @keywords internal
#' @noRd
setMethod(
  f = "style_eval",
  signature = "tinytable_html",
  definition = function(
    x,
    i = NULL,
    j = NULL,
    bold = FALSE,
    italic = FALSE,
    monospace = FALSE,
    underline = FALSE,
    strikeout = FALSE,
    color = NULL,
    background = NULL,
    fontsize = NULL,
    align = NULL,
    alignv = NULL,
    line = NULL,
    line_color = "black",
    line_width = 0.1,
    colspan = NULL,
    rowspan = NULL,
    indent = 0,
    ...
  ) {
    # CSS rule will be handled by finalize() via template substitution
    # Removed duplicate html_setting call that was causing CSS duplication

    sty <- x@style

    sty$alignv[which(sty$alignv == "t")] <- "top"
    sty$alignv[which(sty$alignv == "b")] <- "bottom"
    sty$alignv[which(sty$alignv == "m")] <- "middle"

    sty$align[which(sty$align == "l")] <- "left"
    sty$align[which(sty$align == "c")] <- "center"
    sty$align[which(sty$align == "d")] <- "center"
    sty$align[which(sty$align == "r")] <- "right"

    rec <- expand.grid(
      i = c(-(seq_len(x@nhead) - 1), seq_len(x@nrow)),
      j = seq_len(x@ncol)
    )
    css <- rep("", nrow(rec))

    # Precompute and cache column mappings
    mapping_cache <- header_col_mapping_cache(x)

    # Adjust column indices when any kind of colspan is present
    # Both column groups and user-defined colspans change data-col values in HTML
    # This must happen BEFORE the CSS generation loop so that rec$j matches adjusted sty$j
    rec$j <- html_adjust_column_indices_cached(rec$j, rec$i, mapping_cache)
    rec$j <- rec$j - 1

    # Also adjust column indices in style entries to match the adjusted rec$j values
    for (row in seq_len(nrow(sty))) {
      if (!is.na(sty$j[row])) {
        adjusted_j <- html_adjust_column_indices_cached(sty$j[row], sty$i[row], mapping_cache)
        sty$j[row] <- adjusted_j - 1
      }
    }

    for (row in seq_len(nrow(sty))) {
      # index: sty vs rec
      idx_i <- sty$i[row]
      if (is.na(idx_i)) {
        idx_i <- unique(rec$i)
      }
      idx_j <- sty$j[row]
      if (is.na(idx_j)) {
        idx_j <- unique(rec$j)
      }
      idx <- rec$i == idx_i & rec$j == idx_j

      if (isTRUE(sty[row, "bold"])) {
        css[idx] <- paste(css[idx], "font-weight: bold;")
      }
      if (isTRUE(sty[row, "italic"])) {
        css[idx] <- paste(css[idx], "font-style: italic;")
      }
      if (isTRUE(sty[row, "underline"])) {
        css[idx] <- paste(css[idx], "text-decoration: underline;")
      }
      if (isTRUE(sty[row, "strikeout"])) {
        css[idx] <- paste(css[idx], "text-decoration: line-through;")
      }
      if (isTRUE(sty[row, "monospace"])) {
        css[idx] <- paste(css[idx], "font-family: monospace;")
      }
      if (isTRUE(sty[row, "smallcap"])) {
        css[idx] <- paste(css[idx], "font-variant: small-caps;")
      }
      if (!is.na(sty[row, "color"])) {
        color_val <- standardize_colors(sty[row, "color"], format = "hex")
        css[idx] <- paste(css[idx], paste0("color: ", color_val, ";"))
      }
      if (!is.na(sty[row, "background"])) {
        background_val <- standardize_colors(
          sty[row, "background"],
          format = "hex"
        )
        css[idx] <- paste(
          css[idx],
          paste0("background-color: ", background_val, ";")
        )
      }
      if (!is.na(sty[row, "fontsize"])) {
        css[idx] <- paste(
          css[idx],
          paste0("font-size: ", sty[row, "fontsize"], "em;")
        )
      }
      if (!is.na(sty[row, "alignv"])) {
        css[idx] <- paste(
          css[idx],
          paste0("vertical-align: ", sty[row, "alignv"], ";")
        )
      }
      if (!is.na(sty[row, "align"])) {
        css[idx] <- paste(
          css[idx],
          paste0("text-align: ", sty[row, "align"], ";")
        )
      }
      if (!is.na(sty[row, "indent"])) {
        css[idx] <- paste(
          css[idx],
          paste0("padding-left: ", sty[row, "indent"], "em;")
        )
      }
      if (!is.na(sty[row, "html_css"])) {
        css[idx] <- paste(css[idx], sty[row, "html_css"])
      }

      lin <- ""
      line <- sty$line[row]
      line_width <- sty$line_width[row]
      line_color <- sty$line_color[row]
      line_color <- if (is.na(line_color)) {
        "black"
      } else {
        standardize_colors(line_color, format = "hex")
      }
      line_width <- if (is.na(line_width)) 0.1 else line_width
      left <- grepl("l", line)
      right <- grepl("r", line)
      top <- grepl("t", line)
      bottom <- grepl("b", line)

      # Set side-dependent trimming variables based on border presence and trim specification
      line_trim <- if ("line_trim" %in% names(sty)) sty$line_trim[row] else NA


      # Apply border-directional trimming based on trim specification and active borders
      # Only set trim values when line_trim is explicitly specified AND the border is active
      if (!is.na(line_trim)) {
        # Top border trimming
        trim_top_left <- if (grepl("l", line_trim) && top) "3%" else "0%"
        trim_top_right <- if (grepl("r", line_trim) && top) "3%" else "0%"

        # Bottom border trimming
        trim_bottom_left <- if (grepl("l", line_trim) && bottom) "3%" else "0%"
        trim_bottom_right <- if (grepl("r", line_trim) && bottom) "3%" else "0%"

        # Left border trimming
        trim_left_top <- if (grepl("t", line_trim) && left) "3%" else "0%"
        trim_left_bottom <- if (grepl("b", line_trim) && left) "3%" else "0%"

        # Right border trimming
        trim_right_top <- if (grepl("t", line_trim) && right) "3%" else "0%"
        trim_right_bottom <- if (grepl("b", line_trim) && right) "3%" else "0%"
      } else {
        # When line_trim is NA, don't set trim values (let consolidation handle it)
        trim_top_left <- NA
        trim_top_right <- NA
        trim_bottom_left <- NA
        trim_bottom_right <- NA
        trim_left_top <- NA
        trim_left_bottom <- NA
        trim_right_top <- NA
        trim_right_bottom <- NA
      }


      if (any(c(left, right, top, bottom))) {
        # Use pseudo-elements for all borders - store border info in CSS variables
        lin <- paste(lin, "position: relative;")
        lin <- paste(lin, sprintf("--line-width: %s;", paste0(line_width, "em")))

        # Set color and width variables for each active border direction
        line_width_em <- paste0(line_width, "em")
        if (left) {
          lin <- paste(lin, sprintf("--line-color-left: %s;", line_color))
          lin <- paste(lin, sprintf("--line-width-left: %s;", line_width_em))
        }
        if (right) {
          lin <- paste(lin, sprintf("--line-color-right: %s;", line_color))
          lin <- paste(lin, sprintf("--line-width-right: %s;", line_width_em))
        }
        if (top) {
          lin <- paste(lin, sprintf("--line-color-top: %s;", line_color))
          lin <- paste(lin, sprintf("--line-width-top: %s;", line_width_em))
        }
        if (bottom) {
          lin <- paste(lin, sprintf("--line-color-bottom: %s;", line_color))
          lin <- paste(lin, sprintf("--line-width-bottom: %s;", line_width_em))
        }

        # Apply the directional trimming variables (only when explicitly specified)
        if (!is.na(trim_top_left)) lin <- paste(lin, sprintf("--trim-top-left: %s;", trim_top_left))
        if (!is.na(trim_top_right)) lin <- paste(lin, sprintf("--trim-top-right: %s;", trim_top_right))
        if (!is.na(trim_bottom_left)) lin <- paste(lin, sprintf("--trim-bottom-left: %s;", trim_bottom_left))
        if (!is.na(trim_bottom_right)) lin <- paste(lin, sprintf("--trim-bottom-right: %s;", trim_bottom_right))
        if (!is.na(trim_left_top)) lin <- paste(lin, sprintf("--trim-left-top: %s;", trim_left_top))
        if (!is.na(trim_left_bottom)) lin <- paste(lin, sprintf("--trim-left-bottom: %s;", trim_left_bottom))
        if (!is.na(trim_right_top)) lin <- paste(lin, sprintf("--trim-right-top: %s;", trim_right_top))
        if (!is.na(trim_right_bottom)) lin <- paste(lin, sprintf("--trim-right-bottom: %s;", trim_right_bottom))

        # Store which borders are active
        lin <- paste(lin, sprintf("--border-left: %s;", if (left) "1" else "0"))
        lin <- paste(lin, sprintf("--border-right: %s;", if (right) "1" else "0"))
        lin <- paste(lin, sprintf("--border-top: %s;", if (top) "1" else "0"))
        lin <- paste(lin, sprintf("--border-bottom: %s;", if (bottom) "1" else "0"))
      }
      css[idx] <- paste(css[idx], lin)
    }

    css <- gsub(" +", " ", trimws(css))

    # User row indices should match HTML data-row values directly
    # HTML generation already handles header positioning
    rec$i <- rec$i

    # spans: before styles because we return(x) if there is no style
    for (row in seq_len(nrow(sty))) {
      rowspan <- if (!is.na(sty$rowspan[row])) sty$rowspan[row] else 1
      colspan <- if (!is.na(sty$colspan[row])) sty$colspan[row] else 1
      if (rowspan > 1 || colspan > 1) {
        id <- get_id(stem = "spanCell_")
        listener <- "      window.addEventListener('load', function () { %s(%s, %s, %s, %s) })"
        # Column indices have already been adjusted above, so use sty$j directly
        listener <- sprintf(
          listener,
          id,
          sty$i[row],
          sty$j[row],
          rowspan,
          colspan
        )
        x@table_string <- lines_insert(
          x@table_string,
          listener,
          "tinytable span after",
          "after"
        )
        # x@table_string <- html_setting(x@table_string, listener, component = "cell")
      }
    }

    rec$css_arguments <- css
    rec <- rec[rec$css_arguments != "", , drop = FALSE]
    if (nrow(rec) == 0) {
      return(x)
    }

    # Consolidate CSS variables for cells with multiple border declarations
    rec <- html_consolidate_css_vars(rec)

    # Unique CSS arguments assigned by arrays
    css_table <- unique(rec[, c("css_arguments"), drop = FALSE])
    css_table$id_css <- sapply(
      seq_len(nrow(css_table)),
      function(i) get_id(stem = "tinytable_css_")
    )
    idx <- merge(
      rec[, c("i", "j", "css_arguments")],
      css_table,
      all.x = TRUE,
      sort = FALSE
    )
    # factor is important otherwise we split by a random value and the order can break snapshots
    idx$split_idx <- factor(idx$id_css, levels = unique(idx$id_css))
    if (nrow(idx) > 0) {
      idx <- split(idx, idx$split_idx)
      for (i in seq_along(idx)) {
        id_css <- idx[[i]]$id_css[1]
        arr <- sprintf("{ i: '%s', j: %s }, ", idx[[i]]$i, idx[[i]]$j)
        arr <- c(
          "          {",
          " positions: [ ",
          arr,
          " ],",
          " css_id: '",
          id_css,
          "',",
          "}, "
        )
        arr <- paste(arr, collapse = "")
        x@table_string <- lines_insert(
          x@table_string,
          arr,
          "tinytable style arrays after",
          "after"
        )
        css_rule <- idx[[i]]$css_arguments[1]

        # Check if this CSS rule has any borders using parsed map
        m <- css_parse(css_rule)
        has_border <- any(names(m) %in% c("--border-left", "--border-right", "--border-top", "--border-bottom")) ||
                     any(startsWith(names(m), "border"))

        if (has_border) {
          # Convert any regular border declarations to CSS variables and pseudo-elements
          css_rule <- html_convert_to_pseudo_elements(css_rule)

          # Generate CSS using template
          entry <- pseudo_css_templates(id_css, css_rule)
        } else {
          # Regular CSS rule without pseudo-elements
          entry <- sprintf(
            "      .tinytable td.%s, .tinytable th.%s { %s }",
            id_css, id_css, css_rule
          )
        }
        x@table_string <- lines_insert(
          x@table_string,
          entry,
          "tinytable css entries after",
          "after"
        )
      }
    }


    return(x)
  }
)

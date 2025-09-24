
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

#' Pick last matching key from CSS map with default
#' @keywords internal
#' @noRd
css_pick <- function(named_chr, keys, default = NULL) {
  result <- default
  for (key in keys) {
    if (key %in% names(named_chr)) {
      result <- named_chr[[key]]
    }
  }
  result
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

      # Get mapping from user column index to HTML data-col value for this header row
      # Initialize with 1:1 mapping (1-based, will be converted to 0-based later)
      col_mapping <- seq_len(x@ncol)

      # Only handle column groups for header rows
      if (target_i < 0) {
        # Group rows are processed in reverse order, so map target_i to correct row
        # target_i = -1 corresponds to the first group (nrow), target_i = -2 to second group (nrow-1), etc.
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

      cache[[as.character(target_i)]] <- col_mapping
    }
  }
  cache
}

# Convert CSS map directly to pseudo-element format (no parsing required)
convert_map_to_pseudo_elements <- function(css_map) {
  vars <- pseudo_vars_init()

  # Default values
  line_color <- "black"
  line_width <- "0.1em"

  # Process border declarations from map
  for (key in names(css_map)) {
    val <- css_map[[key]]

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

  # Allow user overrides from existing CSS variables in map
  for (var_name in names(vars)) {
    if (var_name %in% names(css_map)) {
      vars[[var_name]] <- css_map[[var_name]]
    }
  }

  # Extract line properties with user overrides
  line_color <- css_pick(css_map, "--line-color", line_color)
  line_width <- css_pick(css_map, "--line-width", line_width)

  # Remove all border-related keys from regular CSS
  border_keys <- c("border", "border-left", "border-right", "border-top", "border-bottom",
                  "--border-left", "--border-right", "--border-top", "--border-bottom",
                  "--line-color", "--line-width", "--line-width-left", "--line-width-right",
                  "--line-width-top", "--line-width-bottom", "--trim-top-left", "--trim-top-right",
                  "--trim-bottom-left", "--trim-bottom-right", "--trim-left-top", "--trim-left-bottom",
                  "--trim-right-top", "--trim-right-bottom")
  non_border <- css_map[!names(css_map) %in% border_keys]

  # Add position: relative if missing
  if (!"position" %in% names(non_border)) {
    non_border[["position"]] <- "relative"
  }

  # Combine everything
  all_css <- c(non_border,
               c("--line-color" = line_color, "--line-width" = line_width),
               vars)

  all_css
}


# Map-based consolidation that works directly with CSS maps
consolidate_css_maps <- function(rec_with_maps) {
  # Group by cell position (i, j)
  cell_groups <- split(rec_with_maps, paste(rec_with_maps$i, rec_with_maps$j, sep = "_"))

  consolidated_list <- list()
  idx <- 1

  for (group in cell_groups) {
    if (nrow(group) == 1) {
      # Single rule - keep as is
      consolidated_list[[idx]] <- group
    } else {
      # Multiple rules for the same cell - sort by priority to ensure correct override order
      # Process theme styles (higher priority numbers) FIRST, then user styles (lower numbers)
      # This way "last write wins" means user styles override theme styles
      group <- group[order(group$priority, decreasing = TRUE), ]
      maps <- group$css_map

      # Smart consolidation resolver working directly with maps
      combined_map <- character()
      border_flags <- c("--border-left", "--border-right", "--border-top", "--border-bottom")
      color_flags <- c("--line-color-left", "--line-color-right", "--line-color-top", "--line-color-bottom")
      trim_flags <- c("--trim-top-left", "--trim-top-right", "--trim-bottom-left", "--trim-bottom-right",
                      "--trim-left-top", "--trim-left-bottom", "--trim-right-top", "--trim-right-bottom")

      for (map in maps) {
        for (key in names(map)) {
          if (key %in% border_flags) {
            # Numeric max for border flags
            if (key %in% names(combined_map)) {
              combined_map[[key]] <- as.character(max(as.numeric(combined_map[[key]]), as.numeric(map[[key]]), na.rm = TRUE))
            } else {
              combined_map[[key]] <- map[[key]]
            }
          } else if (key %in% color_flags) {
            # Keep first non-black, else last
            if (key %in% names(combined_map)) {
              current_color <- combined_map[[key]]
              new_color <- map[[key]]
              if (current_color == "black" && new_color != "black") {
                combined_map[[key]] <- new_color
              } else {
                # Later styles override earlier ones (last write wins)
                combined_map[[key]] <- new_color
              }
            } else {
              combined_map[[key]] <- map[[key]]
            }
          } else if (key %in% trim_flags) {
            # Numeric max on percentage values
            if (key %in% names(combined_map)) {
              current_val <- as.numeric(gsub("%", "", combined_map[[key]]))
              new_val <- as.numeric(gsub("%", "", map[[key]]))
              max_val <- max(current_val, new_val, na.rm = TRUE)
              combined_map[[key]] <- paste0(max_val, "%")
            } else {
              combined_map[[key]] <- map[[key]]
            }
          } else {
            # Last write wins for regular properties (this is the key fix!)
            combined_map[[key]] <- map[[key]]
          }
        }
      }

      # Create consolidated row using first row as template
      new_row <- group[1, ]
      new_row$css_map <- list(combined_map)
      consolidated_list[[idx]] <- new_row
    }
    idx <- idx + 1
  }

  # Rebuild with do.call to avoid repeated rbind
  if (length(consolidated_list) == 0) {
    return(data.frame())
  }
  do.call(rbind, consolidated_list)
}


# Helper function to adjust column indices using cached mappings
html_adjust_column_indices <- function(user_j, user_i, mapping_cache) {
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
    # Use named character vectors to store CSS properties for each cell
    css_maps <- vector("list", nrow(rec))
    for (i in seq_along(css_maps)) css_maps[[i]] <- character()

    # Precompute and cache column mappings
    mapping_cache <- header_col_mapping_cache(x)

    # Adjust column indices when any kind of colspan is present
    # Both column groups and user-defined colspans change data-col values in HTML
    # This must happen BEFORE the CSS generation loop so that rec$j matches adjusted sty$j
    rec$j <- html_adjust_column_indices(rec$j, rec$i, mapping_cache)
    rec$j <- rec$j - 1

    # Also adjust column indices in style entries to match the adjusted rec$j values
    for (row in seq_len(nrow(sty))) {
      if (!is.na(sty$j[row])) {
        adjusted_j <- html_adjust_column_indices(sty$j[row], sty$i[row], mapping_cache)
        sty$j[row] <- adjusted_j - 1
      }
    }

    # Property mapping table for style-to-CSS conversions
    style_map <- list(
      bold = list(css_prop = "font-weight", css_value = "bold", condition_fn = isTRUE),
      italic = list(css_prop = "font-style", css_value = "italic", condition_fn = isTRUE),
      underline = list(css_prop = "text-decoration", css_value = "underline", condition_fn = isTRUE),
      strikeout = list(css_prop = "text-decoration", css_value = "line-through", condition_fn = isTRUE),
      monospace = list(css_prop = "font-family", css_value = "monospace", condition_fn = isTRUE),
      smallcap = list(css_prop = "font-variant", css_value = "small-caps", condition_fn = isTRUE),
      alignv = list(css_prop = "vertical-align", value_fn = function(x) x, condition_fn = function(x) !is.na(x)),
      align = list(css_prop = "text-align", value_fn = function(x) x, condition_fn = function(x) !is.na(x)),
      color = list(css_prop = "color", value_fn = function(x) standardize_colors(x, format = "hex"), condition_fn = function(x) !is.na(x)),
      background = list(css_prop = "background-color", value_fn = function(x) standardize_colors(x, format = "hex"), condition_fn = function(x) !is.na(x)),
      fontsize = list(css_prop = "font-size", value_fn = function(x) paste0(x, "em"), condition_fn = function(x) !is.na(x)),
      indent = list(css_prop = "padding-left", value_fn = function(x) paste0(x, "em"), condition_fn = function(x) !is.na(x))
    )

    # Generic property setter function
    apply_style_property <- function(css_maps, indices, prop_name, prop_value, style_def) {
      if (style_def$condition_fn(prop_value)) {
        css_value <- if (!is.null(style_def$value_fn)) {
          style_def$value_fn(prop_value)
        } else {
          style_def$css_value
        }
        for (cell_idx in which(indices)) {
          css_maps[[cell_idx]][[style_def$css_prop]] <- css_value
        }
      }
      css_maps
    }

    # Create border CSS map (separated from the old process_border_styles)
    create_border_css_map <- function(line, line_width, line_color, line_trim) {
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

      css_map <- character()

      if (any(c(left, right, top, bottom))) {
        line_width_em <- paste0(line_width, "em")
        css_map[["position"]] <- "relative"
        css_map[["--line-width"]] <- line_width_em

        if (left) {
          css_map[["--line-color-left"]] <- line_color
          css_map[["--line-width-left"]] <- line_width_em
          css_map[["--border-left"]] <- "1"
        }
        if (right) {
          css_map[["--line-color-right"]] <- line_color
          css_map[["--line-width-right"]] <- line_width_em
          css_map[["--border-right"]] <- "1"
        }
        if (top) {
          css_map[["--line-color-top"]] <- line_color
          css_map[["--line-width-top"]] <- line_width_em
          css_map[["--border-top"]] <- "1"
        }
        if (bottom) {
          css_map[["--line-color-bottom"]] <- line_color
          css_map[["--line-width-bottom"]] <- line_width_em
          css_map[["--border-bottom"]] <- "1"
        }

        # Apply trimming values if specified
        if (!is.na(line_trim)) {
          trim_values <- list(
            "trim-top-left" = if (grepl("l", line_trim) && top) "3%" else "0%",
            "trim-top-right" = if (grepl("r", line_trim) && top) "3%" else "0%",
            "trim-bottom-left" = if (grepl("l", line_trim) && bottom) "3%" else "0%",
            "trim-bottom-right" = if (grepl("r", line_trim) && bottom) "3%" else "0%",
            "trim-left-top" = if (grepl("t", line_trim) && left) "3%" else "0%",
            "trim-left-bottom" = if (grepl("b", line_trim) && left) "3%" else "0%",
            "trim-right-top" = if (grepl("t", line_trim) && right) "3%" else "0%",
            "trim-right-bottom" = if (grepl("b", line_trim) && right) "3%" else "0%"
          )

          for (trim_name in names(trim_values)) {
            css_var_name <- paste0("--", trim_name)
            css_map[[css_var_name]] <- trim_values[[trim_name]]
          }
        }
      }
      css_map
    }

    # Border processing function (kept for compatibility, but now calls create_border_css_map)
    process_border_styles <- function(css_maps, indices, line, line_width, line_color, line_trim) {
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

      # Calculate trimming values
      trim_values <- list()
      if (!is.na(line_trim)) {
        trim_values$trim_top_left <- if (grepl("l", line_trim) && top) "3%" else "0%"
        trim_values$trim_top_right <- if (grepl("r", line_trim) && top) "3%" else "0%"
        trim_values$trim_bottom_left <- if (grepl("l", line_trim) && bottom) "3%" else "0%"
        trim_values$trim_bottom_right <- if (grepl("r", line_trim) && bottom) "3%" else "0%"
        trim_values$trim_left_top <- if (grepl("t", line_trim) && left) "3%" else "0%"
        trim_values$trim_left_bottom <- if (grepl("b", line_trim) && left) "3%" else "0%"
        trim_values$trim_right_top <- if (grepl("t", line_trim) && right) "3%" else "0%"
        trim_values$trim_right_bottom <- if (grepl("b", line_trim) && right) "3%" else "0%"
      }

      if (any(c(left, right, top, bottom))) {
        line_width_em <- paste0(line_width, "em")

        for (cell_idx in which(indices)) {
          css_maps[[cell_idx]][["position"]] <- "relative"
          css_maps[[cell_idx]][["--line-width"]] <- line_width_em

          if (left) {
            css_maps[[cell_idx]][["--line-color-left"]] <- line_color
            css_maps[[cell_idx]][["--line-width-left"]] <- line_width_em
            css_maps[[cell_idx]][["--border-left"]] <- "1"
          }
          if (right) {
            css_maps[[cell_idx]][["--line-color-right"]] <- line_color
            css_maps[[cell_idx]][["--line-width-right"]] <- line_width_em
            css_maps[[cell_idx]][["--border-right"]] <- "1"
          }
          if (top) {
            css_maps[[cell_idx]][["--line-color-top"]] <- line_color
            css_maps[[cell_idx]][["--line-width-top"]] <- line_width_em
            css_maps[[cell_idx]][["--border-top"]] <- "1"
          }
          if (bottom) {
            css_maps[[cell_idx]][["--line-color-bottom"]] <- line_color
            css_maps[[cell_idx]][["--line-width-bottom"]] <- line_width_em
            css_maps[[cell_idx]][["--border-bottom"]] <- "1"
          }

          # Apply trimming values if specified
          for (trim_name in names(trim_values)) {
            css_var_name <- paste0("--", gsub("_", "-", trim_name))
            css_maps[[cell_idx]][[css_var_name]] <- trim_values[[trim_name]]
          }
        }
      }
      css_maps
    }


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

    # Create separate records for each style rule applied to preserve priority order
    # This ensures that later style_tt() calls override earlier ones during consolidation
    rec_with_maps <- data.frame()

    for (row in seq_len(nrow(sty))) {
      # Calculate cell indices (same as before)
      idx_i <- sty$i[row]
      if (is.na(idx_i)) {
        idx_i <- unique(rec$i)
      }
      idx_j <- sty$j[row]
      if (is.na(idx_j)) {
        idx_j <- unique(rec$j)
      }

      # Find affected cells
      affected_cells <- which(rec$i %in% idx_i & rec$j %in% idx_j)

      if (length(affected_cells) > 0 && any(!is.na(unlist(sty[row, ])))) {
        # Create one CSS map for this style row
        style_map_for_row <- character()

        # Apply standard CSS properties for this row
        for (prop_name in names(style_map)) {
          if (prop_name %in% names(sty)) {
            prop_value <- sty[row, prop_name]
            style_def <- style_map[[prop_name]]
            if (style_def$condition_fn(prop_value)) {
              css_value <- if (!is.null(style_def$value_fn)) {
                style_def$value_fn(prop_value)
              } else {
                style_def$css_value
              }
              style_map_for_row[[style_def$css_prop]] <- css_value
            }
          }
        }

        # Handle custom CSS for this row
        if (!is.na(sty[row, "html_css"])) {
          custom_css <- css_parse(sty[row, "html_css"])
          for (prop_name in names(custom_css)) {
            style_map_for_row[[prop_name]] <- custom_css[[prop_name]]
          }
        }

        # Handle border properties for this row
        line <- sty$line[row]
        if (!is.na(line)) {
          line_width <- sty$line_width[row]
          line_color <- sty$line_color[row]
          line_trim <- if ("line_trim" %in% names(sty)) sty$line_trim[row] else NA

          # Create border CSS map
          border_css <- create_border_css_map(line, line_width, line_color, line_trim)
          for (prop_name in names(border_css)) {
            style_map_for_row[[prop_name]] <- border_css[[prop_name]]
          }
        }

        # Create records for each affected cell
        if (length(style_map_for_row) > 0) {
          for (cell_idx in affected_cells) {
            new_record <- data.frame(
              i = rec$i[cell_idx],
              j = rec$j[cell_idx],
              priority = row,  # Use row number as priority
              stringsAsFactors = FALSE
            )
            new_record$css_map <- list(style_map_for_row)
            rec_with_maps <- rbind(rec_with_maps, new_record)
          }
        }
      }
    }

    # Add comprehensive rowspan/colspan border transfer logic
    # This transfers borders from cells that will be removed by spans to the spanning cells
    for (row in seq_len(nrow(sty))) {
      rowspan <- if (!is.na(sty$rowspan[row])) sty$rowspan[row] else 1
      colspan <- if (!is.na(sty$colspan[row])) sty$colspan[row] else 1

      if (rowspan > 1 || colspan > 1) {
        span_i <- sty$i[row]
        span_j <- sty$j[row]

        # Find all cells that will be removed by this span
        removed_cells <- expand.grid(
          i = span_i:(span_i + rowspan - 1),
          j = span_j:(span_j + colspan - 1)
        )
        # Remove the spanning cell itself from the list
        removed_cells <- removed_cells[!(removed_cells$i == span_i & removed_cells$j == span_j), ]

        # Check if any removed cells had borders that need to be transferred
        if (nrow(removed_cells) > 0) {
          # Create border transfer for the spanning cell
          # For now, focus on bottom borders for cells in the last row of the span
          max_table_row <- max(rec$i)
          bottom_row_of_span <- span_i + rowspan - 1

          if (bottom_row_of_span >= max_table_row) {
            # This span reaches the bottom of the table, so transfer bottom border
            border_record <- data.frame(
              i = span_i,
              j = span_j,
              priority = if (nrow(rec_with_maps) > 0) max(rec_with_maps$priority, na.rm = TRUE) + 1 else 1000,
              stringsAsFactors = FALSE
            )
            border_css_map <- c("--border-bottom" = "1")
            border_record$css_map <- list(border_css_map)
            rec_with_maps <- rbind(rec_with_maps, border_record)
          }
        }
      }
    }

    if (nrow(rec_with_maps) == 0) {
      return(x)
    }

    # Consolidate CSS maps for cells with multiple declarations
    rec_with_maps <- consolidate_css_maps(rec_with_maps)

    # Process CSS maps directly - render only once and avoid parsing
    css_map_to_string <- function(css_map) {
      if (length(css_map) == 0) return("")
      css_render(css_map)
    }

    # Create unique CSS map combinations and assign IDs
    css_strings <- sapply(rec_with_maps$css_map, css_map_to_string)
    css_unique <- unique(css_strings)
    css_unique <- css_unique[nzchar(css_unique)]

    if (length(css_unique) == 0) {
      return(x)
    }

    # Create CSS table with unique rules
    css_table <- data.frame(
      css_rule = css_unique,
      id_css = sapply(seq_along(css_unique), function(i) get_id(stem = "tinytable_css_")),
      stringsAsFactors = FALSE
    )

    # Map each cell to its CSS ID
    rec_with_ids <- rec_with_maps
    rec_with_ids$css_rule <- css_strings
    rec_with_ids <- merge(rec_with_ids, css_table, by = "css_rule", all.x = TRUE, sort = FALSE)

    # Group by CSS ID for output generation
    idx <- split(rec_with_ids, rec_with_ids$id_css)

    for (group in idx) {
      id_css <- group$id_css[1]
      css_rule <- group$css_rule[1]

      if (!nzchar(css_rule)) next

      # Generate position arrays
      arr <- sprintf("{ i: '%s', j: %s }, ", group$i, group$j)
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

      # Check if this CSS rule has any borders - work with map directly
      css_map <- group$css_map[[1]]  # All maps in group should be identical
      has_border <- any(names(css_map) %in% c("--border-left", "--border-right", "--border-top", "--border-bottom")) ||
                   any(startsWith(names(css_map), "border"))

      if (has_border) {
        # Work directly with CSS map for pseudo-elements
        # Convert border properties to pseudo-element format
        pseudo_map <- convert_map_to_pseudo_elements(css_map)
        css_rule <- css_render(pseudo_map)

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


    return(x)
  }
)


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

#' Centralized CSS configuration object
#' @keywords internal
#' @noRd
css_config <- list(
  # Default CSS variables for pseudo-elements
  default_vars = c(
    "--border-left" = "0", "--border-right" = "0", "--border-top" = "0", "--border-bottom" = "0",
    "--line-width-left" = "0.1em", "--line-width-right" = "0.1em", "--line-width-top" = "0.1em",
    "--line-width-bottom" = "0.1em", "--trim-top-left" = "0%", "--trim-top-right" = "0%",
    "--trim-bottom-left" = "0%", "--trim-bottom-right" = "0%", "--trim-left-top" = "0%",
    "--trim-left-bottom" = "0%", "--trim-right-top" = "0%", "--trim-right-bottom" = "0%"
  ),

  # CSS property flags for consolidation
  consolidation_flags = list(
    border = c("--border-left", "--border-right", "--border-top", "--border-bottom"),
    color = c("--line-color-left", "--line-color-right", "--line-color-top", "--line-color-bottom"),
    trim = c("--trim-top-left", "--trim-top-right", "--trim-bottom-left", "--trim-bottom-right",
             "--trim-left-top", "--trim-left-bottom", "--trim-right-top", "--trim-right-bottom")
  ),

  # Border property mappings for different directions
  border_props = list(
    left = list(border = "--border-left", color = "--line-color-left", width = "--line-width-left"),
    right = list(border = "--border-right", color = "--line-color-right", width = "--line-width-right"),
    top = list(border = "--border-top", color = "--line-color-top", width = "--line-width-top"),
    bottom = list(border = "--border-bottom", color = "--line-color-bottom", width = "--line-width-bottom")
  ),

  # Trim property mappings for different directions and positions
  trim_props = list(
    top = list(left = "--trim-top-left", right = "--trim-top-right"),
    bottom = list(left = "--trim-bottom-left", right = "--trim-bottom-right"),
    left = list(top = "--trim-left-top", bottom = "--trim-left-bottom"),
    right = list(top = "--trim-right-top", bottom = "--trim-right-bottom")
  ),

  # CSS properties that should be excluded from regular CSS output when using pseudo-elements
  border_exclusions = c(
    "border", "border-left", "border-right", "border-top", "border-bottom",
    "--border-left", "--border-right", "--border-top", "--border-bottom",
    "--line-color", "--line-width", "--line-width-left", "--line-width-right",
    "--line-width-top", "--line-width-bottom", "--trim-top-left", "--trim-top-right",
    "--trim-bottom-left", "--trim-bottom-right", "--trim-left-top", "--trim-left-bottom",
    "--trim-right-top", "--trim-right-bottom"
  ),

  # Default values
  defaults = list(
    line_color = "black",
    line_width = "0.1em",
    trim_percentage = "3%"
  ),

  # CSS template components
  templates = list(
    base_selector = "      .tinytable td.{id}, .tinytable th.{id} { {rule} }",

    before_pseudo = paste0(
      "      .tinytable td.{id}::before, .tinytable th.{id}::before { ",
      "content: ''; position: absolute; ",
      "top: var(--trim-top-left, var(--trim-top-right, 0)); ",
      "left: var(--trim-left-top, 0); right: var(--trim-right-top, 0); bottom: 0; ",
      "pointer-events: none; z-index: 1; ",
      "border-left: calc(var(--border-left) * var(--line-width-left, var(--line-width, 0.1em))) solid var(--line-color-left, var(--line-color, black)); ",
      "border-right: calc(var(--border-right) * var(--line-width-right, var(--line-width, 0.1em))) solid var(--line-color-right, var(--line-color, black)); ",
      "border-top: calc(var(--border-top) * var(--line-width-top, var(--line-width, 0.1em))) solid var(--line-color-top, var(--line-color, black)); }"
    ),

    after_pseudo = paste0(
      "      .tinytable td.{id}::after, .tinytable th.{id}::after { ",
      "content: ''; position: absolute; ",
      "left: var(--trim-bottom-left, 0); right: var(--trim-bottom-right, 0); ",
      "bottom: var(--trim-left-bottom, var(--trim-right-bottom, 0)); ",
      "height: calc(var(--border-bottom) * var(--line-width-bottom, var(--line-width, 0.1em))); ",
      "background: var(--line-color-bottom, var(--line-color, black)); ",
      "pointer-events: none; z-index: 2; }"
    )
  )
)

#' Simple template replacement function
#' @keywords internal
#' @noRd
apply_template <- function(template, replacements) {
  result <- template
  for (key in names(replacements)) {
    pattern <- paste0("\\{", key, "\\}")
    result <- gsub(pattern, replacements[[key]], result)
  }
  result
}

#' Generate pseudo-element CSS using templates
#' @keywords internal
#' @noRd
generate_pseudo_css <- function(id, base_rule) {
  replacements <- list(id = id, rule = base_rule)

  base <- apply_template(css_config$templates$base_selector, replacements)
  before <- apply_template(css_config$templates$before_pseudo, replacements)
  after <- apply_template(css_config$templates$after_pseudo, replacements)

  paste(base, before, after, sep = "\n")
}

#' Set border CSS properties using configuration
#' @keywords internal
#' @noRd
set_border_css <- function(css_map, direction, line_color, line_width) {
  props <- css_config$border_props[[direction]]
  if (!is.null(props)) {
    css_map[[props$border]] <- "1"
    css_map[[props$color]] <- line_color
    css_map[[props$width]] <- line_width
  }
  css_map
}

#' Set trim CSS properties using configuration
#' @keywords internal
#' @noRd
set_trim_css <- function(css_map, direction, position, line_trim, active_border, trim_pct = css_config$defaults$trim_percentage) {
  if (!is.na(line_trim) && grepl(substr(position, 1, 1), line_trim) && active_border) {
    trim_prop <- css_config$trim_props[[direction]][[position]]
    if (!is.null(trim_prop)) {
      css_map[[trim_prop]] <- trim_pct
    }
  }
  css_map
}

#' Generate border CSS map for all active directions
#' @keywords internal
#' @noRd
generate_border_css <- function(line, line_width, line_color, line_trim) {
  line_color <- if (is.na(line_color)) css_config$defaults$line_color else standardize_colors(line_color, format = "hex")
  line_width <- if (is.na(line_width)) css_config$defaults$line_width else paste0(line_width, "em")

  # Determine active directions
  directions <- list(
    left = grepl("l", line),
    right = grepl("r", line),
    top = grepl("t", line),
    bottom = grepl("b", line)
  )

  css_map <- character()

  if (any(unlist(directions))) {
    css_map[["position"]] <- "relative"
    css_map[["--line-width"]] <- line_width

    # Set border properties for active directions
    for (dir_name in names(directions)) {
      if (directions[[dir_name]]) {
        css_map <- set_border_css(css_map, dir_name, line_color, line_width)

        # Set trim properties for this direction
        if (!is.na(line_trim)) {
          trim_pct <- css_config$defaults$trim_percentage
          if (dir_name %in% c("top", "bottom")) {
            css_map <- set_trim_css(css_map, dir_name, "left", line_trim, directions$left, trim_pct)
            css_map <- set_trim_css(css_map, dir_name, "right", line_trim, directions$right, trim_pct)
          } else {
            css_map <- set_trim_css(css_map, dir_name, "top", line_trim, directions$top, trim_pct)
            css_map <- set_trim_css(css_map, dir_name, "bottom", line_trim, directions$bottom, trim_pct)
          }
        }
      }
    }
  }

  css_map
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
  vars <- css_config$default_vars

  # Default values
  line_color <- css_config$defaults$line_color
  line_width <- css_config$defaults$line_width

  # Process border declarations from map
  for (key in names(css_map)) {
    val <- css_map[[key]]

    if (key == "border" && grepl("solid", val)) {
      # All borders - use config to set all directions
      for (dir_props in css_config$border_props) {
        vars[[dir_props$border]] <- "1"
      }
      border_parts <- border_parse(val)
      if (!is.na(border_parts$color)) line_color <- border_parts$color
      if (!is.na(border_parts$width)) line_width <- border_parts$width
    } else if (key %in% c("border-left", "border-right", "border-top", "border-bottom") && grepl("solid", val)) {
      # Individual sides - use config to map border names to CSS variables
      direction <- gsub("border-", "", key)
      if (direction %in% names(css_config$border_props)) {
        vars[[css_config$border_props[[direction]]$border]] <- "1"
      }
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

  # Remove all border-related keys from regular CSS using config
  non_border <- css_map[!names(css_map) %in% css_config$border_exclusions]

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


    x@style$alignv[which(x@style$alignv == "t")] <- "top"
    x@style$alignv[which(x@style$alignv == "b")] <- "bottom"
    x@style$alignv[which(x@style$alignv == "m")] <- "middle"
    x@style$align[which(x@style$align == "l")] <- "left"
    x@style$align[which(x@style$align == "c")] <- "center"
    x@style$align[which(x@style$align == "d")] <- "center"
    x@style$align[which(x@style$align == "r")] <- "right"

    sty <- expand_styles(x)

    # sty$lines is a data frame with i, j, line, line_width, and line_color.
    # sty$other contains all other style properties, already reconciled and de-duplicated.

    # Precompute and cache column mappings
    mapping_cache <- header_col_mapping_cache(x)

    # Adjust column indices in both style data frames
    if (!is.null(sty$lines) && nrow(sty$lines) > 0) {
      sty$lines$j <- html_adjust_column_indices(sty$lines$j, sty$lines$i, mapping_cache)
      sty$lines$j <- sty$lines$j - 1
    }

    if (!is.null(sty$other) && nrow(sty$other) > 0) {
      sty$other$j <- html_adjust_column_indices(sty$other$j, sty$other$i, mapping_cache)
      sty$other$j <- sty$other$j - 1
    }

    # Handle rowspan/colspan spans first
    if (!is.null(sty$other) && nrow(sty$other) > 0 && any(c("rowspan", "colspan") %in% names(sty$other))) {
      for (row in seq_len(nrow(sty$other))) {
        rowspan <- if ("rowspan" %in% names(sty$other) && !is.na(sty$other$rowspan[row])) sty$other$rowspan[row] else 1
        colspan <- if ("colspan" %in% names(sty$other) && !is.na(sty$other$colspan[row])) sty$other$colspan[row] else 1
        if (rowspan > 1 || colspan > 1) {
          id <- get_id(stem = "spanCell_")
          listener <- "      window.addEventListener('load', function () { %s(%s, %s, %s, %s) })"
          listener <- sprintf(
            listener,
            id,
            sty$other$i[row],
            sty$other$j[row],
            rowspan,
            colspan
          )
          x@table_string <- lines_insert(
            x@table_string,
            listener,
            "tinytable span after",
            "after"
          )
        }
      }
    }

    # Combine lines and other styles into final CSS entries
    css_entries <- list()

    # Process line styles - these become pseudo-element CSS
    if (!is.null(sty$lines) && nrow(sty$lines) > 0) {
      for (row in seq_len(nrow(sty$lines))) {
        line_data <- sty$lines[row, ]

        # Create border CSS using the utility function
        border_css <- generate_border_css(
          line_data$line,
          line_data$line_width,
          line_data$line_color,
          if ("line_trim" %in% names(line_data)) line_data$line_trim else NA
        )

        # Convert to pseudo-element format
        pseudo_map <- convert_map_to_pseudo_elements(border_css)
        css_rule <- css_render(pseudo_map)

        cell_key <- paste(line_data$i, line_data$j, sep = "_")
        if (is.null(css_entries[[cell_key]])) {
          css_entries[[cell_key]] <- list(
            i = line_data$i,
            j = line_data$j,
            css_map = pseudo_map,
            has_border = TRUE
          )
        } else {
          # Merge with existing entry - only overwrite border properties that are active (value "1")
          for (prop in names(pseudo_map)) {
            # For border flags, only overwrite if the new value is "1" (active)
            if (prop %in% c("--border-left", "--border-right", "--border-top", "--border-bottom")) {
              if (pseudo_map[[prop]] == "1") {
                css_entries[[cell_key]]$css_map[[prop]] <- pseudo_map[[prop]]
              }
            } else {
              # For non-border properties, always overwrite (colors, widths, etc.)
              css_entries[[cell_key]]$css_map[[prop]] <- pseudo_map[[prop]]
            }
          }
          css_entries[[cell_key]]$has_border <- TRUE
        }
      }
    }

    # Process other styles - these become regular CSS
    if (!is.null(sty$other) && nrow(sty$other) > 0) {
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

      for (row in seq_len(nrow(sty$other))) {
        other_data <- sty$other[row, ]
        style_css <- character()

        # Apply standard CSS properties
        for (prop_name in names(style_map)) {
          if (prop_name %in% names(other_data)) {
            prop_value <- other_data[[prop_name]]
            style_def <- style_map[[prop_name]]
            if (style_def$condition_fn(prop_value)) {
              css_value <- if (!is.null(style_def$value_fn)) {
                style_def$value_fn(prop_value)
              } else {
                style_def$css_value
              }
              style_css[[style_def$css_prop]] <- css_value
            }
          }
        }

        # Handle custom CSS
        if ("html_css" %in% names(other_data) && !is.na(other_data$html_css)) {
          custom_css <- css_parse(other_data$html_css)
          for (prop_name in names(custom_css)) {
            style_css[[prop_name]] <- custom_css[[prop_name]]
          }
        }

        if (length(style_css) > 0) {
          cell_key <- paste(other_data$i, other_data$j, sep = "_")
          if (is.null(css_entries[[cell_key]])) {
            css_entries[[cell_key]] <- list(
              i = other_data$i,
              j = other_data$j,
              css_map = style_css,
              has_border = FALSE
            )
          } else {
            # Merge with existing entry
            for (prop in names(style_css)) {
              css_entries[[cell_key]]$css_map[[prop]] <- style_css[[prop]]
            }
          }
        }
      }
    }

    if (length(css_entries) == 0) {
      return(x)
    }

    # Generate CSS rules directly from css_entries
    for (cell_key in names(css_entries)) {
      entry_data <- css_entries[[cell_key]]
      css_map <- entry_data$css_map
      has_border <- entry_data$has_border

      # Render CSS rule
      css_rule <- css_render(css_map)
      if (!nzchar(css_rule)) next

      # Generate unique ID for this CSS rule
      id_css <- get_id(stem = "tinytable_css_")

      # Generate position array for this single cell
      arr <- sprintf("{ i: '%s', j: %s }, ", entry_data$i, entry_data$j)
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

      # Generate CSS entry
      if (has_border) {
        # Generate CSS using template for pseudo-elements
        entry <- generate_pseudo_css(id_css, css_rule)
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

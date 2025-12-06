
# =============================================================================
# CSS Utility Layer
# =============================================================================

#' Get default line color for HTML output
#'
#' Returns CSS variable reference for HTML tinytable engine,
#' otherwise returns "black". When using custom CSS with the tinytable
#' engine, users should define --tt-line-color in their CSS.
#'
#' @param x A tinytable object
#' @return Character string: "var(--tt-line-color)" or "black"
#' @keywords internal
#' @noRd
get_default_line_color <- function(x) {
  use_css_var <- identical(x@output, "html") &&
                 identical(x@html_engine, "tinytable")
  if (use_css_var) "var(--tt-line-color)" else "black"
}

style_to_css <- function(row) {
  out <- character()

  if (!is.na(row$align)) {
    row$align <- switch(
      row$align,
      l = "left",
      c = "center",
      d = "center",
      r = "right",
      row$align
    )
  }
  if (!is.na(row$alignv)) {
    row$alignv <- switch(
      row$alignv,
      t = "top",
      b = "bottom",
      m = "middle",
      row$alignv
    )
  }

  # simple properties
  if (isTRUE(row$bold))      out <- c(out, "font-weight: bold")
  if (isTRUE(row$italic))    out <- c(out, "font-style: italic")
  if (isTRUE(row$monospace)) out <- c(out, "font-family: monospace")
  if (isTRUE(row$smallcap))  out <- c(out, "font-variant: small-caps")
  if (!is.na(row$align))     {
    out <- c(out, paste0("text-align: ", row$align))
  }
  if (!is.na(row$alignv))    out <- c(out, paste0("vertical-align: ", row$alignv))
  if (!is.na(row$fontsize))  out <- c(out, paste0("font-size: ", row$fontsize, "em"))
  if (!is.na(row$indent))    out <- c(out, paste0("padding-left: ", row$indent, "em"))
  if (!is.na(row$color)) {
    out <- c(out, paste0("color: ", standardize_colors(row$color, "hex")))
  }
  if (!is.na(row$background)) {
    out <- c(out, paste0("background-color: ", standardize_colors(row$background, "hex")))
  }
  if (!is.na(row$html_css)) {
    out <- c(out, html_css = row$html_css)
  }

  # text decorations can be multiple
  td <- c()
  if (isTRUE(row$underline)) td <- c(td, "underline")
  if (isTRUE(row$strikeout)) td <- c(td, "line-through")
  if (length(td)) out <- c(out, paste0("text-decoration: ", paste(td, collapse = " ")))

  # combine
  out <- paste(out, collapse = "; ")
  return(out)
}


line_to_css <- function(
  border_top = 0,
  border_right = 0,
  border_bottom = 0,
  border_left = 0,
  color_top = "black",
  color_right = "black",
  color_bottom = "black",
  color_left = "black",
  width_top = .1,
  width_right = .1,
  width_bottom = .1,
  width_left = .1,
  trim_top_left = 0,
  trim_top_right = 0,
  trim_bottom_left = 0,
  trim_bottom_right = 0,
  trim_left_top = 0,
  trim_left_bottom = 0,
  trim_right_top = 0,
  trim_right_bottom = 0
) {
  out <- sprintf('
    position: relative;
    --border-bottom: %s;
    --border-left: %s;
    --border-right: %s;
    --border-top: %s;
    --line-color-bottom: %s;
    --line-color-left: %s;
    --line-color-right: %s;
    --line-color-top: %s;
    --line-width-bottom: %sem;
    --line-width-left: %sem;
    --line-width-right: %sem;
    --line-width-top: %sem;
    --trim-bottom-left: %s%%;
    --trim-bottom-right: %s%%;
    --trim-left-bottom: %s%%;
    --trim-left-top: %s%%;
    --trim-right-bottom: %s%%;
    --trim-right-top: %s%%;
    --trim-top-left: %s%%;
    --trim-top-right: %s%%;
    ',
    border_bottom, border_left, border_right, border_top,
    color_bottom, color_left, color_right, color_top,
    width_bottom, width_left, width_right, width_top,
    trim_bottom_left, trim_bottom_right, trim_left_bottom, trim_left_top,
    trim_right_bottom, trim_right_top, trim_top_left, trim_top_right
  )
  gsub("\\s+", " ", out)
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
    line_color = NULL,
    line_width = 0.1,
    colspan = NULL,
    rowspan = NULL,
    indent = 0,
    ...
  ) {
    # Set default line color based on context
    if (is.null(line_color)) {
      line_color <- get_default_line_color(x)
    }
    # CSS rule will be handled by finalize() via template substitution
    # Removed duplicate html_setting call that was causing CSS duplication

    # Use populated @style_other from build_tt()
    other <- x@style_other

    # Filter to only cells that have actual styles (at least one non-NA value)
    if (nrow(other) > 0) {
      has_style <- rowSums(!is.na(other[, c("bold", "italic", "underline", "strikeout",
                                             "monospace", "smallcap", "align", "alignv",
                                             "color", "background", "fontsize", "indent",
                                             "html_css", "colspan", "rowspan"), drop = FALSE])) > 0
      other <- other[has_style, , drop = FALSE]
    }

    # Use populated @style_lines from build_tt()
    lines <- x@style_lines
    if (nrow(lines) == 0) {
      lines <- NULL
    }


    # rowspan/colspan spans first
    if (!is.null(other) && nrow(other) > 0 && any(c("rowspan", "colspan") %in% names(other))) {
      for (row in seq_len(nrow(other))) {
        rowspan <- if ("rowspan" %in% names(other) && !is.na(other$rowspan[row])) other$rowspan[row] else 1
        colspan <- if ("colspan" %in% names(other) && !is.na(other$colspan[row])) other$colspan[row] else 1
        # Skip JavaScript spans for column group headers (negative i) since HTML already handles them via colspan
        if ((rowspan > 1 || colspan > 1) && other$i[row] >= 0) {
          # Use the factory function approach instead of individual function names
          listener <- "      window.addEventListener('load', function () { tableFns_%s.spanCell(%s, %s, %s, %s) })"
          listener <- sprintf(
            listener,
            x@id,
            other$i[row],
            other$j[row],
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


    # Collect all styles per cell first, then consolidate
    cell_styles <- list()

    # Process line styles - vectorized approach
    if (!is.null(lines) && nrow(lines) > 0) {
      # Use interaction() for compact cell keys
      lines$cell_key <- interaction(lines$i, lines$j, drop = TRUE)

      # Precompute direction booleans once (vectorized regex)
      has_t <- grepl("t", lines$line)
      has_r <- grepl("r", lines$line)
      has_b <- grepl("b", lines$line)
      has_l <- grepl("l", lines$line)

      # Precompute trim booleans once
      lines$line_trim <- ifelse(is.na(lines$line_trim), "", lines$line_trim)
      trim_l <- grepl("l", lines$line_trim)
      trim_r <- grepl("r", lines$line_trim)

      # Normalize colors once: build a map for unique line_color -> hex
      unique_colors <- unique(lines$line_color[!is.na(lines$line_color)])
      if (length(unique_colors) > 0) {
        color_map <- stats::setNames(
          sapply(unique_colors, standardize_colors, format = "hex", USE.NAMES = FALSE),
          unique_colors
        )
      } else {
        color_map <- character(0)
      }

      # Map colors to hex (preserve CSS variables, fallback to "black")
      lines$color_hex <- ifelse(
        !is.na(lines$line_color) & lines$line_color %in% names(color_map),
        color_map[lines$line_color],
        ifelse(
          !is.na(lines$line_color) & grepl("^var\\(", lines$line_color),
          lines$line_color,  # preserve CSS variable references
          "black"
        )
      )

      # Default widths
      lines$line_width <- ifelse(is.na(lines$line_width), 0.1, lines$line_width)

      # Aggregate per direction using tapply per cell_key
      cells_unique <- levels(lines$cell_key)

      # Helper to aggregate border flags (any TRUE)
      agg_any <- function(x, cell_key, has_dir) {
        tapply(has_dir, cell_key, any, default = FALSE)[cells_unique]
      }

      # Helper to aggregate widths (max)
      agg_max_width <- function(x, cell_key, has_dir, width) {
        tapply(
          ifelse(has_dir, width, NA),
          cell_key,
          function(v) if (all(is.na(v))) 0.1 else max(v, na.rm = TRUE),
          default = 0.1
        )[cells_unique]
      }

      # Helper to aggregate colors (first non-default)
      agg_first_color <- function(x_obj, cell_key, has_dir, color) {
        default_color <- get_default_line_color(x)
        tapply(
          ifelse(has_dir, color, NA_character_),
          cell_key,
          function(v) {
            v <- v[!is.na(v)]
            if (length(v) > 0) v[1] else default_color
          },
          default = default_color
        )[cells_unique]
      }

      # Aggregate border flags
      border_top <- agg_any(lines, lines$cell_key, has_t)
      border_right <- agg_any(lines, lines$cell_key, has_r)
      border_bottom <- agg_any(lines, lines$cell_key, has_b)
      border_left <- agg_any(lines, lines$cell_key, has_l)

      # Aggregate widths
      width_top <- agg_max_width(lines, lines$cell_key, has_t, lines$line_width)
      width_right <- agg_max_width(lines, lines$cell_key, has_r, lines$line_width)
      width_bottom <- agg_max_width(lines, lines$cell_key, has_b, lines$line_width)
      width_left <- agg_max_width(lines, lines$cell_key, has_l, lines$line_width)

      # Aggregate colors
      color_top <- agg_first_color(lines, lines$cell_key, has_t, lines$color_hex)
      color_right <- agg_first_color(lines, lines$cell_key, has_r, lines$color_hex)
      color_bottom <- agg_first_color(lines, lines$cell_key, has_b, lines$color_hex)
      color_left <- agg_first_color(lines, lines$cell_key, has_l, lines$color_hex)

      # Aggregate trim flags (any TRUE, multiply by 3 at end)
      trim_top_left <- tapply(trim_l & has_t, lines$cell_key, any, default = FALSE)[cells_unique] * 3
      trim_top_right <- tapply(trim_r & has_t, lines$cell_key, any, default = FALSE)[cells_unique] * 3
      trim_bottom_left <- tapply(trim_l & has_b, lines$cell_key, any, default = FALSE)[cells_unique] * 3
      trim_bottom_right <- tapply(trim_r & has_b, lines$cell_key, any, default = FALSE)[cells_unique] * 3
      trim_left_top <- tapply(trim_l & has_l, lines$cell_key, any, default = FALSE)[cells_unique] * 3
      trim_left_bottom <- tapply(trim_l & has_l, lines$cell_key, any, default = FALSE)[cells_unique] * 3
      trim_right_top <- tapply(trim_r & has_r, lines$cell_key, any, default = FALSE)[cells_unique] * 3
      trim_right_bottom <- tapply(trim_r & has_r, lines$cell_key, any, default = FALSE)[cells_unique] * 3

      # Get first (i,j) per cell_key
      first_idx <- match(cells_unique, lines$cell_key)
      cell_i <- lines$i[first_idx]
      cell_j <- lines$j[first_idx]

      # Filter to cells with at least one border
      has_border_mask <- border_top | border_right | border_bottom | border_left

      if (any(has_border_mask)) {
        # Build consolidated data.frame
        line_params <- data.frame(
          i = cell_i[has_border_mask],
          j = cell_j[has_border_mask],
          border_top = as.integer(border_top[has_border_mask]),
          border_right = as.integer(border_right[has_border_mask]),
          border_bottom = as.integer(border_bottom[has_border_mask]),
          border_left = as.integer(border_left[has_border_mask]),
          color_top = color_top[has_border_mask],
          color_right = color_right[has_border_mask],
          color_bottom = color_bottom[has_border_mask],
          color_left = color_left[has_border_mask],
          width_top = width_top[has_border_mask],
          width_right = width_right[has_border_mask],
          width_bottom = width_bottom[has_border_mask],
          width_left = width_left[has_border_mask],
          trim_top_left = trim_top_left[has_border_mask],
          trim_top_right = trim_top_right[has_border_mask],
          trim_bottom_left = trim_bottom_left[has_border_mask],
          trim_bottom_right = trim_bottom_right[has_border_mask],
          trim_left_top = trim_left_top[has_border_mask],
          trim_left_bottom = trim_left_bottom[has_border_mask],
          trim_right_top = trim_right_top[has_border_mask],
          trim_right_bottom = trim_right_bottom[has_border_mask],
          stringsAsFactors = FALSE
        )

        # Call line_to_css() once per cell via mapply (vectorized)
        css_results <- mapply(
          line_to_css,
          border_top = line_params$border_top,
          border_right = line_params$border_right,
          border_bottom = line_params$border_bottom,
          border_left = line_params$border_left,
          color_top = line_params$color_top,
          color_right = line_params$color_right,
          color_bottom = line_params$color_bottom,
          color_left = line_params$color_left,
          width_top = line_params$width_top,
          width_right = line_params$width_right,
          width_bottom = line_params$width_bottom,
          width_left = line_params$width_left,
          trim_top_left = line_params$trim_top_left,
          trim_top_right = line_params$trim_top_right,
          trim_bottom_left = line_params$trim_bottom_left,
          trim_bottom_right = line_params$trim_bottom_right,
          trim_left_top = line_params$trim_left_top,
          trim_left_bottom = line_params$trim_left_bottom,
          trim_right_top = line_params$trim_right_top,
          trim_right_bottom = line_params$trim_right_bottom,
          SIMPLIFY = TRUE,
          USE.NAMES = FALSE
        )

        # Filter non-empty CSS and build cell_styles list
        has_css <- nzchar(css_results)
        if (any(has_css)) {
          for (idx in which(has_css)) {
            cell_key <- paste(line_params$i[idx], line_params$j[idx], sep = "_")
            cell_styles[[cell_key]] <- list(
              i = line_params$i[idx],
              j = line_params$j[idx],
              css = "",
              css_before = "",
              css_after = "",
              has_border = TRUE,
              consolidated_line_css = css_results[idx]
            )
          }
        }
      }
    }

    # Process other styles - vectorized
    if (!is.null(other) && nrow(other) > 0) {
      # Vectorize style_to_css over all rows
      css_rules <- character(nrow(other))
      for (row in seq_len(nrow(other))) {
        css_rules[row] <- style_to_css(other[row, , drop = FALSE])
      }

      # Filter to non-empty CSS
      has_css <- nzchar(css_rules)
      if (any(has_css)) {
        other_with_css <- other[has_css, , drop = FALSE]
        css_rules <- css_rules[has_css]

        # Keep only last non-empty CSS per (i,j) by reversing, deduplicating, then reversing again
        cell_keys <- paste(other_with_css$i, other_with_css$j, sep = "_")
        # Reverse order to keep last occurrence
        rev_idx <- seq(nrow(other_with_css), 1)
        unique_idx <- !duplicated(cell_keys[rev_idx])
        # Get back to original order
        final_idx <- rev(rev_idx[unique_idx])

        other_final <- other_with_css[final_idx, , drop = FALSE]
        css_final <- css_rules[final_idx]
        cell_keys_final <- cell_keys[final_idx]

        # Build cell_styles entries
        for (idx in seq_along(css_final)) {
          cell_key <- cell_keys_final[idx]

          if (is.null(cell_styles[[cell_key]])) {
            cell_styles[[cell_key]] <- list(
              i = other_final$i[idx],
              j = other_final$j[idx],
              css = "",
              css_before = "",
              css_after = "",
              has_border = FALSE
            )
          }

          cell_styles[[cell_key]]$css <- css_final[idx]
        }
      }
    }

    # Early exit if no styles
    if (length(cell_styles) == 0) {
      return(x)
    }

    # Convert cell_styles list to data.frame for vectorized operations
    n_cells <- length(cell_styles)
    css_df <- data.frame(
      i = integer(n_cells),
      j = integer(n_cells),
      line_css = character(n_cells),
      other_css = character(n_cells),
      has_border = logical(n_cells),
      stringsAsFactors = FALSE
    )

    idx <- 1
    for (cell_key in names(cell_styles)) {
      cell_data <- cell_styles[[cell_key]]
      css_df$i[idx] <- cell_data$i
      css_df$j[idx] <- cell_data$j
      css_df$line_css[idx] <- if (!is.null(cell_data$consolidated_line_css)) cell_data$consolidated_line_css else ""
      css_df$other_css[idx] <- cell_data$css
      css_df$has_border[idx] <- cell_data$has_border
      idx <- idx + 1
    }

    # Merge line_css and other_css
    has_line <- nzchar(css_df$line_css)
    has_other <- nzchar(css_df$other_css)

    css_df$merged_css <- ifelse(
      has_line & has_other,
      paste(css_df$line_css, css_df$other_css, sep = "; "),
      ifelse(has_line, css_df$line_css, css_df$other_css)
    )

    # Filter to non-empty CSS
    css_df <- css_df[nzchar(css_df$merged_css), , drop = FALSE]

    if (nrow(css_df) == 0) {
      return(x)
    }

    # Group by CSS rule and border status using interaction
    css_df$group_key <- interaction(css_df$merged_css, css_df$has_border, drop = TRUE)

    # For each group, collect cells
    group_levels <- levels(css_df$group_key)
    css_groups <- vector("list", length(group_levels))
    names(css_groups) <- group_levels

    for (g_idx in seq_along(group_levels)) {
      group_mask <- css_df$group_key == group_levels[g_idx]
      group_rows <- css_df[group_mask, , drop = FALSE]

      css_groups[[g_idx]] <- list(
        css_rule = group_rows$merged_css[1],
        has_border = group_rows$has_border[1],
        cells = lapply(seq_len(nrow(group_rows)), function(r) {
          list(i = group_rows$i[r], j = group_rows$j[r])
        }),
        first_j = group_rows$j[1],
        first_i = group_rows$i[1]
      )
    }

    # Generate CSS rules for each unique group
    # Sort groups by the first cell's coordinates to ensure deterministic ordering
    group_keys_sorted <- names(css_groups)
    if (length(group_keys_sorted) > 0) {
      # Use precomputed first_j and first_i for efficient sorting
      sort_keys <- sapply(css_groups, function(g) g$first_j * 10000 + g$first_i, USE.NAMES = FALSE)
      group_keys_sorted <- group_keys_sorted[order(sort_keys)]
    }

    for (group_key in group_keys_sorted) {
      group_data <- css_groups[[group_key]]
      css_rule <- group_data$css_rule
      has_border <- group_data$has_border
      cells <- group_data$cells

      # Generate unique ID for this CSS rule
      id_css <- get_id(stem = "tinytable_css_")

      # Generate position array for all cells with this styling
      valid_cells <- cells

      cell_positions <- sapply(valid_cells, function(cell) {
        sprintf("{ i: '%s', j: %s }", cell$i, cell$j)
      })
      arr <- c(
        "          {",
        " positions: [ ",
        paste(cell_positions, collapse = ", "),
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

      # Generate CSS entry - scoped to table ID to prevent CSS cascade conflicts
      table_id <- paste0("tinytable_", x@id)
      entry <- sprintf(
        "    #%s td.%s, #%s th.%s { %s }",
        table_id, id_css, table_id, id_css, css_rule
      )
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

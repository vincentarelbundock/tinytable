
# =============================================================================
# CSS Utility Layer
# =============================================================================

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
  if (!is.na(row$align))     out <- c(out, paste0("text-align: ", row$align))
  if (!is.na(row$alignv))    out <- c(out, paste0("vertical-align: ", row$alignv))
  if (!is.na(row$fontsize))  out <- c(out, paste0("font-size: ", row$fontsize, "em"))
  if (!is.na(row$indent))    out <- c(out, paste0("padding-left: ", row$indent, "em"))
  if (!is.na(row$color)) {
    out <- c(out, paste0("color: ", standardize_colors(row$color, "hex")))
  }
  if (!is.na(row$background)) {
    out <- c(out, paste0("background-color: ", standardize_colors(row$background, "hex")))
  }
  if (!is.na(row$html_css) && nzchar(row$html_css)) {
    custom <- css_parse(row$html_css)
    if (length(custom)) {
      out <- c(out, paste0(names(custom), ": ", unname(custom)))
    }
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
  gsub("\n", " ", out)
}




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

    sty <- expand_styles(x)
    lines <- sty$lines
    other <- sty$other

    # rowspan/colspan spans first
    if (!is.null(other) && nrow(other) > 0 && any(c("rowspan", "colspan") %in% names(other))) {
      for (row in seq_len(nrow(other))) {
        rowspan <- if ("rowspan" %in% names(other) && !is.na(other$rowspan[row])) other$rowspan[row] else 1
        colspan <- if ("colspan" %in% names(other) && !is.na(other$colspan[row])) other$colspan[row] else 1
        if (rowspan > 1 || colspan > 1) {
          id <- get_id(stem = "spanCell_")
          listener <- "      window.addEventListener('load', function () { %s(%s, %s, %s, %s) })"
          listener <- sprintf(
            listener,
            id,
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

    # Process line styles - consolidate first, then generate CSS once per cell
    if (!is.null(sty$lines) && nrow(sty$lines) > 0) {
      # Group line entries by cell (i, j) to consolidate before CSS generation
      cells_with_lines <- split(sty$lines, paste(sty$lines$i, sty$lines$j, sep = "_"))

      for (cell_key in names(cells_with_lines)) {
        cell_lines <- cells_with_lines[[cell_key]]

        # Consolidate line properties for this cell based on all line entries
        consolidated_params <- list(
          border_top = 0, border_right = 0, border_bottom = 0, border_left = 0,
          color_top = "black", color_right = "black", color_bottom = "black", color_left = "black",
          width_top = 0.1, width_right = 0.1, width_bottom = 0.1, width_left = 0.1,
          trim_top_left = 0, trim_top_right = 0, trim_bottom_left = 0, trim_bottom_right = 0,
          trim_left_top = 0, trim_left_bottom = 0, trim_right_top = 0, trim_right_bottom = 0
        )

        for (row in seq_len(nrow(cell_lines))) {
          line_entry <- cell_lines[row, , drop = FALSE]
          line_val <- line_entry$line
          line_width_val <- if ("line_width" %in% names(line_entry)) line_entry$line_width else 0.1
          line_color_val <- if ("line_color" %in% names(line_entry)) line_entry$line_color else "black"
          line_trim_val <- if ("line_trim" %in% names(line_entry)) line_entry$line_trim else ""

          # Set border flags for directions present in line value
          if (grepl("t", line_val)) {
            consolidated_params$border_top <- 1
            consolidated_params$color_top <- if (!is.na(line_color_val)) standardize_colors(line_color_val, "hex") else "black"
            consolidated_params$width_top <- if (!is.na(line_width_val)) line_width_val else 0.1
          }
          if (grepl("r", line_val)) {
            consolidated_params$border_right <- 1
            consolidated_params$color_right <- if (!is.na(line_color_val)) standardize_colors(line_color_val, "hex") else "black"
            consolidated_params$width_right <- if (!is.na(line_width_val)) line_width_val else 0.1
          }
          if (grepl("b", line_val)) {
            consolidated_params$border_bottom <- 1
            consolidated_params$color_bottom <- if (!is.na(line_color_val)) standardize_colors(line_color_val, "hex") else "black"
            consolidated_params$width_bottom <- if (!is.na(line_width_val)) line_width_val else 0.1
          }
          if (grepl("l", line_val)) {
            consolidated_params$border_left <- 1
            consolidated_params$color_left <- if (!is.na(line_color_val)) standardize_colors(line_color_val, "hex") else "black"
            consolidated_params$width_left <- if (!is.na(line_width_val)) line_width_val else 0.1
          }

          # Handle trimming
          if (!is.na(line_trim_val) && nzchar(line_trim_val)) {
            trim_pct <- 3  # Default trim percentage
            if (grepl("l", line_trim_val)) {
              if (grepl("t", line_val)) consolidated_params$trim_top_left <- trim_pct
              if (grepl("b", line_val)) consolidated_params$trim_bottom_left <- trim_pct
              if (grepl("l", line_val)) {
                consolidated_params$trim_left_top <- trim_pct
                consolidated_params$trim_left_bottom <- trim_pct
              }
            }
            if (grepl("r", line_trim_val)) {
              if (grepl("t", line_val)) consolidated_params$trim_top_right <- trim_pct
              if (grepl("b", line_val)) consolidated_params$trim_bottom_right <- trim_pct
              if (grepl("r", line_val)) {
                consolidated_params$trim_right_top <- trim_pct
                consolidated_params$trim_right_bottom <- trim_pct
              }
            }
          }
        }

        # Now call line_to_css() once with consolidated parameters
        if (any(c(consolidated_params$border_top, consolidated_params$border_right,
                  consolidated_params$border_bottom, consolidated_params$border_left) == 1)) {

          consolidated_css <- do.call(line_to_css, consolidated_params)

          if (nzchar(consolidated_css)) {
            first_entry <- cell_lines[1, , drop = FALSE]
            cell_styles[[cell_key]] <- list(
              i = first_entry$i,
              j = first_entry$j,
              css = "",
              css_before = "",
              css_after = "",
              has_border = TRUE,
              consolidated_line_css = consolidated_css
            )
          }
        }
      }
    }

    # Process other styles - create CSS entries immediately
    if (!is.null(other) && nrow(other) > 0) {
      for (row in seq_len(nrow(other))) {
        css_rule <- style_to_css(other[row, , drop = FALSE])

        if (nzchar(css_rule)) {
          cell_key <- paste(other[row, "i"], other[row, "j"], sep = "_")

          if (is.null(cell_styles[[cell_key]])) {
            cell_styles[[cell_key]] <- list(
              i = other[row, "i"],
              j = other[row, "j"],
              css = "",
              css_before = "",
              css_after = "",
              has_border = FALSE
            )
          }

          cell_styles[[cell_key]]$css <- css_rule
        }
      }
    }

    # Now consolidate styles for each cell
    css_entries <- list()
    for (cell_key in names(cell_styles)) {
      cell_data <- cell_styles[[cell_key]]

      # Process consolidated line CSS if it exists
      css_parts <- c()
      if (!is.null(cell_data$consolidated_line_css) && nzchar(cell_data$consolidated_line_css)) {
        css_parts <- c(css_parts, cell_data$consolidated_line_css)
      }
      if (nzchar(cell_data$css)) {
        css_parts <- c(css_parts, cell_data$css)
      }

      merged_css <- if (length(css_parts) > 0) paste(css_parts, collapse = "; ") else ""

      if (nzchar(merged_css)) {
        css_entries[[cell_key]] <- list(
          i = cell_data$i,
          j = cell_data$j,
          css_rule = merged_css,
          has_border = cell_data$has_border
        )
      }
    }


    if (length(css_entries) == 0) {
      return(x)
    }

    # Group css_entries by their CSS rules to eliminate duplicates
    css_groups <- list()

    for (cell_key in names(css_entries)) {
      entry_data <- css_entries[[cell_key]]
      has_border <- entry_data$has_border

      # Get CSS rule - now both borders and other styles use css_rule
      css_rule <- entry_data$css_rule

      if (!nzchar(css_rule)) next

      # Create a key that includes both the CSS rule and border status
      group_key <- paste(css_rule, has_border, sep = "|")

      if (is.null(css_groups[[group_key]])) {
        css_groups[[group_key]] <- list(
          css_rule = css_rule,
          has_border = has_border,
          cells = list()
        )
      }

      css_groups[[group_key]]$cells <- c(
        css_groups[[group_key]]$cells,
        list(list(i = entry_data$i, j = entry_data$j))
      )
    }

    # Generate CSS rules for each unique group
    for (group_key in names(css_groups)) {
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

      # Generate CSS entry with pseudo-elements for border styling
      if (has_border) {
        # Generate base rule with line_to_css() variables
        base_rule <- sprintf(
          "    .tinytable td.%s, .tinytable th.%s { %s }",
          id_css, id_css, css_rule
        )

        # Generate pseudo-element rules that use the CSS variables
        before_rule <- sprintf(
          "    .tinytable td.%s::before, .tinytable th.%s::before { content: ''; position: absolute; top: var(--trim-top-left, var(--trim-top-right, 0)); left: var(--trim-left-top, 0); right: var(--trim-right-top, 0); bottom: 0; pointer-events: none; z-index: 1; border-left: calc(var(--border-left) * var(--line-width-left, var(--line-width, 0.1em))) solid var(--line-color-left, var(--line-color, black)); border-right: calc(var(--border-right) * var(--line-width-right, var(--line-width, 0.1em))) solid var(--line-color-right, var(--line-color, black)); border-top: calc(var(--border-top) * var(--line-width-top, var(--line-width, 0.1em))) solid var(--line-color-top, var(--line-color, black)); }",
          id_css, id_css
        )

        after_rule <- sprintf(
          "    .tinytable td.%s::after, .tinytable th.%s::after { content: ''; position: absolute; left: var(--trim-bottom-left, 0); right: var(--trim-bottom-right, 0); bottom: var(--trim-left-bottom, var(--trim-right-bottom, 0)); height: calc(var(--border-bottom) * var(--line-width-bottom, var(--line-width, 0.1em))); background: var(--line-color-bottom, var(--line-color, black)); pointer-events: none; z-index: 2; }",
          id_css, id_css
        )

        entry <- paste(base_rule, before_rule, after_rule, sep = "\n")
      } else {
        # Regular CSS rule without pseudo-elements
        entry <- sprintf(
          "    .tinytable td.%s, .tinytable th.%s { %s }",
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

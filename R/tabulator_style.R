#' Internal styling function
#'
#' @inheritParams style_tt
#' @keywords internal
#' @noRd
setMethod(
  f = "style_eval",
  signature = "tinytable_tabulator",
  definition = function(x,
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
                        ...) {

    x <- tabulator_apply_styles(x)
    return(x)
  })


#' Apply styles to Tabulator table
#' @keywords internal
#' @noRd
tabulator_apply_styles <- function(x) {
  if (nrow(x@style) == 0) return(x)

  # Use populated @style_other from build_tt()
  other <- x@style_other

  # Filter to only cells that have actual styles
  if (nrow(other) > 0) {
    has_style <- rowSums(!is.na(other[, c("bold", "italic", "underline", "strikeout",
                                           "monospace", "smallcap", "align", "alignv",
                                           "color", "background", "fontsize", "indent"), drop = FALSE])) > 0
    other <- other[has_style, , drop = FALSE]
  }

  # Precompute field names once (dots/spaces -> underscores)
  field_names <- tabulator_clean_column_name(x@names)

  # First pass: handle column-wide alignment via column definitions
  for (idx in seq_len(nrow(x@style))) {
    orig_style <- x@style[idx, ]
    if (tabulator_is_column_alignment(orig_style)) {
      x <- tabulator_apply_column_alignment(x, orig_style)
    }
  }

  # Group cells by their style properties (before CSS conversion)
  # This avoids calling style_to_css() thousands of times
  style_groups <- list()  # style_signature -> list of cells

  if (!is.null(other) && nrow(other) > 0) {
    # Create a signature for each unique style combination
    style_cols <- c("bold", "italic", "monospace", "underline", "strikeout",
                    "color", "background", "fontsize", "align", "alignv", "indent")

    # Vectorized signature creation: build signatures for all rows at once
    sig_matrix <- sapply(style_cols, function(col) {
      if (col %in% names(other)) {
        vals <- other[[col]]
        ifelse(is.na(vals), "NA", as.character(vals))
      } else {
        rep("NA", nrow(other))
      }
    })

    # Create signatures by pasting columns
    signatures <- apply(sig_matrix, 1, function(row) paste(row, collapse = "|"))

    # Group by signature
    for (row_idx in seq_len(nrow(other))) {
      signature <- signatures[row_idx]
      i <- other$i[row_idx]
      j <- other$j[row_idx]

      if (is.null(style_groups[[signature]])) {
        style_groups[[signature]] <- list(
          css = NULL,  # Will compute once
          row = other[row_idx, , drop = FALSE],
          cells = list()
        )
      }

      style_groups[[signature]]$cells[[length(style_groups[[signature]]$cells) + 1]] <- list(i = i, j = j)
    }

    # Now convert each unique style to CSS once
    for (sig in names(style_groups)) {
      css_content <- style_to_css(style_groups[[sig]]$row)
      style_groups[[sig]]$css <- css_content
    }
  }

  # Build column_styles from style_groups
  column_styles <- list()  # j -> list(cell_key -> css_content)

  for (sig in names(style_groups)) {
    group <- style_groups[[sig]]
    css_content <- group$css

    if (nchar(css_content) > 0) {
      for (cell in group$cells) {
        j_str <- as.character(cell$j)
        cell_key <- paste(cell$i, cell$j, sep = "_")

        if (is.null(column_styles[[j_str]])) {
          column_styles[[j_str]] <- list(cells = list(), css = list())
        }

        column_styles[[j_str]]$cells[[cell_key]] <- cell
        column_styles[[j_str]]$css[[cell_key]] <- css_content
      }
    }
  }

  # Check which columns have uniform styling across all rows
  css_rules <- character()
  targets <- list()

  for (j_str in names(column_styles)) {
    j <- as.integer(j_str)
    col_data <- column_styles[[j_str]]
    col_css_values <- col_data$css

    # Check if all cells in this column have identical CSS
    unique_css <- unique(unlist(col_css_values))

    # Column-wide if: single unique CSS and covers all data rows (nrow) plus header (nhead)
    expected_cells <- nrow(x) + x@nhead

    if (length(unique_css) == 1 && length(col_css_values) == expected_cells) {
      # Column-wide: all rows in this column have same CSS
      field_name <- field_names[j]
      css_rule <- sprintf("$TINYTABLE_ID .tabulator-cell[tabulator-field='%s'] { %s }",
                        field_name, unique_css)
      css_rules <- c(css_rules, css_rule)
    } else {
      # Cell-specific: need JavaScript for this column
      # Group cells by CSS to reuse classes
      css_to_cells <- list()
      for (cell_key in names(col_css_values)) {
        css_content <- col_css_values[[cell_key]]
        if (is.null(css_to_cells[[css_content]])) {
          css_to_cells[[css_content]] <- list()
        }
        css_to_cells[[css_content]][[length(css_to_cells[[css_content]]) + 1]] <- cell_key
      }

      # Generate CSS class and targets for each unique CSS
      for (css_content in names(css_to_cells)) {
        class_name <- get_id(stem = "tinytable_style_")
        css_rule <- sprintf("$TINYTABLE_ID .%s { %s }", class_name, css_content)
        css_rules <- c(css_rules, css_rule)

        # Add targets for all cells with this CSS
        for (cell_key in css_to_cells[[css_content]]) {
          cell_data <- col_data$cells[[cell_key]]
          targets[[length(targets) + 1]] <- list(
            pos = cell_data$i,
            field = field_names[cell_data$j],
            class = class_name
          )
        }
      }
    }
  }

  # Append CSS rules once
  if (length(css_rules) > 0) {
    css_block <- paste(css_rules, collapse = "\n")
    if (nchar(x@tabulator_css_rule) > 0) {
      x@tabulator_css_rule <- paste(x@tabulator_css_rule, css_block, sep = "\n")
    } else {
      x@tabulator_css_rule <- css_block
    }
  }

  # Build cellStyles map keyed by row index (stable across sorts)
  if (length(targets) > 0) {
    # Group targets by row index
    row_styles <- list()
    for (t in targets) {
      row_key <- as.character(t$pos)
      if (is.null(row_styles[[row_key]])) {
        row_styles[[row_key]] <- list()
      }
      row_styles[[row_key]][[t$field]] <- t$class
    }

    # Generate Map entries: [rowIdx, { field: class, ... }]
    map_entries <- sapply(names(row_styles), function(row_key) {
      fields <- row_styles[[row_key]]
      field_entries <- sapply(names(fields), function(field) {
        sprintf("'%s':'%s'", field, fields[[field]])
      })
      sprintf("[%s,{%s}]", row_key, paste(field_entries, collapse = ","))
    })
    map_str <- paste0("[", paste(map_entries, collapse = ","), "]")

    js_block <- sprintf(
      "const cellStyles_tinytable_%s = new Map(%s);",
      x@id,
      map_str
    )

    # Append to post_init
    if (is.null(x@tabulator_post_init) || nchar(x@tabulator_post_init) == 0) {
      x@tabulator_post_init <- js_block
    } else {
      x@tabulator_post_init <- paste(x@tabulator_post_init, js_block, sep = "\n")
    }

    # Add rowFormatter to options
    row_formatter <- sprintf(
      "index: '_tinytable_row_index',\n    rowFormatter: function(row) {\n      const d = row.getData();\n      const rowIdx = d._tinytable_row_index;\n      const spec = cellStyles_tinytable_%s.get(rowIdx);\n      if (!spec) return;\n      for (const [field, cls] of Object.entries(spec)) {\n        const cell = row.getCell(field);\n        if (cell) cell.getElement().classList.add(cls);\n      }\n    }",
      x@id
    )

    # Prepend to tabulator_options
    if (nchar(x@tabulator_options) > 0) {
      x@tabulator_options <- paste0(row_formatter, ",\n    ", x@tabulator_options)
    } else {
      x@tabulator_options <- row_formatter
    }
  }

  return(x)
}


#' Check if style is column-wide alignment only
#' @keywords internal
#' @noRd
tabulator_is_column_alignment <- function(style_row) {
  # Column alignment if: has j, no i (or i is NA), and only align/alignv set
  has_align <- !is.na(style_row$align) || !is.na(style_row$alignv)
  has_j <- length(style_row$j[[1]]) > 0 && !all(is.na(style_row$j[[1]]))
  i_vals <- style_row$i[[1]]
  has_no_i <- length(i_vals) == 0 || all(is.na(i_vals))

  if (!has_align || !has_j) return(FALSE)
  if (!has_no_i) return(FALSE)

  # Check if only align/alignv are set (nothing else)
  other_fields <- c("bold", "italic", "monospace", "underline", "strikeout",
                    "color", "background", "fontsize", "indent")
  has_other_styles <- any(sapply(other_fields, function(f) {
    if (f %in% names(style_row)) !is.na(style_row[[f]]) && style_row[[f]]
    else FALSE
  }))

  return(!has_other_styles)
}


#' Apply column-wide alignment
#' @keywords internal
#' @noRd
tabulator_apply_column_alignment <- function(x, style_row) {
  j_indices <- style_row$j[[1]]

  for (j in j_indices) {
    col_name <- x@names[j]

    # Initialize column styling if needed
    if (is.null(x@tabulator_column_styles)) {
      x@tabulator_column_styles <- list()
    }
    if (is.null(x@tabulator_column_styles[[col_name]])) {
      x@tabulator_column_styles[[col_name]] <- list()
    }

    # Map align values
    if (!is.na(style_row$align)) {
      tabulator_align <- switch(style_row$align,
        "l" = "left",
        "c" = "center",
        "r" = "right",
        style_row$align
      )
      x@tabulator_column_styles[[col_name]]$hozAlign <- tabulator_align
    }

    # Map alignv values
    if (!is.na(style_row$alignv)) {
      tabulator_alignv <- switch(style_row$alignv,
        "t" = "top",
        "m" = "middle",
        "b" = "bottom",
        style_row$alignv
      )
      x@tabulator_column_styles[[col_name]]$vertAlign <- tabulator_alignv
    }
  }

  return(x)
}

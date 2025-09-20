# Helper function to adjust column indices based on column grouping only
bootstrap_adjust_column_indices <- function(x, user_j, user_i) {
  # Only adjust for column groups, not user-defined colspans
  # User-defined colspans are handled by JavaScript after HTML generation
  if (nrow(x@group_data_j) == 0) {
    return(user_j)
  }

  adjusted_j <- user_j

  # Handle each row separately since column groups affect data-col numbering per row
  for (unique_i in unique(user_i)) {
    row_mask <- user_i == unique_i
    if (!any(row_mask)) next

    # Only adjust for header rows affected by column grouping
    if (unique_i < 0) {
      col_mapping <- bootstrap_get_datacol_mapping(x, unique_i)

      # Apply mapping to user indices for this row
      for (idx in which(row_mask)) {
        orig_col <- user_j[idx]
        if (orig_col >= 1 && orig_col <= length(col_mapping)) {
          adjusted_j[idx] <- col_mapping[orig_col]
        }
      }
    }
    # For non-header rows (unique_i >= 0), no adjustment needed
    # User-defined colspans don't change the initial data-col values
  }

  return(adjusted_j)
}

# Get mapping from user column index to HTML data-col value for header rows with column groups
bootstrap_get_datacol_mapping <- function(x, target_i) {
  # Initialize with 1:1 mapping (1-based, will be converted to 0-based later)
  col_mapping <- seq_len(x@ncol)

  # Only handle column groups for header rows
  if (nrow(x@group_data_j) > 0 && target_i < 0) {
    # Group rows are processed in reverse order, so map target_i to correct row
    # target_i = -1 corresponds to the first group (nrow), target_i = -2 to second group (nrow-1), etc.
    group_row_idx <- nrow(x@group_data_j) - (abs(target_i) - 1)
    if (group_row_idx >= 1 && group_row_idx <= nrow(x@group_data_j)) {
      groupj <- as.character(x@group_data_j[group_row_idx, ])
      j_list <- bootstrap_groupj_span(groupj)

      if (length(j_list) > 0) {
        # Recreate the j_combined logic from bootstrap_groupj_html
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
  signature = "tinytable_bootstrap",
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
    if (length(x@bootstrap_css_rule) == 1) {
      x@table_string <- bootstrap_setting(
        x@table_string,
        x@bootstrap_css_rule,
        component = "css"
      )
    }

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
      if (!is.na(sty[row, "bootstrap_css"])) {
        css[idx] <- paste(css[idx], sty[row, "bootstrap_css"])
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
      if (all(c(left, right, top, bottom))) {
        template <- "border: solid %s %sem;"
      } else if (any(c(left, right, top, bottom))) {
        template <- "border: solid %s %sem;"
        if (left) {
          template <- "border-left: solid %s %sem;"
        }
        if (right) {
          template <- "border-right: solid %s %sem;"
        }
        if (top) {
          template <- "border-top: solid %s %sem;"
        }
        if (bottom) template <- "border-bottom: solid %s %sem;"
      } else {
        template <- ""
      }
      if (template != "") {
        lin <- paste(lin, sprintf(template, line_color, line_width))
      }
      css[idx] <- paste(css[idx], lin)
    }

    css <- gsub(" +", " ", trimws(css))

    # User row indices should match HTML data-row values directly
    # HTML generation already handles header positioning
    rec$i <- rec$i

    # Adjust column indices when any kind of colspan is present
    # Both column groups and user-defined colspans change data-col values in HTML
    rec$j <- bootstrap_adjust_column_indices(x, rec$j, rec$i)
    rec$j <- rec$j - 1

    # spans: before styles because we return(x) if there is no style
    for (row in seq_len(nrow(sty))) {
      rowspan <- if (!is.na(sty$rowspan[row])) sty$rowspan[row] else 1
      colspan <- if (!is.na(sty$colspan[row])) sty$colspan[row] else 1
      if (rowspan > 1 || colspan > 1) {
        id <- get_id(stem = "spanCell_")
        listener <- "      window.addEventListener('load', function () { %s(%s, %s, %s, %s) })"
        # For user-defined colspans, don't adjust the j index since they don't change initial data-col values
        # Only column groups in headers need adjustment
        if (sty$i[row] < 0) {
          adjusted_j <- bootstrap_adjust_column_indices(x, sty$j[row], sty$i[row])
        } else {
          adjusted_j <- sty$j[row]
        }
        listener <- sprintf(
          listener,
          id,
          sty$i[row],
          adjusted_j - 1,
          rowspan,
          colspan
        )
        x@table_string <- lines_insert(
          x@table_string,
          listener,
          "tinytable span after",
          "after"
        )
        # x@table_string <- bootstrap_setting(x@table_string, listener, component = "cell")
      }
    }

    rec$css_arguments <- css
    rec <- rec[rec$css_arguments != "", , drop = FALSE]
    if (nrow(rec) == 0) {
      return(x)
    }

    # Unique CSS arguments assigne by arrays
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
        entry <- sprintf(
          "      .table td.%s, .table th.%s { %s }",
          id_css,
          id_css,
          idx[[i]]$css_arguments[1]
        )
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

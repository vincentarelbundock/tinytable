#' tinytable S4 method
#'
#' @keywords internal
setMethod(
  f = "group_eval",
  signature = "tinytable_typst",
  definition = function(x, i = NULL, j = NULL, indent = 0, ...) {
    out <- x

    if (!is.null(i)) {
      out <- group_typst_row(out, i, indent)
    }

    if (nrow(x@data_group_j) > 0) {
      out <- group_typst_col(out, j, ...)
    }

    return(out)
  }
)

group_typst_row <- function(x, i, indent, ...) {
  tab <- x@table_string
  tab <- strsplit(tab, split = "\\n")[[1]]
  body_min <- utils::head(grep("tinytable cell content after", tab), 1) + 1
  body_max <- utils::head(grep("end table", tab), 1) - 1
  top <- tab[1:(body_min - 1)]
  mid <- tab[body_min:body_max]
  mid <- mid[mid != ""]
  bot <- tab[(body_max + 1):length(tab)]
  for (idx in rev(seq_along(i))) {
    mid <- append(
      mid,
      sprintf("table.cell(colspan: %s)[%s],", ncol(x), names(i)[idx]),
      after = i[idx] - 1
    )
  }
  tab <- c(top, mid, bot)
  tab <- paste(tab, collapse = "\n")
  x@table_string <- tab
  idx_new <- i + seq_along(i) - 1
  idx_all <- seq_len(nrow(x))
  idx <- setdiff(idx_all, idx_new)
  return(x)
}

group_typst_col <- function(x, j, ihead, ...) {
  # Use @data_group_j matrix instead of j parameter
  if (nrow(x@data_group_j) == 0) {
    return(x)
  }
  

  out <- x@table_string

  # Process all header rows from @data_group_j matrix (from last to first, newest first)
  num_header_rows <- nrow(x@data_group_j)
  
  for (row_idx in num_header_rows:1) {
    header_row_original <- x@data_group_j[row_idx, ]
    
    # Build typst column headers for this row
    col_cells <- character(0)
    i <- 1
    while (i <= length(header_row_original)) {
      if (!is.na(header_row_original[i]) && header_row_original[i] != "") {
        # Find the span of this group
        start_col <- i
        group_name <- header_row_original[i]
        end_col <- i
        
        # Find consecutive columns that belong to this group (colspan continuation with "")
        while (end_col < length(header_row_original) && 
               !is.na(header_row_original[end_col + 1]) && 
               header_row_original[end_col + 1] == "") {
          end_col <- end_col + 1
        }
        
        # Calculate colspan
        colspan <- end_col - start_col + 1
        
        # Create typst cell
        col_cells <- c(col_cells, sprintf(
          "table.cell(stroke: (bottom: .05em + black), colspan: %s, align: center)[%s],",
          colspan,
          group_name
        ))
        
        i <- end_col + 1
      } else {
        # Empty cell (NA or first empty string in a group)
        col_cells <- c(col_cells, "[ ],")
        i <- i + 1
      }
    }
    
    # Insert this header row
    col <- paste(col_cells, collapse = "")
    out <- lines_insert(out, col, "repeat: true", "after")
  }
  
  if (!any(grepl("column-gutter", out))) {
    out <- lines_insert(
      out,
      "    column-gutter: 5pt,",
      "// tinytable table start",
      "after"
    )
  }

  x@table_string <- out

  return(x)
}

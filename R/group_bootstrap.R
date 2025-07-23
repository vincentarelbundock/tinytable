#' tinytable S4 method
#'
#' @keywords internal
setMethod(
  f = "group_eval",
  signature = "tinytable_bootstrap",
  definition = function(x, i = NULL, j = NULL, indent = 1, ...) {
    out <- x
    # columns first to count headers properly
    if (!is.null(j)) {
      out <- group_bootstrap_col(out, j = j, ...)
    }
    if (!is.null(i)) {
      out <- group_bootstrap_row(out, i = i, j = j, indent = indent, ...)
    }
    return(out)
  }
)

group_bootstrap_col <- function(x, j, ihead, ...) {
  out <- x@table_string

  out <- strsplit(out, "\\n")[[1]]
  header <- NULL

  miss <- as.list(setdiff(seq_len(ncol(x)), unlist(j)))
  miss <- stats::setNames(miss, rep(" ", length(miss)))
  j <- c(j, miss)

  max_col <- sapply(j, max)
  idx <- order(max_col)
  j <- j[idx]
  jstring <- lapply(seq_along(names(j)), function(k) {
    sprintf(
      '<th scope="col" align="center" colspan=%s data-row="%d" data-col="%d">%s</th>',
      max(j[[k]]) - min(j[[k]]) + 1,
      ihead,
      k - 1, # 0-based indexing for data-col
      names(j)[k]
    )
  })
  jstring <- paste(unlist(jstring), collapse = "\n")
  jstring <- sprintf("<tr>\n%s\n</tr>", jstring)

  idx <- grep("<thead>", out, fixed = TRUE)[1]
  out <- c(out[seq_len(idx)], jstring, out[(idx + 1):length(out)])

  out <- paste(out, collapse = "\n")

  x@table_string <- out

  # Only apply styling if there are actual column headers (not just the inserted matrix)
  if (x@nhead > 1) {
    x <- style_tt(x, i = ihead, align = "c")

    # midrule on numbered spans (not full columns of body)
    jnames <- names(j)
    jnames <- seq_along(jnames)[trimws(jnames) != ""]
    x <- style_tt(
      x,
      i = ihead,
      j = jnames,
      line = "b",
      line_width = 0.05,
      line_color = "#d3d8dc"
    )
  }

  return(x)
}

group_bootstrap_row <- function(x, i, j, indent = 1, ...) {
  if (is.null(i)) {
    return(x)
  }

  # reverse order is important (like in LaTeX version)
  i <- rev(sort(i))
  
  if (is.null(names(i))) {
    msg <- "`i` must be a named integer vector."
    stop(msg, call. = FALSE)
  }
  
  label <- names(i)

  # Work directly on the HTML table string (similar to LaTeX version)
  tab <- strsplit(x@table_string, "\\n")[[1]]

  # Find tbody boundaries in the HTML
  tbody_start <- grep("<tbody>", tab, fixed = TRUE)[1]
  tbody_end <- grep("</tbody>", tab, fixed = TRUE)[1]
  
  if (is.na(tbody_start) || is.na(tbody_end)) {
    stop("Could not find tbody boundaries in HTML table", call. = FALSE)
  }

  # Split table into parts (like LaTeX version)
  top <- tab[1:tbody_start]
  body_lines <- tab[(tbody_start + 1):(tbody_end - 1)]
  bot <- tab[tbody_end:length(tab)]

  # Find complete <tr>...</tr> blocks within tbody
  # Join body lines and extract complete table rows
  body_content <- paste(body_lines, collapse = "\n")
  
  # Extract complete <tr> blocks using regex with DOTALL mode
  tr_pattern <- "(?s)<tr>.*?</tr>"  # (?s) enables DOTALL mode so . matches newlines
  tr_blocks <- regmatches(body_content, gregexpr(tr_pattern, body_content, perl = TRUE))[[1]]
  
  if (length(tr_blocks) == 0) {
    return(x)  # No rows to process
  }

  # Create new group row HTML (similar to LaTeX separator rows)
  new_group_rows <- sprintf(
    '<tr><td colspan="%s" data-row="GROUP" data-col="0">%s</td></tr>',
    ncol(x),
    label
  )

  # Insert group rows at specified positions (same logic as LaTeX version)
  # Start with the original tr_blocks
  result_rows <- tr_blocks
  
  # Insert group rows from highest position to lowest (same as LaTeX version)
  for (g in seq_along(i)) {
    # Insert after position i[g] - 1 (0-based insertion)
    insert_pos <- i[g] - 1
    if (insert_pos >= 0 && insert_pos <= length(result_rows)) {
      result_rows <- append(result_rows, new_group_rows[g], after = insert_pos)
    }
  }

  # Update data-row attributes sequentially (like LaTeX version) 
  current_data_row <- x@nhead
  for (k in seq_along(result_rows)) {
    result_rows[k] <- gsub(
      'data-row="[^"]*"',
      sprintf('data-row="%s"', current_data_row),
      result_rows[k]
    )
    current_data_row <- current_data_row + 1
  }

  # Rebuild the table with proper indentation
  new_body_content <- paste("                ", result_rows, collapse = "\n")
  
  # Reconstruct the full table (like LaTeX version)
  tab <- c(top, new_body_content, bot)
  x@table_string <- paste(tab, collapse = "\n")

  # Update body tracking for future calls (like LaTeX version)
  x@body <- c(x@body, new_group_rows)

  # Style the group rows to be centered with consistent indentation
  x <- style_tt(x, i = -1, align = "c", indent = indent)

  return(x)
}

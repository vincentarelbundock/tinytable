#' tinytable S4 method
#'
#' @keywords internal
setMethod(
  f = "group_eval",
  signature = "tinytable_tabularray",
  definition = function(x, i = NULL, j = NULL, indent = 1, ...) {
    # columns first to count headers properly
    x <- group_tabularray_col(x, j, ...)
    x <- group_tabularray_row(x, i, indent)
    return(x)
  }
)

group_tabularray_col <- function(x, j, ihead, ...) {
  # Use @data_group_j matrix instead of j parameter
  if (nrow(x@data_group_j) == 0) {
    return(x)
  }

  out <- strsplit(x@table_string, split = "\\n")[[1]]

  # Process all header rows from @data_group_j matrix (from last to first, newest first)
  num_header_rows <- nrow(x@data_group_j)
  all_headers <- character(0)
  current_ihead <- ihead
  
  for (row_idx in num_header_rows:1) {
    header_row_original <- x@data_group_j[row_idx, ]
    # Convert NA to empty string for LaTeX output
    header_row_for_output <- header_row_original
    header_row_for_output[is.na(header_row_for_output)] <- ""
    header <- paste(header_row_for_output, collapse = " & ")

    # Build cmidrule based on original @data_group_j content (with NAs) for this row
    midr <- character(0)
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
        
        midr <- c(midr, sprintf("\\cmidrule[lr]{%s-%s}", start_col, end_col))
        i <- end_col + 1
      } else {
        i <- i + 1
      }
    }
    
    header_with_rules <- paste(header, "\\\\", paste(midr, collapse = ""))
    all_headers <- c(all_headers, trimws(header_with_rules))
    
    # Apply styling for this header row
    i <- 1
    while (i <= length(header_row_original)) {
      if (!is.na(header_row_original[i]) && header_row_original[i] != "") {
        start_col <- i
        end_col <- i
        
        # Find the span of this group (colspan continuation with "")
        while (end_col < length(header_row_original) && 
               !is.na(header_row_original[end_col + 1]) && 
               header_row_original[end_col + 1] == "") {
          end_col <- end_col + 1
        }
        
        cs <- end_col - start_col + 1
        if (cs == 1) {
          cs <- NULL
        }
        
        args <- list(
          tt_build_now = TRUE,
          x = x,
          i = current_ihead,
          j = start_col,
          align = "c",
          colspan = cs
        )
        x <- do.call(style_tt, args)
        
        i <- end_col + 1
      } else {
        i <- i + 1
      }
    }
    
    current_ihead <- current_ihead - 1
  }

  # Insert all headers into the table
  idx <- max(
    c(
      grep("% tabularray inner close", out),
      grep("\\toprule", out, fixed = TRUE)
    )
  )

  out <- c(
    out[1:idx],
    all_headers,
    out[(idx + 1):length(out)]
  )
  out <- paste(out, collapse = "\n")

  # rebuild including meta before style_tt
  x@table_string <- out

  return(x)
}

group_tabularray_row <- function(x, i, indent) {
  if (is.null(i)) {
    return(x)
  }

  # reverse order is important
  i <- rev(sort(i))

  if (is.null(names(i))) {
    msg <- "`i` must be a named integer vector."
    stop(msg, call. = FALSE)
  }
  label <- names(i)

  tab <- strsplit(x@table_string, "\\n")[[1]]

  # store the original body lines when creating the table, and use those to guess the boundaries.
  # a hack, but probably safer than most regex approaches I can think of.
  body_min <- max(grep("TinyTableHeader|toprule|inner close", tab)) + 1
  body_max <- min(grep("bottomrule|end.tblr", tab))
  body <- body_min:body_max
  top <- tab[1:(min(body) - 1)]
  mid <- tab[min(body):max(body)]
  bot <- tab[(max(body) + 1):length(tab)]

  # separator rows
  # add separator rows so they are treated as body in future calls
  new <- paste(label, strrep("&", ncol(x) - 1), "\\\\")
  x@body <- c(x@body, new)
  idx <- insert_values(mid, new, i)

  # rebuild table
  tab <- c(top, idx$vec, bot)
  tab <- paste(tab, collapse = "\n")

  # colspan for row groups
  # can't figure out how to use style_tt() here. Maybe build order?
  cellspec <- sprintf(
    "cell{%s}{%s}={%s}{%s},",
    idx$new[is.na(idx$old)] + x@nhead,
    1,
    paste0("c=", ncol(x)),
    ""
  )
  cellspec <- paste(cellspec, collapse = "")
  tab <- tabularray_insert(tab, content = cellspec, type = "inner")

  x@table_string <- tab

  return(x)
}

insert_values <- function(vec, values, positions) {
  if (length(values) != length(positions)) {
    stop("The length of values and positions must be the same")
  }

  # Sort the positions in decreasing order along with their corresponding values
  ord <- order(positions, decreasing = TRUE)
  values <- values[ord]
  positions <- positions[ord]

  # Create a vector of indices for the original vector
  original_indices <- seq_along(vec)

  # Insert values and update indices
  for (i in seq_along(values)) {
    vec <- append(vec, values[i], after = positions[i] - 1)
    original_indices <- append(original_indices, NA, after = positions[i] - 1)
  }

  # Return the extended vector and the original indices vector
  return(data.frame(vec = vec, old = original_indices, new = seq_along(vec)))
}

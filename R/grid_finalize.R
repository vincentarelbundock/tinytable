grid_notes_caption <- function(x) {
  out <- x@table_string

  # notes
  for (i in seq_along(x@notes)) {
    lines <- strsplit(out, split = "\\n")[[1]]
    target <- max(ansi_aware_nchar(lines)) - 4
    no <- x@notes[[i]]
    if (is.list(no)) {
      txt <- no$text
    } else {
      txt <- no
    }
    if (isTRUE(names(x@notes)[i] != "")) {
      txt <- sprintf("^%s^ %s", names(x@notes)[i], txt)
    }
    txt <- ansi_strwrap(txt, width = target)
    txt <- ansi_format(txt, target)
    txt <- sprintf("| %s |", txt)

    # Find the correct insertion point for notes
    plus_lines <- grep("^+", lines)
    if (length(plus_lines) >= 2) {
      # Multiple border lines - use the last one (bottom border)
      idx <- utils::tail(plus_lines, 1)
    } else if (length(plus_lines) == 1) {
      # Only one border line (likely header separator with hline=FALSE)
      # Insert at the end of the table instead
      idx <- length(lines)
    } else {
      # No border lines - append at the end
      idx <- length(lines)
    }

    if (idx <= length(lines) && idx %in% plus_lines) {
      # We found a border line - replace it and add notes
      bot <- lines[idx]
      bot <- gsub("-", "=", bot)
      lines[idx] <- bot
      out <- c(lines, txt, bot)
    } else {
      # Append at the end with proper borders
      # Create a border line based on the table width
      if (length(lines) > 0) {
        table_width <- max(ansi_aware_nchar(lines))
        border_line <- paste0("+", strrep("=", table_width - 2), "+")
      } else {
        border_line <- "+===+"
      }
      out <- c(lines, border_line, txt, border_line)
    }
    out <- paste(out, collapse = "\n")
  }

  # caption
  cap <- x@caption
  if (is.character(cap) && length(cap) == 1 && ansi_aware_nchar(cap) > 0) {
    out <- paste0(out, "\n", "Table: ", cap, "\n")
  }

  x@table_string <- out
  return(x)
}


setMethod(
  f = "finalize",
  signature = "tinytable_grid",
  definition = function(x, ...) {
    x <- style_grid_notes(x)
    x <- style_grid_caption(x)
    x <- grid_hlines(x)
    x <- grid_notes_caption(x)
    return(x)
  }
)

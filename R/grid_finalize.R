# ANSI-aware utility functions
ansi_aware_strwrap <- function(text, width) {
  # For ANSI text, only wrap if visual width exceeds target width
  visual_width <- calculate_text_width(text)
  if (visual_width <= width) {
    return(text)
  } else {
    # If it's too long, fall back to regular strwrap
    # This is a simplified implementation - could be enhanced for better ANSI handling
    return(strwrap(text, width = width))
  }
}

ansi_aware_format <- function(txt, width) {
  formatted <- character(length(txt))
  for (i in seq_along(txt)) {
    visual_width <- calculate_text_width(txt[i])
    if (visual_width < width) {
      padding_needed <- width - visual_width
      formatted[i] <- paste0(txt[i], strrep(" ", padding_needed))
    } else {
      formatted[i] <- txt[i]
    }
  }
  return(formatted)
}

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
    txt <- ansi_aware_strwrap(txt, width = target)
    txt <- ansi_aware_format(txt, target)
    txt <- sprintf("| %s |", txt)
    idx <- utils::tail(grep("^+", lines), 1)
    bot <- lines[idx]
    bot <- gsub("-", "=", bot)
    lines[idx] <- bot
    out <- c(lines, txt, bot)
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
    x <- style_notes_grid(x)
    x <- style_caption_grid(x)
    x <- grid_hlines(x)
    x <- grid_notes_caption(x)
    return(x)
  })

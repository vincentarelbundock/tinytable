ansi_nchar <- function(x, keepNA = TRUE, z = FALSE) {
  x <- as.character(x)
  is_na <- is.na(x)

  # Replace NA if keepNA = FALSE
  if (!keepNA) x[is_na] <- "NA"

  # Strip ANSI/CSI sequences
  x_stripped <- gsub("\u001B\\[[0-?]*[ -/]*[@-~]", "", x, perl = TRUE)

  if (z) {
    nzchar(x_stripped, keepNA = keepNA)
  } else {
    nchar(x_stripped, type = "chars", keepNA = keepNA)
  }
}


ansi_pad <- function(txt, width) {
    formatted <- character(length(txt))
    for (i in seq_along(txt)) {
        visual_width <- ansi_nchar(txt[i])
        if (visual_width < width) {
            padding_needed <- width - visual_width
            formatted[i] <- paste0(txt[i], strrep(" ", padding_needed))
        } else {
            formatted[i] <- txt[i]
        }
    }
    return(formatted)
}


ansi_strwrap <- function(text, width) {
    # For ANSI text, only wrap if visual width exceeds target width
    visual_width <- ansi_nchar(text)
    if (visual_width <= width) {
        return(text)
    } else {
        # If it's too long, fall back to regular strwrap
        # This is a simplified implementation - could be enhanced for better ANSI handling
        return(strwrap(text, width = width))
    }
}


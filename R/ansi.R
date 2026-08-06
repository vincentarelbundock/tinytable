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
    nchar(x_stripped, type = "width", keepNA = keepNA)
  }
}


ansi_pad <- function(txt, width) {
    w <- ansi_nchar(txt)
    pad <- pmax(width - w, 0)
    paste0(txt, strrep(" ", pad))
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


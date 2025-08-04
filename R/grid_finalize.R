grid_notes_caption <- function(x) {
  out <- x@table_string

  # notes
  for (i in seq_along(x@notes)) {
    lines <- strsplit(out, split = "\\n")[[1]]
    target <- max(nchar(lines)) - 4
    no <- x@notes[[i]]
    if (is.list(no)) {
      txt <- no$text
    } else {
      txt <- no
    }
    if (isTRUE(names(x@notes)[i] != "")) {
      txt <- sprintf("^%s^ %s", names(x@notes)[i], txt)
    }
    txt <- strwrap(txt, width = target)
    txt <- format(txt, width = target)
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
  if (is.character(cap) && length(cap) == 1 && nchar(cap) > 0) {
    out <- paste0(out, "\n", "Table: ", cap, "\n")
  }

  x@table_string <- out
  return(x)
}


setMethod(
  f = "finalize",
  signature = "tinytable_grid",
  definition = function(x, ...) {
    x <- grid_hlines(x)
    x <- grid_notes_caption(x)
    return(x)
  })

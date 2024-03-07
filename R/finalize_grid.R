setMethod(
  f = "finalize",
  signature = "tinytable_grid",
  definition = function(x, ...) {

    # formal grid specification in pandoc includes lines everywhere
    # important for docx output
    hlines <- getOption("tinytable_grid_hlines", default = TRUE)
    if (isTRUE(hlines)) {
      x <- grid_hlines(x)
    }

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

    for (fn in x@lazy_finalize) {
      x <- fn(x)
    }

    return(x)
})



replace_char_at_position <- function(input_string, position, replacement_char) {
  before <- substr(input_string, 1, position - 1)
  after <- substr(input_string, position + 1, nchar(input_string))
  result <- paste0(before, replacement_char, after)
  return(result)
}

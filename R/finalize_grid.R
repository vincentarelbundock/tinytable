
finalize_grid <- function(x) {
    if (!isTRUE(x@output == "markdown")) return(x)


    # formal grid specification in pandoc includes lines everywhere
    # important for docx output
    hlines <- getOption("tinytable_grid_hlines", default = TRUE)
    if (isTRUE(hlines)) {
      x <- grid_hlines(x)
    }

    out <- x@table_string

    # notes
    no <- x@notes
    if (length(no) > 0) {
      if (!is.character(no) || length(no) != 1) {
        msg <- "For Markdown or Word tables, the `notes` argument must be a single string."
        stop(msg, call. = FALSE)
      }
      lines <- strsplit(out, split = "\\n")[[1]]
      target <- max(nchar(lines)) - 4
      no <- strwrap(no, width = target)
      no <- format(no, width = target)
      no <- sprintf("| %s |", no)
      idx <- utils::tail(grep("^+", lines), 1)
      bot <- lines[idx]
      bot <- gsub("-", "=", bot)
      lines[idx] <- bot
      out <- c(lines, no, bot)
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



replace_char_at_position <- function(input_string, position, replacement_char) {
  before <- substr(input_string, 1, position - 1)
  after <- substr(input_string, position + 1, nchar(input_string))
  result <- paste0(before, replacement_char, after)
  return(result)
}

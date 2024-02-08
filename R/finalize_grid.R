
finalize_grid <- function(x) {
    if (!isTRUE(meta(x)$output == "markdown")) return(x)

    out <- x

    # formal grid specification in pandoc includes lines everywhere
    # important for docx output
    hlines <- getOption("tinytable_grid_hlines", default = TRUE)
    if (isTRUE(hlines)) {
      out <- grid_hlines(out)
    }

    # notes
    no <- meta(x, "notes")
    if (!is.null(no)) {
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
    cap <- meta(x, "caption")
    if (is.character(cap) && length(cap) == 1) {
        out <- paste0(out, "\n", "Table: ", cap, "\n")
    }

    return(out)
}
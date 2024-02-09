
finalize_grid <- function(x) {
    if (!isTRUE(meta(x)$output == "markdown")) return(x)

    out <- x

    # formal grid specification in pandoc includes lines everywhere
    # important for docx output
    hlines <- getOption("tinytable_grid_hlines", default = TRUE)
    if (isTRUE(hlines)) {
      out <- grid_hlines(out)
    }

    out <- strsplit(out, split = "\\n")[[1]]

    # colspan: remove pipes
    idx_data <- grep("[^+=-]", out)
    for (l in meta(x, "lazy_style")) {
      rs <- if (is.null(l$rowspan)) 1 else l$rowspan
      cs <- if (is.null(l$colspan)) 1 else l$colspan
      if (length(idx_data) > 0 && (!is.null(rs) || !is.null(cs))) {
        i <- idx_data[l$i + meta(x, "nhead") + seq_len(rs) - 1]
        j <- setdiff(l$j + seq_len(cs) - 1, l$j)
        for (w in i) {
          # rev matters a lot here
          for (z in rev(j)) {
            pipe_loc <- gregexpr("\\|", out[[w]])[[1]][z]
            out[[w]] <- replace_char_at_position(out[[w]], pipe_loc, " ")
          }
        }
      }
    }

    # rowspan: remove +---+
    idx_data <- grep("[^+=-]", out)
    idx_data <- setdiff(idx_data, max(idx_data)) # row after data, except last
    for (l in meta(x, "lazy_style")) {
      rs <- if (is.null(l$rowspan)) 1 else l$rowspan
      cs <- if (is.null(l$colspan)) 1 else l$colspan
      if (length(idx_data) > 0 && (!is.null(rs) || !is.null(cs))) {
        i <- stats::na.omit(idx_data[l$i + meta(x, "nhead") + seq_len(rs) - 1] + 1)
        j <- c(l$j, l$j + seq_len(cs))
        for (w in i) {
          plus_loc <- gregexpr("\\+", out[[w]])[[1]]
          hyphen_loc <- (min(plus_loc[j]) + 1):(max(plus_loc[j]) - 1)
          for (z in seq_along(hyphen_loc)) {
            out[[w]] <- replace_char_at_position(out[[w]], hyphen_loc[z], " ")
          }
        }
      }
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



replace_char_at_position <- function(input_string, position, replacement_char) {
  before <- substr(input_string, 1, position - 1)
  after <- substr(input_string, position + 1, nchar(input_string))
  result <- paste0(before, replacement_char, after)
  return(result)
}

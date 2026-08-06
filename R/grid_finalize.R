grid_notes_caption <- function(x) {
  out <- x@table_string

  # notes
  if (length(x@notes) > 0) {
    lines <- strsplit(out, split = "\\n")[[1]]
    table_width <- max(ansi_nchar(lines))
    target <- table_width - 4

    # Wrap all notes first so the box can widen when a note cannot wrap
    # down to the table width (e.g. very narrow tables or unbreakable words).
    note_lines <- vector("list", length(x@notes))
    for (i in seq_along(x@notes)) {
      no <- x@notes[[i]]
      txt <- if (is.list(no)) no$text else no
      if (isTRUE(names(x@notes)[i] != "")) {
        txt <- sprintf("^%s^ %s", names(x@notes)[i], txt)
      }
      note_lines[[i]] <- ansi_strwrap(txt, width = target)
    }
    box_width <- max(table_width, ansi_nchar(unlist(note_lines)) + 4)

    plus_lines <- grep("^\\+", lines)
    if (length(plus_lines) >= 2) {
      # Bottom border present: convert it into the top of the notes box,
      # widening it when the notes are wider than the table.
      idx <- utils::tail(plus_lines, 1)
      bot <- gsub("-", "=", lines[idx])
      if (box_width > table_width) {
        bot <- paste0(
          sub("\\+$", "", bot),
          strrep("=", box_width - table_width),
          "+"
        )
      }
      lines[idx] <- bot
    } else {
      # No bottom border (e.g. hline = FALSE): open a new box after the table
      bot <- paste0("+", strrep("=", box_width - 2), "+")
      while (length(lines) > 0 && lines[length(lines)] == "") {
        lines <- lines[-length(lines)]
      }
      lines <- c(lines, bot)
    }

    # Each note gets its own box segment, closed by a border line
    for (i in seq_along(note_lines)) {
      txt <- sprintf("| %s |", ansi_pad(note_lines[[i]], box_width - 4))
      lines <- c(lines, txt, bot)
    }
    out <- paste(lines, collapse = "\n")
  }

  # caption
  cap <- x@caption
  if (is.character(cap) && length(cap) == 1 && ansi_nchar(cap) > 0) {
    out <- paste0(out, "\n", "Table: ", cap, "\n")
  }

  x@table_string <- out
  return(x)
}


setMethod(
  f = "finalize",
  signature = "tinytable_grid",
  definition = function(x, ...) {
    x <- style_notes(x)
    x <- style_caption(x)
    x <- grid_hlines(x)
    x <- grid_notes_caption(x)
    return(x)
  }
)

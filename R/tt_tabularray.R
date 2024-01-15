tt_tabularray <- function(x, caption, theme, width, notes) {

  template <- template_tabularray(theme)

  ncols <- ncol(x)
  nrows <- nrow(x)

  # caption
  if (is.null(caption)) {
    idx <- grep("\\$tinytable_CAPTION", template)
    template <- template[-idx]
  } else {
    template <- sub("\\$tinytable_CAPTION", caption, template)
  }

  # body: main
  if (!is.null(colnames(x))) {
    header <- paste(colnames(x), collapse = " & ")
    header <- paste(header, "\\\\")
  } else {
    header <- NULL
  }
  body <- apply(x, 1, paste, collapse = " & ")
  body <- paste(body, "\\\\")

  # theme: booktabs
  if (isTRUE(theme %in% c("default", "striped"))) {
    if (!is.null(colnames(x))) {
      # %% are important to distinguish between potentially redundant data rows
      header[length(header)] <- paste(header[length(header)], "\\midrule %% TinyTableHeader")
    }
  }

  # body: finish
  idx <- grep("\\$tinytable_BODY", template)
  out <- c(
    template[1:(idx - 1)],
    header,
    body,
    template[(idx + 1):length(template)]
  )

  out <- trimws(out)
  out <- paste(out, collapse = "\n")

  if (!is.null(width)) {
    tabularray_cols <- rep("X[]", ncol(x))
    spec <- sprintf("width={%s\\linewidth},", round(width, 4))
    out <- style_tabularray(out, inner = spec)
  } else {
    tabularray_cols <- rep("Q[]", ncol(x))
  }

  # colspec (we don't need rowspec)
  colspec <- sprintf("colspec={%s},", paste(tabularray_cols, collapse = ""))
  out <- style_tabularray(out, inner = colspec)     

  # themes
  if (theme == "grid") {
    out <- style_tabularray(out, inner = "hlines={},vlines={},")     
  } else if (theme == "striped") {
    out <- style_tabularray(out, inner = "row{even}={bg=black!5!white},")
  }

  # notes
  if (!is.null(notes)) {
    out <- sub("\\begin{tblr}", "\\begin{talltblr}", out, fixed = TRUE)
    out <- sub("\\end{tblr}", "\\end{talltblr}", out, fixed = TRUE)
    # otherwise an empty caption is created automatically
    out <- style_tabularray(out, outer = "entry=none,label=none")
    if (is.null(names(notes))) {
      lab <- rep("", length(notes))
    } else {
      lab <- names(notes)
    }
    notes <- unlist(notes)
    for (k in seq_along(notes)) {
      spec <- sprintf("note{%s}={%s}", lab[k], notes[k])
      out <- style_tabularray(out, outer = spec)
    }
  }

  class(out) <- c("tinytable", "knit_asis", class(out))
  return(out)
}



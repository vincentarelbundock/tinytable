setMethod(
  f = "tt_eval",
  signature = "tinytable_tabularray",
  definition = function(x, ...) {

  template <- readLines(system.file("templates/tabularray.tex", package = "tinytable"))

  ncols <- ncol(x)
  nrows <- nrow(x)

  # caption
  if (length(x@caption) > 0) {
    idx <- grep("\\$tinytable_CAPTION", template)
    template[idx] <- sprintf("\\caption{%s}", x@caption)
  } else {
    idx <- grep("\\$tinytable_CAPTION", template)
    template <- template[-idx]
  }

  # placement
  if (length(x@placement) == 1) {
    assert_string(x@placement)
    # dollar sign to avoid [H][H] when we style multiple times
    template <- sub("\\\\begin\\{table\\}", sprintf("\\\\begin{table}[%s]\n", x@placement), template)
  }

  # body: main
  if (length(colnames(x)) > 0) {
    header <- paste(colnames(x), collapse = " & ")
    header <- paste(header, "\\\\")
  } else {
    header <- NULL
  }
  body <- apply(as.data.frame(x@table_dataframe), 1, paste, collapse = " & ")
  body <- paste(body, "\\\\")

  # theme: booktabs
  th <- x@theme[[1]]
  if (is.null(th) || is.function(th) || isTRUE(th %in% c("default", "striped"))) {
    if (length(colnames(x)) > 0) {
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

  if (length(x@width) > 0) {
    tabularray_cols <- rep("X[]", ncol(x))
    spec <- sprintf("width={%s\\linewidth},", round(x@width, 4))
    out <- tabularray_insert(out, content = spec, type = "inner")
  } else {
    tabularray_cols <- rep("Q[]", ncol(x))
  }

  # colspec (we don't need rowspec)
  colspec <- sprintf("colspec={%s},", paste(tabularray_cols, collapse = ""))
  out <- tabularray_insert(out, content = colspec, type = "inner")     

  # notes
  if (length(x@notes) > 0) {
    out <- sub("\\begin{tblr}", "\\begin{talltblr}", out, fixed = TRUE)
    out <- sub("\\end{tblr}", "\\end{talltblr}", out, fixed = TRUE)
    # otherwise an empty caption is created automatically
    out <- tabularray_insert(out, content = "entry=none,label=none", type = "outer")
    if (is.null(names(x@notes))) {
      lab <- rep("", length(x@notes))
    } else {
      lab <- names(x@notes)
    }
    notes <- sapply(x@notes, function(n) if (is.list(n)) n$text else n)
    for (k in seq_along(notes)) {
      spec <- sprintf("note{%s}={%s}", lab[k], notes[k])
      out <- tabularray_insert(out, content = spec, type = "outer")
    }
  }

  x@table_string <- out
  x@body <- body

  return(x)
})



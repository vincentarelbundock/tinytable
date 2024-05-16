setMethod(
  f = "tt_eval",
  signature = "tinytable_tabularray",
  definition = function(x, ...) {

  template <- readLines(system.file("templates/tabularray.tex", package = "tinytable"))
  if(x@landscape){
    template <- c("\\begin{landscape}", template, "\\end{landscape}")
  }

  ncols <- ncol(x)
  nrows <- nrow(x)

  tall <- FALSE
  if (length(x@caption) > 0) tall <- TRUE
  if (length(x@notes) > 0) tall <- TRUE


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

  if (length(x@caption) > 0) {
    spec <- sprintf("caption={%s},", x@caption[1])
    out <- tabularray_insert(out, content = spec, type = "outer")
  }

  if (length(x@width) == 0) {
    tabularray_cols <- rep("Q[]", ncol(x))

  } else if (length(x@width) == 1) {
    tabularray_cols <- rep("X[]", ncol(x))
    spec <- sprintf("width={%s\\linewidth},", round(x@width, 4))
    out <- tabularray_insert(out, content = spec, type = "inner")

  } else if (length(x@width) > 1) {
    tabularray_cols <- sprintf("X[%s]", x@width)
    spec <- sprintf("width={%s\\linewidth},", round(sum(x@width), 4))
    out <- tabularray_insert(out, content = spec, type = "inner")
  }

  # colspec (we don't need rowspec)
  colspec <- sprintf("colspec={%s},", paste(tabularray_cols, collapse = ""))
  out <- tabularray_insert(out, content = colspec, type = "inner")     

  # notes
  if (length(x@notes) > 0) {
    # otherwise an empty caption is created automatically
    out <- tabularray_insert(out, content = "entry=none,label=none", type = "outer")
    if (is.null(names(x@notes))) {
      lab <- sapply(seq_along(x@notes), function(k) strrep(" ", k - 1))
    } else {
      lab <- NULL
      pad <- 0
      for (i in seq_along(x@notes)) {
        # tabularray requires unique labels, but multiple blanks work
        if (names(x@notes)[i] == "") {
          lab[i] <- strrep(" ", pad) # not sure why -1 is necessary in tabularray
          pad <- pad + 1
        } else {
          lab[i] <- names(x@notes)[i]
        }
      }
    }
    notes <- sapply(x@notes, function(n) if (is.list(n)) n$text else n)
    for (k in seq_along(notes)) {
      spec <- sprintf("note{%s}={%s}", lab[k], notes[k])
      out <- tabularray_insert(out, content = spec, type = "outer")
    }
  }

  if (isTRUE(tall)) {
    out <- sub("\\begin{tblr}", "\\begin{talltblr}", out, fixed = TRUE)
    out <- sub("\\end{tblr}", "\\end{talltblr}", out, fixed = TRUE)
  }

  x@table_string <- out
  x@body <- body

  return(x)
})



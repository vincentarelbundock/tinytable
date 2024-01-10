tt_tabularray <- function(x, caption, theme, extendable) {

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
    nrows <- nrows + 1
  } else {
    header <- NULL
  }
  body <- apply(x, 1, paste, collapse = " & ")
  body <- paste(body, "\\\\")

  # theme: booktabs
  if (isTRUE(theme == "booktabs")) {
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

  if (isTRUE(extendable)) {
    tabularray_cols <- rep("X[]", ncol(x))
  } else {
    tabularray_cols <- rep("Q[]", ncol(x))
  }

  # colspec (we don't need rowspec)
  colspec <- sprintf("colspec={%s},", paste(tabularray_cols, collapse = ""))
  out <- style_tabularray(out, inner = colspec)     

  # theme: grid
  if (theme == "grid") {
    out <- style_tabularray(out, inner = "hlines={},vlines={},")     
  }

  attr(out, "nhead") <- if (is.null(colnames(x))) 0 else 1
  attr(out, "ncol") <- ncols
  attr(out, "nrow") <- nrows
  attr(out, "body") <- body
  attr(out, "tabularray_cols") <- tabularray_cols
  class(out) <- c("tinytable_tabularray", "knit_asis", class(out))
  return(out)
}



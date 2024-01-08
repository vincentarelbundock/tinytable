IttyBittyTable_latex <- function(x, caption, settings) {

  template <- settings$template

  ncols <- ncol(x)
  nrows <- nrow(x)

  # caption
  if (is.null(caption)) {
    idx <- grep("\\$IttyBittyTable_CAPTION", template)
    template <- template[-idx]
  } else {
    template <- sub("\\$IttyBittyTable_CAPTION", caption, template)
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
  if (isTRUE(settings$theme == "booktabs")) {

    header <- c("\\toprule", header)
    if (!is.null(colnames(x))) {
      header[length(header)] <- paste(header[length(header)], "\\midrule")
    }
    body[length(body)] <- paste(body[length(body)], "\\bottomrule")
  }

  # body: finish
  idx <- grep("\\$IttyBittyTable_BODY", template)
  out <- c(
    template[1:(idx - 1)],
    header,
    body,
    template[(idx + 1):length(template)]
  )

  out <- trimws(out)
  out <- paste(out, collapse = "\n")

  if (isTRUE(settings$extendable)) {
    tabularray_cols <- rep("X[]", ncol(x))
  } else {
    tabularray_cols <- rep("Q[]", ncol(x))
  }

  # colspec (we don't need rowspec)
  out <- tabularray_setting(
    out,
    sprintf("colspec={%s},", paste(tabularray_cols, collapse = "")),
    inner = TRUE)

  # theme: grid
  if (settings$theme == "grid") {
    out <- tabularray_setting(out, new = "hlines={},vlines={}", inner = TRUE)
  }

  # inner and outer tabularray settings
  if (settings$outer_specs_keys != "") {
    out <- tabularray_setting(out, settings$outer_specs_keys, inner = FALSE)
  }
  if (settings$inner_specs_keys != "") {
    out <- tabularray_setting(out, settings$inner_specs_keys, inner = TRUE)
  }

  attr(out, "ncol") <- ncols
  attr(out, "nrow") <- nrows
  attr(out, "tabularray_cols") <- tabularray_cols
  class(out) <- c("IttyBittyTable_latex", "knit_asis", class(out))
  return(out)
}



tabularray_setting <- function(x, new, inner = TRUE) {
  att <- attributes(x)    
  out <- strsplit(x, "\n")[[1]]
  if (isTRUE(inner)) {
    idx <- grep("% tabularray inner close", out)
  } else {
    idx <- grep("% tabularray outer close", out)
  }
  out <- c(
    out[1:(idx - 1)],
    new,
    out[idx:length(out)]
  )
  out <- paste(out, collapse = "\n")
  attributes(out) <- att
  class(out) <- class(x)
  return(out)
}

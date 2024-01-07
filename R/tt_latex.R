tt_latex <- function(x, caption, settings) {

  template <- settings$template

  # caption
  if (is.null(caption)) {
    idx <- grep("\\$TINYTABLE_CAPTION", template)
    template <- template[-idx]
  } else {
    template <- sub("\\$TINYTABLE_CAPTION", caption, template)
  }

  if (!is.null(colnames(x))) {
    header <- paste(colnames(x), collapse = " & ")
    header <- paste(header, "\\\\")
  } else {
    header <- NULL
  }
  body <- apply(x, 1, paste, collapse = " & ")
  body <- paste(body, "\\\\")

  if (isTRUE(settings$theme == "booktabs")) {
    header <- c("\\toprule", header, "\\midrule")
    body <- c(body, "\\bottomrule")
  }

  idx <- grep("\\$TINYTABLE_BODY", template)
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

  # inner and outer tabularray settings
  if (settings$outer_specs_keys != "") {
    out <- tabularray_setting(out, settings$outer_specs_keys, inner = FALSE)
  }
  if (settings$inner_specs_keys != "") {
    out <- tabularray_setting(out, settings$outer_specs_keys, inner = TRUE)
  }

  attr(out, "ncol") <- ncol(x)
  attr(out, "nrow") <- nrow(x)
  attr(out, "tabularray_cols") <- tabularray_cols
  class(out) <- c("tinytable_latex", "knit_asis", class(out))
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

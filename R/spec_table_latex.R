spec_table_latex <- function(x,
                             caption = NULL,
                             hlines = "booktabs",
                             vlines = FALSE,
                             tabularray_inner = NULL,
                             tabularray_outer = NULL) {

  checkmate::assert_string(tabularray_inner, null.ok = TRUE)
  checkmate::assert_string(tabularray_outer, null.ok = TRUE)
  checkmate::assert(
    checkmate::check_choice(hlines, choices = "booktabs"),
    checkmate::check_integerish(hlines, lower = 1, upper = nrow(x)),
    checkmate::check_flag(hlines)
  )

  template <- readLines(here::here("inst/template_tblr.tex"))

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

  if (isTRUE(hlines == "booktabs")) {
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

  tabularray_cols <- rep("Q[]", ncol(x))
  tabularray_rows <- rep("Q[]", nrow(x))
  if (!is.null(header)) { 
    tabularray_rows <- c("Q[]", tabularray_rows)
  }

  # colspec (we don't need rowspec)
  out <- tabularray_setting(
    out,
    sprintf("colspec={%s},", paste(tabularray_cols, collapse = "")),
    inner = TRUE)

  # vlines
  if (isTRUE(vlines)) {
    out <- tabularray_setting(out, "vlines,", inner = TRUE)
  } else if (isTRUE(checkmate::check_integerish(vlines))) {
    out <- tabularray_setting(
      out,
      sprintf("vline{%s},", paste(vlines, collapse = ",")),
      inner = TRUE)
  }

  # hlines
  if (isTRUE(hlines)) {
    out <- tabularray_setting(out, "hlines,", inner = TRUE)
  } else if (isTRUE(checkmate::check_integerish(hlines))) {
    out <- tabularray_setting(
      out,
      sprintf("hline{%s},", paste(hlines, collapse = ",")),
      inner = TRUE)
  }

  # inner and outer tabularray settings
  if (!is.null(tabularray_inner)) {
    if (!grepl(",$", trimws(tabularray_inner))) inner <- paste0(tabularray_inner, ",")
    out <- tabularray_setting(out, tabularray_inner, inner = TRUE)
  }
  if (!is.null(tabularray_outer)) {
    if (!grepl(",$", trimws(tabularray_outer))) tabularray_outer <- paste0(tabularray_outer, ",")
    out <- tabularray_setting(out, tabularray_outer, inner = FALSE)
  }

  attr(out, "ncol") <- ncol(x)
  attr(out, "nrow") <- nrow(x)
  attr(out, "tabularray_cols") <- tabularray_cols
  attr(out, "tabularray_rows") <- tabularray_rows
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

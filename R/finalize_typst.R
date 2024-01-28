finalize_typst <- function(x) {
  if (!isTRUE(meta(x)$output == "typst")) return(x)

  out <- x
  out <- sub("$TINYTABLE_TYPST_NROW", meta(x, "nrows"), out, fixed = TRUE)
  out <- sub("$TINYTABLE_TYPST_NCOL", meta(x, "ncols"), out, fixed = TRUE)
  out <- sub("$TINYTABLE_TYPST_NHEAD", meta(x, "nhead"), out, fixed = TRUE)

  cap <- meta(x, "caption")
  if (!is.null(cap)) {
    out <- sub("$TINYTABLE_TYPST_CAPTION", sprintf("caption: [%s],", cap), out, fixed = TRUE)
  } else {
    out <- sub("$TINYTABLE_TYPST_CAPTION", "", out, fixed = TRUE)
  }

  return(out)
}
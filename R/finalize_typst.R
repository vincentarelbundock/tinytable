finalize_typst <- function(x) {
  if (!isTRUE(x@output == "typst")) return(x)

  out <- x@table_string
  out <- sub("$TINYTABLE_TYPST_NROW", meta(x, "nrows"), out, fixed = TRUE)
  out <- sub("$TINYTABLE_TYPST_NCOL", meta(x, "ncols"), out, fixed = TRUE)
  out <- sub("$TINYTABLE_TYPST_NHEAD", meta(x, "nhead"), out, fixed = TRUE)

  cap <- x@caption
  if (length(cap) == 1) {
    out <- sub("$TINYTABLE_TYPST_CAPTION", sprintf("caption: [%s],", cap), out, fixed = TRUE)
  } else {
    out <- sub("$TINYTABLE_TYPST_CAPTION", "", out, fixed = TRUE)
  }

  x@table_string <- out

  return(x)
}
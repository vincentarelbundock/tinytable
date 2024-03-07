setMethod(
  f = "finalize",
  signature = "tinytable_typst",
  definition = function(x, ...) {

  out <- x@table_string
  out <- sub("$TINYTABLE_TYPST_NROW", nrow(x), out, fixed = TRUE)
  out <- sub("$TINYTABLE_TYPST_NCOL", ncol(x), out, fixed = TRUE)
  out <- sub("$TINYTABLE_TYPST_NHEAD", x@nhead, out, fixed = TRUE)

  cap <- x@caption
  if (length(cap) == 1) {
    out <- sub("$TINYTABLE_TYPST_CAPTION", sprintf("caption: [%s],", cap), out, fixed = TRUE)
  } else {
    out <- sub("$TINYTABLE_TYPST_CAPTION", "", out, fixed = TRUE)
  }

  x@table_string <- out

  for (fn in x@lazy_finalize) {
    x <- fn(x)
  }

  return(x)
})
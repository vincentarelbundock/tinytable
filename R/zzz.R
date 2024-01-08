.onLoad <- function(libname, pkgname) {
  if(requireNamespace("knitr", quietly = TRUE)) {
    registerS3method("knit_print", "IttyBittyTable_html", knit_print.IttyBittyTable_html, envir = asNamespace("knitr"))
    registerS3method("knit_print", "IttyBittyTable_latex", knit_print.IttyBittyTable_latex, envir = asNamespace("knitr"))
  }
}

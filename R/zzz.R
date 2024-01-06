.onLoad <- function(libname, pkgname) {
  if(requireNamespace("knitr", quietly = TRUE)) {
    registerS3method("knit_print", "tinytable_html", knit_print.tinytable_html, envir = asNamespace("knitr"))
    registerS3method("knit_print", "tinytable_latex", knit_print.tinytable_latex, envir = asNamespace("knitr"))
  }
}

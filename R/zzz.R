.onLoad <- function(libname, pkgname) {
  if(requireNamespace("knitr", quietly = TRUE)) {
    registerS3method("knit_print", "tinytable_bootstrap", knit_print.tinytable_bootstrap, envir = asNamespace("knitr"))
    registerS3method("knit_print", "tinytable_tabularray", knit_print.tinytable_tabularray, envir = asNamespace("knitr"))
  }
}

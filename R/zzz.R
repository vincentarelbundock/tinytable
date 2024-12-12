.onLoad <- function(libname, pkgname) {
  if (requireNamespace("knitr", quietly = TRUE)) {
    registerS3method("knit_print", "tinytable", knit_print.tinytable, envir = asNamespace("knitr"))
  }
}

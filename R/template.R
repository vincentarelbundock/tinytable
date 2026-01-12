template_tabularray <- function(theme = "default") {
  out <- readLines(
    system.file("templates/tabularray_default.tex", package = "tinytable")
  )
  return(out)
}

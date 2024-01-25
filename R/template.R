template_tabularray <- function(theme = "default") {
  if (theme == "default") {
    out <- readLines(system.file("templates/tabularray_default.tex", package = "tinytable"))
  } else if (theme == "bootstrap") {
    out <- readLines(system.file("templates/tabularray_bootstrap.tex", package = "tinytable"))
  } else if (theme == "striped") {
    out <- readLines(system.file("templates/tabularray_default.tex", package = "tinytable"))
  } else if (theme == "void") {
    out <- readLines(system.file("templates/tabularray_void.tex", package = "tinytable"))
  } else if (theme == "grid") {
    out <- readLines(system.file("templates/tabularray_grid.tex", package = "tinytable"))
  }
  return(out)
}

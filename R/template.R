template_tabularray <- function(theme = "default") {
  assert_choice(theme, c("default", "grid", "void"))
  if (theme == "default") {
    out <- readLines(system.file("templates/tabularray_default.tex", package = "tinytable"))
  } else if (theme == "void") {
    out <- readLines(system.file("templates/tabularray_void.tex", package = "tinytable"))
  } else if (theme == "grid") {
    out <- readLines(system.file("templates/tabularray_grid.tex", package = "tinytable"))
  }
  return(out)
}


template_html <- function(theme = "default") {
  if (theme == "default") {
    return(NULL)
  }
}

get_css <- function() {
  css_path <- system.file("tinytable.css", package = "tinytable")
  paste(readLines(css_path, warn = FALSE), collapse = "\n")
}

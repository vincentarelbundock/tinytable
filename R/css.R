get_css <- function(table_id = NULL) {
  css_path <- system.file("tinytable.css", package = "tinytable")
  css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")

  # Framework CSS remains global - no scoping needed
  # Individual cell CSS rules are scoped in html_style.R instead

  return(css)
}

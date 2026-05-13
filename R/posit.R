is_rstudio <- function() {
  identical(Sys.getenv("RSTUDIO"), "1")
}

is_positron <- function() {
  identical(Sys.getenv("POSITRON"), "1")
}

is_rstudio_notebook_context <- function(path, contents = NULL) {
  if (isTRUE(grepl("\\.qmd$|\\.Rmd$", path))) {
    return(TRUE)
  }

  contents <- paste(contents, collapse = "\n")
  if (!nzchar(contents)) {
    return(FALSE)
  }

  has_yaml_header <- grepl("^---\\s*\n", contents)
  has_notebook_format <- grepl("(?m)^\\s*(output|format)\\s*:", contents, perl = TRUE)
  has_chunk <- grepl("(?m)^```\\{[a-zA-Z]", contents, perl = TRUE)

  isTRUE(has_chunk || (has_yaml_header && has_notebook_format))
}

is_rstudio_notebook <- function() {
  flag <- FALSE
  # inline doesn't work in Positron
  if (is_rstudio()) {
    assert_dependency("rstudioapi")
    con <- rstudioapi::getActiveDocumentContext()
    flag <- is_rstudio_notebook_context(con[["path"]], con[["contents"]])
  }
  return(flag)
}

is_rstudio <- function() {
  identical(Sys.getenv("RSTUDIO"), "1")
}

is_positron <- function() {
  identical(Sys.getenv("POSITRON"), "1")
}

is_rstudio_notebook <- function() {
  flag <- FALSE
  # inline doesn't work in Positron
  if (is_rstudio()) {
    assert_dependency("rstudioapi")
    con <- rstudioapi::getActiveDocumentContext()[["path"]]
    if (isTRUE(grepl("\\.qmd$|\\.Rmd$", con))) {
      flag <- TRUE
    }
  }
  return(flag)
}

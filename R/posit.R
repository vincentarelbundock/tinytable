# hack to detect Rstudio vs. Positron
is_rstudio <- function() {
  tryCatch(rstudioapi::getThemeInfo(), error = function(e) FALSE)
}

is_rstudio_notebook <- function() {
  flag <- FALSE
  # inline doesn't work in Positron
  if (is_rstudio()) {
    con <- rstudioapi::getActiveDocumentContext()[["path"]]
    if (isTRUE(grepl("\\.qmd$|\\.Rmd$", con))) {
      flag <- TRUE
    }
  }
  return(flag)
}

is_posit <- function() {
  tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE)
}

is_posit_notebook <- function() {
  flag <- FALSE
  if (is_posit()) {
    con <-  rstudioapi::getSourceEditorContext()[["path"]]
    if (isTRUE(grepl("\\.qmd$|\\.Rmd$", con))) {
      flag <- TRUE
    }
  }
  return(flag)
}



is_posit <- function() {
  tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE)
}

is_posit_notebook <- function() {
  flag <- FALSE
  if (is_posit()) {
    ## this returns a .Rmd/.qmd path in Positron, but Positron doesn't support inline tables
    ## TODO: this is a very ugly hack: https://github.com/rstudio/rstudioapi/issues/310
    # con <-  rstudioapi::getSourceEditorContext()[["path"]]
    con <-  rstudioapi::getActiveDocumentContext()[["path"]]
    if (isTRUE(grepl("\\.qmd$|\\.Rmd$", con))) {
      flag <- TRUE
    }
  }
  return(flag)
}



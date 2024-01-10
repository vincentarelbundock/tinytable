template_tabularray <- function(
  # environment = getOption("tt_tabularray_environment", default = "table+tblr"),
  # extendable = getOption("tt_tabularray_extendable", default = FALSE),
  # placement = getOption("tt_tabularray_placement", default = NULL),
  # theme = getOption("tt_tabularray_theme", default = "booktabs")
  ){
  out <- readLines(system.file("templates/tblr.tex", package = "tinytable"))
  return(out)
}

#' Print a tinytable object in knitr 
#'
#' @keywords internal
#' @return A string with class `knit_asis` to be printed in Rmarkdown or Quarto documents.
#' @export
knit_print.tinytable <- function(x,
                                 output = getOption("tinytable_print_output", default = NULL),
                                 ...) {

  # lazy styles get evaluated here, at the very end
  out <- build_tt(x, output = output)

  if (meta(out)$output == "html") {
    # from htmltools:::html_preserve
    # GPL3
    inline <- grepl(out, "\n", fixed = TRUE)
    if (inline) {
      out <- sprintf("`%s`{=html}", out)
    } else {
      out <- sprintf("\n```{=html}\n%s\n```\n", out)
    }
  }

  class(out) <- "knit_asis"
  return(out)
}


#' @export
print.tinytable <- function(x,
                            output = getOption("tinytable_print_output", default = NULL),
                            ...){

  # lazy styles get evaluated here by build_tt(), at the very end
  if (meta(x, "output") == "latex") {
    out <- build_tt(x, output = "latex")
    class(out) <- "character"
    cat("\n")
    cat(out)
    cat("\n")

  } else if (meta(x, "output") == "markdown") {
    out <- build_tt(x, output = "markdown")
    cat("\n")
    cat(out, sep = "\n")

  } else if (meta(x, "output") == "html") {
    # need to change the output directory to a temporary directory 
    # for plot_tt() inline plots to show up in RStudio
    dir <- tempfile()
    x <- meta(x, "output_dir", dir)
    dir.create(dir)
    out <- build_tt(x, output = "html")
    htmlFile <- file.path(dir, "index.html")
    cat(out, file = htmlFile)
    if (isTRUE(check_dependency("rstudioapi")) && rstudioapi::isAvailable()) {
      rstudioapi::viewer(htmlFile)
    } else if (interactive()) {
      utils::browseURL(htmlFile)
    } else {
      cat("\n")
      cat(out, sep = "\n")
      cat("\n")
    }
  }

}


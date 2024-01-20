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
  # lazy styles get evaluated here, at the very end
  out <- build_tt(x, output = output)

  if (meta(out, "output") == "latex") {
    class(out) <- "character"
    cat("\n")
    cat(out)
    cat("\n")

  } else if (meta(out, "output") == "markdown") {
    cat("\n")
    cat(out, sep = "\n")

  } else if (meta(out, "output") == "html") {
    dir <- tempfile()
    dir.create(dir)
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


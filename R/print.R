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

  if (isTRUE(meta(out)$output == "html")) {
    # from htmltools:::html_preserve
    # GPL3
    inline <- grepl(out, "\n", fixed = TRUE)
    if (inline) {
      out <- sprintf("`%s`{=html}", out)
    } else {
      out <- sprintf("\n```{=html}\n%s\n```\n", out)
    }
  }

  if (isTRUE(meta(out)$output == "typst")) {
    # from htmltools:::html_preserve
    # GPL3
    inline <- grepl(out, "\n", fixed = TRUE)
    if (inline) {
      out <- sprintf("`%s`{=typst}", out)
    } else {
      out <- sprintf("\n```{=typst}\n%s\n```\n", out)
    }
  }

  class(out) <- "knit_asis"
  return(out)
}


#' Print a tinytable to console or viewer pane
#' 
#' @inheritParams tt
#' @param output One of "latex", "markdown", "html". If NULL, will guess the output based on the environment (interactive, RStudio, etc.).
#' @param ... Other arguments are ignored.
#' @return launch a browser window or cat() the table to console.
#' @export
print.tinytable <- function(x,
                            output = getOption("tinytable_print_output", default = NULL),
                            ...){

  assert_choice(output, c("latex", "markdown", "html", "typst"), null.ok = TRUE)

  if (is.null(output)) output <- meta(x, "output")

  # lazy styles get evaluated here by build_tt(), at the very end
  if (output %in% c("latex", "typst")) {
    out <- build_tt(x, output = output)
    class(out) <- "character"
    cat("\n")
    cat(out)
    cat("\n")

  } else if (output == "markdown") {
    out <- build_tt(x, output = "markdown")
    cat("\n")
    cat(out, sep = "\n")

  } else if (output == "html") {
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

  } else {
    stop("here be dragons")
  }


}


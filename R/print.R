#' Print a tinytable object in knitr 
#'
#' @keywords internal
#' @return A string with class `knit_asis` to be printed in Rmarkdown or Quarto documents.
#' @export
knit_print.tinytable <- function(x,
                                 output = getOption("tinytable_print_output", default = NULL),
                                 ...) {

  # lazy styles get evaluated here, at the very end
  x <- build_tt(x, output = output)
  out <- x@table_string

  if (isTRUE(x@output == "html")) {
    # from htmltools:::html_preserve
    # GPL3
    inline <- grepl(out, "\n", fixed = TRUE)
    if (inline) {
      out <- sprintf("`%s`{=html}", out)
    } else {
      out <- sprintf("\n```{=html}\n%s\n```\n", out)
    }
  }

  if (isTRUE(x@output == "typst")) {
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


#' Print, display, or convert a tinytable object
#' 
#' This function is called automatically by `R` whenever a `tinytable` object is anprinted to the console or in an HTML viewer pane.
#' 
#' @inheritParams tt
#' @param output format in which a Tiny Table is printed: `NULL` or one of `"latex"`, `"markdown"`, `"html"`, `"typst"`, `"dataframe"`. If `NULL`, the output is chosen based on these rules:
#' + When called from a script in non-interactive mode, the default is "markdown" (`interactive() == FALSE`).
#' + When called interactively in RStudio, the default is to display an HTML table in the viewer pane.
#' + When called interactively in another development environment, the default is "markdown". 
#' + The default print output can be changed for an entire R session by calling: `options(tinytable_print_output = "html")`
#' + The default print output can be changed for a single `tinytable` object by modifying the `output` S4 slot.
#' @param ... Other arguments are ignored.
#' @return launch a browser window or cat() the table to console.
#' @export
print.tinytable <- function(x,
                            output = getOption("tinytable_print_output", default = NULL),
                            ...){

  if (is.null(output)) {
    output <- sanitize_output(x@output)
  } else {
    output <- sanitize_output(output)
  }

  if (output == "html") {
    dir <- tempfile()
    dir.create(dir)
    x@output_dir <- dir
  }

  x <- build_tt(x, output = output)

  tab <- x@table_string

  # lazy styles get evaluated here by build_tt(), at the very end
  if (output %in% c("latex", "typst", "markdown")) {
    cat(tab, "\n")

  } else if (output == "html") {
    # need to change the output directory to a temporary directory 
    # for plot_tt() inline plots to show up in RStudio
    htmlFile <- file.path(dir, "index.html")
    cat(tab, file = htmlFile)

    if (isTRUE(interactive()) && isTRUE(check_dependency("rstudioapi")) && rstudioapi::isAvailable()) {
      rstudioapi::viewer(htmlFile)

    } else if (interactive()) {
      msg <- "Please choose a default browser with:

      options(browser = 'firefox')
      "
      if (identical(getOption("browser"), "")) stop(msg, call. = FALSE)
      utils::browseURL(htmlFile)

    } else {
      cat(tab, "\n")
    }

  } else {
    return(x@table_dataframe)
  }

  return(invisible(x))
}


setMethod("show", "tinytable", function(object) {
  print.tinytable(object, output = object@output)
})

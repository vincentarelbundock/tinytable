#' Print a tinytable object in knitr 
#'
#' @keywords internal
#' @export
knit_print.tinytable <- function(x, ...) {
  m <- meta(x)

  if (m$output == "html") {
    # from htmltools:::html_preserve
    # GPL3
    inline <- grepl(x, "\n", fixed = TRUE)
    if (inline) {
      out <- sprintf("`%s`{=html}", x)
    } else {
      out <- sprintf("\n```{=html}\n%s\n```\n", x)
    }

  } else if (m$output %in% c("latex", "markdown")) {
    out <- x
  }

  class(out) <- "knit_asis"
  return(out)
}


#' @export
print.tinytable <- function(x, ...) {
  # lazy styles get evaluated here, at the very end
  x <- eval_style(x)

  if (meta(x, "output") == "latex") {
    out <- x
    class(out) <- "character"
    cat("\n")
    cat(out)
    cat("\n")

  } else if (meta(x, "output") == "markdown") {
    cat("\n")
    cat(x, sep = "\n")

  } else if (meta(x, "output") == "html") {
    dir <- tempfile()
    dir.create(dir)
    htmlFile <- file.path(dir, "index.html")
    cat(x, file = htmlFile)
    if (isTRUE(check_dependency("rstudioapi")) && rstudioapi::isAvailable()) {
      rstudioapi::viewer(htmlFile)
    } else {
      utils::browseURL(htmlFile)
    }
  }

}


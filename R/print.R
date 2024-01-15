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
  m <- meta(x)

  if (m$output %in% c("markdown", "latex")) {
    out <- x
    class(out) <- "character"
    cat("\n")
    cat(out)
    cat("\n")

  } else if (m$output == "html") {
    dir <- tempfile()
    dir.create(dir)
    htmlFile <- file.path(dir, "index.html")
    cat(x, file = htmlFile)
    if (check_dependency("rstudioapi") && rstudioapi::isAvailable()) {
      rstudioapi::viewer(htmlFile)
    } else {
      utils::browseURL(htmlFile)
    }
  }

}


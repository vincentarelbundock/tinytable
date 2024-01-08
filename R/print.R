#' Convenience function for Rmarkdown and Quarto documents
#' @noRd
#' @export
knit_print.tinytable_html <- function(x, ...) {
  # from htmltools:::html_preserve
  # GPL3
  inline <- grepl(x, "\n", fixed = TRUE)
  if (inline) {
    out <- sprintf("`%s`{=html}", x)
  } else {
    out <- sprintf("\n```{=html}\n%s\n```\n", x)
  }
  # from knitr::asis_output
  # GPL3
  class(out) <- "knit_asis"
  return(out)
}


#' Convenience function for Rmarkdown and Quarto documents
#' @noRd
#' @export
knit_print.tinytable_latex <- function(x, ...) {
  out <- x
  # from knitr::asis_output
  # GPL3
  class(out) <- "knit_asis"
  return(out)
}

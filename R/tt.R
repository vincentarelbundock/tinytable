#' @export
tt <- function(x,
               output = NULL,
               caption = NULL,
               hlines = "booktabs",
               vlines = NULL,
               tabularray_extendable = getOption("tt_extendable", default = FALSE),
               tabularray_placement = getOption("tt_placement", default = NULL),
               tabularray_inner = NULL,
               tabularray_outer = NULL) {
  assert_data_frame(x)
  assert_string(caption, null.ok = TRUE)
  assert_choice(output, c("tblr", "html"), null.ok = TRUE)


  if (is.null(output)) {
    if (isTRUE(check_dependency("knitr"))) {
      if (isTRUE(knitr::is_latex_output())) {
        output <- "tblr"
      } else if (isTRUE(knitr::is_html_output())) {
        output <- "html"
      } else {
        output <- "tblr"
      }
    }
  }

  if (output == "tblr") {
    out <- spec_table_latex(x,
      caption = caption,
      # hlines = hlines,
      # vlines = vlines,
      tabularray_extendable = tabularray_extendable,
      tabularray_placement = tabularray_placement,
      tabularray_inner = tabularray_inner,
      tabularray_outer = tabularray_outer)
  }
  return(out)
}

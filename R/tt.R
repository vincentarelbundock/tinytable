#' Draw a table
#'
#' @param output "markdown", "latex", or "html". If `output` is `NULL`, then:
#' * "html" if `knitr::is_html_output()` is `TRUE`
#' * "latex" if `knitr::is_latex_output()` is `TRUE`
#' * Otherwise determined by setting a global option: `options(tt_output_default = "markdown")`
#' @template tabularray
#' @export
tt <- function(x,
               output = NULL,
               caption = NULL,
               latex = latexOptions(),   
               html = htmlOptions()
               ) {

  # sanity checks
  output <- sanitize_output(output)
  assert_data_frame(x)
  assert_string(caption, null.ok = TRUE)
  if (!inherits(latex, "tinytable_latexOptions")) {
    msg <- "The `latex` argument must be a call to the `latexOptions()` function. See `?tt` and `?latexOptions` for details and examples."
    stop(msg, call. = FALSE)
  }

  if (!inherits(html, "tinytable_htmlOptions")) {
    msg <- "The `html` argument must be a call to the `htmlOptions()` function. See `?tt` and `?htmlOptions` for details and examples."
    stop(msg, call. = FALSE)
  }

  # build table
  if (output == "latex") {
    out <- tt_latex(x,
      caption = caption,
      settings = latex
    )

  } else if (output == "html"){
    out <- tt_html(x,
      caption = caption,
      settings = html
    )

  } else {
    out <- tt_markdown(x,
      caption = caption
    )
  }

  return(out)
}

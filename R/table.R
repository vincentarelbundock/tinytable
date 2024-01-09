#' Draw a table
#'
#' @param x A data frame
#' @param output "markdown", "latex", or "html". If `output` is `NULL`, then:
#' * "html" if `knitr::is_html_output()` is `TRUE`
#' * "latex" if `knitr::is_latex_output()` is `TRUE`
#' * Otherwise determined by setting a global option: `options(tt_output_default = "markdown")`
#' @param latex Options to customize   LaTeX tables. See `?tabularrayOptions` and the examples section below.
#' @param html Options to customize HTML tables. See `?bootstrapOptions` and the examples section below.
#' @template tabularray
#' @export
ibTable <- function(x,
                    output = NULL,
                    caption = NULL,
                    options = ibOptions()
                    ) {

  # sanity checks
  output <- sanitize_output(output)
  assert_data_frame(x)
  assert_string(caption, null.ok = TRUE)
  if (!inherits(options, "ibOptions")) {
    msg <- "The `options` argument must be a call to the `ibOptions()` or `tabularrayOptions()` function. See `?ibOptions` and `?tabularrayOptions` for details and examples."
    stop(msg, call. = FALSE)
  }

  # build table
  if (output == "latex") {
    out <- IttyBittyTable_latex(x, caption = caption, settings = options)

  } else if (output == "html"){
    out <- IttyBittyTable_html(x, caption = caption, settings = options)

  } else {
    out <- IttyBittyTable_markdown(x, caption = caption)
  }

  return(out)
}

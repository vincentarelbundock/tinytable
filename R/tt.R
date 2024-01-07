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

  } else {
    out <- tt_html(x,
      caption = caption,
      settings = html
    )
  }

  return(out)
}

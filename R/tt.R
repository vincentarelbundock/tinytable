#' @export
tt <- function(x,
               output = NULL,
               caption = NULL,
               latex = latexOptions(),   
               html = htmlOptions()
               ) {

  assert_data_frame(x)
  assert_string(caption, null.ok = TRUE)
  output <- sanitize_output(output)

  if (!inherits(latex, "tinytable_latexOptions")) {
    msg <- "The `latex` argument must be a call to the `latexOptions()` function. See `?tt` and `?latexOptions` for details and examples."
    stop(msg, call. = FALSE)
  }
  if (!inherits(latex, "tinytable_htmlOptions")) {
    msg <- "The `html` argument must be a call to the `htmlOptions()` function. See `?tt` and `?htmlOptions` for details and examples."
    stop(msg, call. = FALSE)
  }

  if (output == "tblr") {
    out <- tt_latex(x,
      caption = caption,
      tabularray_extendable = tabularray_extendable,
      tabularray_placement = tabularray_placement,
      tabularray_inner = tabularray_inner,
      tabularray_outer = tabularray_outer
    )

  } else {
    out <- tt_html(x,
      caption = caption,
      bootstrap_css = bootstrap_css,
      bootstrap_class = bootstrap_class
    )

  }

  return(out)
}

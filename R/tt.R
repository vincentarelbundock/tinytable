#' @export
tt <- function(x,
               output = NULL,
               caption = NULL,
               hlines = "booktabs",
               vlines = NULL,
               bootstrap_class = "table",
               tabularray_extendable = getOption("tt_extendable", default = FALSE),
               tabularray_placement = getOption("tt_placement", default = NULL),
               tabularray_inner = NULL,
               tabularray_outer = NULL) {

  assert_data_frame(x)
  assert_string(caption, null.ok = TRUE)
  assert_string(bootstrap_class, null.ok = FALSE)
  output <- sanitize_output(output)

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
      bootstrap_class = bootstrap_class
    )

  }

  return(out)
}

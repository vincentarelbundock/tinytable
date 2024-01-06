#' @export
tt <- function(x,
               output = "tblr",
               caption = NULL,
               hlines = "booktabs",
               vlines = NULL,
               tabularray_extendable = getOption("tt_extendable", default = FALSE),
               tabularray_placement = getOption("tt_placement", default = NULL),
               tabularray_inner = NULL,
               tabularray_outer = NULL) {
  checkmate::assert_data_frame(x)
  checkmate::assert_string(caption, null.ok = TRUE)
  checkmate::assert_choice(output, c("tblr", "html"))
  if (output == "tblr") {
    out <- spec_table_latex(x,
      caption = caption,
      hlines = hlines,
      vlines = vlines,
      tabularray_extendable = tabularray_extendable,
      tabularray_placement = tabularray_placement,
      tabularray_inner = tabularray_inner,
      tabularray_outer = tabularray_outer)
  }
  return(out)
}

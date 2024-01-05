#' @export
tt <- function(x,
               output = "tblr",
               inner = NULL,
               outer = NULL) {

  checkmate::assert_data_frame(x)
  checkmate::assert_choice(output, c("tblr", "html"))

  if (output == "tblr") {
    out <- spec_table_latex(x, inner = inner, outer = outer)
  }
  return(out)

}

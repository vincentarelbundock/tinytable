#' @export
tt <- function(x,
               format = "tblr",
               inner = NULL,
               outer = NULL) {

  checkmate::assert_data_frame(x)
  checkmate::assert_choice(format, c("tblr", "html"))

  if (format == "tblr") {
    out <- spec_table_latex(x, inner = inner, outer = outer)
  }
  return(out)

}

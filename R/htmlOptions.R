#' @export
htmlOptions <- function(...) {
  out <- list()
  class(out) <- c("tinytable_htmlOptions", class(out))
  return(out)
}


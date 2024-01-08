#' HTML Bootstrap Options
#'
#' @param class TODO
#' @param css TODO
#' @export
htmlOptions <- function(class = "table",
                        css = "") {

  template <- readLines(system.file("templates/bootstrap.html", package = "IttyBittyTable"))
  out <- list(
    template = template,
    class = class,
    css = css
  )
  class(out) <- c("IttyBittyTable_htmlOptions", class(out))
  return(out)
}

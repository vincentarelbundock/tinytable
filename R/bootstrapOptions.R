#' HTML Bootstrap Options
#'
#' @param class TODO
#' @param css TODO
#' @export
bootstrapOptions <- function(class = "table", css = "", ...) {

  template <- readLines(system.file("templates/bootstrap.html", package = "IttyBittyTable"))
  out <- list(
    template = template,
    class = class,
    css = css
  )
  class(out) <- c("IttyBittyTable_bootstrapOptions", class(out))
  return(out)
}

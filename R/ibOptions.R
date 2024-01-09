#' export
ibOptions <- function(bold = TRUE, ...) {

  tabularray <- list()
  bootstrap <- list(css = NULL)

  assert_flag(bold)
  if (isTRUE(bold)) {
    tabularray <- modifyList(tabularray, list("cmd" = "\\bfseries"))
    bootstrap$css <- c(
      bootstrap$css, 
      paste(strrep(" ", 10), "font-weight: bold;")
    )
  }

  out <- list(
    tabularray = do.call(tabularrayOptions, tabularray),
    bootstrap = do.call(bootstrapOptions, bootstrap)
  )

  class(out) <- "ibOptions"

  return(out)
}

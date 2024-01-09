#' export
ibOptions <- function(bold = TRUE, color = NULL, ...) {

  tabularray <- list()
  bootstrap <- list(css = NULL)

  assert_flag(bold)
  if (isTRUE(bold)) {
    tabularray <- modifyList(tabularray, list("cmd" = "\\bfseries"))
    bootstrap$css <- c(bootstrap$css, "font-weight: bold")
  }

  assert_string(color, null.ok = TRUE)
  if (!is.null(color)) {
    tabularray <- modifyList(tabularray, list("fg" = color))
    bootstrap$css <- c(bootstrap$css, sprintf("color: %s", color))
  }

  out <- list(
    tabularray = do.call(tabularrayOptions, tabularray),
    bootstrap = do.call(bootstrapOptions, bootstrap)
  )
  class(out) <- "ibOptions"

  return(out)
}

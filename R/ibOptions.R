#' export
ibOptions <- function(color = NULL,
                      background = NULL,
                      bold = FALSE,
                      italic = FALSE,
                      monospace = FALSE,
                      fontsize = NULL,
                      smallcaps = FALSE,
                      width = NULL
                      ) {

  # TODO: cmd needs to be concatenated when there are many

  tabularray <- list()
  bootstrap <- list(css = "")

  assert_flag(bold)
  if (isTRUE(bold)) {
    tabularray <- modifyList(tabularray, list("cmd" = "\\bfseries"))
    bootstrap$css <- c(bootstrap$css, "font-weight: bold")
  }

  assert_flag(italic)
  if (isTRUE(italic)) {
    tabularray <- modifyList(tabularray, list("cmd" = "\\textit"))
    bootstrap$css <- c(bootstrap$css, "font-style: italic")
  }

  assert_flag(monospace)
  if (isTRUE(monospace)) {
    tabularray <- modifyList(tabularray, list("cmd" = "\\texttt"))
    bootstrap$css <- c(bootstrap$css, "font-family: monospace")
  }

  # TODO: broken in HTML
  # assert_flag(smallcaps)
  # if (isTRUE(smallcaps)) {
  #   tabularray <- modifyList(tabularray, list("cmd" = "\\textsc"))
  #   bootstrap$css <- c(bootstrap$css, "font-variant-caps: smallcaps")
  # }

  assert_string(color, null.ok = TRUE)
  if (!is.null(color)) {
    tabularray <- modifyList(tabularray, list("fg" = color))
    bootstrap$css <- c(bootstrap$css, sprintf("color: %s", color))
  }

  assert_string(background, null.ok = TRUE)
  if (!is.null(background)) {
    tabularray <- modifyList(tabularray, list("fg" = background))
    bootstrap$css <- c(bootstrap$css, sprintf("background-color: %s", background))
  }

  assert_string(fontsize, null.ok = TRUE)
  if (!is.null(fontsize)) {
    # tabularray <- modifyList(tabularray, list("c" = colspan))
    bootstrap$css <- c(bootstrap$css, sprintf("font-size: %s", fontsize))
  }

  assert_string(width, null.ok = TRUE)
  if (!is.null(width)) {
    tabularray <- modifyList(tabularray, list("wd" = width))
    bootstrap$css <- c(bootstrap$css, sprintf("width: %s", width))
  }

  # NULL creates problems
  if (isTRUE(length(bootstrap$css) == 1 && bootstrap$css == "")) {
    bootstrap$css <- NULL
  }


  out <- list(
      tabularray = do.call(tabularrayOptions, tabularray),
      bootstrap = do.call(bootstrapOptions, bootstrap)
  )

  class(out) <- "ibOptions"

  return(out)
}

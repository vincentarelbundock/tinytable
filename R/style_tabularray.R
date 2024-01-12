#' Style LaTeX tables using tabularray
#'
#' @template latex_preamble
#' @template tabularray
#' @export
style_tabularray <- function(x,
                             inner = NULL,
                             outer = NULL,
                             body = NULL) {

  # do not select on tabularray because this gets called in tt() before class assignment
  if (inherits(x, "tinytable_bootstrap") || inherits(x, "tinytable_markdown")) return(x)
  if (is.null(inner) && is.null(outer) && is.null(body)) return(x)

  assert_string(inner, null.ok = TRUE)
  assert_string(outer, null.ok = TRUE)

  att <- attributes(x)    
  out <- strsplit(x, "\n")[[1]]


  if (!is.null(inner)) {
    idx <- grep("% tabularray inner close", out)
    out <- c(
      out[1:(idx - 1)],
      # empty lines can break latex
      trimws(inner),
      out[idx:length(out)])
  }

  if (!is.null(outer)) {
    idx <- grep("% tabularray outer close", out)
    out <- c(
      out[1:(idx - 1)],
      # empty lines can break latex
      trimws(outer),
      out[idx:length(out)])
  }

  if (!is.null(body)) {
    idx <- grep("% tabularray inner close", out)
    out <- c(
      out[1:idx],
      # empty lines can break latex
      trimws(body),
      out[(idx + 1):length(out)])
  }

  # rebuild
  out <- paste(out, collapse = "\n")
  attributes(out) <- att
  class(out) <- class(x)
  return(out)
}  



## not longer used, but took a while to collect and might be useful in the future
# out <- list(
#   rows_keys = c("halign", "valign", "ht", "bg", "fg", "font", "mode", "cmd", "abovesep", "belowsep", "rowsep", "preto", "appto", "indent"),
#   columns_keys = c("halign", "valign", "wd", "co", "bg", "fg", "font", "mode", "cmd", "leftsep", "rightsep", "colsep", "preto", "appto", "indent"),
#   hborders_keys = c("pagebreak", "abovespace", "belowspace"),
#   vborders_keys = c("leftspace", "rightspace"),
#   cells_keys = c("halign", "valign", "wd", "bg", "fg", "font", "mode", "cmd", "preto", "appto"),
#   outer_specs_keys = c("baseline", "long", "tall", "expand"),
#   inner_specs_keys = c("rulesep", "hlines", "vline", "hline", "vlines", "stretch", "abovesep", "belowsep", "rowsep", "leftsep", "rightsep", "colsep", "hspan", "vspan", "baseline"),
#   span = c("r", "c")
# )

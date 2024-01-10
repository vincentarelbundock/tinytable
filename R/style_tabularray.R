#' @export
style_tabularray <- function(x,
                             inner = "",
                             outer = "") {

  # TODO: assertion on x when we have figured out which class to assign
  # TODO: modify style_tabularray() to accept inner, outer, etc. Then, sort the arguments in 
  # buckets, and feed it to style_tabularray()
  
  assert_string(inner)
  assert_string(outer)

  att <- attributes(x)    
  out <- strsplit(x, "\n")[[1]]

  # inner
  idx <- grep("% tabularray inner close", out)
  out <- c(
    out[1:(idx - 1)],
    new,
    out[idx:length(out)])

  # outer
  idx <- grep("% tabularray outer close", out)
  out <- c(
    out[1:(idx - 1)],
    new,
    out[idx:length(out)])

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

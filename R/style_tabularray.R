style_tabularray <- function(x,
                             inner = NULL,
                             outer = NULL,
                             body = NULL) {

  m <- meta(x)
  if (m$output != "latex") return(x)

  if (is.null(inner) && is.null(outer) && is.null(body)) return(x)

  assert_string(inner, null.ok = TRUE)
  assert_string(outer, null.ok = TRUE)

  att <- attributes(x)    
  out <- strsplit(x, "\n")[[1]]

  out <- style_tabularray_inner(out, inner)
  out <- style_tabularray_outer(out, outer)
  out <- style_tabularray_body(out, body)

  # important for group_tt()
  out <- meta(out, "body", body)

  return(out)
}  

style_tabularray_body <- function(x, body = NULL) {
  if (is.null(body)) return(x)
  m <- meta(x)
  out <- strsplit(x, "\n")[[1]]
  idx <- grep("% tabularray inner close", out)
  out <- c(
    out[1:idx],
    # empty lines can break latex
    trimws(body),
    out[(idx + 1):length(out)])
  out <- paste(out, collapse = "\n")
  class(out) <- class(x)
  attr(out, "tinytable_meta") <- m
  return(out)
}

style_tabularray_outer <- function(x, outer = NULL) {
  if (is.null(outer)) return(x)
  m <- meta(x)
  out <- strsplit(x, "\n")[[1]]
  outer <- trimws(outer)
  if (!grepl(",$", outer)) outer <- paste0(outer, ",")
  idx <- grep("% tabularray outer close", out)
  out <- c(
    out[1:(idx - 1)],
    # empty lines can break latex
    outer,
    out[idx:length(out)])
  out <- paste(out, collapse = "\n")
  class(out) <- class(x)
  attr(out, "tinytable_meta") <- m
  return(out)
}

style_tabularray_inner <- function(x, inner = NULL) {
  if (is.null(inner)) return(x)
  m <- meta(x)
  out <- strsplit(x, "\n")[[1]]
  inner <- trimws(inner)
  if (!grepl(",$", inner)) inner <- paste0(inner, ",")
  idx <- grep("% tabularray inner close", out)
  out <- c(
    out[1:(idx - 1)],
    # empty lines can break latex
    inner,
    out[idx:length(out)])
  out <- paste(out, collapse = "\n")
  class(out) <- class(x)
  attr(out, "tinytable_meta") <- m
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

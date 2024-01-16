style_tabularray <- function(x,
                             inner = NULL,
                             outer = NULL,
                             body = NULL) {

  m <- meta(x)
  if (m$output != "latex") return(x)
  out <- x

  out <- tabularray_insert(out, content = inner, type = "inner")
  out <- tabularray_insert(out, content = outer, type = "outer")
  out <- tabularray_insert(out, content = body, type = "body")

  # important for group_tt()
  out <- meta(out, "body", body)

  return(out)
}  

tabularray_insert <- function(x, content = NULL, type = "body") {
  if (is.null(content)) return(x)

  m <- meta(x)
  out <- strsplit(x, "\n")[[1]]
  comment <- switch(type,
                    "body" = "% tabularray inner close",
                    "outer" = "% tabularray outer close",
                    "inner" = "% tabularray inner close")
  idx <- grep(comment, out)
  
  content <- trimws(content)
  if (!grepl(",$", content)) content <- paste0(content, ",")
  
  if (type == "body") {
    out <- c(out[1:idx], content, out[(idx + 1):length(out)])
  } else {
    out <- c(out[1:(idx - 1)], content, out[idx:length(out)])
  }

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

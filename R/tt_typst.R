tt_typst <- function(x, caption, theme, width, notes, ...) {
  out <- readLines(system.file("templates/typst.typ", package = "tinytable"))
  out <- paste(out, collapse = "\n")
  attr(out, "tinytable_meta") <- meta(x)

  # body
  body <- apply(x, 2, function(k) paste0("[", k, "]"))
  if (!is.null(colnames(x))) {
    body <- rbind(paste0("[", colnames(x), "]"), body)
  }
  body <- apply(body, 1, paste, collapse = ", ")
  body <- paste(body, collapse = ",\n")
  body <- paste0(body, ",\n")
  out <- typst_insert(out, body, type = "body")

  # themes ("void" needs nothing)
  if (isTRUE(theme %in% c("default", "striped"))) {
    out <- style_typst(out, i = 1 - meta(out, "nhead"), line = "t", line_width = .1)
    out <- style_typst(out, i = 0, line = "b", line_width = .05)
    out <- style_typst(out, i = meta(out)$nrows, line = "b", line_width = .1)
  } else if (isTRUE(theme == "grid")) {
    out <- sub("auto-lines: false,", "auto-lines: true,", out)
  }
  # striped = default + background
  if (isTRUE(theme == "striped")) {
    out <- style_typst(out, i = seq(1, meta(out)$nrows, by = 2), background = "#ededed")
  } 


  class(out) <- c("tinytable", "knit_asis", class(out))
  return(out)
}


typst_insert <- function(x, content = NULL, type = "body") {
  if (is.null(content)) return(x)

  m <- meta(x)
  out <- strsplit(x, "\n")[[1]]
  comment <- switch(type,
      "lines" = "tinytable lines before",
      "style" = "tinytable cell style before",
      "body" = "tinytable cell content after"
  )
  idx <- grep(comment, out)

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
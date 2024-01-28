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

  # themes
  if (isTRUE(theme == "grid")) {
    out <- sub("auto-lines: false,", "auto-lines: true,", out)
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
      "settings" = "tinytable cell settings before",
      "body" = "tinytable cell content before"
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
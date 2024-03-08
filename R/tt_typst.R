setMethod(
  f = "tt_eval",
  signature = "tinytable_typst",
  definition = function(x, ...) {
  out <- readLines(system.file("templates/typst.typ", package = "tinytable"))
  out <- paste(out, collapse = "\n")

  # body
  body <- apply(x@table_dataframe, 2, function(k) paste0("[", k, "]"))
  if (!is.null(colnames(x)) && length(colnames(x)) > 0) {
    body <- rbind(paste0("[", colnames(x), "]"), body)
  }
  body <- apply(body, 1, paste, collapse = ", ")
  body <- paste(body, collapse = ",\n")
  body <- paste0(body, ",\n")
  out <- typst_insert(out, body, type = "body")

  x@table_string <- out

  return(x)
})


typst_insert <- function(x, content = NULL, type = "body") {
  if (is.null(content)) return(x)

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
  return(out)
}
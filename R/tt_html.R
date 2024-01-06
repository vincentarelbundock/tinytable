tt_html <- function(x, bootstrap_class, ...) {
  template <- readLines(system.file("templates/bootstrap.html", package = "tinytable"))

  # bootstrap class
  template <- gsub(
    "$TINYTABLE_BOOTSTRAP_CLASS",
    bootstrap_class,
    template,
    fixed = TRUE)

  # unique table ID
  id <- sample(c(0:9, letters), 30, replace = TRUE)
  id <- tools::toTitleCase(paste(id, collapse = ""))
  template <- gsub(
    "$TINYTABLE_TABLE_ID",
    paste0(c("tinytable", id), collapse = ""),
    template,
    fixed = TRUE)
  template <- gsub(
    "updateRows",
    paste0("updateRows", id),
    template,
    fixed = TRUE)
  template <- gsub(
    "updateColumns",
    paste0("updateColumns", id),
    template,
    fixed = TRUE)

  # header
  idx <- grep("$TINYTABLE_BOOTSTRAP_HEADER", template, fixed = TRUE)
  if (!is.null(colnames(x))) {
    header <- sprintf('    <th scope="col">%s</th>', colnames(x))
    header <- c("<thead>", "  <tr>", header, "  </tr>", "</thead>")
    header <- paste(strrep(" ", 11), header)
  } else {
    header <- NULL
  }
  template <- c(
    template[1:(idx - 1)],
    header,
    template[(idx + 1):length(template)]
   )
  # body
  makerow <- function(x) {
    out <- c(
      "  <tr>",
      sprintf('    <td>%s</td>', x),
      "  </tr>")
    return(out)
  }
  body <- apply(x, 1, makerow)
  idx <- grep("$TINYTABLE_BOOTSTRAP_BODY", template, fixed = TRUE)
  template <- c(
    template[1:(idx - 1)],
    paste(strrep(" ", 13), body),
    template[(idx + 1):length(template)]
   )

  out <- paste(template, collapse = "\n")
  attr(out, "ncol") <- ncol(x)
  attr(out, "nrow") <- nrow(x)
  class(out) <- c("tinytable_html", "knit_asis", class(out))
  return(out)
}



bootstrap_setting <- function(x, new, component = "row") {
  att <- attributes(x)    
  out <- strsplit(x, "\n")[[1]]
  if (component == "row") {
    idx <- grep("tinytable rows before this", out)
  } else if (component == "column") {
    idx <- grep("tinytable columns before this", out)
  }
  out <- c(
    out[1:(idx - 1)],
    new,
    out[idx:length(out)]
  )
  out <- paste(out, collapse = "\n")
  attributes(out) <- att
  class(out) <- class(x)
  return(out)
}

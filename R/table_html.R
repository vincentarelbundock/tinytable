IttyBittyTable_html <- function(x, caption, settings) {

  if (inherits(settings, "ibOptions")) {
    settings <- settings$bootstrap
  }

  template <- settings$template

  # insert bootstrap class and css
  template <- gsub(
    "$IttyBittyTable_BOOTSTRAP_CLASS",
    settings$class,
    template,
    fixed = TRUE)
  template <- gsub(
    "$IttyBittyTable_BOOTSTRAP_CSS",
    settings$css,
    template,
    fixed = TRUE)

  # caption
  if (is.null(caption)) {
    template <- sub(
      "$IttyBittyTable_BOOTSTRAP_CAPTION",
      "",
      template,
      fixed = TRUE
    )
  } else {
    template <- sub(
      "$IttyBittyTable_BOOTSTRAP_CAPTION",
      sprintf("<caption>%s</caption>", caption),
      template,
      fixed = TRUE
    )
  }

  # (pseudo-)unique table IDs
  id <- get_id("")
  template <- gsub(
    "$IttyBittyTable_TABLE_ID",
    paste0(c("IttyBittyTable", id), collapse = ""),
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
  template <- gsub(
    "updateCells",
    paste0("updateCells", id),
    template,
    fixed = TRUE)

  # header
  idx <- grep("$IttyBittyTable_BOOTSTRAP_HEADER", template, fixed = TRUE)
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
  idx <- grep("$IttyBittyTable_BOOTSTRAP_BODY", template, fixed = TRUE)
  template <- c(
    template[1:(idx - 1)],
    paste(strrep(" ", 13), body),
    template[(idx + 1):length(template)]
   )

  out <- paste(template, collapse = "\n")
  attr(out, "ncol") <- ncol(x)
  attr(out, "nrow") <- nrow(x)
  class(out) <- c("IttyBittyTable_html", "knit_asis", class(out))
  return(out)
}



bootstrap_setting <- function(x, new, component = "row") {
  att <- attributes(x)    
  out <- strsplit(x, "\n")[[1]]
  if (component == "row") {
    idx <- grep("IttyBittyTable rows before this", out)
  } else if (component == "column") {
    idx <- grep("IttyBittyTable columns before this", out)
  } else if (component == "cell") {
    idx <- grep("IttyBittyTable cells before this", out)
  } else if (component == "css") {
    idx <- grep("IttyBittyTable css before this", out)
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

tt_bootstrap <- function(x, caption, theme, width, notes) {

  template <- template_bootstrap(theme)


  # caption
  if (is.null(caption)) {
    template <- sub(
      "$tinytable_BOOTSTRAP_CAPTION",
      "",
      template,
      fixed = TRUE
    )
  } else {
    template <- sub(
      "$tinytable_BOOTSTRAP_CAPTION",
      sprintf("<caption>%s</caption>", caption),
      template,
      fixed = TRUE
    )
  }

  # note
  if (is.null(notes)) {
    template <- sub(
      "$tinytable_BOOTSTRAP_NOTE",
      "",
      template,
      fixed = TRUE
    )
  } else {
    notes_tmp <- NULL
    for (k in seq_along(notes)) {
      if (!is.null(names(notes))) {
        tmp <- sprintf("<tr><td colspan='%s'><sup>%s</sup> %s</td></tr>", ncol(x), names(notes)[k], notes[k])
      } else {
        tmp <- sprintf("<tr><td colspan='%s'>%s</td></tr>", ncol(x), notes[k])
      }
      notes_tmp <- c(notes_tmp, tmp)
    }
    notes <- paste(notes_tmp, collapse = "\n")
    notes <- paste0("<tfoot>", notes, "</tfoot>")
    template <- sub(
      "$tinytable_BOOTSTRAP_NOTE",
      notes,
      template,
      fixed = TRUE
    )
  }

  # width
  if (is.numeric(width)) {
    template <- sub(
      "width: auto;",
      sprintf('table-layout: fixed; width: %s%% !important;', round(width * 100)),
      template,
      fixed = TRUE
    )
  }

  # (pseudo-)unique table IDs
  id <- get_id("")

  # table and styling function in JS must have different names when there is more than one table on a page.
  template <- gsub("styleCell", paste0("styleCell_", id), template, fixed = TRUE)
  template <- gsub("$tinytable_TABLE_ID", paste0("tinytable_", id), template, fixed = TRUE)

  # header
  idx <- grep("$tinytable_BOOTSTRAP_HEADER", template, fixed = TRUE)
  if (!is.null(colnames(x))) {
    header <- sprintf('    <th scope="col">%s</th>', colnames(x))
    header <- c("  <tr>", header, "  </tr>")
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
  idx <- grep("$tinytable_BOOTSTRAP_BODY", template, fixed = TRUE)
  template <- c(
    template[1:(idx - 1)],
    paste(strrep(" ", 13), body),
    template[(idx + 1):length(template)]
   )

  out <- paste(template, collapse = "\n")
  attr(out, "nhead") <- if (is.null(colnames(x))) 0 else 1
  attr(out, "ncol") <- ncol(x)
  attr(out, "nrow") <- nrow(x)
  class(out) <- c("tinytable_bootstrap", "knit_asis", class(out))
  return(out)
}



bootstrap_setting <- function(x, new, component = "row") {
  att <- attributes(x)    
  out <- strsplit(x, "\n")[[1]]
  if (component == "row") {
    idx <- grep("tinytable rows before this", out)
  } else if (component == "column") {
    idx <- grep("tinytable columns before this", out)
  } else if (component == "cell") {
    idx <- grep("tinytable cells before this", out)
  } else if (component == "css") {
    idx <- grep("</style>", out, fixed = TRUE)
  } else if (component == "newrows") {
    idx <- grep("tinytable new rows before this", out)
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

setMethod(
  f = "tt_eval",
  signature = "tinytable_bootstrap",
  definition = function(x, ...) {

  template <- readLines(system.file("templates/bootstrap.html", package = "tinytable"))

  # caption
  if (length(x@caption) != 1) {
    template <- sub(
      "$tinytable_BOOTSTRAP_CAPTION",
      "",
      template,
      fixed = TRUE
    )
  } else {
    template <- sub(
      "$tinytable_BOOTSTRAP_CAPTION",
      sprintf("<caption>%s</caption>", x@caption),
      template,
      fixed = TRUE
    )
  }

  # note
  if (length(x@notes) == 0) {
    template <- sub(
      "$tinytable_BOOTSTRAP_NOTE",
      "",
      template,
      fixed = TRUE
    )
  } else {
    notes_tmp <- NULL
    for (k in seq_along(x@notes)) {
      if (!is.null(names(x@notes))) {
        if (is.list(x@notes[[k]])) {
          tmp <- sprintf("<tr><td colspan='%s'><sup>%s</sup> %s</td></tr>",
            ncol(x),
            names(x@notes)[k],
            x@notes[[k]]$text)
        # note is a string
        } else {
          tmp <- sprintf("<tr><td colspan='%s'><sup>%s</sup> %s</td></tr>",
            ncol(x),
            names(x@notes)[k],
            x@notes[k])
        }
      } else {
        tmp <- sprintf("<tr><td colspan='%s'>%s</td></tr>", ncol(x), x@notes[[k]])
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
  if (length(x@width) == 1) {
    template <- sub(
      "width: auto;",
      sprintf('table-layout: fixed; width: %s%% !important;', round(x@width * 100)),
      template,
      fixed = TRUE
    )
  }

  # (pseudo-)unique table IDs
  id <- get_id("")

  # table and styling function in JS must have different names when there is more than one table on a page.
  template <- gsub("styleCell", paste0("styleCell_", id), template, fixed = TRUE)
  template <- gsub("spanCell", paste0("spanCell_", id), template, fixed = TRUE)
  template <- gsub("$tinytable_TABLE_ID", paste0("tinytable_", id), template, fixed = TRUE)

  # header
  idx <- grep("$tinytable_BOOTSTRAP_HEADER", template, fixed = TRUE)
  if (length(colnames(x)) > 0) {
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
  makerow <- function(k) {
    out <- c(
      "  <tr>",
      sprintf('    <td>%s</td>', k),
      "  </tr>")
    return(out)
  }
  body <- apply(x@table_dataframe, 1, makerow)
  idx <- grep("$tinytable_BOOTSTRAP_BODY", template, fixed = TRUE)
  template <- c(
    template[1:(idx - 1)],
    paste(strrep(" ", 13), body),
    template[(idx + 1):length(template)]
   )

  out <- paste(template, collapse = "\n")

  # before style_eval()
  x@table_string <- out

  if (is.null(x@theme[[1]]) || is.function(x@theme[[1]]) || isTRUE("default" %in% x@theme[[1]])) {
    x <- style_eval(x, bootstrap_class = "table table-borderless",
      i = 0, line = "b", line_color = "#d3d8dc", line_width = .1)
  } else if ("bootstrap" %in% x@theme[[1]]) {
    x <- style_eval(x, bootstrap_class = "table")
  } else if ("striped" %in% x@theme[[1]]) {
    x <- style_eval(x, bootstrap_class = "table table-striped")
  } else if ("grid" %in% x@theme[[1]]) {
    x <- style_eval(x, bootstrap_class = "table table-bordered")
  } else if ("void" %in% x@theme[[1]]) {
    x <- style_eval(x, bootstrap_class = "table table-borderless")
  }

  return(x)
})



bootstrap_setting <- function(x, new, component = "row") {
  att <- attributes(x)    
  out <- strsplit(x, "\n")[[1]]
  if (component == "row") {
    idx <- grep("tinytable rows before this", out)
  } else if (component == "column") {
    idx <- grep("tinytable columns before this", out)
  } else if (component == "cell") {
    idx <- utils::tail(grep("</script>", out, fixed = TRUE), 1)
    # idx <- grep("tinytable cells before this", out)
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

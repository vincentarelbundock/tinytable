init_html <- function(x) {
  template <- readLines(system.file("templates/bootstrap.html", package = "tinytable"))
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
  writeLines(template, con = "trash.html")
}
x <- mtcars[1:3, 1:4]
colnames(x) <- NULL
init_html(x) 


style_columns_latex <- function(j,
                                halign = NULL,
                                valign = NULL,
                                wd = NULL,
                                fg = NULL,
                                bg = NULL,
                                bold = FALSE,
                                italic = FALSE,
                                monospace = FALSE,
                                smallcaps = FALSE) {

  js_code <- ""

  if (!is.null(halign)) {
    js_code <- paste(js_code, sprintf("table.rows[i].cells[%d].style.textAlign = '%s';\n", j, halign), sep = "")

  }

  if (!is.null(valign)) {
    js_code <- paste(js_code, sprintf("table.rows[i].cells[%d].style.verticalAlign = '%s';\n", j, valign), sep = "")
  }


  if (!is.null(wd)) {
    js_code <- paste(js_code, sprintf("table.rows[i].cells[%d].style.width = '%s';\n", j, wd), sep = "")
  }

  if (!is.null(fg)) {
    js_code <- paste(js_code, sprintf("table.rows[i].cells[%d].style.color = '%s';\n", j, fg), sep = "")
  }

  if (!is.null(bg)) {
    js_code <- paste(js_code, sprintf("table.rows[i].cells[%d].style.backgroundColor = '%s';\n", j, bg), sep = "")
  }

  if (bold) {
    js_code <- paste(js_code, sprintf("table.rows[i].cells[%d].style.fontWeight = 'bold';\n", j), sep = "")
  }

  if (italic) {
    js_code <- paste(js_code, sprintf("table.rows[i].cells[%d].style.fontStyle = 'italic';\n", j), sep = "")
  }

  if (monospace) {
    js_code <- paste(js_code, sprintf("table.rows[i].cells[%d].style.fontFamily = 'monospace';\n", j), sep = "")
  }


  if (smallcaps) {
    js_code <- paste(js_code, sprintf("table.rows[i].cells[%d].style.fontVariant = 'small-caps';\n", j), sep = "")
  }

  return(js_code)
}


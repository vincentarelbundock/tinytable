#' Save a Tiny Table to File
#'
#' This function saves an object of class tinytable to a specified file and format, with an option to overwrite existing files.
#'
#' @param x The tinytable object to be saved.
#' @param output String or file path. 
#' + If `output` is "markdown", "latex", or "html", the table is returned in a string as an `R` object. 
#' + If `output` is a valid file path, the table is saved to file. The supported extensions are: .html, .png, .pdf, .tex and .md (with aliases .txt, .Rmd and .qmd).
#' @param overwrite A logical value indicating whether to overwrite an existing file. 
#' @return A string or `TRUE` when the table is written to file.
#' @export
#' @examples
#'
#' library(tinytable)
#' filename <- file.path(tempdir(), "table.tex")
#' tt(mtcars[1:4, 1:4]) |> save_tt(filename)
#'
save_tt <- function(x, output, overwrite = FALSE) {
  m <- meta(x)

  assert_string(output)
  assert_flag(overwrite)
  if (file.exists(output) && !overwrite) {
    stop("File already exists and overwrite is set to FALSE.", call. = FALSE)
  }
  if (is.null(m)) {
    stop("`x` must be an object produced by the `tinytable::tt()` function.", call. = FALSE)
  }

  if (identical(output, "markdown")) {
    out <- build_tt(x, output = "markdown")
    return(as.character(out))
  } else if (identical(output, "html")) {
    out <- build_tt(x, output = "html")
    return(as.character(out))
  } else if (identical(output, "latex")) {
    out <- build_tt(x, output = "latex")
    return(as.character(out))
  }


  file_ext <- tools::file_ext(output)

  output_format <- switch(file_ext,
                          "png" = "html",
                          "html" = "html",
                          "pdf" = "latex",
                          "tex" = "latex",
                          "md" = "markdown",
                          "Rmd" = "markdown",
                          "qmd" = "markdown",
                          "txt" = "markdown",
                          "docx" = "markdown",
                          stop("The supported file extensions are: .png, .html, .pdf, .tex, and .md.", call. = FALSE))

  # evaluate styles at the very end of the pipeline, just before writing
  x <- build_tt(x, output = output_format)

  if (file_ext %in% c("html", "tex", "md", "Rmd", "qmd", "txt")) {
    write(x, file = output)

  } else if (file_ext == "png") {
    assert_dependency("webshot2")
    d <- tempdir()
    f <- file.path(d, "index.html")
    write(x, file = f)
    webshot2::webshot(
                      f,
                      file = output,
                      selector = "body > div > table",
                      zoom = 4)

  } else if (file_ext == "pdf") {
    assert_dependency("tinytex")
    # \documentclass{standalone} does not support \begin{table}
    tmp <- strsplit(x, "\\n")[[1]]
    tmp <- tmp[!grepl("\\begin{table}", tmp, fixed = TRUE)]
    tmp <- tmp[!grepl("\\end{table}", tmp, fixed = TRUE)]
    tmp <- paste(tmp, collapse = "\n")
    tmp <- sprintf("
\\documentclass{standalone}
\\usepackage{tabularray}
\\usepackage{float}
\\usepackage{codehigh}
\\usepackage[normalem]{ulem}
\\UseTblrLibrary{booktabs}
\\newcommand{\\tinytableTabularrayUnderline}[1]{\\underline{#1}}
\\newcommand{\\tinytableTabularrayStrikeout}[1]{\\sout{#1}}
\\NewTableCommand{\\tinytableDefineColor}[3]{\\definecolor{#1}{#2}{#3}}
\\begin{document}
%s
\\end{document}",
                         tmp)
    d <- tempdir()
    f <- file.path(d, "index.tex")
    write(tmp, f)
    tinytex::xelatex(f, pdf_file = output)

    } else if (file_ext == "docx") {
      assert_dependency("pandoc")
      tmp <- markdown_hlines(x)
      fn <- file.path(tempdir(), "temp.md")
      writeLines(x, fn)
      writeLines(x, "~/Downloads/trash.md")
      pandoc::pandoc_convert(file = fn, to = "docx", output = output)
    }

  return(invisible(TRUE))

}


# insert horizontal rules everywhere (important for word)
markdown_hlines <- function(x) {
  rule_line <- grid_line(meta(x, "col_widths"), "-")
  lines <- strsplit(x, split = "\\n")[[1]]
  if (length(lines) > 1) {
    for (idlines in length(lines):2) {
      if (!startsWith(lines[idlines - 1], "+") && !startsWith(lines[idlines], "+")) {
        lines <- c(lines[1:(idlines - 1)], rule_line, lines[idlines:length(lines)])
      }
    }
  }
  out <- paste(lines, collapse = "\n")
  return(out)
}

#' Save a Tiny Table to File
#'
#' This function saves an object of class tinytable to a specified file and format, with an option to overwrite existing files.
#'
#' @param x The tinytable object to be saved.
#' @param filename A string representing the path to the file where the object should be saved. The supported file formats are: .html, .png, .md, .pdf, and .tex.
#' @param overwrite A logical value indicating whether to overwrite an existing file. 
#' @return invisible(TRUE)
#' @export
#' @examples
#' \dontrun{
#'
#' library(tinytable)
#' tab  <- tt(mtcars[1:4, 1:4])
#' save_tt(tt, "path/to/file.txt")
#'
#' }
#'
save_tt <- function(x, filename, overwrite = FALSE) {
m <- meta(x)

assert_string(filename)
assert_flag(overwrite)
if (file.exists(filename) && !overwrite) {
  stop("File already exists and overwrite is set to FALSE.", call. = FALSE)
}
if (is.null(m)) {
  stop("`x` must be an object produced by the `tinytable::tt()` function.", call. = FALSE)
}

file_ext <- tools::file_ext(filename)

sanity_file_extension(x, file_ext)

# evaluate styles at the very end of the pipeline, just before writing
x <- build_tt(x)

if (file_ext %in% c("html", "tex", "md", "Rmd", "qmd", "txt")) {
  write(x, file = filename)

} else if (file_ext == "png") {
  assert_dependency("webshot2")
  d <- tempdir()
  f <- file.path(d, "index.html")
  write(x, file = f)
  webshot2::webshot(
    f,
    file = filename,
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
  tinytex::xelatex(f, pdf_file = filename)
}

return(invisible(TRUE))
}


sanity_file_extension <- function(x, file_ext) {
  m <- meta(x)

  # Define the expected output for each extension
  expected_output <- switch(file_ext,
  "png" = "html",
  "html" = "html",
  "pdf" = "latex",
  "tex" = "latex",
  "md" = "markdown",
  "Rmd" = "markdown",
  "qmd" = "markdown",
  "txt" = "markdown",
  stop("Unsupported file extension", call. = FALSE))

  # Check if the actual output matches the expected output
  if (!is.null(m) && !is.null(m$output) && m$output != expected_output) {
    stop(paste("For", file_ext, "files, the `output` argument should be:", expected_output), call. = FALSE)
  }

  # If everything is fine, return a success message or perform other actions
  return(paste("File extension and output format are compatible."))
}


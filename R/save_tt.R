#' Save a Tiny Table to File
#'
#' This function saves an object of class tinytable to a specified file and format, with an option to overwrite existing files.
#'
#' @param x The tinytable object to be saved.
#' @param output String or file path.
#' + If `output` is "markdown", "latex", "html", "html_portable", "typst", or "tabulator", the table is returned in a string as an `R` object.
#' + If `output` is a valid file path, the table is saved to file. The supported extensions are: .docx, .html, .png, .pdf, .tex, .typ, and .md (with aliases .txt, .Rmd and .qmd).
#' + If `output` is "html_portable" or the global option `tinytable_html_portable` is `TRUE`,
#' the images are included in the HTML as base64 encoded string instead of link to a local file.
#' @param overwrite A logical value indicating whether to overwrite an existing file.
#' @return A string with the table when `output` is a format, and the file path when `output` is a valid path.
#' @template dependencies
#' @template latex_preamble
#' @template global_options
#' @export
#' @examples
#' library(tinytable)
#' x <- mtcars[1:4, 1:5]
#'
#' fn <- file.path(tempdir(), "test.html")
#' tt(x) |> save_tt(fn, overwrite = TRUE)
#'
#' library(tinytable)
#' filename <- file.path(tempdir(), "table.tex")
#' tt(mtcars[1:4, 1:4]) |> save_tt(filename)
#'
save_tt <- function(
    x,
    output = get_option("tinytable_save_output", default = NULL),
    overwrite = get_option("tinytable_save_overwrite", default = FALSE)) {
  assert_class(x, "tinytable")
  assert_string(output)
  assert_flag(overwrite)

  if (file.exists(output) && !overwrite) {
    stop("File already exists and overwrite is set to FALSE.", call. = FALSE)
  }

  if (isTRUE(getOption("tinytable_html_portable", default = FALSE))) {
    assert_dependency("base64enc")
    x@portable <- TRUE
  }

  if (identical(output, "html_portable")) {
    assert_dependency("base64enc")
    output <- "bootstrap"
    x@portable <- TRUE
  }

  if (identical(output, "markdown")) {
    out <- build_tt(x, output = "markdown")@table_string
    return(as.character(out))
  } else if (identical(output, "gfm")) {
    out <- build_tt(x, output = "gfm")@table_string
    return(as.character(out))
  } else if (identical(output, "html")) {
    out <- build_tt(x, output = "html")@table_string
    return(as.character(out))
  } else if (identical(output, "bootstrap")) {
    out <- build_tt(x, output = "bootstrap")@table_string
    return(as.character(out))
  } else if (identical(output, "latex")) {
    out <- build_tt(x, output = "latex")@table_string
    return(as.character(out))
  } else if (identical(output, "typst")) {
    out <- build_tt(x, output = "typst")@table_string
    return(as.character(out))
  } else if (identical(output, "tabulator")) {
    out <- build_tt(x, output = "tabulator")@table_string
    return(as.character(out))
  } else if (identical(output, "dataframe")) {
    out <- build_tt(x, output = "dataframe")@data_body
    return(out)
  }

  x@output_dir <- dirname(output)

  file_ext <- tools::file_ext(output)

  output_format <- switch(file_ext,
    "png" = "html",
    "html" = get_option("tinytable_html_engine", default = "bootstrap"),
    "pdf" = "latex",
    "tex" = "latex",
    "md" = "markdown",
    "Rmd" = "markdown",
    "qmd" = "markdown",
    "txt" = "markdown",
    "docx" = "markdown",
    "typ" = "typst",
    stop(
      "The supported file extensions are: .png, .html, .pdf, .tex, .typ, .qmd, .Rmd, .txt, .docx, and .md. Supported output formats are: markdown, latex, html, typst, tabulator, and dataframe.",
      call. = FALSE
    )
  )

  # evaluate styles at the very end of the pipeline, just before writing
  x <- build_tt(x, output = output_format)

  if (file_ext %in% c("html", "tex", "md", "Rmd", "qmd", "txt", "typ")) {
    write(x@table_string, file = output)
  } else if (file_ext == "png") {
    assert_dependency("webshot2")
    # this doesn't work in tempdir() for some reason.
    # probably webshot2's fault. we need to build in `output`
    tmp <- file.path(dirname(output), paste0(get_id(), ".html"))
    write(x@table_string, file = tmp)
    webshot2::webshot(
      tmp,
      file = output,
      selector = "body > div > table",
      zoom = 4,
      quiet = TRUE
    )
    unlink(tmp)
  } else if (file_ext == "pdf") {
    assert_dependency("tinytex")
    # \documentclass{standalone} does not support \begin{table}
    tmp <- strsplit(x@table_string, "\\n")[[1]]
    # tmp <- tmp[!grepl("\\begin{table}", tmp, fixed = TRUE)]
    # tmp <- tmp[!grepl("\\end{table}", tmp, fixed = TRUE)]
    tmp <- paste(tmp, collapse = "\n")
    tmp <- sprintf(latex_standalone, tmp)
    # tinytex is fiddly with file paths, so we need to hack
    # it by changing the working directory
    wd <- getwd()
    on.exit(setwd(wd))
    temp_wd <- dirname(output)
    setwd(temp_wd)
    f <- paste0(get_id(), ".tex")
    write(tmp, f)
    # tinytex sometimes generates logfiles when there are warnings, e.g because
    # table is too wide. See #260.
    # We delete additional logfiles generated by tinytex
    existing_log_files <- list.files(
      temp_wd,
      pattern = "\\.log$",
      full.names = TRUE
    )

    # render
    engine <- get_option("tinytable_pdf_engine", default = "xelatex")
    assert_choice(
      engine,
      c("xelatex", "pdflatex", "lualatex"),
      name = "tinytable_pdf_engine"
    )
    tinytex::latexmk(f, pdf_file = output, engine = engine)

    # clean
    flag <- get_option("tinytable_pdf_clean", default = TRUE)
    assert_flag(flag, name = "tinytable_pdf_clean")
    if (flag) {
      new_log_files <- setdiff(
        list.files(temp_wd, pattern = "\\.log$", full.names = TRUE),
        existing_log_files
      )
      invisible(file.remove(new_log_files))
    }
    unlink(f)
  } else if (file_ext == "docx") {
    assert_dependency("pandoc")
    pandoc::pandoc_convert(text = x@table_string, to = "docx", output = output)
  }

  return(invisible(path.expand(output)))
}

latex_standalone <- "
\\documentclass{standalone}
\\usepackage{tabularray}
\\usepackage{graphicx}
\\usepackage{rotating}
\\usepackage{float}
\\usepackage[normalem]{ulem}
\\usepackage[x11names, svgnames]{xcolor}
\\UseTblrLibrary{booktabs}
\\UseTblrLibrary{siunitx}
\\newcommand{\\tinytableTabularrayUnderline}[1]{\\underline{#1}}
\\newcommand{\\tinytableTabularrayStrikeout}[1]{\\sout{#1}}
\\NewTableCommand{\\tinytableDefineColor}[3]{\\definecolor{#1}{#2}{#3}}
\\begin{document}
\\minipage{\\textwidth}
%s
\\endminipage
\\end{document}
"

#' @exportS3Method xfun::record_print
record_print.tinytable <- function(x, ...) {
  if (!isTRUE(check_dependency("litedown"))) {
    return(x)
  }

  # litedown
  fmt <- tryCatch(litedown::get_context("format"), error = function(e) NULL)
  if (is.null(fmt)) {
    return(x)
  }

  if (!isTRUE(fmt %in% c("latex", "markdown", "html"))) {
    stop(
      "tinytable in litedown only supports latex, markdown, or html output",
      call. = FALSE
    )
  }

  x <- build_tt(x, output = fmt)
  out <- x@table_string

  out <- litedown::raw_text(out, fmt)
  return(out)
}

#' Print a tinytable object in knitr
#'
#' @keywords internal
#' @return A string with class `knit_asis` to be printed in Rmarkdown or Quarto documents.
#' @rawNamespace S3method(knitr::knit_print, tinytable)
#' @export
knit_print.tinytable <- function(
    x,
    output = get_option("tinytable_print_output", default = NULL),
    ...) {
  # lazy styles get evaluated here, at the very end
  x <- build_tt(x, output = output)
  out <- x@table_string

  if (isTRUE(x@output == "html")) {
    # from htmltools:::html_preserve
    # GPL3
    inline <- grepl(out, "\n", fixed = TRUE)
    if (inline) {
      out <- sprintf("`%s`{=html}", out)
    } else {
      out <- sprintf("\n```{=html}\n%s\n```\n", out)
    }
  }

  if (isTRUE(x@output == "typst")) {
    # from htmltools:::html_preserve
    # GPL3
    inline <- grepl(out, "\n", fixed = TRUE)
    if (inline) {
      out <- sprintf("`%s`{=typst}", out)
    } else {
      out <- sprintf("\n```{=typst}\n%s\n```\n", out)
    }
  }

  class(out) <- "knit_asis"
  return(out)
}

#' Print, display, or convert a tinytable object
#'
#' This function is called automatically by `R` whenever a `tinytable` object is anprinted to the console or in an HTML viewer pane.
#'
#' @inheritParams tt
#' @param output format in which a Tiny Table is printed: `NULL` or one of `"latex"`, `"markdown"`, `"html"`, `"typst"`, `"dataframe"`, `"tabulator"`. If `NULL`, the output is chosen based on these rules:
#' + When called from a script in non-interactive mode, the default is "markdown" (`interactive() == FALSE`).
#' + When called interactively in RStudio, the default is to display an HTML table in the viewer pane.
#' + When called interactively in another development environment, the default is "markdown".
#' + The default print output can be changed for an entire R session by calling: `options(tinytable_print_output = "html")`
#' + The default print output can be changed for a single `tinytable` object by modifying the `output` S4 slot.
#' @details
#' When printing to HTML in `interactive()` mode, a temporary file is created and `viewer()` is called to preview the file with the local browser (ex: Firefox or Chrome). The temporary file is then automatically cleaned up. On some operating systems, like some Linux distributions, browser do not have read access to the `/tmp/` directory. In such cases, users can specify a custom location to store temporary HTML files. Note that this prevents `tinytable` from automatically cleaning up temporary files automatically.
#'
#' `options(tinytable_tempdir = "/home/username/temp_directory")`
#' @param ... Other arguments are ignored.
#' @return launch a browser window or cat() the table to console.
#' @export
print.tinytable <- function(
    x,
    output = get_option("tinytable_print_output", default = NULL),
    ...) {
  x <- sanitize_output(x, output)
  output <- infer_output(x)

  dir <- getOption("tinytable_tempdir", default = tempdir())
  dir <- sub("\\/$", "", dir)

  if (identical(output, "html")) {
    x@output_dir <- dir
    x@html_portable <- TRUE
  }


  x <- build_tt(x, output = output)

  tab <- x@table_string

  # lazy styles get evaluated here by build_tt(), at the very end
  if (identical(output, "dataframe")) {
    out <- x@data_body
    colnames(out) <- NULL
    if (x@ansi) {
      rows <- apply(out, 1, paste, collapse = " ")
      tabl <- paste(rows, collapse = "\n")
      cat("\n", tabl, "\n")
      return(invisible(out))
    } else {
      return(out)
    }
  } else if (output %in% c("latex", "typst", "markdown")) {
    cat(tab, "\n")
  } else if (output == "html") {
    if (is_rstudio_notebook()) {
      html_tinytable <- htmltools_browsable(tab)
      print(html_tinytable)
      return(invisible(NULL))
    }

    # need to change the output directory to a temporary directory
    # for plot_tt() inline plots to show up in RStudio
    random_name <- paste0(get_id("tinytable_"), ".html")
    htmlFile <- path.expand(normalizePath(file.path(dir, random_name), mustWork = FALSE))
    # on.exit(unlink(htmlFile))
    cat(tab, file = htmlFile)

    if (interactive()) {
      msg <- "Please choose a default browser with:

      options(browser = 'firefox')
      "
      if (identical(getOption("browser"), "")) {
        stop(msg, call. = FALSE)
      }

      viewer <- getOption("viewer", utils::browseURL)
      viewer(htmlFile)
    } else {
      cat(tab, "\n")
    }
  } else {
    return(x@data_body)
  }

  return(invisible(x))
}

setMethod("show", "tinytable", function(object) {
  print.tinytable(
    object,
    output = get_option("tinytable_print_output", default = NULL)
  )
})

# adapted from {htmltools} under GPL3
htmltools_browsable <- function(x) {
  htmlText <- paste(enc2utf8(x), collapse = " ")
  attr(htmlText, "html") <- TRUE
  attr(htmlText, "browsable_html") <- TRUE
  class(htmlText) <- c("html", "character")
  class(htmlText) <- "shiny.tag.list"
  htmlText
}

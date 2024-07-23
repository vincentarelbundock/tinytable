#'
#' @section Global options:
#'
#' ## Quarto data processing
#'
#' The `format_tt(quarto=TRUE)` argument activates Quarto data processing for specific cells. This funcationality comes with  a few warnings:
#'
#' 1. Currently, Quarto provides a `\QuartoMarkdownBase64{}` LaTeX macro, but it does not appear to do anything with it. References and markdown codes may not be processed as expected in LaTeX.
#' 2. Quarto data processing can enter in conflict with `tinytable` styling or formatting options. See below for how to disable it.
#'
#' `options(tinytable_quarto_disable_processing = TRUE)`
#'
#' Disable Quarto processing of cell content. Setting this global option to `FALSE` may lead to conflicts with some `tinytable` features, but it also allows use of markdown and Quarto-specific code in table cells, such as cross-references.
#'
#' ```r
#' x <- data.frame(Math = "x^2^", Citation = "@Lovelace1842")
#' fn <- function(z) sprintf("<span data-qmd='%s'></span>", z)
#' tt(x) |> format_tt(i = 1, fn = fn)
#' ```
#'
#' See this link for more details: https://quarto.org/docs/authoring/tables.html#disabling-quarto-table-processing
#'
#' ## HTML
#'
#' * EXPERIMENTAL `options(tinytable_html_mathjax = TRUE)` inserts MathJax scripts in the HTML document. Warning: This may conflict with other elements of the page if MathJax is otherwise loaded.
#'
#' ## PDF
#'
#' * `options(tinytable_pdf_clean = TRUE)` deletes temporary and log files.
#' * `options(tinytable_pdf_engine = "xelatex")`: "xelatex", "pdflatex", "lualatex"
#'
#'

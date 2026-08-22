#'
#' @section Global options:
#'
#' Options can be set with `options()` and change the default behavior of tinytable. For example:
#'
#' ```r
#' options(tinytable_tt_digits = 4)
#' tt(head(iris))
#' ```
#'
#' You can set options in a script or via `.Rprofile`. Note: be cautious with `.Rprofile` settings as they may affect reproducibility.
#'
#' ## Default values for function arguments
#'
#' Nearly all of the package's functions retrieve their default values from global options. This allows you to set defaults once and apply them to all tables without needing to specify them each time. For example, to fix the the `digits` argument of the `tt()` function globally, call:
#'
#' ```r
#' options(tinytable_tt_digits = 4)
#' ```
#'
#' In addition, some more specific options are available to control the behavior of the package in specific contexts.
#'
#' * `tinytable_html_mathjax`: Insert MathJax scripts (warning: may conflict if MathJax is loaded elsewhere)
#' * `tinytable_pdf_clean`: Delete temporary and log files for pdf output in `save_tt()`
#' * `tinytable_color_name_normalization`: Enable/disable automatic color name processing (default: TRUE). When enabled, R color names recognized by `col2rgb()` are converted to hex format for consistent rendering across HTML, LaTeX, and Typst formats. If R color conversion fails, LaTeX color names are used as fallback. Colors explicitly supplied as hex values with "#" prefix are passed through unchanged. Set to FALSE to disable processing and pass color names unchanged.
#'
#' ### Quarto
#'
#' The `format_tt(quarto=TRUE)` argument enables Quarto data processing with some limitations:
#'
#' 1. The `\QuartoMarkdownBase64{}` LaTeX macro may not process references and markdown as expected
#' 2. Quarto processing may conflict with `tinytable` styling/formatting
#' 3. `quarto=TRUE` should not be combined with `markdown=TRUE`. Quarto hands the cell content to Pandoc, which renders the markdown itself. Pre-rendering the markdown in `tinytable` feeds HTML back to Pandoc and inserts spurious line breaks.
#' 4. In HTML, cell content is wrapped in a `<span>`, which cannot legally hold block-level elements. Pandoc thus flattens block content such as bulleted lists to inline elements: list markers are dropped and block boundaries become `<br>`. See below for a workaround.
#'
#' Options:
#'
#' * `tinytable_quarto_disable_processing`: Disable Quarto cell processing
#'
#' Example of Quarto-specific code in cells:
#'
#' ```r
#' x <- data.frame(Math = "x^2^", Citation = "@Lovelace1842")
#' fn <- function(z) sprintf("<span data-qmd='%s'></span>", z)
#' tt(x) |> format_tt(i = 1, fn = fn)
#' ```
#'
#' To render block-level markdown such as bulleted lists, emit a `<div>` instead of a `<span>`:
#'
#' ```r
#' options(tinytable_quarto_disable_processing = FALSE)
#'
#' x <- data.frame(Thing = "Apple", Values = "- 50\n- 60\n- 70")
#' div_it <- function(z) sprintf('<div data-qmd="%s"></div>', z)
#' tt(x) |> format_tt(j = 2, fn = div_it, output = "html")
#' ```
#'
#' A `<div>` wraps cell content in `<p>` or `<ul>` tags, which carry a bottom margin. Add this to the document to keep cells tight:
#'
#' ```html
#' <style>.table td > :last-child { margin-bottom: 0; }</style>
#' ```
#'
#' For more details on Quarto table processing: https://quarto.org/docs/authoring/tables.html#disabling-quarto-table-processing
#'

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
#' ### tt()
#'
#' * `tinytable_tt_digits`
#' * `tinytable_tt_caption`
#' * `tinytable_tt_notes`
#' * `tinytable_tt_width`
#' * `tinytable_tt_theme`
#' * `tinytable_tt_rownames`
#'
#' ### format_tt()
#'
#' * `tinytable_format_digits`
#' * `tinytable_format_num_fmt`
#' * `tinytable_format_num_zero`
#' * `tinytable_format_num_suffix`
#' * `tinytable_format_num_mark_big`
#' * `tinytable_format_num_mark_dec`
#' * `tinytable_format_date`
#' * `tinytable_format_bool`
#' * `tinytable_format_other`
#' * `tinytable_format_replace`
#' * `tinytable_format_escape`
#' * `tinytable_format_markdown`
#' * `tinytable_format_quarto`
#' * `tinytable_format_fn`
#' * `tinytable_format_sprintf`
#'
#' ### save_tt()
#'
#' * `tinytable_save_overwrite`
#'
#' ### theme_tt()
#'
#' Placement:
#' * `tinytable_theme_placement_float`
#' * `tinytable_theme_placement_horizontal`
#'
#' Resize:
#' * `tinytable_theme_resize_width`
#' * `tinytable_theme_resize_direction`
#'
#' Multipage:
#' * `tinytable_theme_multipage_rowhead`
#' * `tinytable_theme_multipage_rowfoot`
#'
#' ### print.tinytable()
#'
#' * `tinytable_print_output`
#'
#' ## Output-specific options
#'
#' ### HTML
#'
#' * `tinytable_html_mathjax`: Insert MathJax scripts (warning: may conflict if MathJax is loaded elsewhere)
#' * `tinytable_html_portable`: Insert base64 encoded images directly in HTML for `plot_tt()`
#' * `tinytable_html_engine`: Default HTML engine (default: "bootstrap"). Set to "tabulator" to use interactive tables by default in HTML documents instead of static Bootstrap tables.
#'
#' ### PDF
#'
#' * `tinytable_pdf_clean`: Delete temporary and log files
#' * `tinytable_pdf_engine`: Choose between "xelatex", "pdflatex", "lualatex"
#'
#' ## Color processing
#'
#' * `tinytable_color_name_normalization`: Enable/disable automatic color name processing (default: TRUE). When enabled, R color names recognized by `col2rgb()` are converted to hex format for consistent rendering across HTML, LaTeX, and Typst formats. If R color conversion fails, LaTeX color names are used as fallback. Colors explicitly supplied as hex values with "#" prefix are passed through unchanged. Set to FALSE to disable processing and pass color names unchanged.
#'
#' ### Quarto
#'
#' The `format_tt(quarto=TRUE)` argument enables Quarto data processing with some limitations:
#'
#' 1. The `\QuartoMarkdownBase64{}` LaTeX macro may not process references and markdown as expected
#' 2. Quarto processing may conflict with `tinytable` styling/formatting
#'
#' Options:
#'
#' * `tinytable_quarto_disable_processing`: Disable Quarto cell processing
#' * `tinytable_print_rstudio_notebook`: Display tables "inline" or in "viewer" for RStudio notebooks
#' * `tinytable_quarto_figure`: Control Typst figure environment in Quarto
#'
#' Example of Quarto-specific code in cells:
#'
#' ```r
#' x <- data.frame(Math = "x^2^", Citation = "@Lovelace1842")
#' fn <- function(z) sprintf("<span data-qmd='%s'></span>", z)
#' tt(x) |> format_tt(i = 1, fn = fn)
#' ```
#'
#' For more details on Quarto table processing: https://quarto.org/docs/authoring/tables.html#disabling-quarto-table-processing
#'

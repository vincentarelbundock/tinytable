#' 
#'
#' @section Global options:
#'
#' Quarto data processing:
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

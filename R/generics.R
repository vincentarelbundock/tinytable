#' Style a `IttyBittyTable`
#'
#' @description
#' This function applies styling to specified parts of a table created by the `ibTable()` function of the `IttyBittyTable` package.
#'
#' The arguments described below allow users to customize text style and color. The `latex` and `html` arguments allow deep customization using the `tabularray` package for LaTeX, and the Bootstrap framework for HTML. For details, see:
#' * `?tabularrayOptions`
#' * `?bootstrapOptions`
#'
#' @param x A table created by the `ibTable()` function.
#' @param i Integer vector indicating row positions. If `NULL`, applies to all rows.
#' @param j Integer vector indicating column positions. If `NULL`, applies to all columns.
#' @param color String specifying the text color. If `NULL`, no color is applied.
#' @param background String specifying the background color. If `NULL`, no background is applied.
#' @param bold Logical; if `TRUE`, applies bold styling to the text.
#' @param italic Logical; if `TRUE`, applies italic styling to the text.
#' @param latex sytling options for the `tabularray` framework for LaTeX. See `?tabularrayOptions` for details.
#' @param html styling options for the Bootstrap framework for HTML. See `?bootstrapOptions` for details.
#' @return Returns the modified table object with the specified cell styles applied.
#' @template tabularray
#' @export
ibStyle <- function(x,
                  i,
                  j,
                  options,
                  ...) {
  UseMethod("ibStyle", x)
}


#' @export
ibStyle.default <- function(x, ...) return(x)

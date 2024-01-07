#' Style Cells in a `tinytable`
#'
#' @description
#' This function applies styling to specified parts of a table created by the `tinytable()` function of the `tinytable` package.
#'
#' The arguments described below allow users to customize text style and color. The `latex` and `html` arguments allow deep customization using the `tabularray` package for LaTeX, and the Bootstrap framework for HTML. For details, see:
#' * `?latexOptions`
#' * `?htmlOptions`
#'
#' @param x A table created by the `tinytable()` function.
#' @param i Integer vector indicating row positions. If `NULL`, applies to all rows.
#' @param j Integer vector indicating column positions. If `NULL`, applies to all columns.
#' @param color String specifying the text color. If `NULL`, no color is applied.
#' @param background String specifying the background color. If `NULL`, no background is applied.
#' @param bold Logical; if `TRUE`, applies bold styling to the text.
#' @param italic Logical; if `TRUE`, applies italic styling to the text.
#' @param latex sytling options for the `tabularray` framework for LaTeX. See `?latexOptions` for details.
#' @param html styling options for the Bootstrap framework for HTML. See `?htmlOptions` for details.
#' @return Returns the modified table object with the specified cell styles applied.
#' @template tabularray
#' @export
style_cell <- function(x,
                       i = NULL,
                       j = NULL,
                       color = NULL,
                       background = NULL,
                       bold = FALSE,
                       italic = FALSE,
                       latex = latexOptions(),
                       html = htmlOptions()) {
  UseMethod("style_cell", x)
}


#' @export
style_cell.default <- function(x, ...) return(x)


#' Style Rows in a `tinytable`
#'
#' @inheritParams style_cell
#' @inherit style_cell return
#' @inherit style_cell description
#' @template tabularray
#' @export
style_row <- function(x,
                      i = NULL,
                      color = NULL,
                      background = NULL,
                      bold = FALSE,
                      italic = FALSE,
                      latex = latexOptions(),
                      html = htmlOptions()) {
  UseMethod("style_row", x)
}


#' @export
style_row.default <- function(x, ...) return(x)


#' Style Columns in a `tinytable`
#'
#' @inheritParams style_cell
#' @inherit style_cell return
#' @inherit style_cell description
#' @template tabularray
#' @export
style_column <- function(x,
                         j = NULL,
                         color = NULL,
                         background = NULL,
                         bold = FALSE,
                         italic = FALSE,
                         align = NULL,
                         latex = latexOptions(),
                         html = htmlOptions()) {
  UseMethod("style_column", x)
}


#' @export
style_column.default <- function(x, ...) return(x)

#' Draw a Tiny Table
#'
#' @description
#' The `tt` function renders a table in different formats (HTML, Markdown, or LaTeX) with various styling options. The table can be customized with additional functions:
#'
#' * `style_tt()` to style fonts, colors, alignment, etc.
#' * `format_tt()` to format numbers, dates, strings, etc.
#' * `save_tt()` to save the table to a file or return the table as a string.
#'
#' `tinytable` attempts to determine the appropriate way to print the table based on interactive use, RStudio availability, and output format in RMarkdown or Quarto documents. Users can call `print(x, output="markdown")` to print the table in a specific format. Alternatively, they can set a global option: `options("tinytable_print_output"="markdown")`
#'
#' @param x A data frame or data table to be rendered as a table.
#' @param digits Number of significant digits to keep for numeric variables. When `digits` is an integer, `tt()` calls `format_tt(x, digits = digits)` before proceeding to draw the table. Users who need more control can proceed in two steps: (1) format the data with `format_tt()` or other functions, and (2) pass the formatted data to `tt()` for drawing. See `?format_tt` for more details on formating options (ex: decimal, scientific notation, dates, boolean variables, etc.).
#' @param caption A string that will be used as the caption of the table.
#' @param width A numeric value between 0 and 1 indicating the proportion of the line width that the table should cover.
#' @param theme The theme to apply to the table.
#' * LaTeX: "default", "striped", "void", or "grid".
#' * HTML: "default", "striped", "void", "grid", or a (composite) Bootstrap class such as `"table table-dark"` or `"table table-dark table-hover"`. See 
#' @param notes A single string or a (named) list of strings to append at the bottom of the table.
#' 
#' @param placement A string to control the position of tables in LaTeX. Will be inserted in square brackets like: `\\begin{table}[H]`
#' @return An object of class `tt` representing the table.
#' @template latex_preamble
#' 
#' @examplesIf getOption("tt_local", default = FALSE)
#' @examples
#' library(tinytable)
#' x <- mtcars[1:4, 1:5]
#'
#' tt(x)
#' 
#' tt(x,
#'   theme = "striped",
#'   width = 0.5,
#'   caption = "Data about cars.")
#' 
#' @export
tt <- function(x,
               digits = NULL,
               caption = NULL,
               width = NULL,
               notes = NULL,
               theme = "default",
               placement = getOption("tinytable_tabularray_placement", default = NULL)) {

  output <- meta(x, "output")

  # sanity checks
  assert_data_frame(x)
  assert_string(caption, null.ok = TRUE)
  assert_numeric(width, len = 1, lower = 0, upper = 1, null.ok = TRUE)
  assert_integerish(digits, len = 1, null.ok = TRUE)

  
  # notes can be a single string or a (named) list of strings
  if (is.character(notes) && length(notes)) {
    notes <- list(notes)
  }
  assert_list(notes, null.ok = TRUE)

  # before style_tt() call for align
  out <- x 
  out <- meta(out, "x_character", data.frame(lapply(x, as.character)))
  out <- meta(out, "output", output)
  out <- meta(out, "colnames", names(x))
  out <- meta(out, "xdim", dim(x))
  out <- meta(out, "output", output)
  out <- meta(out, "id", get_id("tinytable_"))
  out <- meta(out, "nhead", if (is.null(colnames(x))) 0 else 1)
  out <- meta(out, "nrows", nrow(x))
  out <- meta(out, "ncols", ncol(x))
  class(out) <- c("tinytable", class(out))

  # build table
  cal <- call("tt_tabularray", x = out, caption = caption, theme = theme, width = width, notes = notes, placement = placement)
  # if (output == "latex") {
  #
  # } else if (output == "html"){
  #   cal <- call("tt_bootstrap", x = out, caption = caption, theme = theme, width = width, notes = notes)
  #
  # } else if (output == "markdown") {
  #   cal <- call("tt_grid", x = out, caption = caption)
  #
  # } else {
  #   stop("here be dragons")
  # }

  out <- meta(out, "lazy_tt", cal)

  # formatting options are limited here
  # after creating the table since the new lazy system
  if (!is.null(digits)) {
    out <- format_tt(out, digits = digits)
  }

  return(out)
}

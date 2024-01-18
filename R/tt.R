#' Draw a Tiny Table
#'
#' The `tt` function renders a table in different formats (HTML, Markdown, or LaTeX) with various styling options.
#' 
#' @param x A data frame or data table to be rendered as a table.
#' @param output The format of the output table. Can be "html", "latex", or "markdown". If NULL, the format is automatically detected in Quarto or Rmarkdown documents.
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
               output = NULL,
               digits = getOption("digits"),
               caption = NULL,
               width = NULL,
               notes = NULL,
               theme = "default",
               placement = getOption("tt_tabularray_placement", default = NULL)) {

  # sanity checks
  output <- sanitize_output(output)
  assert_data_frame(x)
  assert_string(caption, null.ok = TRUE)
  assert_numeric(width, len = 1, lower = 0, upper = 1, null.ok = TRUE)
  assert_integerish(digits, len = 1, null.ok = TRUE)

  # formatting options are limited here
  if (!is.null(digits)) {
    x <- format_tt(x, digits = digits)
  }
  
  # notes can be a single string or a (named) list of strings
  if (is.character(notes) && length(notes)) {
    notes <- list(notes)
  }
  assert_list(notes, null.ok = TRUE)

  out <- x

  # build table
  if (output == "latex") {
    out <- tt_tabularray(out, caption = caption, theme = theme, width = width, notes = notes)

  } else if (output == "html"){
    out <- tt_bootstrap(out, caption = caption, theme = theme, width = width, notes = notes)

  } else {
    out <- tt_markdown(out, caption = caption)
  }

  # before style_tt() call for align
  out <- meta(out, "colnames", names(x))
  out <- meta(out, "xdim", dim(x))
  out <- meta(out, "output", output)
  out <- meta(out, "id", get_id("tinytable_"))
  out <- meta(out, "nhead", if (is.null(colnames(x))) 0 else 1)
  out <- meta(out, "nrows", nrow(x))
  out <- meta(out, "ncols", ncol(x))
  out <- meta(out, "lazy_style", list())

  # placement
  assert_string(placement, null.ok = TRUE)
  if (!is.null(placement)) {
    # dollar sign to avoid [H][H] when we style multiple times
    out <- sub("\\\\begin\\{table\\}", sprintf("\\\\begin{table}[%s]\n", placement), out)
  }

  return(out)
}

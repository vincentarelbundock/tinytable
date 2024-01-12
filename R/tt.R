#' Draw a Tiny Table
#'
#' The `tt` function renders a table in different formats (HTML, Markdown, or LaTeX) with various styling options.
#' 
#' @param x A data frame or data table to be rendered as a table.
#' @param output The format of the output table. Can be "html", "latex", or "markdown". If NULL, the format is automatically detected in Quarto or Rmarkdown documents.
#' @param align A string specifying the alignment of columns. Each character in the string corresponds to a column; 'l' for left, 'c' for center, and 'r' for right alignment. The length of the string must match the number of columns in `x`.
#' @param caption A string that will be used as the caption of the table.
#' @param width A numeric value between 0 and 1 indicating the proportion of the line width that the table should cover.
#' @param theme The theme to apply to the table.
#' * LaTeX: "default", "striped", "void", or "grid".
#' * HTML: "default", "striped", "void", "grid", or a (composite) Bootstrap class such as `"table table-dark"` or `"table table-dark table-hover"`. See 
#' 
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
#'   align = "ccrrl",
#'   caption = "Data about cars.")
#' 
#' @export
tt <- function(x,
               output = NULL,
               align = NULL,
               caption = NULL,
               width = NULL,
               theme = "default") {

  # sanity checks
  output <- sanitize_output(output)
  assert_data_frame(x)
  assert_string(caption, null.ok = TRUE)
  assert_string(align, null.ok = TRUE)
  assert_numeric(width, len = 1, lower = 0, upper = 1, null.ok = TRUE)

  out <- x

  # build table
  if (output == "latex") {
    out <- tt_tabularray(out, caption = caption, theme = theme, width = width)

  } else if (output == "html"){
    out <- tt_bootstrap(out, caption = caption, theme = theme, width = width)

  } else {
    out <- tt_markdown(out, caption = caption)
  }

  if (!is.null(align)) {
    if (nchar(align) != ncol(x)) {
      msg <- sprintf("`align` must have length %s, equal to the number of columns in `x`.", ncol(x))
      stop(msg, call. = FALSE)
    }
    align <- strsplit(align, split = "")[[1]]
    if (!all(align %in% c("l", "c", "r"))) {
      msg <- "Elements of `align` must be 'c', 'l', or 'r'."
      stop(msg, call. = FALSE)
    }
    for (col in seq_along(align)) {
      out <- style_tt(out, j = col, align = align[[col]])
    }
  }

  return(out)
}

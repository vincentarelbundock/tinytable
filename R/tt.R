#' Draw a Tiny Table
#'
#' @description
#' The `tt` function renders a table in different formats with various styling options: HTML, Markdown, LaTeX, Word, PDF, PNG, or Typst. The table can be customized with additional functions:
#'
#' * `style_tt()` to style fonts, colors, alignment, etc.
#' * `format_tt()` to format numbers, dates, strings, etc.
#' * `group_tt()` for row or column group labels.
#' * `save_tt()` to save the table to a file or return the table as a string.
#' * `print()` to print to a specific format, ex: `print(x, "latex")`
#'
#' `tinytable` attempts to determine the appropriate way to print the table based on interactive use, RStudio availability, and output format in RMarkdown or Quarto documents. Users can call `print(x, output="markdown")` to print the table in a specific format. Alternatively, they can set a global option: `options("tinytable_print_output"="markdown")`
#'
#' @param x A data frame or data table to be rendered as a table.
#' @param digits Number of significant digits to keep for numeric variables. When `digits` is an integer, `tt()` calls `format_tt(x, digits = digits)` before proceeding to draw the table. Users who need more control can use the `format_tt()` function.
#' @param caption A string that will be used as the caption of the table.
#' @param width A numeric value between 0 and 1 indicating the proportion of the line width that the table should cover.
#' @param theme The theme to apply to the table: "default", "striped", "bootstrap", "void", or "grid".
#' @param notes Notes to append to the bottom of the table. This argument accepts several different inputs:
#' * Single string insert a single note: `"blah blah"`
#' * Multiple strings insert multiple notes sequentially: `list("Hello world", "Foo bar")`
#' * A named list inserts a list with the name as superscript: `list("a" = list("Hello World"))`
#' * A named list with positions inserts markers as superscripts inside table cells: `list("a" = list(i = 0:1, j = 2, text = "Hello World"))`
#' @param placement A string to control the position of tables in LaTeX. Will be inserted in square brackets like: `\\begin{table}[H]`
#' @return An object of class `tt` representing the table.
#' 
#' The table object has S4 slots which hold information about the structure of the table. This meta-data can be accessed with the usual `@` accessor. In general, modifying the content of these slots is not recommended, but it can be useful to some developers, such as those who want to force print to a specific output format without calling `print()`.
#' @template latex_preamble
#' 
#' @examples
#' library(tinytable)
#' x <- mtcars[1:4, 1:5]
#'
#' tt(x)
#' 
#' tt(x,
#'    theme = "striped",
#'    width = 0.5,
#'    caption = "Data about cars.")
#' 
#' tt(x, notes = "Hello World!")
#'
#' fn <- list(i = 0:1, j = 2, text = "Hello World!")
#' tab <- tt(x, notes = list("*" = fn))
#' print(tab, "latex")
#' 
#' k <- data.frame(x = c(0.000123456789, 12.4356789))
#' tt(k, digits=2)
#' 
#' @export
tt <- function(x,
               digits = getOption("tinytable_tt_digits", default = NULL),
               caption = NULL,
               width = NULL,
               notes = NULL,
               theme = getOption("tinytable_tt_theme", default = NULL),
               placement = getOption("tinytable_tt_placement", default = NULL)) {

  # sanity checks
  assert_string(caption, null.ok = TRUE)
  assert_numeric(width, len = 1, lower = 0, upper = 1, null.ok = TRUE)
  assert_integerish(digits, len = 1, null.ok = TRUE)
  notes <- sanitize_notes(notes)

  td <- getOption("tinytable_themes", default = theme_dictionary)
  na <- setdiff(unique(sort(names(td))), "default")
  assert_choice(theme, na, null.ok = TRUE)
  if (is.null(theme)) {
    theme <- "default"
  } else {
    theme <- unique(c("default", theme))
  }

  # x should be a data frame, not a tibble, for indexing convenience
  assert_data_frame(x, min_rows = 1, min_cols = 1)
  if (inherits(x, "tbl_df")) {
    cn <- colnames(x)
    x <- as.data.frame(x, check.names = FALSE)
    colnames(x) <- cn
  }

  # formatting options are limited here
  # after creating the table since the new lazy system
  tab <- x
  if (!is.null(digits)) {
    tab <- format_tt(tab, digits = digits)
  }

  # baseline character format
  # twice because format() leaves Date type, which cannot be partially reasigned
  # with indexed format_tt(i)
  tab <- data.frame(lapply(tab, format))
  colnames(tab) <- colnames(x)

  out <- methods::new("tinytable",
    data = x,
    table = tab,
    caption = caption,
    notes = notes,
    theme = theme,
    placement = placement,
    width = width)

  for (th in theme) {
    out <- theme_tt(out, th)
  }

  return(out)
}



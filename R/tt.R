#' Draw a Tiny Table
#'
#' @description
#' The `tt` function renders a table in different formats with various styling options: HTML, Markdown, LaTeX, Word, PDF, PNG, or Typst. The table can be customized with additional functions:
#'
#' * `style_tt()`: style fonts, colors, alignment, etc.
#' * `format_tt()`: format numbers, dates, strings, etc.
#' * `group_tt()`: row or column group labels.
#' * `theme_tt()`: apply a collection of transformations to a `tinytable.`
#' * `save_tt()`: save the table to a file or return the table as a string.
#' * `print()`: print to a specific format, ex: `print(x, "latex")`
#'
#' `tinytable` attempts to determine the appropriate way to print the table based on interactive use, RStudio availability, and output format in RMarkdown or Quarto documents. Users can call `print(x, output="markdown")` to print the table in a specific format. Alternatively, they can set a global option: `options("tinytable_print_output"="markdown")`
#'
#' @param x A data frame or data table to be rendered as a table.
#' @param digits Number of significant digits to keep for numeric variables. When `digits` is an integer, `tt()` calls `format_tt(x, digits = digits)` before proceeding to draw the table. Note that this will apply all default argument values of `format_tt()`, such as replacing `NA` by "". Users who need more control can use the `format_tt()` function instead.
#' @param caption A string that will be used as the caption of the table. This argument should *not* be used in Quarto or Rmarkdown documents. In that context, please use the appropriate chunk options.
#' @param width Table or column width. 
#' - Single numeric value smaller than or equal to 1 determines the full table width, in proportion of line width. 
#' - Numeric vector of length equal to the number of columns in `x` determines the width of each column, in proportion of line width. If the sum of `width` exceeds 1, each element is divided by `sum(width)`. This makes the table full-width with relative column sizes.
#' @param theme Function or string.
#' - String: `r paste(setdiff(names(theme_dictionary), "default"), collapse = ", ")`
#' - Function: Applied to the `tinytable` object.
#' @param notes Notes to append to the bottom of the table. This argument accepts several different inputs:
#' * Single string insert a single note: `"blah blah"`
#' * Multiple strings insert multiple notes sequentially: `list("Hello world", "Foo bar")`
#' * A named list inserts a list with the name as superscript: `list("a" = list("Hello World"))`
#' * A named list with positions inserts markers as superscripts inside table cells: `list("a" = list(i = 0:1, j = 2, text = "Hello World"))`
#' @param ... Additional arguments are ignored
#' @return An object of class `tt` representing the table.
#' 
#' The table object has S4 slots which hold information about the structure of the table. This meta-data can be accessed with the usual `@` accessor. In general, modifying the content of these slots is not recommended, but it can be useful to some developers, such as those who want to force print to a specific output format without calling `print()`.
#' @template latex_preamble
#' @template global_options
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
               caption = getOption("tinytable_tt_caption", default = NULL),
               notes = getOption("tinytable_tt_notes", default = NULL),
               width = getOption("tinytable_tt_width", default = NULL),
               theme = getOption("tinytable_tt_theme", default = NULL),
               ...) {


  dots <- list(...)
  if ("placement" %in% names(dots)) {
    warning("The `placement` argument in `tt()` is deprecated. Please use this instead: `theme_tt(table, 'placement')`", call. = FALSE)
  }

  # sanity checks
  assert_string(caption, null.ok = TRUE)
  assert_integerish(digits, len = 1, null.ok = TRUE)
  notes <- sanitize_notes(notes)

  # x should be a data frame, not a tibble or slopes, for indexing convenience
  assert_data_frame(x, min_rows = 1, min_cols = 1)
  if (!isTRUE(identical(class(x), "data.frame"))) {
    cn <- colnames(x)
    x <- as.data.frame(x, check.names = FALSE)
    colnames(x) <- cn
  }

  # non-numeric should all be characters (for replace, etc.)
  for (i in seq_along(x)) {
    if (!is.numeric(x[[i]])) {
       x[[i]] <- as.character(x[[i]])
    }
  }

  assert_numeric(width, lower = 0, null.ok = TRUE)
  if (!length(width) %in% c(0, 1, ncol(x))) {
      msg <- sprintf("The `width` argument must have length 1 or %s.", ncol(x))
      stop(msg, call. = FALSE)
  }
  if (sum(width) > 1) {
      width <- width / sum(width)
  }

  # bind the row names if the user explicitly asks for it in global option. 
  # Same name as tibble::rownames_to_column()
  if (isTRUE(getOption("tinytable_tt_rownames", default = FALSE)) && !is.null(row.names(x))) {
    rn <- data.frame(format(row.names(x)))
    rn <- stats::setNames(rn, "rowname")
    x <- cbind(rn, x)
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
    theme = list(theme),
    width = width)

  if (is.null(theme)) {
    out <- theme_tt(out, theme = "default")
  } else {
    out <- theme_tt(out, theme = theme)
  }

  if ("placement" %in% names(dots)) {
    out <- theme_tt(out, "placement", latex_float = dots[["placement"]])
  }

  return(out)
}

#' Draw a Tiny Table
#'
#' @description
#' The `tt` function renders a table in different formats with various styling options: HTML, Markdown, LaTeX, Word, PDF, PNG, or Typst. The table can be customized with additional functions:
#'
#' * `style_tt()`: style fonts, colors, alignment, etc.
#' * `format_tt()`: format numbers, dates, strings, etc.
#' * `group_tt()`: row or column group labels.
#' * `save_tt()`: save the table to a file or return the table as a string.
#' * `print()`: print to a specific format, ex: `print(x, "latex")`
#' * `theme_*()` functions apply a collection of format-specific or visual transformations to a `tinytable.`
#'
#' `tinytable` attempts to determine the appropriate way to print the table based on interactive use, RStudio availability, and output format in RMarkdown or Quarto documents. Users can call `print(x, output="markdown")` to print the table in a specific format. Alternatively, they can set a global option: `options("tinytable_print_output"="markdown")`
#'
#' @param x A data frame, data table, or tibble to be rendered as a table.
#' @param digits Number of significant digits to keep for numeric variables. When `digits` is an integer, `tt()` calls `format_tt(x, digits = digits)` before proceeding to draw the table. Note that this will apply all default argument values of `format_tt()`, such as replacing `NA` by "". Users who need more control can use the `format_tt()` function instead.
#' @param caption A string that will be used as the caption of the table. This argument should *not* be used in Quarto or Rmarkdown documents. In that context, please use the appropriate chunk options.
#' @param width Table or column width.
#' - Single numeric value smaller than or equal to 1 determines the full table width, in proportion of line width.
#' - Numeric vector of length equal to the number of columns in `x` determines the width of each column, in proportion of line width. If the sum of `width` exceeds 1, each element is divided by `sum(width)`. This makes the table full-width with relative column sizes.
#' @param height Row height in em units. Single numeric value greater than zero that determines the row height spacing.
#' @param theme Function or string.
#' - String: `r paste(setdiff(names(theme_dictionary), "default"), collapse = ", ")`
#' - Function: Applied to the `tinytable` object.
#' @param notes Notes to append to the bottom of the table. This argument accepts several different inputs:
#' * Single string insert a single note: `"blah blah"`
#' * Multiple strings insert multiple notes sequentially: `list("Hello world", "Foo bar")`
#' * A named list inserts a list with the name as superscript: `list("a" = list("Hello World"))`
#' * A named list with positions inserts markers as superscripts inside table cells: `list("a" = list(i = 0:1, j = 2, text = "Hello World"))`
#' @param colnames `TRUE`, `FALSE`, or "label". If "label", use the `attr(x$col,"label")` attribute if available and fall back on column names otherwise.
#' @param rownames Logical. If `TRUE`, rownames are included as the first column
#' @param escape Logical. If `TRUE`, escape special characters in the table. Equivalent to `format_tt(tt(x), escape = TRUE)`.
#' @param ... Additional arguments are ignored
#' @return An object of class `tt` representing the table.
#'
#' The table object has S4 slots which hold information about the structure of the table. For example, the `table@group_index_i` slot includes the row indices for grouping labels added by `group_tt()`.
#'
#' Warning: Relying on or modifying the contents of these slots is strongly discouraged. Their names and contents could change at any time, and the `tinytable` developers do not consider changes to the internal structure of the output object to be a "breaking  change" for versioning or changelog purposes.
#'
#' @template dependencies
#' @template latex_preamble
#' @template limitations_word_markdown
#' @template tabulator
#' @template global_options
#' @template order_of_operations
#'
#' @examples
#' library(tinytable)
#' x <- mtcars[1:4, 1:5]
#'
#' tt(x)
#'
#' tt(x,
#'   theme = "striped",
#'   width = 0.5,
#'   caption = "Data about cars."
#' )
#'
#' tt(x, notes = "Hello World!")
#'
#' fn <- list(i = 0:1, j = 2, text = "Hello World!")
#' tab <- tt(x, notes = list("*" = fn))
#' print(tab, "latex")
#'
#' k <- data.frame(x = c(0.000123456789, 12.4356789))
#' tt(k, digits = 2)
#'
#' # use variable labels stored in attributes as column names
#' dat = mtcars[1:5, c("cyl", "mpg", "hp")]
#' attr(dat$cyl, "label") <- "Cylinders"
#' attr(dat$mpg, "label") <- "Miles per Gallon"
#' attr(dat$hp, "label") <- "Horse Power"
#' tt(dat, colnames = "label")
#'
#' @export
# Generic function
tt <- function(x, ...) {
  UseMethod("tt")
}

#' @rdname tt
#' @export

# Default method (for data.frame, data.table, tbl_df, and other data frame-like objects)
tt.default <- function(
    x,
    digits = get_option("tinytable_tt_digits", default = NULL),
    caption = get_option("tinytable_tt_caption", default = NULL),
    notes = get_option("tinytable_tt_notes", default = NULL),
    width = get_option("tinytable_tt_width", default = NULL),
    height = get_option("tinytable_tt_height", default = NULL),
    theme = get_option("tinytable_tt_theme", default = "default"),
    colnames = get_option("tinytable_tt_colnames", default = TRUE),
    rownames = get_option("tinytable_tt_rownames", default = FALSE),
    escape = get_option("tinytable_tt_escape", default = FALSE),
    ...) {
  dots <- list(...)

  # sanity checks
  assert_string(caption, null.ok = TRUE)
  assert_integerish(digits, len = 1, null.ok = TRUE)
  notes <- sanitize_notes(notes)

  if (identical(theme, "void")) {
    theme <- "empty"
    warning("`theme = 'void'` is deprecated. Please use `theme = 'empty'` instead.", call. = FALSE)
  }
  if (!isTRUE(check_function(theme)) && !isTRUE(check_string(theme))) {
    stop("The `theme` argument must be a function or a string.", call. = FALSE)
  }
  if (isTRUE(check_string(theme))) {
    assert_choice(theme, names(theme_dictionary), null.ok = TRUE)
  }

  # x should be a data frame, not a tibble or slopes, for indexing convenience
  assert_data_frame(x, min_rows = 1, min_cols = 1)
  if (!isTRUE(identical(class(x), "data.frame"))) {
    cn <- colnames(x)
    # weird bug on as.data.frame.data.table() when there is no columns. in general, we don't want to modify in-place.
    if (is.null(cn) && inherits(x, "data.table")) {
      assert_dependency("data.table")
      data.table::setDF(x)
    } else {
      x <- as.data.frame(x, check.names = FALSE)
      colnames(x) <- cn
    }
  }

  if (!(is.logical(colnames) && length(colnames) == 1) && !identical(colnames, "label")) {
    stop("The `colnames` argument must be TRUE, FALSE, or 'label'.", call. = FALSE)
  }

  # factors should all be characters (for replace, etc.)
  # it might be dangerous to leave non-numerics, but what about dates and other character-coercibles?
  for (i in seq_along(x)) {
    if (is.factor(x[[i]])) {
      x[[i]] <- as.character(x[[i]])
    }
  }

  assert_numeric(width, lower = 0, null.ok = TRUE)
  if (!length(width) %in% c(0, 1, ncol(x))) {
    msg <- sprintf("The `width` argument must have length 1 or %s.", ncol(x))
    stop(msg, call. = FALSE)
  }
  if (sum(width) > 1) {
    # handle remainders gracefully to avoid sum > 100% after rounding
    width <- percent_sum_100(width) / 100
  }

  assert_numeric(height, lower = 0, len = 1, null.ok = TRUE)

  # bind the row names if the user explicitly asks for it in global option.
  # Same name as tibble::rownames_to_column()
  assert_flag(rownames)
  if (isTRUE(rownames) && !is.null(row.names(x))) {
    nocol <- is.null(colnames(x))
    rn <- data.frame(format(row.names(x)))
    rn <- stats::setNames(rn, "rowname")
    x <- cbind(rn, x)
    if (isTRUE(nocol)) {
      colnames(x) <- NULL
    }
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
  tab <- data.frame(lapply(tab, trimws))
  colnames(tab) <- colnames(x)

  out <- methods::new(
    "tinytable",
    data = x,
    data_body = tab,
    caption = caption,
    notes = notes,
    theme = list(theme),
    width = width,
    height = height,
    colnames = colnames
  )

  if (is.null(theme)) {
    out <- theme_default(out)
  } else if (is.function(theme)) {
    out <- theme(out)
  } else if (is.character(theme)) {
    for (th in theme) {
      if (!th %in% names(theme_dictionary)) {
        stop(sprintf("Unknown theme: '%s'. Available themes: %s", th, paste(names(theme_dictionary), collapse = ", ")), call. = FALSE)
      }
    }
    out <- theme_dictionary[[theme]](out)
  }

  if (isTRUE(escape)) {
    out <- format_tt(out, escape = TRUE)
  }

  if (!is.null(height)) {
    # LaTeX tabularray: use height/2 as rowsep
    out <- theme_latex(
      out,
      inner = sprintf("rowsep={%sem}", height / 2)
    )

    # HTML: use CSS padding for row height
    out <- theme_html(
      out,
      css = sprintf(
        "padding-top: %sem; padding-bottom: %sem;",
        height / 2,
        height / 2
      )
    )

    # Typst: use rows parameter in table settings
    fun_typst_height <- function(table) {
      if (!is.null(table@height)) {
        table@table_string <- gsub(
          "rows: auto,",
          sprintf("rows: %sem,", table@height),
          table@table_string
        )
      }
      return(table)
    }
    out <- style_tt(out, finalize = fun_typst_height)
  }

  # HTML: add column width styles if multiple widths specified
  if (!is.null(width) && length(width) > 1) {
    for (j in seq_len(ncol(x))) {
      css <- sprintf("width: %s%%;", width[j] / sum(width) * 100)
      out <- theme_html(out, j = j, css = css)
    }
  }

  return(out)
}

#' @export
tt.data.frame <- function(x, ...) {
  tt.default(x, ...)
}

#' @export
tt.data.table <- function(x, ...) {
  tt.default(x, ...)
}

#' @export
tt.tbl_df <- function(x, ...) {
  tt.default(x, ...)
}

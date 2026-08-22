#' Format columns of a data frame
#'
#' This function formats the columns of a data frame based on the column type (logical, date, numeric).
#' It allows various formatting options like significant digits, decimal points, and scientific notation.
#' It also includes custom formatting for date and boolean values.
#' If this function is applied several times to the same cell, the last transformation is retained and the previous calls are ignored, except for the `escape` argument which can be applied to previously transformed data.
#' Each call to `format_tt()` is recorded and later replayed sequentially at render time, so later calls operate on the output of earlier ones.
#' Arguments are executed in a fixed order inside each call (see code below), so combining features such as `linebreak`, `escape`, `markdown`, etc. in a single call may produce surprising results.
#' Chain multiple calls when you need finer control (e.g., `format_tt(escape = TRUE) |> format_tt(linebreak = "<br>")`), keeping in mind that the second call will see the already-escaped text.
#'
#' @param x A data frame or a vector to be formatted.
#' @param i Numeric vector or string.
#'   - Numeric vector: Row indices where the styling should be applied. Can be a single value or a vector.
#'   - String: Table components to format "caption", "colnames", "groupi" (row group labels), "~groupi" (non-group rows), "groupj" (column group labels), "notes".
#'   - If both the `i` and `j` are omitted (default: NULL), formatting is applied to all table elements, including caption, notes, and group labels.
#' @param digits Number of significant digits or decimal places, depending on the `num_fmt` argument.
#'   - `num_fmt = "significant"` (default): `digits` is the minimum number of significant digits. Formatting is applied column-wise, so the smallest value in the column determines the decimal representation for the whole column.
#'   - `num_fmt = "significant_cell"`: same as `"significant"`, but formatting is applied cell-by-cell. Each cell displays exactly `digits` significant figures, regardless of other values in the column.
#'   - `num_fmt = "decimal"`: `digits` is the number of decimal places (digits after the decimal point) for all values.
#'   - `num_fmt = "scientific"`: `digits` is the number of decimal places in the mantissa of scientific notation (e.g., `digits = 2` gives `1.23e+04`).
#' @param num_fmt The format for numeric values; one of 'significant', 'significant_cell', 'decimal', or 'scientific'. See the `digits` argument for details on how each format interprets the `digits` value.
#' @param num_zero Logical; if TRUE, trailing zeros are kept in "decimal" format (but not in "significant" format).
#' @param num_mark_big Character to use as a thousands separator.
#' @param num_mark_dec Decimal mark character. Default is the global option 'OutDec'.
#' @param num_suffix Logical; if TRUE display short numbers with `digits` significant digits and K (thousands), M (millions), B (billions), or T (trillions) suffixes.
#' @param date A string passed to the `format()` function, such as "%Y-%m-%d". See the "Details" section in `?strptime`
#' @param bool A function to format logical columns. Defaults to title case.
#' @param math Logical. If TRUE, wrap cell values in math mode `$..$`. This is useful for LaTeX output or with HTML MathJax `options(tinytable_html_mathjax=TRUE)`.
#' @param other A function to format columns of other types. Defaults to `as.character()`.
#' @param replace Logical, String or Named list of vectors
#' - TRUE: Replace `NA` and `NaN` by an empty string.
#' - FALSE: Print `NA` and `NaN` as strings.
#' - String: Replace `NA` and `NaN` entries by the user-supplied string.
#' - Named list: Replace matching elements of the vectors in the list by theirs names. Example:
#'      - `list("-" = c(NA, NaN), "Tiny" = -Inf, "Massive" = Inf)`
#' @param escape Logical or "latex" or "html". If TRUE, escape special characters to display them as text in the format of the output of a `tt()` table.
#' - If `i` and `j` are both `NULL`, escape all cells, column names, caption, notes, and spanning labels created by `group_tt()`.
#' @param markdown Logical; if TRUE, render markdown syntax in cells. Ex: `_italicized text_` is properly italicized in HTML and LaTeX.
#' @param fn Function for custom formatting. Accepts a vector and returns a character vector of the same length.
#' @param quarto Logical. Enable Quarto data processing and wrap cell content in a `data-qmd` span (HTML) or `\QuartoMarkdownBase64{}` macro (LaTeX). See warnings in the Global Options section below.
#' @param sprintf String passed to the `?sprintf` function to format numbers or interpolate strings with a user-defined pattern (similar to the `glue` package, but using Base R).
#' @param linebreak NULL or a single string. If it is a string, replaces that string with appropriate line break sequences depending on the output format (HTML: `<br>`, LaTeX: `\\\\`, Typst: `\\ `). Markdown output is excluded from line break replacement.
#' @param output Apply formatting only if the `tt()` object is rendered in the specified format. A character vector of one or more of "latex", "html", "typst", or "markdown". If `NULL` (default), apply formatting regardless of the output format.
#' @inheritParams tt
#' @inheritParams style_tt
#' @template global_options
#'
#' @return A data frame with formatted columns.
#' @export
#' @examples
#' dat <- data.frame(
#'   a = rnorm(3, mean = 10000),
#'   b = rnorm(3, 10000)
#' )
#' tab <- tt(dat)
#' format_tt(tab,
#'   digits = 2,
#'   num_mark_dec = ",",
#'   num_mark_big = " "
#' )
#'
#' k <- tt(data.frame(x = c(0.000123456789, 12.4356789)))
#' format_tt(k, digits = 2, num_fmt = "significant_cell")
#'
#' dat <- data.frame(
#'   a = c("Burger", "Halloumi", "Tofu", "Beans"),
#'   b = c(1.43202, 201.399, 0.146188, 0.0031),
#'   c = c(98938272783457, 7288839482, 29111727, 93945)
#' )
#' tt(dat) |>
#'   format_tt(j = "a", sprintf = "Food: %s") |>
#'   format_tt(j = 2, digits = 1, num_fmt = "decimal", num_zero = TRUE) |>
#'   format_tt(j = "c", digits = 2, num_suffix = TRUE)
#'
#' y <- tt(data.frame(x = c(123456789.678, 12435.6789)))
#' format_tt(y, digits = 3, num_mark_big = " ")
#'
#' x <- tt(data.frame(Text = c("_italicized text_", "__bold text__")))
#' format_tt(x, markdown = TRUE)
#'
#' # Line breaks using linebreak argument
#' d <- data.frame(Text = "First line<br>Second line")
#' tt(d) |> format_tt(linebreak = "<br>")
#'
#' # Non-standard evaluation (NSE)
#' dat <- data.frame(
#'   w = c(143002.2092, 201399.181, 100188.3883),
#'   x = c(1.43402, 201.399, 0.134588),
#'   y = as.Date(c(897, 232, 198), origin = "1970-01-01"),
#'   z = c(TRUE, TRUE, FALSE)
#' )
#' tt(dat) |>
#'   format_tt(i = w > 150000, j = w, digits = 0, num_mark_big = ",")
#'
#' tab <- data.frame(a = c(NA, 1, 2), b = c(3, NA, 5))
#' tt(tab) |> format_tt(replace = "-")
#'
#' dat <- data.frame(
#'   "LaTeX" = c("Dollars $", "Percent %", "Underscore _"),
#'   "HTML" = c("<br>", "<sup>4</sup>", "<emph>blah</emph>")
#' )
#' tt(dat) |> format_tt(escape = TRUE)
#'
format_tt <- function(
    x,
    i = NULL,
    j = NULL,
    digits = get_option("tinytable_format_digits", default = NULL),
    num_fmt = get_option("tinytable_format_num_fmt", default = "significant"),
    num_zero = get_option("tinytable_format_num_zero", default = FALSE),
    num_suffix = get_option("tinytable_format_num_suffix", default = FALSE),
    num_mark_big = get_option("tinytable_format_num_mark_big", default = ""),
    num_mark_dec = get_option(
      "tinytable_format_num_mark_dec",
      default = getOption("OutDec", default = ".")
    ),
    date = get_option("tinytable_format_date", default = NULL),
    bool = get_option("tinytable_format_bool", default = NULL),
    math = get_option("tinytable_format_math", default = FALSE),
    other = get_option("tinytable_format_other", default = NULL),
    replace = get_option("tinytable_format_replace", default = FALSE),
    escape = get_option("tinytable_format_escape", default = FALSE),
    markdown = get_option("tinytable_format_markdown", default = FALSE),
    quarto = get_option("tinytable_format_quarto", default = FALSE),
    fn = get_option("tinytable_format_fn", default = NULL),
    sprintf = get_option("tinytable_format_sprintf", default = NULL),
    linebreak = get_option("tinytable_format_linebreak", default = NULL),
    output = get_option("tinytable_format_output", default = NULL)) {
  assert_integerish(digits, len = 1, null.ok = TRUE)
  assert_choice(
    num_fmt,
    c("significant", "significant_cell", "decimal", "scientific")
  )
  assert_flag(num_zero)
  assert_flag(num_suffix)
  assert_string(num_mark_big)
  assert_string(num_mark_dec)
  assert_string(date, null.ok = TRUE)
  assert_function(bool, null.ok = TRUE)
  assert_flag(math)
  assert_function(other, null.ok = TRUE)
  assert_flag(markdown)
  assert_flag(quarto)
  assert_function(fn, null.ok = TRUE)
  assert_subset(output, c("latex", "html", "typst", "markdown"), null.ok = TRUE)
  assert_string(sprintf, null.ok = TRUE)
  assert_string(linebreak, null.ok = TRUE)
  replace <- sanitize_replace(replace)
  sanity_num_mark(digits, num_mark_big, num_mark_dec)

  out <- x

  # non-standard evaluation before anything else
  tmp <- nse_i_j(x, i_expr = substitute(i), j_expr = substitute(j), pf = parent.frame())
  list2env(tmp, environment())

  # single argument list shared by the lazy call and the direct invocation
  args <- list(
    i = i,
    j = j,
    digits = digits,
    num_fmt = num_fmt,
    num_zero = num_zero,
    num_suffix = num_suffix,
    num_mark_big = num_mark_big,
    num_mark_dec = num_mark_dec,
    replace = replace,
    fn = fn,
    sprintf = sprintf,
    date_format = date,
    bool = bool,
    math = math,
    escape = escape,
    markdown = markdown,
    quarto = quarto,
    other = other,
    linebreak = linebreak,
    output = output
  )

  if (inherits(out, "tinytable")) {
    cal <- as.call(c(list(quote(format_tt_lazy)), args))
    attr(cal, "output") <- output
    out@lazy_format <- c(out@lazy_format, list(cal))
  } else {
    out <- do.call(format_tt_lazy, c(list(x = out), args))
  }

  return(out)
}


format_tt_lazy <- function(
    x,
    i,
    j,
    digits,
    num_fmt,
    num_zero,
    num_suffix,
    num_mark_big,
    num_mark_dec,
    replace,
    fn,
    sprintf,
    date_format,
    bool,
    math,
    escape,
    markdown,
    quarto,
    other,
    linebreak,
    output) {
  # Row indices derived from an NSE predicate were computed eagerly in
  # format_tt() against the pre-group data rows. Although the @lazy_format
  # loop runs before rbind_body_groupi(), apply_format() interprets numeric
  # `i` in final-table coordinates (it matches `x@index_body %in% i`), so
  # predicate-derived indices must be remapped here as well. @index_body is
  # already available: build_tt() computes it before the lazy_format loop.
  if (inherits(x, "tinytable")) {
    i <- nse_remap_i(i, x)
  }

  if (inherits(x, "tbl_df")) {
    assert_dependency("tibble")
    x_is_tibble <- TRUE
    x <- as.data.frame(x, check.names = FALSE)
  } else {
    x_is_tibble <- FALSE
  }

  # important for tabulator
  if (!is.null(bool) && inherits(x, "tinytable")) {
    x@tabulator_format_bool <- TRUE
  }

  # Check if i contains component names (do this before processing tinytable objects)
  tmp <- resolve_i_components(x, i, j, default = "all")
  i <- tmp$i
  components <- tmp$components

  # cell-targeting components only: the type-based formatters (logical, date,
  # numeric, other) must not reformat body cells when `i` targets a component
  # such as "caption" or "colnames"
  components_cells <- intersect(
    if ("all" %in% components) c("cells", "groupi", "~groupi") else components,
    c("cells", "groupi", "~groupi")
  )

  # format_tt() supports vectors
  if (isTRUE(check_atomic_vector(x))) {
    atomic_vector <- TRUE
    if (is.factor(x)) {
      x <- as.character(x)
    }
    x <- data.frame(tinytable = x, stringsAsFactors = FALSE)
    j <- 1
  } else if (is.data.frame(x)) {
    atomic_vector <- FALSE
  } else if (inherits(x, "tinytable")) {
    atomic_vector <- FALSE
  } else {
    stop(
      "`x` must be a `tinytable` object, a data frame, or an atomic vector.",
      call. = FALSE
    )
  }

  # data frames and vectors do not carry their raw data the way tinytable
  # objects do in @data; keep a copy so `replace` can match typed original
  # values even after digits/date formatting stringified the working copy
  original_input <- if (!inherits(x, "tinytable")) x else NULL
  output_format <- if (inherits(x, "tinytable")) x@output else NULL

  # In sanity_tt(), we fill in missing NULL `j` in the format-specific versions,
  # because tabularray can do whole column styling. Here, we need to fill in
  # NULL for all formats since this is applied before creating the table.
  # nrow(out) because nrow(x) sometimes includes rows that will be added **in the lazy future** by group_tt()
  i <- sanitize_i(i, x, lazy = FALSE, calling_function = "format_tt")
  j <- sanitize_j(j, x, skip_tabulator_types = TRUE)

  x <- apply_format(
    x = x,
    i = i,
    j = j,
    components = components_cells,
    format_fn = format_vector_logical,
    inherit_class = "logical",
    bool_fn = bool
  )

  x <- apply_format(
    x = x,
    i = i,
    j = j,
    components = components_cells,
    format_fn = format_vector_date,
    inherit_class = "Date",
    date_format = date_format
  )

  x <- apply_format(
    x = x,
    i = i,
    j = j,
    components = components_cells,
    format_fn = format_vector_numeric,
    num_suffix = num_suffix,
    digits = digits,
    num_mark_big = num_mark_big,
    num_mark_dec = num_mark_dec,
    num_zero = num_zero,
    num_fmt = num_fmt,
    inherit_class = is.numeric
  )

  is_other <- function(x) {
    !is.numeric(x) && !inherits(x, "Date") && !is.logical(x)
  }
  x <- apply_format(
    x = x,
    i = i,
    j = j,
    components = components_cells,
    format_fn = format_vector_other,
    inherit_class = is_other,
    other_fn = other
  )

  # after other formatting
  if (!is.null(sprintf)) {
    x <- apply_format(
      x = x,
      i = i,
      j = j,
      components = components,
      format_fn = format_vector_sprintf,
      original_data = FALSE,
      sprintf_pattern = sprintf
    )
  }

  # Custom functions overwrite all the other formatting, but is before markdown
  # before escaping
  if (is.function(fn)) {
    x <- apply_format(
      x = x,
      i = i,
      j = j,
      format_fn = format_vector_custom,
      components = components,
      fn = fn
    )
  }

  # close to last
  if (isTRUE(math)) {
    x <- apply_format(
      x = x,
      i = i,
      j = j,
      format_fn = format_vector_math,
      components = components,
      original_data = FALSE,
      math = math
    )
  }

  # linebreak before replace and escape
  if (!is.null(linebreak)) {
    x <- apply_format(
      x = x,
      i = i,
      j = j,
      format_fn = format_vector_linebreak,
      components = components,
      original_data = FALSE,
      linebreak = linebreak,
      output = output_format
    )
  }

  # replace before escape, otherwise overaggressive removal
  x <- apply_format(
    x = x,
    i = i,
    j = j,
    format_fn = format_vector_replace,
    components = components,
    replace = replace,
    original_data = FALSE,
    original = original_input
  )

  # escape latex characters
  if (!isFALSE(escape)) {
    if (isTRUE(escape == "latex")) {
      o <- "latex"
    } else if (isTRUE(escape == "html")) {
      o <- "html"
    } else if (isTRUE(escape == "typst")) {
      o <- "typst"
    } else if (inherits(x, "tinytable")) {
      o <- x@output
    } else {
      o <- FALSE
    }

    x <- apply_format(
      x = x,
      i = i,
      j = j,
      format_fn = format_vector_escape,
      components = components,
      original_data = FALSE,
      output = o
    )
  }

  # markdown and quarto at the very end
  if (isTRUE(markdown)) {
    assert_dependency("litedown")
    x <- apply_format(
      x = x,
      i = i,
      j = j,
      format_fn = format_vector_markdown,
      components = components,
      original_data = FALSE,
      output_format = output_format
    )
  }

  # quarto processing needs a rendered table string; no-op on plain data
  # frames and vectors
  if (isTRUE(quarto) && inherits(x, "tinytable")) {
    # assert at the call site because `apply_format()` swallows errors raised
    # by the formatter, which would silently skip the cells
    if (isTRUE(x@output == "latex")) {
      assert_dependency("base64enc")
    }
    if (isTRUE(x@output %in% c("html", "bootstrap", "tabulator"))) {
      fun <- function(z) {
        z@table_string <- sub(
          "data-quarto-disable-processing='true'",
          "data-quarto-disable-processing='false'",
          z@table_string,
          fixed = TRUE
        )
        return(z)
      }
      x <- style_tt(x, finalize = fun)
    }
    x <- apply_format(
      x = x,
      i = i,
      j = j,
      format_fn = format_vector_quarto,
      components = components,
      original_data = FALSE,
      output_format = output_format
    )
  }

  # output
  if (isTRUE(atomic_vector)) {
    x <- x[[1]]
  }

  if (!inherits(x, "tinytable") && x_is_tibble) {
    x <- tibble::as_tibble(x)
  }

  return(x)
}


# Resolve the `i` argument into row indices and a vector of component names.
# `i` may be a numeric vector of rows, "groupi"/"~groupi" (row group labels or
# non-group rows), or a character vector of component names such as "caption",
# "colnames", "notes", or "groupj". Shared by format_tt_lazy() and
# rotate_cells_setup(); `default` is used when neither `i` nor `j` is supplied.
resolve_i_components <- function(x, i, j, default = "all") {
  if (identical(i, "groupi")) {
    components <- "cells"
    i <- x@group_index_i
  } else if (identical(i, "~groupi")) {
    components <- "cells"
    i <- setdiff(seq_len(nrow(x)), x@group_index_i)
  } else if (is.character(i)) {
    components <- i # before wiping i
    i <- NULL
  } else if (!is.null(i) || !is.null(j)) {
    components <- "cells"
  } else {
    components <- default
  }
  list(i = i, components = components)
}

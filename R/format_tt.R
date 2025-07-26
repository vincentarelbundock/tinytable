format_quarto <- function(i, col, x) {
  out <- x@data_body
  if (isTRUE(x@output == "html")) {
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
    out[i, col] <- sprintf(
      '<span data-qmd="%s"></span>',
      out[i, col, drop = TRUE]
    )
  } else if (isTRUE(x@output == "latex")) {
    assert_dependency("base64enc")
    tmp <- sapply(
      out[i, col, drop = TRUE],
      function(z) base64enc::base64encode(charToRaw(z))
    )
    out[i, col] <- sprintf("\\QuartoMarkdownBase64{%s}", tmp)
  }

  x@data_body <- out
  return(x)
}

apply_caption <- function(x, format_fn, ...) {
  if (!inherits(x, "tinytable")) {
    return(x)
  }
  x@caption <- format_fn(x@caption, ...)
  return(x)
}

apply_notes <- function(x, format_fn, ...) {
  if (!inherits(x, "tinytable")) {
    return(x)
  }

  for (idx in seq_along(x@notes)) {
    n <- x@notes[[idx]]
    if (is.character(n) && length(n) == 1) {
      x@notes[[idx]] <- format_fn(n, ...)
    } else if (is.list(n) && "text" %in% names(n)) {
      n$text <- format_fn(n$text, ...)
      x@notes[[idx]] <- n
    }
  }
  return(x)
}

apply_group_j <- function(x, format_fn, ...) {
  if (!inherits(x, "tinytable")) return(x)
  data_slot <- x@data_group_j
  if (nrow(data_slot) > 0) {
    for (row_idx in seq_len(nrow(data_slot))) {
      for (col_idx in seq_len(ncol(data_slot))) {
        current_value <- data_slot[row_idx, col_idx]
        if (!is.na(current_value) && trimws(current_value) != "") {
          formatted_value <- format_fn(current_value, ...)
          if (!is.null(formatted_value)) {
            data_slot[row_idx, col_idx] <- formatted_value
          }
        }
      }
    }
  }
  x@data_group_j <- data_slot
  return(x)
}

apply_colnames <- function(x, format_fn, ...) {
  colnames(x) <- format_fn(colnames(x), ...)
  return(x)
}

# Global dispatcher function for applying formatting functions
apply_format <- function(
  x,
  i,
  j,
  format_fn,
  components = NULL,
  inherit_class = NULL,
  original_data = TRUE,
  ...
) {
  if (is.character(components)) {
    if ("all" %in% components) {
      components <- c("colnames", "caption", "notes", "groupi", "groupj", "cells")
    }
  }

  if (identical(components, "groupi")) {
    i <- x@index_group_i
  }

  if (inherits(x, "tinytable")) {
    out <- x@data_body
    ori <- x@data
  } else {
    out <- ori <- x
  }

  # Apply formatting to specified components only
  if ("colnames" %in% components) {
    x <- apply_colnames(x, format_fn, ...)
  }
  if ("caption" %in% components) {
    x <- apply_caption(x, format_fn, ...)
  }
  if ("notes" %in% components) {
    x <- apply_notes(x, format_fn, ...)
  }
  if ("groupj" %in% components) {
    x <- apply_group_j(x, format_fn, ...)
  }

  if (is.null(components) || any(c("cells", "groupi") %in% components)) {
    # Apply to specific cells
    # Filter columns based on inherits argument and original data
    j_filtered <- j

    if (inherits(x, "tinytable")) {
      classref <- x@data_body
    } else {
      classref <- out
    }

    if (is.character(inherit_class)) {
      j_filtered <- j[sapply(j, function(col) inherits(classref[[col]], inherit_class))]
    } else if (is.function(inherit_class)) {
      j_filtered <- j[sapply(j, function(col) inherit_class(classref[[col]]))]
    }

    # Default behavior: use original values, fall back to out if ori is not available
    for (col in j_filtered) {
      idx_body <- if (inherits(x, "tinytable")) x@index_body else seq_len(nrow(out))

      # original data rows
      if (original_data) {
        idx_out <- i

        idx_out <- intersect(i, idx_body)
        idx_ori <- seq_len(nrow(ori))[idx_body %in% i]
        tmp <- tryCatch(format_fn(ori[idx_ori, col, drop = TRUE], ...),
          error = function(e) NULL)
        if (length(tmp) > 0) out[idx_out, col] <- tmp

        # row groups
        idx_out <- setdiff(i, idx_body)
        tmp <- tryCatch(format_fn(out[idx_out, col, drop = TRUE], ...),
          error = function(e) NULL)
        if (length(tmp) > 0) out[idx_out, col] <- tmp

      } else {
        tmp <- tryCatch(format_fn(out[i, col, drop = TRUE], ...),
          error = function(e) NULL)
        if (length(tmp) > 0) out[i, col] <- tmp
      }
    }

    if (inherits(x, "tinytable")) {
      x@data_body <- out
    } else {
      x <- out
    }
  }


  return(x)
}


#' Format columns of a data frame
#'
#' This function formats the columns of a data frame based on the column type (logical, date, numeric).
#' It allows various formatting options like significant digits, decimal points, and scientific notation.
#' It also includes custom formatting for date and boolean values.
#' If this function is applied several times to the same cell, the last transformation is retained and the previous calls are ignored, except for the `escape` argument which can be applied to previously transformed data.
#'
#' @param x A data frame or a vector to be formatted.
#' @param i Numeric vector or string.
#'   - Numeric vector: Row indices where the styling should be applied. Can be a single value or a vector.
#'   - String: Table components to format, "all", "cells", "colnames", "caption", "notes", "groupi" (row group labels), "groupj" (column group labels).
#'   - If both the `i` and `j` are omitted (default: NULL), formatting is applied to all table elements, including caption, notes, and group labels.
#' @param digits Number of significant digits or decimal places.
#' @param num_fmt The format for numeric values; one of 'significant', 'significant_cell', 'decimal', or 'scientific'.
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
  sprintf = get_option("tinytable_format_sprintf", default = NULL)
) {
  assert_integerish(digits, len = 1, null.ok = TRUE)
  assert_choice(
    num_fmt,
    c("significant", "significant_cell", "decimal", "scientific")
  )
  assert_flag(num_zero)
  assert_string(num_mark_big)
  assert_string(num_mark_dec)
  assert_string(date, null.ok = TRUE)
  assert_function(bool, null.ok = TRUE)
  assert_function(identity, null.ok = TRUE)
  assert_function(other, null.ok = TRUE)
  assert_flag(markdown)
  assert_flag(quarto)
  assert_function(fn, null.ok = TRUE)
  assert_string(sprintf, null.ok = TRUE)
  replace <- sanitize_replace(replace)
  sanity_num_mark(digits, num_mark_big, num_mark_dec)

  # Check if i contains component names (do this before processing tinytable objects)
  if (is.character(i)) {
    components <- i
    i <- NULL
  } else if (!is.null(i) || !is.null(j)) {
    components <- "cells"
  } else {
    components <- "all"
  }

  out <- x

  if (inherits(out, "tinytable")) {
    cal <- call(
      "format_tt_lazy",
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
      components = components
    )
    out@lazy_format <- c(out@lazy_format, list(cal))
  } else {
    out <- format_tt_lazy(
      out,
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
      other = other,
      escape = escape,
      quarto = quarto,
      markdown = markdown,
      components = components
    )
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
  components = NULL
) {
  if (inherits(x, "tbl_df")) {
    assert_dependency("tibble")
    x_is_tibble <- TRUE
    x <- as.data.frame(x, check.names = FALSE)
  } else {
    x_is_tibble <- FALSE
  }

  # format_tt() supports vectors
  if (isTRUE(check_atomic_vector(x))) {
    atomic_vector <- TRUE
    if (is.factor(x)) {
      x <- as.character(x)
    }
    ori <- out <- x <- data.frame(tinytable = x, stringsAsFactors = FALSE)
    j <- 1
  } else if (is.data.frame(x)) {
    atomic_vector <- FALSE
    ori <- out <- x
  } else if (inherits(x, "tinytable")) {
    atomic_vector <- FALSE
    # if no other format_tt() call has been applied, we ctan have numeric values
    out <- x@data_body
    ori <- x@data
  } else {
    stop(
      "`x` must be a `tinytable` object, a data frame, or an atomic vector.",
      call. = FALSE
    )
  }

  # In sanity_tt(), we fill in missing NULL `j` in the format-specific versions,
  # because tabularray can do whole column styling. Here, we need to fill in
  # NULL for all formats since this is applied before creating the table.
  # nrow(out) because nrow(x) sometimes includes rows that will be added **in the lazy future** by group_tt()
  i <- sanitize_i(i, x, lazy = FALSE, calling_function = "format_tt")
  j <- sanitize_j(j, x)

  x <- apply_format(
    x = x,
    i = i,
    j = j,
    format_fn = format_vector_logical,
    inherit_class = "logical",
    bool_fn = bool
  )

  x <- apply_format(
    x = x,
    i = i,
    j = j,
    format_fn = format_vector_date,
    inherit_class = "Date",
    date_format = date_format
  )

  x <- apply_format(
    x = x,
    i = i,
    j = j,
    format_fn = format_numeric,
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
      sprintf_pattern = sprintf
    )
  }

  # Custom functions overwrite all the other formatting, but is before markdown
  # before escaping
  if (is.function(fn)) {
    if (!is.null(components)) {
      # Use apply_format for component-specific formatting
      x <- apply_format(
        x = x,
        i = i,
        j = j,
        format_fn = format_vector_custom,
        components = components,
        fn = fn
      )
    } else {
      # Original behavior for cell-specific formatting
      for (col in j) {
        tmp <- tryCatch(
          format_vector_custom(ori[i, col, drop = TRUE], fn), 
          error = function(e) NULL)
        out[i, col] <- if (length(tmp) > 0) tmp else out[i, col]
      }
    }
  }

  # close to last
if (isTRUE(math)) {
  x <- apply_format(
      x = x,
      i = i,
      j = j,
      format_fn = format_vector_math,
      components = components,
      math = math
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
    original_data = FALSE
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
      format_fn = escape_text,
      components = components,
      original_data = FALSE,
      output = o
    )

    # column names when 0 is in i
    if (0 %in% i && !identical(components, "all")) {
      x <- apply_colnames(x, escape_text, output = o)
    }
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
      output_format = x@output
    )
  }

  if (isTRUE(quarto)) {
    for (col in j) {
      x <- format_quarto(i = i, col = col, x = x)
    }
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

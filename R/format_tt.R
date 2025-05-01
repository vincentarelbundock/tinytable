#' Format columns of a data frame
#'
#' This function formats the columns of a data frame based on the column type (logical, date, numeric).
#' It allows various formatting options like significant digits, decimal points, and scientific notation.
#' It also includes custom formatting for date and boolean values.
#' If this function is applied several times to the same cell, the last transformation is retained and the previous calls are ignored, except for the `escape` argument which can be applied to previously transformed data.
#'
#' @param x A data frame or a vector to be formatted.
#' @param i Row indices where the formatting should be applied.
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
      url = url,
      date = date,
      bool = bool,
      math = math,
      escape = escape,
      markdown = markdown,
      quarto = quarto,
      other = other,
      inull = is.null(i),
      jnull = is.null(j)
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
      url = url,
      date = date,
      bool = bool,
      math = math,
      other = other,
      escape = escape,
      quarto = quarto,
      markdown = markdown,
      inull = is.null(i),
      jnull = is.null(j)
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
  url,
  date,
  bool,
  math,
  escape,
  markdown,
  quarto,
  other,
  inull,
  jnull
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
    if (is.factor(x)) x <- as.character(x)
    ori <- out <- x <- data.frame(tinytable = x, stringsAsFactors = FALSE)
    j <- 1
  } else if (is.data.frame(x)) {
    atomic_vector <- FALSE
    ori <- out <- x
  } else if (inherits(x, "tinytable")) {
    atomic_vector <- FALSE
    # if no other format_tt() call has been applied, we ctan have numeric values
    out <- x@table_dataframe
    ori <- x@data
  } else {
    stop(
      "`x` must be a `tinytable` object, a data frame, or an atomic vector.",
      call. = FALSE
    )
  }

  i <- sanitize_i(i, x, lazy = FALSE)
  j <- sanitize_j(j, x)

  # In sanity_tt(), we fill in missing NULL `j` in the format-specific versions,
  # because tabularray can do whole column styling. Here, we need to fill in
  # NULL for all formats since this is applied before creating the table.
  # nrow(out) because nrow(x) sometimes includes rows that will be added **in the lazy future** by group_tt()

  # format each column
  # Issue #230: drop=TRUE fixes bug which returned a character dput-like vector
  for (col in j) {
    # sprintf() is self-contained
    if (!is.null(sprintf)) {
      out[i, col] <- base::sprintf(sprintf, ori[i, col, drop = TRUE])
    } else {
      # logical
      if (!is.null(bool) && is.logical(ori[i, col])) {
        out[i, col] <- bool(ori[i, col, drop = TRUE])

        # date
      } else if (!is.null(date) && inherits(ori[i, col], "Date")) {
        out[i, col] <- format(ori[i, col, drop = TRUE], date)

        # numeric
      } else if (!is.null(digits) && is.numeric(ori[i, col, drop = TRUE])) {
        tmp <- format_numeric(
          ori[i, col],
          num_suffix = num_suffix,
          digits = digits,
          num_mark_big = num_mark_big,
          num_mark_dec = num_mark_dec,
          num_zero = num_zero,
          num_fmt = num_fmt
        )
        if (!is.null(tmp)) out[i, col] <- tmp

        # other
      } else if (is.function(other)) {
        out[i, col] <- other(ori[i, col, drop = TRUE])
      }
    }

    for (k in seq_along(replace)) {
      idx <- ori[i, col, drop = TRUE] %in% replace[[k]]
      if (identical(names(replace)[[k]], " ")) {
        out[i, col][idx] <- ""
      } else {
        out[i, col][idx] <- names(replace)[[k]]
      }
    }
  } # loop over columns

  # Custom functions overwrite all the other formatting, but is before markdown
  # before escaping
  if (is.function(fn)) {
    for (col in j) {
      out[i, col] <- fn(ori[i, col, drop = TRUE])
    }
  }

  if (isTRUE(math)) {
    for (row in i) {
      for (col in j) {
        out[row, col] <- format_math(out[row, col], math)
      }
    }
    if (inull && jnull) {
      x@caption <- format_math(x@caption, math)
      colnames(x) <- format_math(colnames(x), math)
      for (idx in seq_along(x@notes)) {
        n <- x@notes[[idx]]
        if (is.character(n) && length(n) == 1) {
          x@notes[[idx]] <- format_math(n, math = math)
        } else if (is.list(n) && "text" %in% names(n)) {
          n$text <- format_math(n$text, math = math)
          x@notes[[idx]] <- n
        }
      }
      for (idx in seq_along(x@lazy_group)) {
        g <- x@lazy_group[[idx]]
        if (!is.null(g$j)) {
          names(g$j) <- format_math(names(g$j), math = math)
        }
        if (!is.null(g$i)) {
          names(g$i) <- format_math(names(g$i), math = math)
        }
        x@lazy_group[[idx]] <- g
      }
    }
  }

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

    # caption & groups: if i and j are both null
    if (inull && jnull) {
      if (inherits(x, "tinytable")) {
        x@caption <- escape_text(x@caption, output = o)

        for (idx in seq_along(x@notes)) {
          n <- x@notes[[idx]]
          if (is.character(n) && length(n) == 1) {
            x@notes[[idx]] <- escape_text(n, output = o)
          } else if (is.list(n) && "text" %in% names(n)) {
            n$text <- escape_text(n$text, output = o)
            x@notes[[idx]] <- n
          }
        }

        for (idx in seq_along(x@lazy_group)) {
          g <- x@lazy_group[[idx]]
          if (!is.null(g$j)) {
            names(g$j) <- escape_text(names(g$j), output = o)
          }
          if (!is.null(g$i)) {
            names(g$i) <- escape_text(names(g$i), output = o)
          }
          x@lazy_group[[idx]] <- g
        }
      }
      colnames(x) <- escape_text(colnames(x), output = o)
      for (col in seq_len(ncol(out))) {
        out[, col] <- escape_text(out[, col], output = o)
      }
    } else {
      # body
      for (row in i) {
        for (col in j) {
          out[row, col] <- escape_text(out[row, col], output = o)
        }
      }

      # column names
      if (0 %in% i) {
        colnames(x) <- escape_text(colnames(x), output = o)
      }
    }
  }

  # markdown and quarto at the very end
  for (col in j) {
    if (isTRUE(markdown)) {
      assert_dependency("litedown")
      out <- format_markdown(out = out, i = i, col = col, x = x)
    }

    if (isTRUE(quarto)) {
      tmp <- format_quarto(out = out, i = i, col = col, x = x)
      out <- tmp$out
      x <- tmp$x
    }
  }

  if (inull && jnull && isTRUE(markdown)) {
    colnames(x) <- format_markdown(colnames(x), x = x)
    if (inherits(x, "tinytable")) {
      for (k in seq_along(x@lazy_group)) {
        g <- x@lazy_group[[k]]
        if (!is.null(g$j)) {
          names(g$j) <- format_markdown(names(g$j), x = x)
        }
        if (!is.null(g$i)) {
          names(g$i) <- format_markdown(names(g$i), x = x)
        }
        x@lazy_group[[k]] <- g
      }
    }
  }

  # output
  if (isTRUE(atomic_vector)) {
    return(out[[1]])
  } else if (!inherits(x, "tinytable")) {
    if (x_is_tibble) {
      out <- tibble::as_tibble(out)
    }
    return(out)
  } else {
    x@table_dataframe <- out
    return(x)
  }
}

format_math <- function(out, math) {
  if (isTRUE(math)) {
    out <- sprintf("$%s$", out)
  }
  return(out)
}

format_markdown <- function(out, i = NULL, col = NULL, x) {
  tmpfun_html <- function(k) {
    k <- litedown::mark(I(k), "html")
    k <- sub("<p>", "", k, fixed = TRUE)
    k <- sub("</p>", "", k, fixed = TRUE)
    return(k)
  }

  tmpfun_latex <- function(k) {
    k <- litedown::mark(I(k), "latex")
    return(k)
  }

  if (inherits(out, "data.frame")) {
    ipos <- i[i > 0]
    if (length(ipos) > 0) {
      if (inherits(x, "tinytable_bootstrap")) {
        out[ipos, col] <- sapply(out[ipos, col], function(k) tmpfun_html(k))
      } else if (inherits(x, "tinytable_tabularray")) {
        out[ipos, col] <- sapply(out[ipos, col], function(k) tmpfun_latex(k))
      }
    }
  } else {
    if (inherits(x, "tinytable_bootstrap")) {
      out <- sapply(out, function(k) tmpfun_html(k))
    } else if (inherits(x, "tinytable_tabularray")) {
      out <- sapply(out, function(k) tmpfun_latex(k))
    }
  }

  return(out)
}

format_quarto <- function(out, i, col, x) {
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

  return(list("out" = out, "x" = x))
}

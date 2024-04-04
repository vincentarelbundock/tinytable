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
#' @param other A function to format columns of other types. Defaults to `as.character()`.
#' @param replace String or Named list of vectors
#' - String: Replace `NA` entries by the string.
#' - Named list: Matching elements of the vectors by theirs names. Ex: `replace=list("-"=c(NA,NaN), "Small"=-Inf, "Big"=Inf)`
#' @param escape Logical or "latex" or "html". If TRUE, escape special characters to display them as text in the format of the output of a `tt()` table.
#' - If `i` is `NULL`, escape the `j` columns and column names.
#' - If `i` and `j` are both `NULL`, escape all cells, column names, caption, notes, and spanning labels created by `group_tt()`.
#' @param markdown Logical; if TRUE, render markdown syntax in cells. Ex: `_italicized text_` is properly italicized in HTML and LaTeX.
#' @param fn Function for custom formatting. Accepts a vector and returns a character vector of the same length.
#' @param sprintf String passed to the `?sprintf` function to format numbers or interpolate strings with a user-defined pattern (similar to the `glue` package, but using Base R).
#' @param ... Additional arguments are ignored.
#' @inheritParams tt
#' @inheritParams style_tt
#'
#' @return A data frame with formatted columns.
#' @export
#' @examples
#' dat <- data.frame(
#'   a = rnorm(3, mean = 10000),
#'   b = rnorm(3, 10000))
#' tab <- tt(dat)
#' format_tt(tab,
#'  digits = 2,
#'  num_mark_dec = ",",
#'  num_mark_big = " ")
#'  
#' k <- tt(data.frame(x = c(0.000123456789, 12.4356789)))
#' format_tt(k, digits = 2, num_fmt = "significant_cell")
#'  
#' dat <- data.frame(
#'    a = c("Burger", "Halloumi", "Tofu", "Beans"),
#'    b = c(1.43202, 201.399, 0.146188, 0.0031),
#'    c = c(98938272783457, 7288839482, 29111727, 93945))
#' tt(dat) |>
#'  format_tt(j = "a", sprintf = "Food: %s") |>
#'  format_tt(j = 2, digits = 1, num_fmt = "decimal", num_zero = TRUE) |>
#'  format_tt(j = "c", digits = 2, num_suffix = TRUE)
#'  
#' y <- tt(data.frame(x = c(123456789.678, 12435.6789)))
#' format_tt(y, digits=3, num_mark_big=" ")
#'
#' x <- tt(data.frame(Text = c("_italicized text_", "__bold text__")))
#' format_tt(x, markdown=TRUE)
#'
#' tab <- data.frame(a = c(NA, 1, 2), b = c(3, NA, 5))
#' tt(tab) |> format_tt(replace = "-")
#'
#' dat <- data.frame(
#'    "LaTeX" = c("Dollars $", "Percent %", "Underscore _"),
#'    "HTML" = c("<br>", "<sup>4</sup>", "<emph>blah</emph>")
#' )
#' tt(dat) |> format_tt(escape = TRUE)   
#'
format_tt <- function(x,
                      i = NULL,
                      j = NULL,
                      digits = getOption("tinytable_format_digits", default = NULL),
                      num_fmt = getOption("tinytable_format_num_fmt", default = "significant"),
                      num_zero = getOption("tinytable_format_num_zero", default = FALSE),
                      num_suffix = getOption("tinytable_format_num_suffix", default = FALSE),
                      num_mark_big = getOption("tinytable_format_num_mark_big", default = ""),
                      num_mark_dec = getOption("tinytable_format_num_mark_dec", default = getOption("OutDec", default = ".")),
                      date = getOption("tinytable_format_date", default = "%Y-%m-%d"),
                      bool = getOption("tinytable_format_bool", default = function(column) tools::toTitleCase(tolower(column))),
                      other = getOption("tinytable_format_other", default = as.character),
                      replace = getOption("tinytable_format_replace", default = ""),
                      escape = getOption("tinytable_format_escape", default = FALSE),
                      markdown = getOption("tinytable_format_markdown", default = FALSE),
                      fn = getOption("tinytable_format_fn", default = NULL),
                      sprintf = getOption("tinytable_format_sprintf", default = NULL),
                      ...
                      ) {


  out <- x

  dots <- list(...)
  if ("replace_na" %in% names(dots)) {
      replace <- dots[["replace_na"]]
      warning("The `replace_na` argument was renamed `replace`.", call. = FALSE)
  }

  if (inherits(out, "tinytable")) {
    cal <- call("format_tt_lazy", 
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
                escape = escape,
                markdown = markdown,
                other = other)
    out@lazy_format <- c(out@lazy_format, list(cal))
  } else {

    out <- format_tt_lazy(out,
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
                          other = other,
                          escape = escape,
                          markdown = markdown)
  }

  return(out)
}

format_tt_lazy <- function(x,
                           i = NULL,
                           j = NULL,
                           digits,
                           num_fmt = "significant",
                           num_zero = FALSE,
                           num_suffix = FALSE,
                           num_mark_big = "",
                           num_mark_dec = NULL,
                           replace = "",
                           fn = NULL,
                           sprintf = NULL,
                           url = FALSE,
                           date = "%Y-%m-%d",
                           bool = identity,
                           escape = FALSE,
                           markdown = FALSE,
                           other = as.character
                           ) {

  if (isTRUE(check_atomic_vector(x))) {
    atomic_vector <- TRUE
    if (is.factor(x)) x <- as.character(x)
    ori <- out <- x <- data.frame(tinytable = x, stringsAsFactors = FALSE)
    j <- 1
  } else if (is.data.frame(x)) {
    atomic_vector <- FALSE
    ori <- out <- x
  } else if (inherits(x, "tinytable")){
    atomic_vector <- FALSE
    # if no other format_tt() call has been applied, we ctan have numeric values
    out <- x@table_dataframe
    ori <- x@data
  } else {
    stop("`x` must be a `tinytable` object, a data frame, or an atomic vector.", call. = FALSE)
  }

  assert_integerish(digits, len = 1, null.ok = TRUE)
  assert_integerish(i, null.ok = TRUE)
  assert_choice(num_fmt, c("significant", "significant_cell", "decimal", "scientific"))
  assert_flag(num_zero)
  assert_string(num_mark_big)
  assert_string(num_mark_dec)
  assert_string(date)
  assert_function(bool)
  assert_function(identity)
  assert_function(fn, null.ok = TRUE)
  assert_string(sprintf, null.ok = TRUE)
  assert_flag(markdown)
  if (is.null(j)) jnull <- TRUE else jnull <- FALSE
  if (is.null(i)) inull <- TRUE else inull <- FALSE
  j <- sanitize_j(j, ori)
  if (!isTRUE(check_string(replace))) {
      assert_list(replace, named = TRUE)
  } else {
      replace <- stats::setNames(list(NA), replace)
  }

  if (is.null(digits)) {
    if (num_mark_big != "") stop("`num_mark_big` requires a `digits` value.", call. = FALSE)
    if (num_mark_dec != ".") stop("`num_mark_dec` requires a `digits` value.", call. = FALSE)
  }

  # In sanity_tt(), we fill in missing NULL `j` in the format-specific versions,
  # because tabularray can do whole column styling. Here, we need to fill in
  # NULL for all formats since this is applied before creating the table.
  # nrow(out) because nrow(x) sometimes includes rows that will be added **in the lazy future** by group_tt()
  if (is.null(i)) i <- seq_len(nrow(out))
  if (is.null(j)) j <- seq_len(ncol(out))

  # format each column
  for (col in j) {
    # sprintf() is self-contained
    if (!is.null(sprintf)) {
      out[i, col] <- base::sprintf(sprintf, ori[i, col])
    } else {
      # logical
      if (is.logical(ori[i, col])) {
        out[i, col] <- bool(ori[i, col])

      # date
      } else if (inherits(ori[i, col], "Date")) {
        out[i, col] <- format(ori[i, col], date)

      # numeric
      } else if (is.numeric(ori[i, col])) {
        # digits check needs to be done here to avoid the other() formatting from ori, which zaps the original setting

        # numeric suffix
        if (isTRUE(num_suffix) && !is.null(digits)) {
          out[i, col] <- format_num_suffix(
            ori[i, col],
            digits = digits,
            num_mark_big = num_mark_big,
            num_mark_dec = num_mark_dec,
            num_zero = num_zero,
            num_fmt = num_fmt)

        # non-integer numeric
        } else if (is.numeric(ori[i, col]) && !isTRUE(check_integerish(ori[i, col])) && !is.null(digits)) {
          if (num_fmt == "significant") {
            out[i, col] <- format(ori[i, col],
              digits = digits, drop0trailing = !num_zero,
              big.mark = num_mark_big, decimal.mark = num_mark_dec,
              scientific = FALSE)
          } else if (num_fmt == "significant_cell") {
            zzz <- function(z) {
              format(z,
                digits = digits, drop0trailing = !num_zero,
                big.mark = num_mark_big, decimal.mark = num_mark_dec,
                scientific = FALSE)
            }
            out[i, col] <- sapply(ori[i, col], zzz)
          } else if (num_fmt == "decimal") {
            out[i, col] <- formatC(ori[i, col],
              digits = digits, format = "f", drop0trailing = !num_zero,
              big.mark = num_mark_big, decimal.mark = num_mark_dec)

            if (num_fmt == "scientific") {
              out[i, col] <- formatC(ori[i, col],
                digits = digits, format = "e", drop0trailing = !num_zero,
                big.mark = num_mark_big, decimal.mark = num_mark_dec)
            }
          }

        # integer
        } else if (isTRUE(check_integerish(ori[i, col])) && !is.null(digits)) {
          if (num_fmt == "scientific") {
            out[i, col] <- formatC(ori[i, col],
              digits = digits, format = "e", drop0trailing = !num_zero,
              big.mark = num_mark_big, decimal.mark = num_mark_dec)
          }
        }
        
      } else {
        out[i, col] <- other(ori[i, col])
      }
    }

    for (k in seq_along(replace)) {
        idx <- ori[i, col] %in% replace[[k]]
        out[i, col][idx] <- names(replace)[[k]]
    }

  } # loop over columns

  # Custom functions overwrite all the other formatting, but is before markdown
  # before escaping
  if (is.function(fn)) {
    for (col in j) {
      out[i, col] <- fn(ori[i, col])
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

    if (!inull && jnull) {
      for (col in j) {
        out[i, col] <- escape_text(out[i, col], output = o)
      }

    } else if (inull && jnull) {
      for (col in j) {
        out[, col] <- escape_text(out[, col], output = o)
      }

      colnames(out) <- escape_text(colnames(out), output = o)

      if (inherits(x, "tinytable")) {
        x@names <- escape_text(x@names, output = o)
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

    } else {
      for (row in i) {
        for (col in j) {
          out[row, col] <- escape_text(out[row, col], output = o)
        }
      }
    }
  }

  # markdown at the very end
  if (isTRUE(markdown)) {
    assert_dependency("markdown")
    for (col in j) {
      if (inherits(x, "tinytable_bootstrap")) {
        fun <- function(k) {
          k <- trimws(markdown::mark_html(text = k, template = FALSE))
          k <- sub("<p>", "", k, fixed = TRUE)
          k <- sub("</p>", "", k, fixed = TRUE)
          return(k)
        }
        out[i, col] <- sapply(out[i, col], function(k) fun(k))
      } else if (inherits(x, "tinytable_tabularray")) {
        fun <- function(k) {
          k <- trimws(markdown::mark_latex(text = k, template = FALSE))
          return(k)
        }
        out[i, col] <- sapply(out[i, col], function(k) fun(k))
      }
    }
  }

  if (isTRUE(atomic_vector)) {
    return(out[[1]])
  } else if (!inherits(x, "tinytable")) {
    return(out)
  } else {
    x@table_dataframe <- out
    return(x)
  }

}



format_num_suffix <- function(x, digits, num_mark_big, num_mark_dec, num_zero, num_fmt) {
  suffix <- number <- rep("", length(x))
  suffix <- ifelse(x > 1e3, "K", suffix)
  suffix <- ifelse(x > 1e6, "M", suffix)
  suffix <- ifelse(x > 1e9, "B", suffix)
  suffix <- ifelse(x > 1e12, "T", suffix)
  fun <- function(x) {
    out <- sapply(x, function(k) {
      format(k,
        digits = digits, drop0trailing = !num_zero, type = "f",
        big.mark = num_mark_big, decimal.mark = num_mark_dec,
        scientific = FALSE)
    })
  }
  number <- fun(x)
  number <- ifelse(x > 1e3, fun(x / 1e3), number)
  number <- ifelse(x > 1e6, fun(x / 1e6), number)
  number <- ifelse(x > 1e9, fun(x / 1e9), number)
  number <- ifelse(x > 1e12, fun(x / 1e12), number)
  number <- paste0(number, suffix)
  return(number)
}

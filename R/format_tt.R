#' Format columns of a data frame
#'
#' This function formats the columns of a data frame based on the column type (logical, date, numeric). 
#' It allows various formatting options like significant digits, decimal points, and scientific notation.
#' It also includes custom formatting for date and boolean values.
#'
#' @param x A data frame or a vector to be formatted.
#' @param i Row indices where the formatting should be applied.
#' @param digits Number of significant digits or decimal places.
#' @param num_fmt The format for numeric values; one of 'significant', 'decimal', or 'scientific'.
#' @param num_zero Logical; if TRUE, trailing zeros are kept in "decimal" format (but not in "significant" format).
#' @param num_mark_big Character to use as a thousands separator.
#' @param num_mark_dec Decimal mark character. Default is the global option 'OutDec'.
#' @param num_suffix Logical; if TRUE display short numbers with `digits` significant digits and K (thousands), M (millions), B (billions), or T (trillions) suffixes.
#' @param date A string passed to the `format()` function, such as "%Y-%m-%d". See the "Details" section in `?strptime`
#' @param bool A function to format logical columns. Defaults to title case.
#' @param other A function to format columns of other types. Defaults to `as.character()`.
#' @param replace_na String to display for missing values.
#' @param escape Logical or String; if TRUE, escape special characters to display them as text in the format of the output of a `tt()` table. If `format_tt()` is called as a standalone function instead of on a `tt()` table, the `escape` argument accepts strings to specify the escaping method: "latex" or "html".
#' @param markdown Logical; if TRUE, render markdown syntax in cells. Ex: `_italicized text_` is properly italicized in HTML and LaTeX.
#' @param sprintf String passed to the `?sprintf` function to format numbers or interpolate strings with a user-defined pattern (similar to the `glue` package, but using Base R).
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
format_tt <- function(x,
                      i = NULL,
                      j = NULL,
                      digits = getOption("digits"),
                      num_fmt = "significant",
                      num_zero = TRUE,
                      num_suffix = FALSE,
                      num_mark_big = "",
                      num_mark_dec = getOption("OutDec", default = "."),
                      date = "%Y-%m-%d",
                      bool = function(column) tools::toTitleCase(tolower(column)),
                      other = as.character,
                      replace_na = "",
                      escape = FALSE,
                      markdown = FALSE,
                      sprintf = NULL
                      ) {


  out <- x

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
                replace_na = replace_na,
                sprintf = sprintf,
                url = url,
                date = date,
                bool = bool,
                escape = escape,
                markdown = markdown,
                other = other)
    out <- meta(out, "lazy_format", c(meta(out)$lazy_format, list(cal)))
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
                          replace_na = replace_na,
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
                           replace_na = "",
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
    ori <- out <- x <- data.frame(tinytable = x)
    j <- 1
  } else if (!inherits(x, "tinytable") && is.data.frame(x)) {
    atomic_vector <- FALSE
    ori <- out <- x
  } else {
    atomic_vector <- FALSE
    # if no other format_tt() call has been applied, we can have numeric values
    ori <- meta(x, "x_original")
    if (!all(sapply(x, is.character))) {
      out <- meta(x, "x_character")
    } else {
      out <- x
    }
  }

  if (!inherits(x, "data.frame")) {
    msg <- "`x` must be a data frame or an atomic vector."
    stop(msg, call. = FALSE)
  }

  assert_data_frame(x)
  assert_integerish(digits, len = 1)
  assert_integerish(i, null.ok = TRUE)
  assert_choice(num_fmt, c("significant", "decimal", "scientific"))
  assert_flag(num_zero)
  assert_string(num_mark_big)
  assert_string(num_mark_dec)
  assert_string(replace_na)
  assert_string(date)
  assert_function(bool)
  assert_function(identity)
  assert_string(sprintf, null.ok = TRUE)
  assert_flag(markdown)
  if (is.null(j)) jnull <- TRUE else jnull <- FALSE
  j <- sanitize_j(j, x)

  # In sanity_tt(), we fill in missing NULL `j` in the format-specific versions,
  # because tabularray can do whole column styling. Here, we need to fill in
  # NULL for all formats since this is applied before creating the table.
  if (is.null(i)) i <- seq_len(nrow(x))
  if (is.null(j)) j <- seq_len(ncol(x))

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
      } else if (is.numeric(ori[i, col]) && !is.null(digits)) {
        # numeric suffix
        if (isTRUE(num_suffix)) {
          out[i, col] <- format_num_suffix(ori[i, col], digits = digits, num_mark_big = num_mark_big, num_mark_dec = num_mark_dec, num_zero = num_zero)

          # non-integer numeric
        } else if (is.numeric(ori[i, col]) && !isTRUE(check_integerish(ori[i, col]))) {
          if (num_fmt == "significant") {
            out[i, col] <- format(ori[i, col],
              digits = digits, drop0trailing = !num_zero,
              big.mark = num_mark_big, decimal.mark = num_mark_dec,
              scientific = FALSE)
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
        } else if (isTRUE(check_integerish(ori[i, col]))) {
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

    # replace missing values by `na`
    out[i, col][is.na(ori[i, col])] <- replace_na
  } # loop over columns


  # escape latex characters
  if (!isFALSE(escape)) {
    if (isTRUE(escape == "latex")) {
      o <- "latex"
    } else if (isTRUE(escape == "html")) {
      o <- "html"
    } else if (isTRUE(escape == "typst")) {
      o <- "typst"
    } else {
      o <- meta(x)$output
    }
    # if j includes all columns, the user wants to escape the full table, including the column headers
    if (jnull) {
      colnames(out) <- escape_text(colnames(out), output = o)
    }
    for (col in j) {
      out[i, col] <- escape_text(out[i, col], output = o)
    }
  }

  # markdown at the very end
  if (isTRUE(markdown)) {
    assert_dependency("markdown")
    for (col in j) {
      if (isTRUE(meta(x)$output == "html")) {
        fun <- function(x, i) {
          x[i] <- trimws(markdown::mark_html(text = x[i], template = FALSE))
          x[i] <- sub("<p>", "", x[i], fixed = TRUE)
          x[i] <- sub("</p>", "", x[i], fixed = TRUE)
          return(x)
        }
        out[, col] <- sapply(out[, col], function(x) fun(x, i))
      } else if (isTRUE(meta(x)$output == "latex")) {
        fun <- function(x, i) {
          x[i] <- trimws(markdown::mark_latex(text = x[i], template = FALSE))
          return(x)
        }
        out[, col] <- sapply(out[, col], function(x) fun(x, i))
      }
    }
  }


  if (isTRUE(atomic_vector)) {
    return(out[[1]])
  } else {
    attr(out, "tinytable_meta") <- meta(x)
    class(out) <- c("tinytable", "data.frame")
    return(out)
  }

}



format_num_suffix <- function(x, digits, num_mark_big, num_mark_dec, num_zero) {
  suffix <- number <- rep("", length(x))
  suffix <- ifelse(x > 1e3, "K", suffix) 
  suffix <- ifelse(x > 1e6, "M", suffix) 
  suffix <- ifelse(x > 1e9, "B", suffix) 
  suffix <- ifelse(x > 1e12, "T", suffix) 
  number <- format_tt(x, num_fmt = "decimal", digits = digits, num_mark_big = num_mark_big, num_mark_dec = num_mark_dec, num_zero = num_zero)
  number <- ifelse(x > 1e3, format_tt(x / 1e3, num_fmt = "decimal", digits = digits, num_mark_big = num_mark_big, num_mark_dec = num_mark_dec, num_zero = num_zero), number)
  number <- ifelse(x > 1e6, format_tt(x / 1e6, num_fmt = "decimal", digits = digits, num_mark_big = num_mark_big, num_mark_dec = num_mark_dec, num_zero = num_zero), number)
  number <- ifelse(x > 1e9, format_tt(x / 1e9, num_fmt = "decimal", digits = digits, num_mark_big = num_mark_big, num_mark_dec = num_mark_dec, num_zero = num_zero), number)
  number <- ifelse(x > 1e12, format_tt(x / 1e12, num_fmt = "decimal", digits = digits, num_mark_big = num_mark_big, num_mark_dec = num_mark_dec, num_zero = num_zero), number)
  number <- paste0(number, suffix)
  return(number)
}

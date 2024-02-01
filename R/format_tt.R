#' Format columns of a data frame
#'
#' This function formats the columns of a data frame based on the column type (logical, date, numeric). 
#' It allows various formatting options like significant digits, decimal points, and scientific notation.
#' It also includes custom formatting for date and boolean values.
#'
#' @param x A data frame or a vector to be formatted.
#' @param digits Number of significant digits or decimal places.
#' @param num_fmt The format for numeric values; one of 'significant', 'decimal', or 'scientific'.
#' @param num_zero Logical; if TRUE, trailing zeros are kept in "decimal" format (but not in "significant" format).
#' @param num_mark_big Character to use as a thousands separator.
#' @param num_mark_dec Decimal mark character. Default is the global option 'OutDec'.
#' @param num_suffix Logical; if TRUE display short numbers with `digits` significant digits and K (thousands), M (millions), B (billions), or T (trillions) suffixes.
#' @param date A string passed to the `format()` function, such as "%Y-%m-%d". See the "Details" section in `?strptime`
#' @param bool A function to format logical columns. Defaults to title case.
#' @param other A function to format columns of other types. Defaults to `as.character()`.
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
                      escape = FALSE,
                      markdown = FALSE,
                      sprintf = NULL
                      ) {

  out <- x

  if (inherits(out, "tinytable")) {
    cal <- call("format_tt_lazy", 
                j = j,
                digits = digits,
                num_fmt = num_fmt,
                num_zero = num_zero,
                num_suffix = num_suffix,
                num_mark_big = num_mark_big,
                num_mark_dec = num_mark_dec,
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
                          j = j,
                          digits = digits,
                          num_fmt = num_fmt,
                          num_zero = num_zero,
                          num_suffix = num_suffix,
                          num_mark_big = num_mark_big,
                          num_mark_dec = num_mark_dec,
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
                           j = NULL,
                           digits,
                           num_fmt = "significant",
                           num_zero = FALSE,
                           num_suffix = FALSE,
                           num_mark_big = "",
                           num_mark_dec = NULL,
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
    x <- data.frame(tinytable = x)
    j <- 1
  } else {
    atomic_vector <- FALSE
  }

  if (!inherits(x, "data.frame")) {
    msg <- "`x` must be a data frame or an atomic vector."
    stop(msg, call. = FALSE)
  }

  assert_data_frame(x)
  assert_integerish(digits, len = 1)
  assert_choice(num_fmt, c("significant", "decimal", "scientific"))
  assert_flag(num_zero)
  assert_string(num_mark_big)
  assert_string(num_mark_dec)
  assert_string(date)
  assert_function(bool)
  assert_function(identity)
  assert_string(sprintf, null.ok = TRUE)
  assert_flag(markdown)


  # column index NULL or regex or integer vector
  if (is.null(j)) {
    j <- seq_len(ncol(x))
  } else if (is.character(j) && length(j) == 1 && !is.null(names(x))) {
    j <- grep(j, colnames(x), perl = TRUE)
  } else {
    assert_integerish(j, lower = 1, upper = ncol(x))
  }

  # format each column
  for (col in j) {
    # sprintf() is self-contained
    if (!is.null(sprintf)) {
      x[[col]] <- base::sprintf(sprintf, x[[col]])

    } else {

      # logical 
      if (is.logical(x[[col]])) {
        x[[col]] <- bool(x[[col]])

        # date
      } else if (inherits(x[[col]], "Date")) {
        x[[col]] <- format(x[[col]], date)

        # numeric
      } else if (is.numeric(x[[col]]) && !is.null(digits)) {

        # numeric suffix
        if (isTRUE(num_suffix)) {
          x[[col]] <- format_num_suffix(x[[col]], digits = digits, num_mark_big = num_mark_big, num_mark_dec = num_mark_dec, num_zero = num_zero)

          # non-integer numeric
        } else if (is.numeric(x[[col]]) && !isTRUE(check_integerish(x[[col]]))) {
          if (num_fmt == "significant") {
            x[[col]] <- format(x[[col]],
                               digits = digits, drop0trailing = !num_zero,
                               big.mark = num_mark_big, decimal.mark = num_mark_dec,
                               scientific = FALSE)

          } else if (num_fmt == "decimal") {
            x[[col]] <- formatC(x[[col]],
                                digits = digits, format = "f", drop0trailing = !num_zero,
                                big.mark = num_mark_big, decimal.mark = num_mark_dec)

            if (num_fmt == "scientific") {
              x[[col]] <- formatC(x[[col]],
                                  digits = digits, format = "e", drop0trailing = !num_zero,
                                  big.mark = num_mark_big, decimal.mark = num_mark_dec)
            }
          }

          # integer
        } else if (isTRUE(check_integerish(x[[col]]))) {
          if (num_fmt == "scientific") {
            x[[col]] <- formatC(x[[col]],
                                digits = digits, format = "e", drop0trailing = !num_zero,
                                big.mark = num_mark_big, decimal.mark = num_mark_dec)
          }
        }

      } else {
        x[[col]] <- other(x[[col]])
      }

    }

  } # loop over columns

  # escape latex characters
  if (!isFALSE(escape)) {
    if (isTRUE(escape == "latex")) {
      o <- "latex"
    } else if (isTRUE(escape == "html")) {
      o <- "html"
    } else {
      o <- meta(x)$output
    }
    for (col in j) {
      x[[col]] <- escape_text(x[[col]], output = o)
    }
  }

  # markdown at the very end
  if (isTRUE(markdown)) {
    assert_dependency("markdown")
    for (col in j) {
      if (isTRUE(meta(x)$output == "html")) {
        fun <- function(x) {
          out <- trimws(markdown::mark_html(text = x, template = FALSE))
          out <- sub("<p>", "", out, fixed = TRUE)
          out <- sub("</p>", "", out, fixed = TRUE)
          return(out)
        }
        x[, col] <- sapply(x[, col], fun)
      } else if (isTRUE(meta(x)$output == "latex")) {
        fun <- function(x) trimws(markdown::mark_latex(text = x, template = FALSE))
        x[, col] <- sapply(x[, col], fun)
      }
    }
  }

  if (isTRUE(atomic_vector)) {
    return(x[[1]])
  } else {
    return(x)
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

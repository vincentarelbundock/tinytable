# output is selected automatically if format_tt is called in tt()
# x is inserted automatically if format_tt is called in tt()


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
#' @param url Logical; if TRUE, treats the column as a URL.
#' @param date A function to format Date columns. Defaults to ISO format.
#' @param bool A function to format logical columns. Defaults to title case.
#' @param other A function to format columns of other types. Defaults to identity (no formatting).
#' @inheritParams tt
#' @inheritParams style_tt
#'
#' @return A data frame with formatted columns.
#'
#' @examples
#' # Example usage
#' data_frame <- data.frame(
#'   logical_col = c(TRUE, FALSE, TRUE),
#'   date_col = as.Date(c('2020-01-01', '2021-02-02', '2022-03-03')),
#'   numeric_col = c(12345.67, 8901.23, 4567.89)
#' )
#' formatted_data_frame <- format_tt(data_frame)
#'
#' @export
format_tt <- function(x = NULL,
                      j = NULL,
                      output = NULL,
                      digits = NULL,
                      num_fmt = "significant",
                      num_zero = TRUE,
                      num_mark_big = "",
                      num_mark_dec = getOption("OutDec", default = "."),
                      url = FALSE,
                      date = function(column) format(column, "%Y-%m-%d"),
                      bool = function(column) tools::toTitleCase(tolower(column)),
                      other = identity
                      ) {

  if (inherits(x, "tinytable")) {
    msg <- "`format_tt()` must be called *before* `tt()`. You must format your dataset before drawing a table."
  }

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
  assert_integerish(digits, len = 1, null.ok = TRUE)
  assert_choice(num_fmt, c("significant", "decimal", "scientific"))
  assert_flag(num_zero)
  assert_string(num_mark_big)
  assert_string(num_mark_dec)
  assert_flag(url)
  assert_function(date)
  assert_function(bool)
  assert_function(identity)

  output <- sanitize_output(output)

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

    # logical 
    if (is.logical(x[[col]])) {
      x[[col]] <- bool(x[[col]])

    # date
    } else if (inherits(x[[col]], "Date")) {
      x[[col]] <- date(x[[col]])

    # numeric
    } else if (is.numeric(x[[col]]) && !is.null(digits)) {
      if (num_fmt == "significant") {
        x[[col]] <- format(x[[col]],
          digits = digits, drop0trailing = !num_zero,
          big.mark = num_mark_big, decimal.mark = num_mark_dec,
          scientific = FALSE)
      } else if (num_fmt == "decimal") {
        x[[col]] <- formatC(x[[col]],
          digits = digits, format = "f", drop0trailing = !num_zero,
          big.mark = num_mark_big, decimal.mark = num_mark_dec)
      } else if (num_fmt == "scientific") {
        x[[col]] <- formatC(x[[col]],
          digits = digits, format = "e", drop0trailing = !num_zero,
          big.mark = num_mark_big, decimal.mark = num_mark_dec)
      }

    } else {
      x[[col]] <- other(x[[col]])
    }

  }

  if (isTRUE(atomic_vector)) {
    return(x[[1]])
  } else {
    return(x)
  }

}

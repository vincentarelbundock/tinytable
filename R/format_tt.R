# output is selected automatically if format_tt is called in tt()
# x is inserted automatically if format_tt is called in tt()


#' Format columns of a data frame
#'
#' This function formats the columns of a data frame based on the column type (logical, date, numeric). 
#' It allows various formatting options like significant digits, decimal points, and scientific notation.
#' It also includes custom formatting for date and boolean values.
#'
#' @param x A data frame to be formatted.
#' @param digits Number of significant digits or decimal places.
#' @param num_fmt The format for numeric values; one of 'significant', 'decimal', or 'scientific'.
#' @param num_mark_big Character to use as a thousands separator.
#' @param num_mark_dec Decimal mark character. Default is the global option 'OutDec'.
#' @param num_zero Logical; if TRUE, trailing zeros are kept.
#' @param url Logical; if TRUE, treats the column as a URL.
#' @param date A function to format Date columns. Defaults to ISO format.
#' @param bool A function to format logical columns. Defaults to title case.
#' @param ... Additional arguments are ignored.
#' @inheritParams tt
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
                      num_mark_big = "",
                      num_mark_dec = getOption("OutDec"),
                      num_zero = TRUE,
                      url = FALSE,
                      date = function(column) format(column, "%Y-%m-%d"),
                      bool = function(column) tools::toTitleCase(tolower(column)),
                      ...
                      ) {

  if (is.null(x)) {
    out <- match.call()
    class(out) <- c("tinytable_format_tt", class(out))
    return(out)
  }

  assert_data_frame(x)

  output <- sanitize_output(output)

  assert_choice(num_fmt, c("significant", "decimal", "scientific"))

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
    } else if (is.numeric(x[[col]])) {
      if (num_fmt == "significant") {
        x[[col]] <- formatC(x[[col]],
          digits = digits, format = "g", drop0trailing = !num_zero,
          big.mark = num_mark_big, decimal.mark = num_mark_dec)
      } else if (num_fmt == "decimal") {
        x[[col]] <- formatC(x[[col]],
          digits = digits, format = "f", drop0trailing = !num_zero,
          big.mark = num_mark_big, decimal.mark = num_mark_dec)
      } else if (num_fmt == "scientific") {
        x[[col]] <- formatC(x[[col]],
          digits = digits, format = "e", drop0trailing = !num_zero,
          big.mark = num_mark_big, decimal.mark = num_mark_dec)
      }
    }

  }

  return(x)

}

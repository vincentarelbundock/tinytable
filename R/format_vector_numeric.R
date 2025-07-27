# Format numeric values with different formats
# digits check needs to be done here to avoid the other() formatting from ori, which zaps the original setting
format_vector_numeric <- function(
    vec,
    num_suffix,
    digits,
    num_mark_big,
    num_mark_dec,
    num_zero,
    num_fmt,
    ...) {
  if (!is.numeric(vec) || is.null(digits)) {
    return(NULL)
  }

  # numeric suffix
  if (isTRUE(num_suffix) && !is.null(digits)) {
    out <- format_num_suffix(
      vec,
      digits = digits,
      num_mark_big = num_mark_big,
      num_mark_dec = num_mark_dec,
      num_zero = num_zero,
      num_fmt = num_fmt
    )
    # non-integer numeric
  } else if (
    is.numeric(vec) && !isTRUE(check_integerish(vec)) && !is.null(digits)
  ) {
    out <- format_non_integer_numeric(
      vec,
      digits = digits,
      num_mark_big = num_mark_big,
      num_mark_dec = num_mark_dec,
      num_zero = num_zero,
      num_fmt = num_fmt
    )
    # integer
  } else if (isTRUE(check_integerish(vec))) {
    out <- format_integer(
      vec,
      digits = digits,
      num_mark_big = num_mark_big,
      num_mark_dec = num_mark_dec,
      num_zero = num_zero,
      num_fmt = num_fmt
    )
  } else {
    out <- NULL
  }
  if (is.character(out)) {
    out <- trimws(out)
  }
  return(out)
}

format_num_suffix <- function(
    x,
    digits,
    num_mark_big,
    num_mark_dec,
    num_zero,
    num_fmt) {
  suffix <- number <- rep("", length(x))
  suffix <- ifelse(x > 1e3, "K", suffix)
  suffix <- ifelse(x > 1e6, "M", suffix)
  suffix <- ifelse(x > 1e9, "B", suffix)
  suffix <- ifelse(x > 1e12, "T", suffix)
  fun <- function(x) {
    out <- sapply(x, function(k) {
      format(
        k,
        digits = digits,
        drop0trailing = !num_zero,
        type = "f",
        big.mark = num_mark_big,
        decimal.mark = num_mark_dec,
        scientific = FALSE
      )
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

# Format non-integer numeric values
format_non_integer_numeric <- function(
    vec,
    digits,
    num_mark_big,
    num_mark_dec,
    num_zero,
    num_fmt) {
  if (num_fmt == "significant") {
    return(
      format(
        vec,
        digits = digits,
        drop0trailing = !num_zero,
        big.mark = num_mark_big,
        decimal.mark = num_mark_dec,
        scientific = FALSE
      )
    )
  } else if (num_fmt == "significant_cell") {
    return(
      sapply(
        vec,
        function(z) {
          format(
            z,
            digits = digits,
            drop0trailing = !num_zero,
            big.mark = num_mark_big,
            decimal.mark = num_mark_dec,
            scientific = FALSE
          )
        })
    )
  } else if (num_fmt == "decimal") {
    return(
      formatC(
        vec,
        digits = digits,
        format = "f",
        drop0trailing = !num_zero,
        big.mark = num_mark_big,
        decimal.mark = num_mark_dec
      )
    )
  } else if (num_fmt == "scientific") {
    return(
      formatC(
        vec,
        digits = digits,
        format = "e",
        drop0trailing = !num_zero,
        big.mark = num_mark_big,
        decimal.mark = num_mark_dec
      )
    )
  }
  return(vec)
}

# Format integer values
format_integer <- function(
    vec,
    digits,
    num_mark_big,
    num_mark_dec,
    num_zero,
    num_fmt) {
  if (num_fmt == "scientific") {
    return(
      formatC(
        vec,
        digits = digits,
        format = "e",
        drop0trailing = !num_zero,
        big.mark = num_mark_big,
        decimal.mark = num_mark_dec
      )
    )
  } else {
    return(format(vec, big.mark = num_mark_big, scientific = FALSE))
  }
}

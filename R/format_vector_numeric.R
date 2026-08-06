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
  if (isTRUE(num_suffix)) {
    out <- format_num_suffix(
      vec,
      digits = digits,
      num_mark_big = num_mark_big,
      num_mark_dec = num_mark_dec,
      num_zero = num_zero,
      num_fmt = num_fmt
    )
    # non-integer numeric
  } else if (!isTRUE(check_integerish(vec))) {
    out <- format_non_integer_numeric(
      vec,
      digits = digits,
      num_mark_big = num_mark_big,
      num_mark_dec = num_mark_dec,
      num_zero = num_zero,
      num_fmt = num_fmt
    )
    # integerish columns (including whole-number doubles) take a shortcut that
    # ignores `digits`, `num_zero`, and `num_mark_dec` unless num_fmt = "scientific"
  } else {
    out <- format_integer(
      vec,
      digits = digits,
      num_mark_big = num_mark_big,
      num_mark_dec = num_mark_dec,
      num_zero = num_zero,
      num_fmt = num_fmt
    )
  }
  if (is.character(out)) {
    out <- trimws(out)
  }
  return(out)
}

# `digits` significant digits; column-wise when called on a vector, cell-wise
# when vapply()ed element by element. Shared by the "significant",
# "significant_cell", and num_suffix paths.
format_significant <- function(x, digits, num_zero, num_mark_big, num_mark_dec) {
  format(
    x,
    digits = digits,
    drop0trailing = !num_zero,
    big.mark = num_mark_big,
    decimal.mark = num_mark_dec,
    scientific = FALSE
  )
}

format_scientific <- function(vec, digits, num_zero, num_mark_big, num_mark_dec) {
  formatC(
    vec,
    digits = digits,
    format = "e",
    drop0trailing = !num_zero,
    big.mark = num_mark_big,
    decimal.mark = num_mark_dec
  )
}

format_num_suffix <- function(
    x,
    digits,
    num_mark_big,
    num_mark_dec,
    num_zero,
    num_fmt) {
  # tier 0 = no suffix; thresholds compare abs(x) with `>=` so exact powers
  # get the higher tier (1e6 -> "1M", not "1000K") and negatives abbreviate
  tier <- findInterval(abs(x), c(1e3, 1e6, 1e9, 1e12))
  # NA/NaN/Inf get no suffix and pass through format() untouched
  tier[!is.finite(x)] <- 0L
  scaled <- x / 1000^tier
  number <- vapply(
    scaled,
    format_significant,
    character(1),
    digits = digits,
    num_zero = num_zero,
    num_mark_big = num_mark_big,
    num_mark_dec = num_mark_dec
  )
  paste0(number, c("", "K", "M", "B", "T")[tier + 1])
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
    return(format_significant(
      vec,
      digits = digits,
      num_zero = num_zero,
      num_mark_big = num_mark_big,
      num_mark_dec = num_mark_dec
    ))
  } else if (num_fmt == "significant_cell") {
    return(vapply(
      vec,
      format_significant,
      character(1),
      digits = digits,
      num_zero = num_zero,
      num_mark_big = num_mark_big,
      num_mark_dec = num_mark_dec
    ))
  } else if (num_fmt == "decimal") {
    out <- formatC(
      vec,
      digits = digits,
      format = "f",
      drop0trailing = !num_zero,
      big.mark = num_mark_big,
      decimal.mark = num_mark_dec
    )
    # no signed zero: -0.0001 with digits = 2 must print "0.00"/"0", not "-0"
    dec <- gsub("(\\W)", "\\\\\\1", num_mark_dec)
    neg_zero <- grepl(paste0("^-0(", dec, "0*)?$"), out)
    out[neg_zero] <- sub("-", "", out[neg_zero], fixed = TRUE)
    return(out)
  } else if (num_fmt == "scientific") {
    return(format_scientific(
      vec,
      digits = digits,
      num_zero = num_zero,
      num_mark_big = num_mark_big,
      num_mark_dec = num_mark_dec
    ))
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
    return(format_scientific(
      vec,
      digits = digits,
      num_zero = num_zero,
      num_mark_big = num_mark_big,
      num_mark_dec = num_mark_dec
    ))
  } else {
    return(format(vec, big.mark = num_mark_big, scientific = FALSE))
  }
}

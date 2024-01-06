#' check if dependency is installed
#'
#' @noRd
check_dependency <- function(library_name) {
  flag <- requireNamespace(library_name, quietly = TRUE)
  if (isFALSE(flag)) {
      msg <- sprintf("Please install the `%s` package.", library_name)
      return(msg)
  } else {
      return(TRUE)
  }
}

assert_dependency <- function(library_name){
  flag <- check_dependency(library_name)
  if (!isTRUE(flag)) stop(flag, call. = FALSE)
  return(invisible())
}

assert_choice <- function(x, choice, null.ok = FALSE) {
    if (is.null(x) && isTRUE(null.ok)) return(TRUE)
    if (is.character(x) && length(x) == 1 && x %in% choice) return(TRUE)
    msg <- sprintf(
      "`%s` must be one of: %s",
      as.character(substitute(x)),
      paste(choice, collapse = ", "))
    stop(msg, call. = FALSE)
}

assert_string <- function(x, null.ok = FALSE) {
    if (is.null(x) && isTRUE(null.ok)) return(TRUE)
    if (is.character(x) && length(x) == 1) return(TRUE)
    msg <- sprintf("`%s` must be a string.", as.character(substitute(x)))
    stop(msg, call. = FALSE)
}

check_flag <- function(x, null.ok = FALSE) {
    if (is.null(x) && isTRUE(null.ok)) return(TRUE)
    if (is.logical(x) && length(x) == 1) return(TRUE)
    return(FALSE)
}

assert_flag <- function(x, null.ok = FALSE) {
  msg <- sprintf("`%s` must be a logical flag.", as.character(substitute(x)))
  if (!isTRUE(check_flag(x, null.ok = null.ok))) {
    stop(msg, call. = FALSE)
  }
}

check_integerish <- function(x, lower = NULL, upper = NULL, null.ok = TRUE) {
  if (is.null(x) && isTRUE(null.ok)) return(TRUE)
  if (!is.numeric(x)) return(FALSE)
  if (!is.null(lower) && any(x < lower)) return(FALSE)
  if (!is.null(upper) && any(x > upper)) return(FALSE)
  if (any(abs(x - round(x)) > (.Machine$double.eps)^0.5)) return(FALSE)
  return(TRUE)
}

assert_integerish <- function(x, lower = NULL, upper = NULL, null.ok = FALSE) {
  msg <- sprintf("`%s` must be integer-ish", as.character(substitute(x)))
  if (!isTRUE(check_integerish(x, null.ok = null.ok))) {
    stop(msg, call. = FALSE)
  }
}

assert_data_frame <- function(x) {
  msg <- sprintf("`%s` must be a data.frame.", as.character(substitute(x)))
  if (!is.data.frame(x)) stop(msg, call. = FALSE)
}

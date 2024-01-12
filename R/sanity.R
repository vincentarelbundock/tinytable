usepackage_latex <- function(name, options = NULL, extra_lines = NULL) {
  invisible(knitr::knit_meta_add(list(rmarkdown::latex_dependency(name, options, extra_lines))))
}


sanitize_output <- function(output) {
  assert_choice(output, choice = c("markdown", "latex", "html"), null.ok = TRUE)

  # default output format
  if (is.null(output)) {
    in_rstudio <- interactive() && check_dependency("rstudioapi") && rstudioapi::isAvailable()
    out <- getOption("tt_output_default", default = if (in_rstudio) "html" else "markdown")

  } else {
    out <- output
  }

  if (isTRUE(check_dependency("knitr")) && isTRUE(check_dependency("rmarkdown"))) {

    if (isTRUE(knitr::is_latex_output())) {
      usepackage_latex("codehigh")
      usepackage_latex("float")
      usepackage_latex("tabularray", extra_lines = "\\UseTblrLibrary{booktabs}")
      if (is.null(output)) out <- "latex"

    } else if (isTRUE(knitr::is_html_output())) {
      if (is.null(output)) out <- "html"
    }

  }

  return(out)
}


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

assert_choice <- function(x, choice, null.ok = FALSE, name = as.character(substitute(x))) {
    if (is.null(x) && isTRUE(null.ok)) return(TRUE)
    if (is.character(x) && length(x) == 1 && x %in% choice) return(TRUE)
    msg <- sprintf(
      "`%s` must be one of: %s",
      name,
      paste(choice, collapse = ", "))
    stop(msg, call. = FALSE)
}

assert_string <- function(x, null.ok = FALSE, name = as.character(substitute(x))) {
    if (is.null(x) && isTRUE(null.ok)) return(invisible(TRUE))
    if (is.character(x) && length(x) == 1) return(invisible(TRUE))
    msg <- sprintf("`%s` must be a string.", name)
    stop(msg, call. = FALSE)
}

check_flag <- function(x, null.ok = FALSE) {
    if (is.null(x) && isTRUE(null.ok)) return(TRUE)
    if (is.logical(x) && length(x) == 1) return(TRUE)
    return(FALSE)
}

assert_flag <- function(x, null.ok = FALSE, name = as.character(substitute(x))) {
  msg <- sprintf("`%s` must be a logical flag.", name)
  if (!isTRUE(check_flag(x, null.ok = null.ok))) {
    stop(msg, call. = FALSE)
  }
}


check_integerish <- function(x, len = NULL, lower = NULL, upper = NULL, null.ok = TRUE) {
  if (is.null(x) && isTRUE(null.ok)) return(TRUE)
  if (!is.numeric(x)) return(FALSE)
  if (!is.null(len) && length(x) != len) return(FALSE)
  if (!is.null(lower) && any(x < lower)) return(FALSE)
  if (!is.null(upper) && any(x > upper)) return(FALSE)
  if (any(abs(x - round(x)) > (.Machine$double.eps)^0.5)) return(FALSE)
  return(TRUE)
}


assert_integerish <- function(x, len = NULL, lower = NULL, upper = NULL, null.ok = FALSE, name = as.character(substitute(x))) {
  msg <- sprintf("`%s` must be integer-ish", name)
  if (!isTRUE(check_integerish(x, len = len, lower = lower, upper = upper, null.ok = null.ok))) {
    if (!is.numeric(x)) msg <- paste0(msg, "; it is not numeric")
    if (!is.null(len) && length(x) != len) msg <- paste0(msg, sprintf("; its length must be %s", len))
    if (!is.null(lower) && any(x < lower)) msg <- paste0(msg, sprintf("; all values must be greater than or equal to %s", lower))
    if (!is.null(upper) && any(x > upper)) msg <- paste0(msg, sprintf("; all values must be less than or equal to %s", upper))
    if (any(abs(x - round(x)) > (.Machine$double.eps)^0.5)) msg <- paste0(msg, "; all values must be close to integers")
    stop(msg, call. = FALSE)
  }
}


check_numeric <- function(x, len = NULL, lower = NULL, upper = NULL, null.ok = TRUE) {
  if (is.null(x) && isTRUE(null.ok)) return(TRUE)
  if (!is.numeric(x)) return(FALSE)
  if (!is.null(len) && length(x) != len) return(FALSE)
  if (!is.null(lower) && any(x < lower)) return(FALSE)
  if (!is.null(upper) && any(x > upper)) return(FALSE)
  return(TRUE)
}


assert_numeric <- function(x, len = NULL, lower = NULL, upper = NULL, null.ok = FALSE, name = as.character(substitute(x))) {
  msg <- sprintf("`%s` must be numeric", name)
  if (!isTRUE(check_numeric(x, len = len, lower = lower, upper = upper, null.ok = null.ok))) {
    if (!is.numeric(x)) msg <- paste0(msg, "; it is not numeric")
    if (!is.null(len) && length(x) != len) msg <- paste0(msg, sprintf("; its length must be %s", len))
    if (!is.null(lower) && any(x < lower)) msg <- paste0(msg, sprintf("; all values must be greater than or equal to %s", lower))
    if (!is.null(upper) && any(x > upper)) msg <- paste0(msg, sprintf("; all values must be less than or equal to %s", upper))
    stop(msg, call. = FALSE)
  }
}


assert_data_frame <- function(x, name = as.character(substitute(x))) {
  msg <- sprintf("`%s` must be a data.frame.", name)
  if (!is.data.frame(x)) stop(msg, call. = FALSE)
}


assert_character <- function(x, len = NULL, name = as.character(substitute(x))) {
  if (!is.character(x)) {
    msg <- sprintf("`%s` must be character.", name)
    stop(msg, call. = FALSE)
  }
  if (!is.null(len)) {
    if (length(x) != len) {
      msg <- sprintf("`%s` must have length %s.", name, len)
      stop(msg, call. = FALSE)
    }
  }
}


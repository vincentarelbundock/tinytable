#' Deprecated: Use format-specific theme functions instead
#'
#' @description
#' **DEPRECATED**: The `theme_tt()` function has been deprecated. Please use the format-specific or style-specific theme functions instead.
#'
#' @param x deprecated
#' @param theme deprecated
#' @param ... Additional arguments
#'
#' @return Throws an informative error message
#' @export
theme_tt <- function(x, theme, ...) {
  format_functions <- paste(c(
    "* theme_html()",
    "* theme_latex()",
    "* theme_typst()"
  ), collapse = "\n")

  style_functions <- paste(c(
    "* theme_grid()",
    "* theme_revealjs()",
    "* theme_rotate()",
    "* theme_striped()",
    "* theme_empty()"
  ), collapse = "\n")

  format_msg <- paste0("\nFormat-specific functions:\n\n", format_functions)
  style_msg <- paste0("\nStyle-specific functions:\n\n", paste(style_functions, collapse = "\n"))

  warning(
    "The theme_tt() function is deprecated. Please use format-specific or style-specific theme functions instead:\n",
    format_msg,
    "\n",
    style_msg,
    call. = FALSE
  )


  if (identical(theme, "default")) {
    x <- theme_default(x)
  } else if (identical(theme, "grid")) {
    x <- theme_grid(x)
  } else if (identical(theme, "revealjs")) {
    x <- theme_revealjs(x, ...)
  } else if (identical(theme, "striped")) {
    x <- theme_striped(x)
  } else if (identical(theme, "empty")) {
    x <- theme_empty(x)
  } else if (identical(theme, "void")) {
    x <- theme_empty(x)
  } else if (identical(theme, "rotate")) {
    x <- theme_rotate(x, ...)
  }
  return(x)
}

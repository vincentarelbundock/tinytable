theme_dictionary <- list(
  "default" = theme_default,
  "grid" = theme_grid,
  "revealjs" = theme_revealjs,
  "rotate" = theme_rotate,
  "striped" = theme_striped,
  "void" = theme_void
)

#' Deprecated: Use format-specific theme functions instead
#'
#' @description
#' **DEPRECATED**: The `theme_tt()` function has been deprecated. Please use the format-specific or style-specific theme functions instead.
#'
#' @param x A `tinytable` object
#' @param theme String. Name of the theme to apply.
#' @param ... Additional arguments
#'
#' @return Throws an informative error message
#' @export
theme_tt <- function(...) {
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
    "* theme_void()"
  ), collapse = "\n")

  format_msg <- paste0("\nFormat-specific functions:\n\n", format_functions)
  style_msg <- paste0("\nStyle-specific functions:\n\n", paste(style_functions, collapse = "\n"))

  stop(
    "The theme_tt() function has been deprecated. Please use format-specific or style-specific theme functions instead:\n",
    format_msg,
    "\n",
    style_msg,
    call. = FALSE
  )
}

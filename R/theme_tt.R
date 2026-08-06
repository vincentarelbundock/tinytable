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


  # "void" is a legacy alias for "empty"
  if (identical(theme, "void")) {
    theme <- "empty"
  }

  # dispatch through the same dictionary used by tt(theme = ...)
  if (isTRUE(check_string(theme)) && theme %in% names(theme_dictionary)) {
    x <- theme_dictionary[[theme]](x, ...)
  } else {
    legacy <- c(
      "bootstrap" = "theme_html(x, engine = \"bootstrap\", class = ...)",
      "tabular" = "theme_latex(x, environment = \"tabular\")",
      "resize" = "theme_latex(x, resize_width = ..., resize_direction = ...)",
      "placement" = "theme_latex(x, placement = ...)",
      "multipage" = "theme_latex(x, multipage = TRUE)",
      "spacing" = "the `height` argument of tt()",
      "rotating" = "theme_rotate(x, angle = ...)"
    )
    if (is.character(theme) && length(theme) == 1 && theme %in% names(legacy)) {
      msg <- sprintf(
        "The \"%s\" theme was removed from `tinytable`. Use %s instead.",
        theme,
        legacy[[theme]]
      )
    } else {
      msg <- sprintf(
        "Invalid `theme` argument. `theme_tt()` supports these theme names: %s.",
        paste(sprintf('"%s"', c(
          "default", "grid", "revealjs", "striped", "empty", "void", "rotate"
        )), collapse = ", ")
      )
    }
    stop(msg, call. = FALSE)
  }
  return(x)
}

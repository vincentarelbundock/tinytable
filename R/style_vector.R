#' Style a Vector with Text Formatting
#'
#' @details
#' This function applies styling to a vector. It allows customization of text style (bold, italic, monospace), text color, font size, and text decorations (underline, strikeout). The styling is applied element-wise to the vector. Vectors are coerced with `as.character()` before styling.
#'
#' @param x A vector to be styled.
#' @param output Output format for styling. One of "html", "latex", "typst", "markdown", "ansi". Defaults to "html".
#' @param bold Logical; if `TRUE`, text is styled in bold. Must be of length 1 or `length(x)`.
#' @param italic Logical; if `TRUE`, text is styled in italic. Must be of length 1 or `length(x)`.
#' @param monospace Logical; if `TRUE`, text is styled in monospace font. Must be of length 1 or `length(x)`.
#' @param underline Logical; if `TRUE`, text is underlined. Must be of length 1 or `length(x)`.
#' @param strikeout Logical; if `TRUE`, text has a strike through line. Must be of length 1 or `length(x)`.
#' @param color Text color. Must be of length 1 or `length(x)`. There are several ways to specify colors:
#' + HTML:
#'   - Hex code composed of # and 6 characters, ex: #CC79A7.
#'   - Keywords: black, silver, gray, white, maroon, red, purple, fuchsia, green, lime, olive, yellow, navy, blue, teal, aqua
#' + LaTeX:
#'   - Hex code composed of # and 6 characters, ex: "#CC79A7".
#'   - Keywords: black, blue, brown, cyan, darkgray, gray, green, lightgray, lime, magenta, olive, orange, pink, purple, red, teal, violet, white, yellow.
#'   - Color blending using xcolor, ex: `white!80!blue`, `green!20!red`.
#'   - Color names with luminance levels from the `ninecolors` package.
#' @param fontsize Font size in em units. Must be of length 1 or `length(x)`. Can be `NULL` for default size.
#' @param indent Text indentation in em units. Must be of length 1 or `length(x)`. Positive values only.
#' @return A character vector with applied styling.
#' @export
#' @examples
#' # Basic styling
#' style_vector(c("Hello", "World"), bold = TRUE, color = "red")
#'
#' # Different styles per element
#' style_vector(
#'   c("Bold text", "Italic text", "Monospace"),
#'   bold = c(TRUE, FALSE, FALSE),
#'   italic = c(FALSE, TRUE, FALSE),
#'   monospace = c(FALSE, FALSE, TRUE)
#' )
#'
#' # Single style applied to all elements
#' style_vector(c("A", "B", "C"), color = "blue", fontsize = 1.2)
style_vector <- function(
    x,
    output = "html",
    bold = FALSE,
    italic = FALSE,
    monospace = FALSE,
    underline = FALSE,
    strikeout = FALSE,
    color = NULL,
    fontsize = NULL,
    indent = NULL) {
  assert_atomic_vector(x)

  # Convert input to character if needed
  if (!is.character(x)) {
    x <- as.character(x)
  }

  # Input validation
  assert_choice(output, choice = c("html", "latex", "typst", "markdown", "ansi"))

  # Length validation - all styling arguments must be length 1 or length(x)
  n <- length(x)
  assert_length(bold, len = c(1, n), name = "bold")
  assert_length(italic, len = c(1, n), name = "italic")
  assert_length(monospace, len = c(1, n), name = "monospace")
  assert_length(underline, len = c(1, n), name = "underline")
  assert_length(strikeout, len = c(1, n), name = "strikeout")
  assert_length(color, len = c(1, n), null.ok = TRUE, name = "color")
  assert_length(fontsize, len = c(1, n), null.ok = TRUE, name = "fontsize")
  assert_length(indent, len = c(1, n), null.ok = TRUE, name = "indent")

  # Type validation
  assert_logical(bold, name = "bold")
  assert_logical(italic, name = "italic")
  assert_logical(monospace, name = "monospace")
  assert_logical(underline, name = "underline")
  assert_logical(strikeout, name = "strikeout")
  assert_character(color, null.ok = TRUE, name = "color")
  assert_numeric(fontsize, null.ok = TRUE, name = "fontsize")
  assert_numeric(indent, lower = 0, null.ok = TRUE, name = "indent")

  # Return early if empty vector
  if (n == 0) {
    return(character(0))
  }

  # Recycle arguments to match vector length
  bold <- rep_len(bold, n)
  italic <- rep_len(italic, n)
  monospace <- rep_len(monospace, n)
  underline <- rep_len(underline, n)
  strikeout <- rep_len(strikeout, n)

  if (!is.null(color)) {
    color <- rep_len(standardize_colors(color), n)
  }
  if (!is.null(fontsize)) {
    fontsize <- rep_len(fontsize, n)
  }
  if (!is.null(indent)) {
    indent <- rep_len(indent, n)
  }

  # Build styles data frame
  styles <- data.frame(
    bold = bold,
    italic = italic,
    monospace = monospace,
    underline = underline,
    strikeout = strikeout,
    color = if (is.null(color)) rep(NA_character_, n) else color,
    fontsize = if (is.null(fontsize)) rep(NA_real_, n) else fontsize,
    indent = if (is.null(indent)) rep(NA_real_, n) else indent,
    stringsAsFactors = FALSE
  )

  # Select appropriate styling function based on output format
  style_fun <- switch(output,
    "html" = style_string_html,
    "latex" = style_string_latex,
    "typst" = style_string_typst,
    "markdown" = style_string_markdown,
    "ansi" = style_string_ansi
  )

  # Apply styling to each element
  styled_x <- character(n)
  for (i in seq_len(n)) {
    # Extract style for current element
    current_style <- lapply(styles[i, ], function(col) {
      if (is.na(col)) NULL else col
    })
    # Convert logical NA to FALSE for style functions
    current_style$bold <- isTRUE(current_style$bold)
    current_style$italic <- isTRUE(current_style$italic)
    current_style$monospace <- isTRUE(current_style$monospace)
    current_style$underline <- isTRUE(current_style$underline)
    current_style$strikeout <- isTRUE(current_style$strikeout)

    styled_x[i] <- style_fun(x[i], current_style)
  }

  return(styled_x)
}

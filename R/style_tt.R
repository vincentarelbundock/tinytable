# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Apply styling to notes or caption
#' @keywords internal
#' @noRd
apply_notes_caption_styling <- function(
  x,
  i,
  color = NULL,
  fontsize = NULL,
  italic = FALSE,
  monospace = FALSE,
  strikeout = FALSE,
  underline = FALSE
) {
  style_params <- list(
    color = color,
    fontsize = fontsize,
    italic = italic,
    monospace = monospace,
    strikeout = strikeout,
    underline = underline
  )

  if (identical(i, "notes")) {
    x@style_notes <- style_params
  } else if (identical(i, "caption")) {
    x@style_caption <- style_params
  }

  return(x)
}

#' Process logical matrix input for styling
#' @keywords internal
#' @noRd
process_logical_matrix_input <- function(x, i, j) {
  if (!is.null(j)) {
    stop("When `i` is a logical matrix, `j` must be NULL.", call. = FALSE)
  }

  settings <- which(i == TRUE, arr.ind = TRUE)
  settings <- stats::setNames(data.frame(settings), c("i", "j"))
  return(settings)
}

#' Process regular input for styling
#' @keywords internal
#' @noRd
process_regular_input <- function(x, i, j) {
  ival <- sanitize_i(i, x, calling_function = "style_tt")
  jval <- sanitize_j(j, x)

  # Create settings grid
  settings <- expand.grid(i = ival, j = jval, tabularray = "")

  # Order may be important for recycling
  if (is.null(i) && !is.null(j)) {
    settings <- settings[order(settings$i, settings$j), ]
  }

  return(settings)
}


#' Process align argument and add to settings
#' @keywords internal
#' @noRd
process_align_argument <- function(settings, align, jval) {
  if (is.null(align)) {
    settings[["align"]] <- NA
    return(settings)
  }

  if (nchar(align) == length(jval)) {
    align_string <- strsplit(align, "")[[1]]
    if (!all(align_string %in% c("c", "l", "r", "d"))) {
      stop("`align` must be characters c, l, r, or d.", call. = FALSE)
    }
    align_string <- data.frame(j = jval, align = align_string)
    settings <- merge(
      settings,
      align_string,
      by = "j",
      all.x = TRUE,
      sort = FALSE
    )
  } else if (nchar(align) == 1) {
    if (!align %in% c("c", "l", "r", "d")) {
      stop("`align` must be characters c, l, r, or d.", call. = FALSE)
    }
    align_string <- data.frame(j = jval, align = align)
    settings <- merge(
      settings,
      align_string,
      by = "j",
      all.x = TRUE,
      sort = FALSE
    )
  } else {
    msg <- sprintf(
      "`align` must be a single character or a string of length %s.",
      length(jval)
    )
    stop(msg, call. = FALSE)
  }

  return(settings)
}

#' Remove empty settings from dataframe
#' @keywords internal
#' @noRd
remove_empty_settings <- function(settings) {
  empty <- settings[, 4:ncol(settings)]
  empty <- sapply(empty, function(x) is.na(x) | (is.logical(x) && !any(x)))

  if (nrow(settings) == 1) {
    empty <- all(empty)
    settings <- settings[!empty, , drop = FALSE]
  } else {
    empty <- apply(empty, 1, all)
    settings <- settings[!empty, , drop = FALSE]
  }

  return(settings)
}

#' Merge settings with existing styles
#' @keywords internal
#' @noRd
merge_with_existing_styles <- function(x, settings) {
  if (nrow(settings) == 0) {
    return(x)
  }

  if (nrow(x@style) > 0 && ncol(x@style) != ncol(settings)) {
    # Handle column mismatch
    a <- x@style
    b <- settings
    if (!"tabularray" %in% colnames(a)) {
      a$tabularray <- ""
    }
    if (!"tabularray" %in% colnames(b)) {
      b$tabularray <- ""
    }
    settings <- rbind(a, b[, colnames(a)])
    x@style <- unique(settings)
  } else {
    x@style <- rbind(x@style, settings)
  }

  return(x)
}

# =============================================================================
# MAIN FUNCTION
# =============================================================================

#' Style a Tiny Table
#'
#' @details
#' This function applies styling to a table created by `tt()`. It allows customization of text style (bold, italic, monospace), text and background colors, font size, cell width, text alignment, column span, and indentation. The function also supports passing native instructions to LaTeX (tabularray) and HTML (bootstrap) formats.
#'
#' @param x A table object created by `tt()`.
#' @param i Numeric vector, logical matrix, or string.
#'   - Numeric vector: Row indices where the styling should be applied. Can be a single value or a vector.
#'   - Logical matrix: A matrix with the same number of rows and columns as `x`. `i=0` is the header, and negative values are higher level headers. Row indices refer to rows *after* the insertion of row labels by `group_tt()`, when applicable.
#'   - String: Table components "caption", "colnames", "groupi" (row group labels), "~groupi" (non-group rows), "groupj" (column group labels), "notes".
#' @param j Column indices where the styling should be applied. Can be:
#' + Integer vectors indicating column positions.
#' + Character vector indicating column names.
#' + A single string specifying a Perl-style regular expression used to match column names.
#' @param bold Logical; if `TRUE`, text is styled in bold.
#' @param italic Logical; if `TRUE`, text is styled in italic.
#' @param monospace Logical; if `TRUE`, text is styled in monospace font.
#' @param underline Logical; if `TRUE`, text is underlined.
#' @param strikeout Logical; if `TRUE`, text has a strike through line.
#' @param color Text color. There are several ways to specify colors, depending on the output format.
#' + HTML:
#'   - Hex code composed of # and 6 characters, ex: #CC79A7.
#'   - Keywords: black, silver, gray, white, maroon, red, purple, fuchsia, green, lime, olive, yellow, navy, blue, teal, aqua
#' + LaTeX:
#'   - Hex code composed of # and 6 characters, ex: "#CC79A7". See the section below for instructions to add in LaTeX preambles.
#'   - Keywords: black, blue, brown, cyan, darkgray, gray, green, lightgray, lime, magenta, olive, orange, pink, purple, red, teal, violet, white, yellow.
#'   - Color blending using xcolor`, ex: `white!80!blue`, `green!20!red`.
#'   - Color names with luminance levels from [the `ninecolors` package](https://mirror.quantum5.ca/CTAN/macros/latex/contrib/ninecolors/ninecolors.pdf) (ex: "azure4", "magenta8", "teal2", "gray1", "olive3").
#' @param background Background color. Specified as a color name or hexadecimal code. Can be `NULL` for default color.
#' @param fontsize Font size in em units. Can be `NULL` for default size.
#' @param align A single character or a string with a number of characters equal to the number of columns in `j`. Valid characters include 'c' (center), 'l' (left), 'r' (right), 'd' (decimal). Decimal alignment is only available in LaTeX via the `siunitx` package. The width of columns is determined by the maximum number of digits to the left and to the right in all cells specified by `i` and `j`.
#' @param alignv A single character specifying vertical alignment. Valid characters include 't' (top), 'm' (middle), 'b' (bottom).
#' @param colspan Number of columns a cell should span. `i` and `j` must be of length 1.
#' @param rowspan Number of rows a cell should span. `i` and `j` must be of length 1.
#' @param indent Text indentation in em units. Positive values only.
#' @param line String determines if solid lines (rules or borders) should be drawn around the cell, row, or column.
#' + "t": top
#' + "b": bottom
#' + "l": left
#' + "r": right
#' + Can be combined such as: "lbt" to draw borders at the left, bottom, and top.
#' @param line_color Color of the line. See the `color` argument for details.
#' @param line_width Width of the line in em units (default: 0.1).
#' @param finalize A function applied to the table object at the very end of table-building, for post-processing. For example, the function could use regular expressions to add LaTeX commands to the text version of the table hosted in `x@table_string`, or it could programmatically change the caption in `x@caption`.
#' @param ... extra arguments are ignored
#' @return An object of class `tt` representing the table.
#' @template limitations_word_markdown
#' @export
#' @examplesIf knitr::is_html_output()
#' @examples
#' if (knitr::is_html_output()) options(tinytable_print_output = "html")
#'
#' library(tinytable)
#'
#' tt(mtcars[1:5, 1:6])
#'
#' # Alignment
#' tt(mtcars[1:5, 1:6]) |>
#'   style_tt(j = 1:5, align = "lcccr")
#'
#' # Colors and styles
#' tt(mtcars[1:5, 1:6]) |>
#'   style_tt(i = 2:3, background = "black", color = "orange", bold = TRUE)
#'
#' # column selection with `j``
#' tt(mtcars[1:5, 1:6]) |>
#'   style_tt(j = 5:6, background = "pink")
#'
#' tt(mtcars[1:5, 1:6]) |>
#'   style_tt(j = "drat|wt", background = "pink")
#'
#' tt(mtcars[1:5, 1:6]) |>
#'   style_tt(j = c("drat", "wt"), background = "pink")
#'
#' tt(mtcars[1:5, 1:6], theme = "void") |>
#'   style_tt(
#'     i = 2, j = 2,
#'     colspan = 3,
#'     rowspan = 2,
#'     align = "c",
#'     alignv = "m",
#'     color = "white",
#'     background = "black",
#'     bold = TRUE)
#'
#' tt(mtcars[1:5, 1:6], theme = "void") |>
#'   style_tt(
#'     i = 0:3,
#'     j = 1:3,
#'     line = "tblr",
#'     line_width = 0.4,
#'     line_color = "teal")
#'
#' tt(mtcars[1:5, 1:6], theme = "striped") |>
#'   style_tt(
#'     i = c(2, 5),
#'     j = 3,
#'     strikeout = TRUE,
#'     fontsize = 0.7)
#'
#' tt(mtcars[1:5, 1:6]) |>
#'   theme_html(class = "table table-dark table-hover")
#'
#'
#' inner <- "
#' column{1-4}={halign=c},
#' hlines = {fg=white},
#' vlines = {fg=white},
#' cell{1,6}{odd} = {bg=teal7},
#' cell{1,6}{even} = {bg=green7},
#' cell{2,4}{1,4} = {bg=red7},
#' cell{3,5}{1,4} = {bg=purple7},
#' cell{2}{2} = {r=4,c=2}{bg=azure7},
#' "
#' tt(mtcars[1:5, 1:4], theme = "void") |>
#'   theme_latex(inner = inner)
#'
#' # Style group rows and non-group rows
#' dat <- data.frame(x = 1:6, y = letters[1:6])
#' dat |>
#'   tt() |>
#'   group_tt(i = list("Group A" = 3)) |>
#'   style_tt(i = "groupi", background = "lightblue") |>
#'   style_tt(i = "~groupi", background = "lightgray")
#'
style_tt <- function(
  x,
  i = NULL,
  j = NULL,
  bold = FALSE,
  italic = FALSE,
  monospace = FALSE,
  underline = FALSE,
  strikeout = FALSE,
  color = NULL,
  background = NULL,
  fontsize = NULL,
  align = NULL,
  alignv = NULL,
  colspan = NULL,
  rowspan = NULL,
  indent = NULL,
  line = NULL,
  line_color = "black",
  line_width = 0.1,
  finalize = NULL,
  ...
) {
  out <- x

  if ("tabularray_inner" %in% ...names()) {
    x <- theme_latex(inner = ...get("tabularray_inner"))
    warning("The `tabularray_inner` argument is deprecated. Use `theme_latex(x, inner = ...)` instead.",
      call. = FALSE
    )
  }
  if ("tabularray_outer" %in% ...names()) {
    x <- theme_latex(outer = ...get("tabularray_outer"))
    warning("The `tabularray_outer` argument is deprecated. Use `theme_latex(x, outer = ...)` instead.",
      call. = FALSE
    )
  }
  if ("bootstrap_class" %in% ...names()) {
    x <- theme_html(x, class = ...get("bootstrap_class"))
    warning(
      "The `bootstrap_class` argument is deprecated. Use `theme_html(x, class = ...)` instead.",
      call. = FALSE
    )
  }
  if ("bootstrap_css_rule" %in% ...names()) {
    x <- theme_html(x, css_rule = ...get("bootstrap_css_rule"))
    warning("The `bootstrap_css_rule` argument is deprecated. Use `theme_html(x, css_rule = ...)` instead.",
      call. = FALSE
    )
  }

  # this must be handled here rather than theme_html() because it is a cell-level issue
  bootstrap_css <- ...get("bootstrap_css")

  # Handle special cases first (before validation)
  if (isTRUE(i %in% c("notes", "caption"))) {
    return(apply_notes_caption_styling(
      out,
      i,
      color,
      fontsize,
      italic,
      monospace,
      strikeout,
      underline
    ))
  }

  # Validate inputs (after special cases)
  assert_style_tt(
    x = out,
    i = i,
    j = j,
    bold = bold,
    italic = italic,
    monospace = monospace,
    underline = underline,
    strikeout = strikeout,
    color = color,
    background = background,
    fontsize = fontsize,
    align = align,
    alignv = alignv,
    colspan = colspan,
    rowspan = rowspan,
    indent = indent,
    line = line,
    line_color = line_color,
    line_width = line_width,
    bootstrap_css = bootstrap_css,
    finalize = finalize,
    ...
  )

  sanity_align(align, i)

  # Process inputs and create settings
  if (
    is.matrix(i) && is.logical(i) && nrow(i) == nrow(x) && ncol(i) == ncol(x)
  ) {
    settings <- process_logical_matrix_input(x, i, j)
  } else {
    settings <- process_regular_input(x, i, j)
  }

  # Build complete settings
  settings[["color"]] <- if (is.null(color)) NA else as.vector(color)
  settings[["background"]] <- if (is.null(background)) {
    NA
  } else {
    as.vector(background)
  }
  settings[["fontsize"]] <- if (is.null(fontsize)) NA else as.vector(fontsize)
  settings[["alignv"]] <- if (is.null(alignv)) NA else alignv
  settings[["line"]] <- if (is.null(line)) NA else line
  settings[["line_color"]] <- if (is.null(line)) NA else line_color
  settings[["line_width"]] <- if (is.null(line)) NA else line_width
  settings[["bold"]] <- bold
  settings[["italic"]] <- italic
  settings[["monospace"]] <- monospace
  settings[["strikeout"]] <- strikeout
  settings[["underline"]] <- underline
  settings[["indent"]] <- if (is.null(indent)) NA else as.vector(indent)
  settings[["colspan"]] <- if (is.null(colspan)) NA else colspan
  settings[["rowspan"]] <- if (is.null(rowspan)) NA else rowspan
  settings[["bootstrap_css"]] <- if (!is.null(bootstrap_css)) {
    bootstrap_css
  } else {
    NA
  }

  if (!is.matrix(i) || !is.logical(i)) {
    jval <- sanitize_j(j, x)
    settings <- process_align_argument(settings, align, jval)
  }

  settings <- remove_empty_settings(settings)

  out <- merge_with_existing_styles(out, settings)

  if (is.function(finalize)) {
    out@lazy_finalize <- c(out@lazy_finalize, list(finalize))
  }

  return(out)
}

assert_style_tt <- function(
  x,
  i,
  j,
  bold,
  italic,
  monospace,
  underline,
  strikeout,
  color,
  background,
  fontsize,
  align,
  alignv,
  colspan,
  rowspan,
  indent,
  line,
  line_color,
  line_width,
  tabularray_inner,
  tabularray_outer,
  finalize = NULL,
  ...
) {
  # Validate alignv choice
  assert_choice(alignv, c("t", "m", "b"), null.ok = TRUE)


  # Validate finalize function
  assert_function(finalize, null.ok = TRUE)

  # Check for deprecated width argument
  if ("width" %in% names(list(...))) {
    stop("The `width` argument is now in the `tt()` function.", call. = FALSE)
  }

  assert_integerish(colspan, len = 1, lower = 2, null.ok = TRUE)
  assert_integerish(rowspan, len = 1, lower = 2, null.ok = TRUE)
  assert_numeric(indent, len = 1, lower = 0, null.ok = TRUE)
  assert_character(background, null.ok = TRUE)
  assert_character(color, null.ok = TRUE)
  assert_numeric(fontsize, null.ok = TRUE)
  assert_logical(bold)
  assert_logical(italic)
  assert_logical(monospace)
  assert_logical(underline)
  assert_logical(strikeout)
  assert_string(line, null.ok = TRUE)
  assert_string(line_color, null.ok = FALSE) # black default
  assert_numeric(line_width, len = 1, lower = 0, null.ok = FALSE) # 0.1 default

  # must be handled here rather than theme_html() because it is a cell-level issue
  bootstrap_css <- ...get("bootstrap_css")
  assert_character(bootstrap_css, null.ok = TRUE)

  if (is.character(line)) {
    line <- strsplit(line, split = "")[[1]]
    if (!all(line %in% c("t", "b", "l", "r"))) {
      msg <- "`line` must be a string of characters t, b, l, or r."
      stop(msg, call. = FALSE)
    }
  }

  ival <- sanitize_i(i, x, calling_function = "style_tt")
  jval <- sanitize_j(j, x)
  inull <- isTRUE(attr(ival, "null"))
  jnull <- isTRUE(attr(jval, "null"))

  # 1
  if (inull && jnull) {
    assert_length(color, len = 1, null.ok = TRUE)
    assert_length(background, len = 1, null.ok = TRUE)
    assert_length(fontsize, len = 1, null.ok = TRUE)
    assert_length(bold, len = 1)
    assert_length(italic, len = 1)
    assert_length(monospace, len = 1)
    assert_length(underline, len = 1)
    assert_length(strikeout, len = 1)

    # 1 or #rows
  } else if (!inull && jnull) {
    assert_length(color, len = c(1, length(ival)), null.ok = TRUE)
    assert_length(background, len = c(1, length(ival)), null.ok = TRUE)
    assert_length(fontsize, len = c(1, length(ival)), null.ok = TRUE)
    assert_length(bold, len = c(1, length(ival)))
    assert_length(italic, len = c(1, length(ival)))
    assert_length(monospace, len = c(1, length(ival)))
    assert_length(underline, len = c(1, length(ival)))
    assert_length(strikeout, len = c(1, length(ival)))

    # 1 or #cols
  } else if (inull && !jnull) {
    assert_length(color, len = c(1, length(jval)), null.ok = TRUE)
    assert_length(background, len = c(1, length(jval)), null.ok = TRUE)
    assert_length(fontsize, len = c(1, length(jval)), null.ok = TRUE)
    assert_length(bold, len = c(1, length(jval)))
    assert_length(italic, len = c(1, length(jval)))
    assert_length(monospace, len = c(1, length(jval)))
    assert_length(underline, len = c(1, length(jval)))
    assert_length(strikeout, len = c(1, length(jval)))

    # 1 or #cells
  } else if (!inull && !jnull) {
    assert_length(
      color,
      len = c(1, length(ival) * length(jval)),
      null.ok = TRUE
    )
    assert_length(
      background,
      len = c(1, length(ival) * length(jval)),
      null.ok = TRUE
    )
    assert_length(
      fontsize,
      len = c(1, length(ival) * length(jval)),
      null.ok = TRUE
    )
    assert_length(bold, len = c(1, length(ival) * length(jval)))
    assert_length(italic, len = c(1, length(ival) * length(jval)))
    assert_length(monospace, len = c(1, length(ival) * length(jval)))
    assert_length(underline, len = c(1, length(ival) * length(jval)))
    assert_length(strikeout, len = c(1, length(ival) * length(jval)))
  }
}

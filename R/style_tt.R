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
  bold = NULL,
  italic = NULL,
  monospace = NULL,
  smallcap = NULL,
  strikeout = NULL,
  underline = NULL
) {
  style_params <- list(
    color = color,
    fontsize = fontsize,
    bold = bold,
    italic = italic,
    monospace = monospace,
    smallcap = smallcap,
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

  settings <- which(i, arr.ind = TRUE)
  settings <- stats::setNames(data.frame(settings), c("i", "j"))
  return(settings)
}

#' Process regular input for styling
#' @keywords internal
#' @noRd
process_regular_input <- function(x, i, j) {
  ival <- sanitize_i(i, x, calling_function = "style_tt")
  jval <- sanitize_j(j, x)

  # Handle empty index case - return empty settings dataframe with proper structure
  if (length(ival) == 0) {
    return(data.frame(i = integer(0), j = integer(0)))
  }

  # Create settings grid
  settings <- expand.grid(i = ival, j = jval)

  # Order may be important for recycling
  if (is.null(i) && !is.null(j)) {
    settings <- settings[order(settings$i, settings$j), ]
  }

  return(settings)
}


#' Process align argument and add to settings
#' @keywords internal
#' @noRd
process_align_argument <- function(x, settings, align) {
  if (is.null(align)) {
    if (nrow(settings) > 0) {
      settings[["align"]] <- NA_character_
    }
    return(settings)
  }

  # Get the number of columns being styled
  n_cols_styled <- length(unique(settings$j))

  if (nchar(align) == n_cols_styled) {
    align <- strsplit(align, "")[[1]]
  } else if (nchar(align) == 1) {
    align <- rep(align, n_cols_styled)
  } else {
    msg <- sprintf("`align` must be a single character or a string of length %s.", n_cols_styled)
    stop(msg, call. = FALSE)
  }

  if (!all(align %in% c("c", "l", "r", "d"))) {
    stop("`align` must be characters c, l, r, or d.", call. = FALSE)
  }

  # Initialize align column if it doesn't exist
  if (!"align" %in% names(settings)) {
    settings$align <- NA_character_
  }

  unique_j <- unique(settings$j)
  for (j in seq_along(align)) {
    idx <- which(settings$j == unique_j[j])
    settings$align[idx] <- align[j]
  }

  return(settings)
}



style_tt_lazy <- function(
  x,
  i = NULL,
  j = NULL,
  bold = NULL,
  italic = NULL,
  monospace = NULL,
  smallcap = NULL,
  underline = NULL,
  strikeout = NULL,
  color = NULL,
  background = NULL,
  fontsize = NULL,
  align = NULL,
  alignv = NULL,
  colspan = NULL,
  rowspan = NULL,
  indent = NULL,
  line = NULL,
  line_color = NULL,
  line_width = 0.1,
  line_trim = NULL,
  finalize = NULL,
  ...) {

  out <- x

  # Set default line_color if NULL
  if (is.null(line_color) && !is.null(line)) {
    if (identical(x@output, "html") && identical(x@html_engine, "tinytable")) {
      line_color <- "var(--tt-line-color)"
    } else {
      line_color <- "black"
    }
  }

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
  if ("html_class" %in% ...names()) {
    x <- theme_html(x, class = ...get("html_class"))
    warning(
      "The `html_class` argument is deprecated. Use `theme_html(x, class = ...)` instead.",
      call. = FALSE
    )
  }
  if ("html_css_rule" %in% ...names()) {
    x <- theme_html(x, css_rule = ...get("html_css_rule"))
    warning("The `html_css_rule` argument is deprecated. Use `theme_html(x, css_rule = ...)` instead.",
      call. = FALSE
    )
  }

  # this must be handled here rather than theme_html() because it is a cell-level issue
  html_css <- ...get("html_css")

  # Handle special cases first (before validation)
  if (isTRUE(i %in% c("notes", "caption"))) {
    return(apply_notes_caption_styling(
      out,
      i,
      color,
      fontsize,
      bold,
      italic,
      monospace,
      smallcap,
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
    smallcap = smallcap,
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
    line_trim = line_trim,
    html_css = html_css,
    finalize = finalize,
    ...
  )

  # Add Tabulator-specific validation via lazy_prepare
  # This checks alignment restrictions when output is determined
  if (!is.null(align) || !is.null(alignv)) {
    validate_fn <- function(x) {
      if (x@html_engine == "tabulator") {
        if (!is.null(i)) {
          stop(
            "Tabulator does not support row-specific alignment. ",
            "When using `align` or `alignv` with Tabulator output, `i` must be NULL to apply alignment to entire columns.",
            call. = FALSE
          )
        }
      }
      return(x)
    }
    out <- build_prepare(out, validate_fn, output = "html")
  }

  sanity_align(align, i)

  # Process inputs and create settings
  if (
  is.matrix(i) && is.logical(i) && nrow(i) == nrow(x) && ncol(i) == ncol(x)
) {
    settings <- process_logical_matrix_input(x, i, j)
  } else {
    settings <- process_regular_input(x, i, j)
  }

  # Build complete settings - skip if no rows to style
  if (nrow(settings) > 0) {
    settings[["color"]] <- if (is.null(color)) NA else as.vector(color)
    settings[["background"]] <- if (is.null(background)) {
      NA
    } else {
        as.vector(background)
      }
    settings[["fontsize"]] <- if (is.null(fontsize)) NA else as.vector(fontsize)
    settings[["align"]] <- if (is.null(alignv)) NA else align
    settings[["alignv"]] <- if (is.null(alignv)) NA else alignv
    settings[["line_color"]] <- if (is.null(line)) NA else line_color
    settings[["line_width"]] <- if (is.null(line)) NA else line_width
    settings[["bold"]] <- if (is.null(bold)) NA else bold
    settings[["italic"]] <- if (is.null(italic)) NA else italic
    settings[["monospace"]] <- if (is.null(monospace)) NA else monospace
    settings[["smallcap"]] <- if (is.null(smallcap)) NA else smallcap
    settings[["strikeout"]] <- if (is.null(strikeout)) NA else strikeout
    settings[["underline"]] <- if (is.null(underline)) NA else underline
    settings[["indent"]] <- if (is.null(indent)) NA else as.vector(indent)
    settings[["colspan"]] <- if (is.null(colspan)) NA else colspan
    settings[["rowspan"]] <- if (is.null(rowspan)) NA else rowspan
    settings[["html_css"]] <- if (!is.null(html_css)) {
      html_css
    } else {
      NA
    }
    # Always create tabularray column for consistency
    settings[["tabularray"]] <- ""

    # Expand compound line directions like "tblr" into separate entries
    if (!is.null(line) && nchar(line) > 1) {
      line_chars <- strsplit(line, "")[[1]]
      # Create multiple rows for each line direction
      expanded_settings <- do.call(rbind, lapply(line_chars, function(direction) {
        new_settings <- settings
        new_settings[["line"]] <- direction
        new_settings
      }))
      settings <- expanded_settings
    } else {
      settings[["line"]] <- if (is.null(line)) NA else line
    }

    if (!is.null(line_trim)) {
      split_consecutive <- function(x) {
        x <- sort(unique(x))  # optional: ensure sorted & unique
        groups <- cumsum(c(1, diff(x) != 1))
        split(x, groups)
      }
      idx <- split_consecutive(settings$j)
      left <- grepl("l", line_trim)
      right <- grepl("r", line_trim)
      for (d in idx) {
        if (length(d) == 1) {
          settings[["line_trim"]][settings$j == d] <- line_trim
        } else {
          if (left) {
            settings[["line_trim"]][settings$j == min(d)] <- "l"
          }
          if (right) {
            settings[["line_trim"]][settings$j == max(d)] <- "r"
          }
        }
      }
      ends <- unlist(lapply(idx, range))
      settings[["line_trim"]][!settings$j %in% ends] <- NA
    } else {
      settings[["line_trim"]] <- NA
    }

  }

  if (!is.matrix(i) || !is.logical(i)) {
    settings <- process_align_argument(x, settings, align)
  } else {
    if (nrow(settings) > 0) {
      settings$align <- NA_character_
    }
  }

  # sort column: important for bind
  cols <- unique(c("i", "j", sort(colnames(settings))))
  settings <- settings[, cols, drop = FALSE]

  # Only add settings if there are rows to add
  if (nrow(settings) > 0) {
    if (nrow(out@style) == 0) {
      out@style <- settings
    } else {
      out@style <- rbind(out@style, settings)
    }
  }

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
  smallcap,
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
  line_trim,
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
  assert_logical(bold, null.ok = TRUE)
  assert_logical(italic, null.ok = TRUE)
  assert_logical(monospace, null.ok = TRUE)
  assert_logical(smallcap, null.ok = TRUE)
  assert_logical(underline, null.ok = TRUE)
  assert_logical(strikeout, null.ok = TRUE)
  assert_string(line, null.ok = TRUE)
  assert_string(line_color, null.ok = TRUE) # default determined by output format
  assert_numeric(line_width, len = 1, lower = 0, null.ok = FALSE) # 0.1 default
  assert_choice(line_trim, c("l", "r", "lr"), null.ok = TRUE)

  # Validate that line_trim is only used with bottom lines
  if (!is.null(line_trim) && !is.null(line)) {
    if (!identical("b", line)) {
      stop("line_trim can only be used with bottom lines (line must contain 'b').", call. = FALSE)
    }
  }

  # must be handled here rather than theme_html() because it is a cell-level issue
  html_css <- ...get("html_css")
  assert_character(html_css, null.ok = TRUE)

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
    assert_length(bold, len = 1, null.ok = TRUE)
    assert_length(italic, len = 1, null.ok = TRUE)
    assert_length(monospace, len = 1, null.ok = TRUE)
    assert_length(smallcap, len = 1, null.ok = TRUE)
    assert_length(underline, len = 1, null.ok = TRUE)
    assert_length(strikeout, len = 1, null.ok = TRUE)

    # 1 or #rows
  } else if (!inull && jnull) {
    assert_length(color, len = c(1, length(ival)), null.ok = TRUE)
    assert_length(background, len = c(1, length(ival)), null.ok = TRUE)
    assert_length(fontsize, len = c(1, length(ival)), null.ok = TRUE)
    assert_length(bold, len = c(1, length(ival)), null.ok = TRUE)
    assert_length(italic, len = c(1, length(ival)), null.ok = TRUE)
    assert_length(monospace, len = c(1, length(ival)), null.ok = TRUE)
    assert_length(smallcap, len = c(1, length(ival)), null.ok = TRUE)
    assert_length(underline, len = c(1, length(ival)), null.ok = TRUE)
    assert_length(strikeout, len = c(1, length(ival)), null.ok = TRUE)

    # 1 or #cols
  } else if (inull && !jnull) {
    assert_length(color, len = c(1, length(jval)), null.ok = TRUE)
    assert_length(background, len = c(1, length(jval)), null.ok = TRUE)
    assert_length(fontsize, len = c(1, length(jval)), null.ok = TRUE)
    assert_length(bold, len = c(1, length(jval)), null.ok = TRUE)
    assert_length(italic, len = c(1, length(jval)), null.ok = TRUE)
    assert_length(monospace, len = c(1, length(jval)), null.ok = TRUE)
    assert_length(smallcap, len = c(1, length(jval)), null.ok = TRUE)
    assert_length(underline, len = c(1, length(jval)), null.ok = TRUE)
    assert_length(strikeout, len = c(1, length(jval)), null.ok = TRUE)

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
    assert_length(bold, len = c(1, length(ival) * length(jval)), null.ok = TRUE)
    assert_length(italic, len = c(1, length(ival) * length(jval)), null.ok = TRUE)
    assert_length(monospace, len = c(1, length(ival) * length(jval)), null.ok = TRUE)
    assert_length(smallcap, len = c(1, length(ival) * length(jval)), null.ok = TRUE)
    assert_length(underline, len = c(1, length(ival) * length(jval)), null.ok = TRUE)
    assert_length(strikeout, len = c(1, length(ival) * length(jval)), null.ok = TRUE)
  }
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
#' @param i Numeric vector, logical matrix, string, or unquoted expression.
#'   - Numeric vector: Row indices where the styling should be applied. Can be a single value or a vector.
#'   - Logical matrix: A matrix with the same number of rows and columns as `x`. `i=0` is the header, and negative values are higher level headers. Row indices refer to rows *after* the insertion of row labels by `group_tt()`, when applicable.
#'   - String: Table components "caption", "colnames", "groupi" (row group labels), "~groupi" (non-group rows), "groupj" (column group labels), "notes".
#'   - Unquoted expression: When supplying an unquoted expression, it is first evaluated in the calling environment, then in the data frame passed to `tt()`.
#' @param j Column indices where the styling should be applied. Can be:
#' + Integer vectors indicating column positions.
#' + Character vector indicating column names.
#' + A single string specifying a Perl-style regular expression used to match column names.
#' + Unquoted expression: Non-standard evaluation is supported. When supplying an unquoted expression, it is first evaluated in the calling environment, then in an environment that includes the columns of the original data passed to `tt()`, and `groupi` indices. See examples below.
#' @param bold Logical; if `TRUE`, text is styled in bold.
#' @param italic Logical; if `TRUE`, text is styled in italic.
#' @param monospace Logical; if `TRUE`, text is styled in monospace font.
#' @param smallcap Logical; if `TRUE`, text is styled in small caps. In Markdown output format, text is converted to uppercase.
#' @param underline Logical; if `TRUE`, text is underlined.
#' @param strikeout Logical; if `TRUE`, text has a strike through line.
#' @param color Text color. Colors are standardized across output formats and can be specified as:
#'   - Hex codes: "#CC79A7", "#FF0000", "#123ABC"
#'   - R color names: Any color recognized by R, such as "red", "blue", "forestgreen", "lightblue"
#'   - Extended color names: 749+ named colors from the LaTeX xcolor package (see `tinytable:::latex_colors` for the full list)
#'   - LaTeX color blending (LaTeX output only): "white!80!blue", "red!50", "green!20!red"
#' @param background Background color. Same color specification options as the `color` parameter. Can be `NULL` for default color.
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
#' @param line_trim String specifying line trimming. Acceptable values: "l" (left), "r" (right), "lr" (both sides). When specified, shortens the lines by 0.8pt on the specified side(s). Default: NULL (no trimming).
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
#' tt(mtcars[1:5, 1:6], theme = "empty") |>
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
#' tt(mtcars[1:5, 1:6], theme = "empty") |>
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
#' # Non-standard evaluation (NSE)
#' dat <- data.frame(
#'   w = c(143002.2092, 201399.181, 100188.3883),
#'   x = c(1.43402, 201.399, 0.134588),
#'   y = as.Date(c(897, 232, 198), origin = "1970-01-01"),
#'   z = c(TRUE, TRUE, FALSE)
#' )
#' tt(dat) |>
#'   style_tt(i = w > 150000, j = c("w", "x"), 
#'            color = "white", background = "black")
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
#' tt(mtcars[1:5, 1:4], theme = "empty") |>
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
#' # unquote expressions
#' dat <- mtcars[1:10,]
#' dat <- dat[order(dat$am),]
#' tt(dat) |>
#'    subset(mpg > 20) |>
#'    group_tt(am)
#'
#' # style elements: captions and colnames
#' notes <- list(
#'   "*" = "Hello world", 
#'   "a" = "Bacon ipsum dolor amet kevin t-bone porchetta.")
#' tt(head(iris), 
#'    width = .8,
#'    caption = "This is a Caption Example.", 
#'    notes = notes) |>
#'   style_tt(2, 2, background = "pink", rowspan = 2, colspan = 2, 
#'     alignv = "m", align = "c", line = "tblr") |>
#'   style_tt("colnames", italic = TRUE) |>
#'   style_tt("caption", smallcap = TRUE)
#'
style_tt <- function(
  x,
  i = NULL,
  j = NULL,
  bold = NULL,
  italic = NULL,
  monospace = NULL,
  smallcap = NULL,
  underline = NULL,
  strikeout = NULL,
  color = NULL,
  background = NULL,
  fontsize = NULL,
  align = NULL,
  alignv = NULL,
  colspan = NULL,
  rowspan = NULL,
  indent = NULL,
  line = NULL,
  line_color = NULL,
  line_width = 0.1,
  line_trim = NULL,
  finalize = NULL,
  ...
) {

  
  # non-standard evaluation before anything else
  tmp <- nse_i_j(x, i_expr = substitute(i), j_expr = substitute(j), pf = parent.frame())
  list2env(tmp, environment())

  obj <- match.call()

  # evaluate arguments immediately, except i and j, to avoid scoping issues
  obj <- list(
    style_tt_lazy,
    x = quote(x),
    i = i,
    j = j,
    bold = bold,
    italic = italic,
    monospace = monospace,
    smallcap = smallcap,
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
    line_trim = line_trim,
    finalize = finalize)
  obj <- c(obj, list(...))
  obj <- as.call(obj)

  x@lazy_style <- c(x@lazy_style, list(obj))
  return(x)
}




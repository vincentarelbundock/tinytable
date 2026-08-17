# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# Style arguments evaluated eagerly in style_tt() and forwarded to the lazy
# call. Keep in sync with the formals of style_tt() and style_tt_lazy().
# (i and j are excluded: they go through non-standard evaluation.)
STYLE_TT_ARGS <- c(
  "bold", "italic", "monospace", "smallcap", "underline", "strikeout",
  "color", "background", "fontsize", "align", "alignv", "colspan", "rowspan",
  "indent", "line", "line_color", "line_width", "line_type", "line_trim",
  "finalize"
)

# Line types supported by every backend that draws rules. This is the
# intersection of CSS `border-style`, tabularray hline/vline specs, and Typst
# `stroke(dash:)`. Do not extend without checking all three.
LINE_TYPES <- c("solid", "dashed", "dotted")

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

  # Style values recycle over cells in USER-specified order, matching the
  # behavior of `i`: style_tt(j = c(3, 1), background = c("red", "blue"))
  # puts red on column 3 and blue on column 1. sanitize_j() returns positions
  # in column order for character `j`, so restore the user's order here.
  if (is.character(j) && length(j) > 1) {
    jval <- match(j, colnames(x))
  }

  # Handle empty index case - return empty settings dataframe with proper structure
  if (length(ival) == 0) {
    return(data.frame(i = integer(0), j = integer(0)))
  }

  # Create settings grid. expand.grid() varies `i` fastest, so rows follow the
  # user-specified `j` order (important for recycling style value vectors).
  settings <- expand.grid(i = ival, j = jval)

  return(settings)
}


#' Process align argument and add to settings
#' @keywords internal
#' @noRd
process_align_argument <- function(x, settings, align) {
  # Empty selection (e.g., i = integer(0)): nothing to style, and assigning
  # any column to a 0-row frame would fail with a replacement-length error.
  if (nrow(settings) == 0) {
    return(settings)
  }

  if (is.null(align)) {
    settings[["align"]] <- NA_character_
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
  line_type = NULL,
  line_trim = NULL,
  finalize = NULL,
  ...) {

  out <- x
  # Each lazy call exposes only its own settings frame for build_tt() to
  # collect. In particular, notes and caption styling return early below and
  # must not expose the preceding call's frame again.
  out@style <- data.frame()

  # Row indices derived from an NSE predicate (e.g., i = x > 30) were computed
  # eagerly in style_tt() against the pre-group data rows. Here, at lazy-eval
  # time, group_tt() label rows have been inserted and numeric `i` refers to
  # final-table positions, so remap through @index_body.
  i <- nse_remap_i(i, x)

  # Set default line_color if NULL. HTML and Typst both have a native mechanism
  # to inherit the color from the surrounding document (a CSS variable, and
  # Typst stroke folding respectively), so we leave the paint unspecified there
  # rather than hard-coding black and overriding the user's global styling.
  if (is.null(line_color) && !is.null(line)) {
    if (identical(x@output, "html") && identical(x@html_engine, "tinytable")) {
      line_color <- "var(--tt-line-color)"
    } else if (!identical(x@output, "typst")) {
      line_color <- "black"
    }
  }

  # Deprecated argument shims. These must mutate `out`, which is the object
  # returned to build_tt(); changes made to `x` after `out <- x` would be lost.
  if ("tabularray_inner" %in% ...names()) {
    out <- theme_latex(out, inner = ...get("tabularray_inner"))
    warning("The `tabularray_inner` argument is deprecated. Use `theme_latex(x, inner = ...)` instead.",
      call. = FALSE
    )
  }
  if ("tabularray_outer" %in% ...names()) {
    out <- theme_latex(out, outer = ...get("tabularray_outer"))
    warning("The `tabularray_outer` argument is deprecated. Use `theme_latex(x, outer = ...)` instead.",
      call. = FALSE
    )
  }
  if ("html_class" %in% ...names()) {
    out <- theme_html(out, class = ...get("html_class"))
    warning(
      "The `html_class` argument is deprecated. Use `theme_html(x, class = ...)` instead.",
      call. = FALSE
    )
  }
  if ("html_css_rule" %in% ...names()) {
    out <- theme_html(out, css_rule = ...get("html_css_rule"))
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
    line_type = line_type,
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

  # Process inputs and create settings.
  # Logical-matrix `i` dimensions were validated in assert_style_tt().
  if (is.matrix(i) && is.logical(i)) {
    settings <- process_logical_matrix_input(x, i, j)
  } else {
    settings <- process_regular_input(x, i, j)
  }

  # Build complete settings - skip if no rows to style
  if (nrow(settings) > 0) {
    # Cell-style columns: NA when unset, otherwise the user value recycled over
    # the settings rows. STYLE_PROPS is the canonical property vector defined
    # in style_maps.R; align, alignv, and html_css need special handling below.
    for (prop in setdiff(STYLE_PROPS, c("align", "alignv", "html_css"))) {
      value <- get(prop, inherits = FALSE)
      settings[[prop]] <- if (is.null(value)) NA else as.vector(value)
    }
    settings[["alignv"]] <- if (is.null(alignv)) NA else alignv
    settings[["line_color"]] <- if (is.null(line) || is.null(line_color)) {
      NA_character_
    } else {
      line_color
    }
    settings[["line_width"]] <- if (is.null(line)) NA else line_width
    settings[["line_type"]] <- if (is.null(line) || is.null(line_type)) {
      NA_character_
    } else {
      line_type
    }
    settings[["html_css"]] <- if (!is.null(html_css)) {
      html_css
    } else {
      NA
    }
    # Always create tabularray column for consistency
    settings[["tabularray"]] <- ""

    # Expand compound line directions like "tblr" into separate entries.
    # Only the first copy keeps the non-line properties (background, color,
    # etc.); the extra copies carry line-relevant columns only, so other
    # properties are not resolved multiple times per cell.
    if (!is.null(line) && nchar(line) > 1) {
      line_chars <- strsplit(line, "")[[1]]
      line_only <- settings
      for (prop in setdiff(colnames(line_only), c("i", "j", "line_color", "line_width", "line_type", "tabularray"))) {
        line_only[[prop]] <- NA
      }
      expanded_settings <- vector("list", length(line_chars))
      for (k in seq_along(line_chars)) {
        new_settings <- if (k == 1) settings else line_only
        new_settings[["line"]] <- line_chars[k]
        expanded_settings[[k]] <- new_settings
      }
      settings <- do.call(rbind, expanded_settings)
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
    if (!is.null(align)) {
      warning("`align` is not supported when `i` is a logical matrix; it is ignored.", call. = FALSE)
    }
    if (nrow(settings) > 0) {
      settings$align <- NA_character_
    }
  }

  # sort column: important for bind
  cols <- unique(c("i", "j", sort(colnames(settings))))
  settings <- settings[, cols, drop = FALSE]

  # Expose this call's settings frame in @style. build_tt() collects the
  # per-call frames and rbind()s them once at the end of the lazy loop, instead
  # of growing @style with an incremental rbind() on every style_tt() call.
  # That per-call rbind() was O(N^2) and dominated the lazy-style evaluation
  # phase on tables with many style_tt() calls (e.g. per-cell heat-maps).
  out@style <- settings

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
  line_type,
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
  assert_choice(line_type, LINE_TYPES, null.ok = TRUE)
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

  # Valid recycling lengths for style value vectors
  if (is.matrix(ival) && is.logical(ival)) {
    # Logical-matrix selection: dimensions must match the table exactly,
    # otherwise TRUE cells would be silently coerced to garbage indices.
    if (nrow(ival) != nrow(x) || ncol(ival) != ncol(x)) {
      msg <- sprintf(
        "When `i` is a logical matrix, its dimensions must match the table: %s rows and %s columns (received %s by %s).",
        nrow(x), ncol(x), nrow(ival), ncol(ival)
      )
      stop(msg, call. = FALSE)
    }
    # One settings row per TRUE cell, filled in column-major order
    len <- c(1, sum(ival))
  } else if (inull && jnull) {
    # 1
    len <- 1
  } else if (!inull && jnull) {
    # 1 or #rows
    len <- c(1, length(ival))
  } else if (inull && !jnull) {
    # 1 or #cols
    len <- c(1, length(jval))
  } else {
    # 1 or #cells
    len <- c(1, length(ival) * length(jval))
  }

  recycled_props <- list(
    color = color,
    background = background,
    fontsize = fontsize,
    bold = bold,
    italic = italic,
    monospace = monospace,
    smallcap = smallcap,
    underline = underline,
    strikeout = strikeout
  )
  for (nm in names(recycled_props)) {
    assert_length(recycled_props[[nm]], len = len, null.ok = TRUE, name = nm)
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
#' Vector values for style arguments (e.g., `color`, `background`, `fontsize`) are recycled over the selected cells in the order specified by the user in `i` and `j`, with `i` varying fastest. For example, `style_tt(x, j = c(3, 1), background = c("red", "blue"))` colors column 3 red and column 1 blue. When `i` is a logical matrix, values are recycled over the `TRUE` cells in column-major order.
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
#' @param line String determines if lines (rules or borders) should be drawn around the cell, row, or column. See `line_type` to draw dashed or dotted lines.
#' + "t": top
#' + "b": bottom
#' + "l": left
#' + "r": right
#' + Can be combined such as: "lbt" to draw borders at the left, bottom, and top.
#' @param line_color Color of the line. See the `color` argument for details.
#' @param line_width Width of the line in em units (default: 0.1).
#' @param line_type Style of the line: "solid" (default), "dashed", or "dotted". Ignored in Markdown and Word output, and by the Tabulator HTML engine, which always draw solid rules.
#' @param line_trim String specifying line trimming. Acceptable values: "l" (left), "r" (right), "lr" (both sides). When specified, shortens the lines by 0.8pt on the specified side(s). Default: NULL (no trimming).
#' @param finalize A function applied to the table object at the very end of table-building, for post-processing. For example, the function could use regular expressions to add LaTeX commands to the text version of the table hosted in `x@table_string`, or it could programmatically change the caption in `x@caption`.
#' @param output Apply styling only when the table is rendered in the specified format. A character vector of one or more of "latex", "html", "typst", or "markdown". If `NULL` (default), styling is applied regardless of the output format.
#' @param ... extra arguments are ignored
#' @return An object of class `tt` representing the table.
#' @template limitations_word_markdown
#' @export
#' @examplesIf knitr::is_html_output()
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
  line_type = NULL,
  line_trim = NULL,
  finalize = NULL,
  output = NULL,
  ...
) {

  
  assert_subset(output, c("latex", "html", "typst", "markdown"), null.ok = TRUE)

  # non-standard evaluation before anything else
  tmp <- nse_i_j(x, i_expr = substitute(i), j_expr = substitute(j), pf = parent.frame())
  list2env(tmp, environment())

  # evaluate arguments immediately, except i and j, to avoid scoping issues
  obj <- c(
    list(style_tt_lazy, x = quote(x), i = i, j = j),
    mget(STYLE_TT_ARGS, envir = environment()),
    list(...)
  )
  obj <- as.call(obj)
  attr(obj, "output") <- output

  x@lazy_style <- c(x@lazy_style, list(obj))
  return(x)
}


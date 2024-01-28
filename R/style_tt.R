#' Style a Tiny Table in either LaTeX or HTML format
#'
#' @details
#' This function applies styling to a table created by `tt()`. It allows customization of text style (bold, italic, monospace), text and background colors, font size, cell width, text alignment, column span, and indentation. The function supports both LaTeX (tabularray) and HTML (bootstrap) formats.
#'
#' Note that Markdown and Word formats are limited to these styles: italic, bold, strikeout.
#'
#' @param x A table object created by `tt()`.
#' @param i Row indices where the styling should be applied. Can be a single value or a vector. `i=0` is the header, and negative values are higher level headers. If `colspan` is used, `i` must be of length 1.
#' @param j Column indices where the styling should be applied. Can be a single value, a vector, or a Perl-style regular expression applied to column names of the original data frame. If `colspan` is used, `j` must be of length 1.
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
#' @param width Width of the cell or column. Can be `NULL` for default width.
#' @param fontsize Integer Font size in pt units.
#' @param align A single character or a string with a number of characters equal to the number of columns in `j`. Valid characters include 'c' (center), 'l' (left), or 'r' (right).
#' @param colspan Number of columns a cell should span. Can only be used if both `i` and `j` are of length 1. Must be an integer greater than 1.
#' @param indent Text indentation in em units. Positive values only.
#' @param line String determines if solid lines (rules or borders) should be drawn around the cell, row, or column. 
#' + "t": top
#' + "b": bottom
#' + "l": left
#' + "r": right
#' + Can be combined such as: "lbt" to draw borders at the left, bottom, and top.
#' @param line_color Color of the line. See the `color` argument for details.
#' @param line_width Width of the line in em units (default: 0.1).
#' @param bootstrap_class String. A Bootstrap table class such as `"table"`, `"table table-dark"` or `"table table-dark table-hover"`. See the bootstrap documentation. 
#' @param bootstrap_css A vector of CSS style declarations to be applied (ex: `"font-weight: bold"`). Each element corresponds to a cell defined by `i` and `j`.
#' @param bootstrap_css_rule A string with complete CSS rules that apply to the table class specified using the `theme` argument of the `tt()` function.
#' @param tabularray_inner A string that specifies the "inner" settings of a tabularray LaTeX table. 
#' @param tabularray_outer A string that specifies the "outer" settings of a tabularray LaTeX table.
#' @param ... extra arguments are ignored
#' @return An object of class `tt` representing the table.
#' @template latex_preamble
#' @export
#' @examples
#' library(tinytable)
#' x <- mtcars[1:5, 1:5]
#' tab <- tt(x)
#' tab <- style_tt(tab, j = 1:5, align = "lcccr")
#' tab <- style_tt(tab, i = 2:3, background = "black", color = "orange", bold = TRUE)
#' tab
#'
style_tt <- function (x,
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
                      width = NULL,
                      align = NULL,
                      colspan = NULL,
                      indent = 0,
                      line = NULL,
                      line_color = "black",
                      line_width = 0.1,
                      tabularray_inner = NULL,
                      tabularray_outer = NULL,
                      bootstrap_class = NULL,
                      bootstrap_css = NULL,
                      bootstrap_css_rule = NULL,
                      ...) {

  out <- x
  cal <- call("style_tt_lazy",
  # out <- style_tt_lazy(
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
              width = width,
              align = align,
              colspan = colspan,
              indent = indent,
              line = line,
              line_color = line_color,
              line_width = line_width,
              tabularray_inner = tabularray_inner,
              tabularray_outer = tabularray_outer,
              bootstrap_class = bootstrap_class,
              bootstrap_css = bootstrap_css,
              bootstrap_css_rule = bootstrap_css_rule)

  if (isTRUE(list(...)[["tt_build_now"]])) {
    out <- eval(cal)
  } else {
    out <- meta(out, "lazy_style", c(meta(out)$lazy_style, list(cal)))
  }

  return(out)
}



style_tt_lazy <- function (x,
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
                           width,
                           align,
                           colspan,
                           indent,
                           line,
                           line_color,
                           line_width,
                           tabularray_inner,
                           tabularray_outer,
                           bootstrap_class,
                           bootstrap_css,
                           bootstrap_css_rule) {

  # sanity x
  if (is.null(meta(x))) stop("`x` must be generated by `tinytable::tt()`.", call. = FALSE)
  if (!isTRUE(meta(x)$output %in% c("html", "latex", "markdown", "typst"))) return(x)

  out <- x

  # j is a regular expression
  if (is.character(j) && !is.null(meta(x, "colnames"))) {
    j <- grep(j, meta(x, "colnames"), perl = TRUE)
  }

  # align can be "c" or "clrrlc"takes many possible values
  assert_string(align, null.ok = TRUE)

  if (!is.null(align)) {
    if (nchar(align) == 1) {
      assert_choice(align, c("c", "l", "r"))
    } else {
      align_split <- strsplit(align, split = "")[[1]]
      for (align_character in align_split){
        assert_choice(align_character, c("c", "l", "r"))
      }
      if (is.null(j)) {
        msg <- "Please specify the `j` argument."
        stop(msg, call. = FALSE)
      }
    }
  }
    
  nalign <- if (is.null(j)) meta(x, "ncols") else length(j)
  if (!is.null(align)) {
    align <- strsplit(align, split = "")[[1]]
    if (length(align) != 1 && length(align) != nalign) {
      msg <- sprintf("`align` must be a single character or a string of length %s.", nalign)
      stop(msg, call. = FALSE)
    }
    if (any(!align %in% c("c", "l", "r"))) {
      msg <- "`align` must be characters c, l, or r."
      stop(msg, call. = FALSE)
    }
  }

  assert_style_tt(
    x = out, i = i, j = j, bold = bold, italic = italic, monospace = monospace, underline = underline, strikeout = strikeout,
    color = color, background = background, fontsize = fontsize, width = width, align = align, colspan = colspan, indent = indent,
    line = line, line_color = line_color, line_width = line_width,
    tabularray_inner = tabularray_inner, tabularray_outer = tabularray_outer, bootstrap_css = bootstrap_css,
    bootstrap_css_rule = bootstrap_css_rule, bootstrap_class = bootstrap_class)

  out <- style_tabularray(
    x = out, i = i, j = j, bold = bold, italic = italic, monospace = monospace, underline = underline, strikeout = strikeout,
    color = color, background = background, fontsize = fontsize, width = width, align = align, colspan = colspan, indent = indent,
    tabularray_inner = tabularray_inner, tabularray_outer = tabularray_outer,
    line = line, line_color = line_color, line_width = line_width)

  out <- style_bootstrap(
    x = out, i = i, j = j, bold = bold, italic = italic, monospace = monospace, underline = underline, strikeout = strikeout,
    color = color, background = background, fontsize = fontsize, width = width, align = align, colspan = colspan, indent = indent,
    bootstrap_css = bootstrap_css, bootstrap_css_rule = bootstrap_css_rule, bootstrap_class = bootstrap_class,
    line = line, line_color = line_color, line_width = line_width)

  out <- style_grid(
    x = out, i = i, j = j, bold = bold, italic = italic, monospace = monospace, underline = underline, strikeout = strikeout,
    color = color, background = background, fontsize = fontsize, width = width, align = align, colspan = colspan, indent = indent,
    bootstrap_css = bootstrap_css, bootstrap_css_rule = bootstrap_css_rule,
    line = line, line_color = line_color, line_width = line_width)

  out <- style_typst(
    x = out, i = i, j = j, bold = bold, italic = italic, monospace = monospace, underline = underline, strikeout = strikeout,
    color = color, background = background, fontsize = fontsize, width = width, align = align, colspan = colspan, indent = indent,
    bootstrap_css = bootstrap_css, bootstrap_css_rule = bootstrap_css_rule,
    line = line, line_color = line_color, line_width = line_width)

  return(out)
}


assert_style_tt <- function (x,
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
                             width,
                             align,
                             colspan,
                             indent,
                             line,
                             line_color,
                             line_width,
                             tabularray_inner,
                             tabularray_outer,
                             bootstrap_class = NULL,
                             bootstrap_css = NULL,
                             bootstrap_css_rule = NULL) {

  m <- meta(x)

  assert_integerish(colspan, len = 1, lower = 1, null.ok = TRUE)
  assert_string(width, null.ok = TRUE)
  assert_numeric(indent, len = 1, lower = 0)
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
  assert_character(bootstrap_class, null.ok = TRUE)
  assert_character(bootstrap_css, null.ok = TRUE)
  assert_string(bootstrap_css_rule, null.ok = TRUE)

  if (is.character(line)) {
    line <- strsplit(line, split = "")[[1]]
    if (!all(line %in% c("t", "b", "l", "r"))) {
      msg <- "`line` must be a string of characters t, b, l, or r."
      stop(msg, call. = FALSE)
    }
  }

  ival <- if (is.null(i)) meta(x, "nrows") else i
  jval <- if (is.null(j)) meta(x, "ncols") else j

  # 1
  if (is.null(i) && is.null(j)) {
    assert_length(color, len = 1, null.ok = TRUE)
    assert_length(background, len = 1, null.ok = TRUE)
    assert_length(fontsize, len = 1, null.ok = TRUE)
    assert_length(bold, len = 1)
    assert_length(italic, len = 1)
    assert_length(monospace, len = 1)
    assert_length(underline, len = 1)
    assert_length(strikeout, len = 1)

  # 1 or #rows
  } else if (!is.null(i) && is.null(j)) {
    assert_length(color, len = c(1, length(ival)), null.ok = TRUE)
    assert_length(background, len = c(1, length(ival)), null.ok = TRUE)
    assert_length(fontsize, len = c(1, length(ival)), null.ok = TRUE)
    assert_length(bold, len = c(1, length(ival)))
    assert_length(italic, len = c(1, length(ival)))
    assert_length(monospace, len = c(1, length(ival)))
    assert_length(underline, len = c(1, length(ival)))
    assert_length(strikeout, len = c(1, length(ival)))

  # 1 or #cols
  } else if (is.null(i) && !is.null(j)) {
    assert_length(color, len = c(1, length(jval)), null.ok = TRUE)
    assert_length(background, len = c(1, length(jval)), null.ok = TRUE)
    assert_length(fontsize, len = c(1, length(jval)), null.ok = TRUE)
    assert_length(bold, len = c(1, length(jval)))
    assert_length(italic, len = c(1, length(jval)))
    assert_length(monospace, len = c(1, length(jval)))
    assert_length(underline, len = c(1, length(jval)))
    assert_length(strikeout, len = c(1, length(jval)))

  # 1 or #cells
  } else if (!is.null(i) && !is.null(j)) {
    assert_length(color, len = c(1, length(ival) * length(jval)), null.ok = TRUE)
    assert_length(background, len = c(1, length(ival) * length(jval)), null.ok = TRUE)
    assert_length(fontsize, len = c(1, length(ival) * length(jval)), null.ok = TRUE)
    assert_length(bold, len = c(1, length(ival) * length(jval)))
    assert_length(italic, len = c(1, length(ival) * length(jval)))
    assert_length(monospace, len = c(1, length(ival) * length(jval)))
    assert_length(underline, len = c(1, length(ival) * length(jval)))
    assert_length(strikeout, len = c(1, length(ival) * length(jval)))
  }
}



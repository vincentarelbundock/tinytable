#' Style a Tiny Table in either LaTeX or HTML format
#'
#' This function applies styling to a table created by `tt()`. It allows customization of text style (bold, italic, monospace), text and background colors, font size, cell width, text alignment, column span, and indentation. The function supports both LaTeX (tabularray) and HTML (bootstrap) formats.
#'
#' @param x A table object created by `tt()`. The function checks if it is a `tinytable_tabularray` or `tinytable_bootstrap` object.
#' @param i Row indices where the styling should be applied. Can be a single value or a vector. If `colspan` is used, `i` must be of length 1. When i=0, the header is styled.
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
#' @param fontsize Font size. Can be `NULL` for default size.
#' @param width Width of the cell or column. Can be `NULL` for default width.
#' @param fontsize Integer Font size in pt units.
#' @param align Text alignment within the cell. Options are 'c' (center), 'l' (left), or 'r' (right). Can be `NULL` for default alignment.
#' @param colspan Number of columns a cell should span. Can only be used if both `i` and `j` are of length 1. Must be an integer greater than 1.
#' @param indent Text indentation in em units. Positive values only.
#' @param bootstrap_css A vector of CSS style declarations to be applied (ex: `"font-weight: bold"`). Each element corresponds to a cell defined by `i` and `j`.
#' @param bootstrap_css_rule A string with complete CSS rules that apply to the table class specified using the `theme` argument of the `tt()` function.
#' @param tabularray_inner A string that specifies the "inner" settings of a tabularray LaTeX table. 
#' @param tabularray_outer A string that specifies the "outer" settings of a tabularray LaTeX table.
#' @return Returns a modified `tinytable` object with the applied styles.
#' @export
#' @template latex_preamble
style_tt <- function(
  x,
  i, j,
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
  tabularray_inner = NULL,
  tabularray_outer = NULL,
  bootstrap_css = NULL,
  bootstrap_css_rule = NULL) {
 
  # supported formats
  if (inherits(x, "tinytable_markdown")) {
    return(x)
  } else if (!inherits(x, "tinytable_tabularray") && !inherits(x, "tinytable_bootstrap")) {
    stop("`x` must be a table produced by `tt()`.", call. = FALSE)
  }

  out <- x

  if (!missing(i) && !missing(j)) {
    component <- "cell"
  } else if (missing(i) && !missing(j)) {
    component <- "col"
  } else {
    component <- "row"
  } 

  # j is a regular expression
  if (!missing(j) && is.character(j) && length(j) == 1 && is.character(attr(x, "tt_colnames"))) {
    j <- grep(j, attr(x, "tt_colnames"), perl = TRUE)
  }

  if (!is.null(colspan)) {
    if (missing(j) || missing(i) ||
        (!missing(i) && length(i) != 1) ||
        (!missing(j) && length(j) != 1)) {
      stop("`i` and `j` must be of length 1 when using `colspan`.", call. = FALSE)
    }
    assert_integerish(colspan, len = 1, lower = 1, upper = j + attr(x, "ncol"))
  }

  # fill missing indices

  if (missing(i)) i <- seq_len(attr(x, "nrow"))
  if (missing(j)) j <- seq_len(attr(x, "ncol"))
  assert_integerish(i, lower = 0, upper = attr(x, "nrow"))
  assert_integerish(j, lower = 1, upper = attr(x, "ncol"))
  # assert_string(background, null.ok = TRUE)
  assert_string(width, null.ok = TRUE)
  assert_choice(align, c("c", "l", "r"), null.ok = TRUE)
  assert_flag(bold)
  assert_flag(italic)
  assert_numeric(indent, len = 1, lower = 0)
  assert_integerish(fontsize, len = 1, null.ok = TRUE)
  assert_character(color, null.ok = TRUE)

  if (inherits(x, "tinytable_tabularray")) {
    if (component == "cell") {
      settings <- expand.grid(i = i, j = j)
    } else if (component == "row") {
      settings <- data.frame(i = i)
    } else if (component == "col") {
      settings <- data.frame(j = j)
    }
  } else {
    settings <- expand.grid(i = i, j = j)
  }
  settings$tabularray <- settings$bootstrap <- ""

  # do not style header by default. JS index starts at 0
  if (inherits(x, "tinytable_tabularray") && "i" %in% colnames(settings)) {
    settings$i <- settings$i + attr(x, "nhead")
  }

  if (isTRUE(bold)) settings$bootstrap <- sprintf("%s font-weight: bold;", settings$bootstrap)
  if (isTRUE(italic)) settings$bootstrap <- sprintf("%s font-style: italic;", settings$bootstrap)
  if (isTRUE(underline)) settings$bootstrap <- sprintf("%s text-decoration: underline;", settings$bootstrap)
  if (isTRUE(strikeout)) settings$bootstrap <- sprintf("%s text-decoration: line-through;", settings$bootstrap)
  if (isTRUE(monospace)) settings$bootstrap <- sprintf("%s font-family: monospace;", settings$bootstrap)
  if (is.numeric(fontsize)) settings$bootstrap <- sprintf("%s font-size: %s;", settings$bootstrap, paste0(1.33333 * fontsize, "px"))
  if (!is.null(align)) settings$bootstrap <- sprintf("%s text-align: %s;", settings$bootstrap, switch(align, r = "right", l = "left", c = "center"))
  if (!is.null(color)) settings$bootstrap <- sprintf("%s color: %s;", settings$bootstrap, color)
  if (!is.null(background)) settings$bootstrap <- sprintf("%s background-color: %s;", settings$bootstrap, background)
  if (!is.null(width)) settings$bootstrap <- sprintf("%s width: %s;", settings$bootstrap, width)

  if (is.numeric(fontsize)) {
    settings$tabularray <- sprintf("%s font=\\fontsize{%s}{%s}\\selectfont,", settings$tabularray, fontsize, fontsize + 2)
  }
  if (!is.null(align)) settings$tabularray <- sprintf("%s halign=%s,", settings$tabularray, align)
  if (!is.null(background)) settings$tabularray <- sprintf("%s bg=%s,", settings$tabularray, background)
  if (!is.null(width)) settings$tabularray <- sprintf("%s wd={%s},", settings$tabularray, width)
  if (indent > 0) settings$tabularary <- sprintf("%s preto={\\hspace{%sem}},", settings$tabularray, indent)

  # tabularray_cmd <- list(
  #   if (isTRUE(bold)) "\\bfseries"
  #   if (isTRUE(italic)) "\\textit{}"
  #   if (isTRUE(underline)) "\\tinytableTabularrayUnderline"
  #   if (isTRUE(strikeout)) "\\tinytableTabularrayStrikeout"
  #   if (isTRUE(monospace)) "\\texttt{}"
  # )
  #
  # tabularray_cmd <- do.call("cbind", tabularray_cmd)
  # if (is.null(tabularray_cmd)) {
  #   tabularray_cmd <- ""
  # } else {
  #   tabularray_cmd <- apply(tabularray_cmd, 1, paste, collapse = "")
  # }
  # if (tabularray_cmd != "") tabularray_cmd <- sprintf("cmd=%s,", tabularray_cmd)
  #
  # tabularray <- paste(tabularray, tabularray_cmd)

  # if (inherits(x, "tinytable_tabularray") && !is.null(color) && any(grepl("^#", color))) {
  #   color_latex <- color
  #   for (k in seq_along(color)) {
  #     if (grepl("^#", color[k])) {
  #       color_latex[k] <- gsub("^#", "c", color[k])
  #       out <- style_tabularray(out,
  #         body = sprintf(
  #           "\\tinytableDefineColor{%s}{HTML}{%s}",
  #           color_latex[k], sub("^#", "", color[k]))
  #       )
  #     }
  #   }
  # } else {
  #   color_latex <- color
  # }
  # vectorized settings with a unique entry
    # if (!is.null(color)) sprintf("%s fg=%s,", settings$tabularray, color_latex)
 #
  

  # Apply bootstrap styles
  for (k in seq_len(nrow(settings))) {
    out <- style_bootstrap(out, i = settings$i[k], j = settings$j[k], css = bootstrap_css)
    out <- style_bootstrap(out, i = settings$i[k], j = settings$j[k], css = settings$bootstrap[k])
  }

  # Apply tabularray styles
  span <- if (!is.null(colspan)) paste0("c=", colspan, ",") else ""

  if (inherits(x, "tinytable_tabularray")) {
    for (k in seq_len(nrow(settings))) {
      if (all(c("i", "j") %in% colnames(settings))) {
        # R is column-major
        spec <- sprintf("cell{%s}{%s}={%s}{%s},",
          # do not style header by default
          settings$i[k],
          settings$j[k],
          span,
          settings$tabularray[k])
      } else if ("i" %in% colnames(settings)) {
        spec <- sprintf("row{%s}={%s},",
          # do not style header by default
          settings$i[k],
          settings$tabularray[k])
      } else if ("j" %in% colnames(settings)) {
        spec <- sprintf("column{%s}={%s},",
          settings$j[k],
          settings$tabularray[k])
      }
      out <- style_tabularray(out, inner = spec)
    }
  }

  # Manual settings
  if (!is.null(tabularray_inner) || !is.null(tabularray_outer)) {
    out <- style_tabularray(out, inner = tabularray_inner, outer = tabularray_outer)
  }
  if (!is.null(bootstrap_css) || !is.null(bootstrap_css_rule)) {
    out <- style_bootstrap(out, i = i, j = j, css = bootstrap_css, css_rule = bootstrap_css_rule)
  }

  return(out)
}

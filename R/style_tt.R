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
style_tt <- function (x, i, j, bold = FALSE, italic = FALSE, monospace = FALSE, 
                      underline = FALSE, strikeout = FALSE, color = NULL, background = NULL, 
                      fontsize = NULL, width = NULL, align = NULL, colspan = NULL, 
                      indent = 0, tabularray_inner = NULL, tabularray_outer = NULL, 
                      bootstrap_css = NULL, bootstrap_css_rule = NULL) {

  if (inherits(x, "tinytable_markdown")) {
    return(x)
  } else if (!inherits(x, "tinytable_tabularray") && !inherits(x, "tinytable_bootstrap")) {
    stop("`x` must be a table produced by `tt()`.", call. = FALSE)
  }
  out <- x
  if (missing(i)) {
    ival <- seq_len(attr(x, "nrow"))
  } else {
    ival <- i
  }
  if (missing(j)) {
    jval <- seq_len(attr(x, "ncol"))
  } else {
    if (is.character(j) && length(j) == 1 && is.character(attr(x, "tt_colnames"))) {
      jval <- grep(j, attr(x, "tt_colnames"), perl = TRUE)
    } else {
      jval <- j
    }
  }
  ncells <- length(ival) * length(jval)
  ncols <- length(jval)
  nrows <- length(ival)

  assert_integerish(ival, lower = 0, upper = attr(x, "nrow"), name = "i")
  assert_integerish(jval, lower = 1, upper = attr(x, "ncol"), name = "j")
  assert_string(width, null.ok = TRUE)
  assert_choice(align, c("c", "l", "r"), null.ok = TRUE)
  assert_numeric(indent, len = 1, lower = 0)
  assert_character(background, null.ok = TRUE)
  assert_character(color, null.ok = TRUE)
  assert_integerish(fontsize, null.ok = TRUE)
  assert_logical(bold)
  assert_logical(italic)
  assert_logical(monospace)
  assert_logical(underline)
  assert_logical(strikeout)
  if (missing(i) && missing(j)) {
    assert_length(color, len = 1, null.ok = TRUE)
    assert_length(background, len = 1, null.ok = TRUE)
    assert_length(fontsize, len = 1, null.ok = TRUE)
    assert_length(bold, len = 1)
    assert_length(italic, len = 1)
    assert_length(monospace, len = 1)
    assert_length(underline, len = 1)
    assert_length(strikeout, len = 1)
  } else if (!missing(i) && missing(j)) {
    assert_length(color, len = c(1, nrows), null.ok = TRUE)
    assert_length(background, len = c(1, nrows), null.ok = TRUE)
    assert_length(fontsize, len = c(1, nrows), null.ok = TRUE)
    assert_length(bold, len = c(1, nrows))
    assert_length(italic, len = c(1, nrows))
    assert_length(monospace, len = c(1, nrows))
    assert_length(underline, len = c(1, nrows))
    assert_length(strikeout, len = c(1, nrows))
  } else if (missing(i) && !missing(j)) {
    assert_length(color, len = c(1, ncols), null.ok = TRUE)
    assert_length(background, len = c(1, ncols), null.ok = TRUE)
    assert_length(fontsize, len = c(1, ncols), null.ok = TRUE)
    assert_length(bold, len = c(1, ncols))
    assert_length(italic, len = c(1, ncols))
    assert_length(monospace, len = c(1, ncols))
    assert_length(underline, len = c(1, ncols))
    assert_length(strikeout, len = c(1, ncols))
  } else if (!missing(i) && !missing(j)) {
    assert_length(color, len = c(1, ncells), null.ok = TRUE)
    assert_length(background, len = c(1, ncells), null.ok = TRUE)
    assert_length(fontsize, len = c(1, ncells), null.ok = TRUE)
    assert_length(bold, len = c(1, ncells))
    assert_length(italic, len = c(1, ncells))
    assert_length(monospace, len = c(1, ncells))
    assert_length(underline, len = c(1, ncells))
    assert_length(strikeout, len = c(1, ncells))
  }

  if (!is.null(colspan)) {
    if (missing(j) || missing(i) || (!missing(i) && length(ival) != 1) || (!missing(j) && length(jval) != 1)) {
      stop("`i` and `j` must be of length 1 when using `colspan`.", call. = FALSE)
    }
    assert_integerish(colspan, len = 1, lower = 1, upper = jval + attr(x, "ncol"))
  }

  settings <- expand.grid(i = ival, j = jval)
  if (missing(i) && !missing(j)) {
    settings <- settings[order(settings$i, settings$j), ]
  }

  if (inherits(x, "tinytable_tabularray")) {
    if (missing(i) && missing(j)) {
      settings <- unique(settings[, "i", drop = FALSE])
    }
    else if (missing(i)) {
      settings <- unique(settings[, "j", drop = FALSE])
    }
    else if (missing(j)) {
      settings <- unique(settings[, "i", drop = FALSE])
    }
  }

  settings$tabularray <- settings$bootstrap <- ""

  # do not style the header. JS 0-indexing
  if (inherits(x, "tinytable_tabularray") && "i" %in% colnames(settings)) {
    settings$i <- settings$i + attr(x, "nhead")
  }

  # settings have a different size for latex, so bootstrap breaks
  if (inherits(x, "tinytable_bootstrap")) {
    vectorize_bootstrap <- function(setting, userinput, string) {
      if (is.null(userinput)) {
        return(setting)
      }
      if (check_flag(userinput) && isTRUE(userinput))  {
        return(paste(setting, string))
      }
      if (is.logical(userinput)) {
        return(ifelse(userinput, paste(setting, string), setting))
      }
      return(paste(setting, sprintf(string, userinput)))
    }

    if (!is.null(align)) {
      align_bootstrap <- ifelse(align == "c", "center", align)
      align_bootstrap <- ifelse(align == "l", "left", align_bootstrap)
      align_bootstrap <- ifelse(align == "r", "right", align_bootstrap)
    } else {
      align_bootstrap <- align
    }

    if (!is.null(fontsize)) {
      fontsize_bootstrap <- sprintf("%spx", (1 + 1 / 3) * fontsize)
    } else {
      fontsize_bootstrap <- fontsize
    }

    settings$bootstrap <- ""
    settings$bootstrap <- vectorize_bootstrap(settings$bootstrap, bold, "font-weight: bold;")
    settings$bootstrap <- vectorize_bootstrap(settings$bootstrap, italic, "font-style: italic;")
    settings$bootstrap <- vectorize_bootstrap(settings$bootstrap, underline, "text-decoration: underline;")
    settings$bootstrap <- vectorize_bootstrap(settings$bootstrap, strikeout, "text-decoration: line-through;")
    settings$bootstrap <- vectorize_bootstrap(settings$bootstrap, monospace, "font-family: monospace;")
    settings$bootstrap <- vectorize_bootstrap(settings$bootstrap, fontsize_bootstrap, "font-size: %s;")
    settings$bootstrap <- vectorize_bootstrap(settings$bootstrap, align_bootstrap, "text-align: %s;")
    settings$bootstrap <- vectorize_bootstrap(settings$bootstrap, color, "color: %s;")
    settings$bootstrap <- vectorize_bootstrap(settings$bootstrap, background, "background-color: %s;")
    settings$bootstrap <- vectorize_bootstrap(settings$bootstrap, width, "width: %s;")
  }

  # convert to tabularray now that we've filled the bootstrap settings
  if (is.numeric(fontsize)) settings$tabularray <- sprintf("%s font=\\fontsize{%s}{%s}\\selectfont,", settings$tabularray, fontsize, fontsize + 2) 
  if (!is.null(align)) settings$tabularray <- sprintf("%s halign=%s,", settings$tabularray, align)
  if (!is.null(width)) settings$tabularray <- sprintf("%s wd={%s},", settings$tabularray, width)
  if (indent > 0) settings$tabularary <- sprintf("%s preto={\\hspace{%sem}},", settings$tabularray, indent)

  vectorize_tabularray <- function(z) {
    if (is.null(z)) {
      return(rep(FALSE, nrow(settings)))
    }
    if (check_flag(z))  {
      return(rep(z, nrow(settings)))
    }
    return(z)
  }

  bold <- vectorize_tabularray(bold)
  italic <- vectorize_tabularray(italic)
  underline <- vectorize_tabularray(underline)
  strikeout <- vectorize_tabularray(strikeout)
  monospace <- vectorize_tabularray(monospace)
  cmd <- rep("", nrow(settings))
  cmd <- ifelse(bold, paste0(cmd, "\\bfseries"), cmd)
  cmd <- ifelse(italic, paste0(cmd, "\\textit"), cmd)
  cmd <- ifelse(underline, paste0(cmd, "\\tinytableTabularrayUnderline"), cmd)
  cmd <- ifelse(strikeout, paste0(cmd, "\\tinytableTabularrayStrikeout"), cmd)
  cmd <- ifelse(monospace, paste0(cmd, "\\texttt"), cmd)
  settings$tabularray <- sprintf("%s, cmd=%s,", settings$tabularray, cmd)

  # hex must be treated differently in LaTeX
  cols <- c(color, background)
  if (!is.null(cols)) {
    hex <- cols[grepl("^#", cols)]
    for (h in hex) {
      out <- style_tabularray(out, body = sprintf("\\tinytableDefineColor{%s}{HTML}{%s}", sub("^#", "c", h), sub("^#", "", h)))
    }
  }
  if (!is.null(background)) {
    settings$tabularray <- sprintf("%s bg=%s,", settings$tabularray, sub("^#", "c", background))
  }
  if (!is.null(color)) {
    settings$tabularray <- sprintf("%s fg=%s,", settings$tabularray, sub("^#", "c", color))
  }

  settings$tabularray <- trimws(gsub("cmd=,", "", settings$tabularray))
  settings$tabularray <- trimws(gsub("\\s+", "", settings$tabularray))
  settings$tabularray <- trimws(gsub(",+", ",", settings$tabularray))

  for (k in seq_len(nrow(settings))) {
    out <- style_bootstrap(out, i = settings$i[k], j = settings$j[k], css = bootstrap_css)
    out <- style_bootstrap(out, i = settings$i[k], j = settings$j[k], css = settings$bootstrap[k])
  }

  span <- if (!is.null(colspan)) paste0("c=", colspan, ",") else ""
  if (inherits(x, "tinytable_tabularray")) {
    for (k in seq_len(nrow(settings))) {
      if (all(c("i", "j") %in% colnames(settings))) {
        spec <- sprintf("cell{%s}{%s}={%s}{%s},", settings$i[k], settings$j[k], span, settings$tabularray[k])
      }
      else if ("i" %in% colnames(settings)) {
        spec <- sprintf("row{%s}={%s},", settings$i[k], settings$tabularray[k])
      }
      else if ("j" %in% colnames(settings)) {
        spec <- sprintf("column{%s}={%s},", settings$j[k], settings$tabularray[k])
      }
      out <- style_tabularray(out, inner = spec)
    }
  }
  if (!is.null(tabularray_inner) || !is.null(tabularray_outer)) {
    out <- style_tabularray(out, inner = tabularray_inner, outer = tabularray_outer)
  }
  if (!is.null(bootstrap_css) || !is.null(bootstrap_css_rule)) {
    out <- style_bootstrap(out, i = ival, j = jval, css = bootstrap_css, css_rule = bootstrap_css_rule)
  }
  return(out)
}


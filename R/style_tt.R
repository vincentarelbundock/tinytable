#' Style a Tiny Table in either LaTeX or HTML format
#'
#' This function applies styling to a table created by `tt()`. It allows customization of text style (bold, italic, monospace), text and background colors, font size, cell width, text alignment, column span, and indentation. The function supports both LaTeX (tabularray) and HTML (bootstrap) formats.
#'
#' @param x A table object created by `tt()`. The function checks if it is a `tinytable_tabularray` or `tinytable_bootstrap` object.
#' @param i Row index or indices where the styling should be applied. Can be a single value or a vector. If `colspan` is used, `i` must be of length 1.
#' @param j Column index or indices where the styling should be applied. Can be a single value or a vector. If `colspan` is used, `j` must be of length 1.
#' @param bold Logical; if `TRUE`, text is styled in bold.
#' @param italic Logical; if `TRUE`, text is styled in italic.
#' @param monospace Logical; if `TRUE`, text is styled in monospace font.
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
#' @param align Text alignment within the cell. Options are 'c' (center), 'l' (left), or 'r' (right). Can be `NULL` for default alignment.
#' @param colspan Number of columns a cell should span. Can only be used if both `i` and `j` are of length 1. Must be an integer greater than 1.
#' @param indent Text indentation in em units. Positive values only.
#' @return Returns a modified `tinytable` object with the applied styles.
#' @export
style_tt <- function(
  x,
  i, j,
  bold = FALSE,
  italic = FALSE,
  monospace = FALSE,
  color = NULL,
  background = NULL,
  fontsize = NULL,
  width = NULL,
  align = NULL,
  colspan = NULL,
  indent = 0) {
 
  out <- x

  # no markdown styling
  if (inherits(x, "tinytable_markdown")) return(x)

  if (!inherits(x, "tinytable_tabularray") && !inherits(x, "tinytable_bootstrap")) {
    stop("`x` must be a table produced by `tt()`.", call. = FALSE)
  }

  nhead <- attr(x, "nhead")

  assert_string(color, null.ok = TRUE)
  assert_string(background, null.ok = TRUE)
  assert_string(fontsize, null.ok = TRUE)
  assert_string(width, null.ok = TRUE)
  assert_choice(align, c("c", "l", "r"), null.ok = TRUE)
  assert_flag(bold)
  assert_flag(italic)
  assert_numeric(indent, len = 1, lower = 0)

  if (!is.null(colspan)) {
    if (missing(j) || missing(i) ||
        (!missing(i) && length(i) != 1) ||
        (!missing(j) && length(j) != 1)) {
      stop("`i` and `j` must be of length 1 when using `colspan`.", call. = FALSE)
    }
    assert_integerish(colspan, len = 1, lower = 2, upper = j + attr(x, "ncol"))
  }


  arguments <- list()

  if (isTRUE(bold)) {
    arguments$bold <- list(
      tabularray = "cmd=\\bfseries",
      bootstrap = "font-weight: bold"
    )
  } 
  if (isTRUE(italic)) {
    arguments$italic <- list(
      tabularray = "cmd=\\textit",
      bootstrap = "font-style: italic"
    )
  } 
  if (isTRUE(monospace)) {
    arguments$monospace <- list(
      tabularray = "cmd=\\texttt",
      bootstrap = "font-family: monospace"
    )
  }
  if (!is.null(align)) {
    arguments$align <- list(
      tabularray = sprintf("halign=%s", align),
      bootstrap = paste("text-align:", switch(align, r = "right", l = "left", c = "center"))
    )
  }
  if (!is.null(colspan)) {
    arguments$colspan <- list(
      tabularray = paste0("c=", colspan),
      bootstrap = ""
    )
  }
  if (!is.null(color)) {
    if (inherits(x, "tinytable_tabularray") && isTRUE(grepl("^#", color))) {
      color_latex <- sub("^#", "c", color)
      out <- style_tabularray(out,
        body = sprintf(
          "\\tinytableDefineColor{%s}{HTML}{%s}",
          color_latex, sub("^#", "", color))
      )
    } else {
      color_latex <- color
    }
    arguments$color <- list(
      tabularray = paste0("fg=", color_latex),
      bootstrap = paste0("color: ", color)
    )
  }
  if (!is.null(background)) {
    arguments$background <- list(
      tabularray = paste0("bg=", background),
      bootstrap = paste0("background-color: ", background)
    )
  }
  if (!is.null(fontsize)) {
    arguments$fontsize <- list(
      bootstrap = paste("font-size:", fontsize)
    )
  }
  if (!is.null(width)) {
    arguments$width <- list(
      tabularray = sprintf("wd={%s}", width),
      bootstrap = paste("width:%s", width)
    )
  }
  if (indent > 0) {
    arguments$indent <- list(
      tabularray = sprintf("preto={\\hspace{%sem}}", indent),
      bootstrap = sprintf("text-indent: %sem", indent)
    )
  }
  if (inherits(x, "tinytable_bootstrap")) {
    css <- sapply(arguments, function(x) x[["bootstrap"]])
    css <- paste(css, collapse = "; ")
    out <- style_bootstrap(out, i, j, css, colspan = colspan)
    return(out)
  }


  tabularray <- sapply(arguments, function(x) x[["tabularray"]])
  # important for things like colspan
  tabularray <- Filter(function(x) !is.null(x), tabularray)

 
  if (any(c("colspan", "rowspan") %in% names(tabularray))) {
    span <- tabularray[names(tabularray) %in% c("colspan", "rowspan")]
    span <- paste(span, collapse = ",")
    tabularray <- tabularray[!names(tabularray) %in% c("colspan", "rowspan")]
  } else {
    span <- ""
  }

  tabularray <- paste0(paste(tabularray, collapse = ","), ",")

  if (inherits(x, "tinytable_tabularray")) {
    # specified columns or all cells
    if (missing(i)) {
      if (missing(j)) {
        j <- seq_len(attr(x, "ncol"))
      }
      colspec <- sprintf("column{%s}={%s},", paste(j, collapse = ","), tabularray)
      out <- style_tabularray(out, inner = colspec)

    # specified rows
    } else if (missing(j)) {
      # do not style header by default
      rowspec <- sprintf("row{%s}={%s},", paste(i + nhead, collapse = ","), tabularray)
      out <- style_tabularray(out, inner = rowspec)

    # specified cells
    } else {
      cellspec <- sprintf("cell{%s}{%s}={%s}{%s},",
                          # do not style header by default
                          paste(i + nhead, collapse = ","),
                          paste(j, collapse = ","),
                          span,
                          tabularray)
      out <- style_tabularray(out, inner = cellspec)
    }
  }

  return(out)
}

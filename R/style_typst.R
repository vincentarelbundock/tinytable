# TODO: 
# colspan, 
# indent, 
# align,
# alignv,



#' Internal styling function
#'
#' @inheritParams style_tt
#' @keywords internal
#' @noRd
setMethod(
  f = "style_eval",
  signature = "tinytable_typst",
  definition = function(x,
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
                        line = NULL,
                        line_color = "black",
                        line_width = .1,
                        colspan = NULL,
                        indent = 0,
                        midrule = FALSE, # undocumented, only used by `group_tt()`
                        ...) {

  out <- x@table_string

  text_style_flag <- isTRUE(bold) || isTRUE(italic) || isTRUE(monospace) || isTRUE(underline) || isTRUE(strikeout) || !is.null(color) || !is.null(fontsize)
  fill_style_flag <- !is.null(background)


  ival <- sanitize_i(i, x)
  jval <- sanitize_j(j, x)
  inull <- isTRUE(attr(ival, "null"))
  jnull <- isTRUE(attr(jval, "null"))

  # # only columns means we also want to style headers
  # if (inull && !jnull) {
  #   ival <- c(-1 * rev(seq_len(x@nhead) - 1), ival)
  # }

  # 0- & header-indexing
  jval <- jval - 1
  ival <- ival - 1 + x@nhead

  # browser()

  if (isTRUE(grepl("^#", background))) background <- sprintf('rgb("%s")', background)
  if (isTRUE(grepl("^#", line_color))) line_color <- sprintf('rgb("%s")', line_color)

  if (is.null(color)) {
    color <- "black"
  } else if (isTRUE(grepl("^#", color))) {
    color <- sprintf('rgb("%s")', color)
  }

  if (is.null(fontsize)) {
    fontsize <- "1em"
  } else {
    fontsize <- sprintf("%sem", fontsize)
  }

  if (text_style_flag) {
    if (length(color) == 1) color <- rep(color, length(ival) * length(jval))
    if (length(underline) == 1) underline <- rep(underline, length(ival) * length(jval))
    if (length(italic) == 1) italic <- rep(italic, length(ival) * length(jval))
    if (length(bold) == 1) bold <- rep(bold, length(ival) * length(jval))
    if (length(monospace) == 1) monospace <- rep(monospace, length(ival) * length(jval))
    if (length(strikeout) == 1) strikeout <- rep(strikeout, length(ival) * length(jval))
    if (length(fontsize) == 1) fontsize <- rep(fontsize, length(ival) * length(jval))
    counter <- 0
    for (k in ival) {
      for (w in jval) {
        counter <- counter + 1
        style <- sprintf(
          "    (y: %s, x: %s, color: %s, underline: %s, italic: %s, bold: %s, mono: %s, strikeout: %s, fontsize: %s),",
          k,
          w,
          color[counter],
          tolower(underline[counter]),
          tolower(italic[counter]),
          tolower(bold[counter]),
          tolower(monospace[counter]),
          tolower(strikeout[counter]),
          fontsize[counter]
        )
        out <- lines_insert(out, style, "tinytable cell style after", "after")
      }
    }
  }

  if (fill_style_flag) {
    if (length(background) == 1) background <- rep(background, length(ival) * length(jval))
    counter <- 0
    for (k in ival) {
      for (w in jval) {
        counter <- counter + 1
        fill <- sprintf(
          "    (y: %s, x: %s, fill: %s),",
          k,
          w,
          background[counter])
        out <- lines_insert(out, fill, "tinytable cell fill after", "after")
      }
    }
  }

  # align
  if (!is.null(align)) {
    if (!length(align) %in% c(1, length(jval))) {
      stop("Length of `j` must be 1 or equal to the length of `align`.", call. = FALSE)
    }
    align <- sapply(align,
      switch,
      c = "center",
      d = "center",
      r = "right",
      l = "left")
    align <- sprintf("align: (%s),", paste(align, collapse = ", "))
    out <- lines_insert(out, align, "tinytable table start", "after")
  }


  # Lines are not part of cellspec/rowspec/columnspec. Do this separately.
  if (!is.null(line)) {
    iline <- NULL
    if (grepl("b", line)) iline <- c(iline, ival + 1) # -1 for 0-indexing
    if (grepl("t", line)) iline <- c(iline, ival)
    iline <- unique(iline)
    for (i in iline) {
      # TODO: `expand` in #tablex does not seem available in #table
      if (midrule) {
        tmp <- "table.hline(y: %s, start: %s, end: %s, stroke: %sem + %s),"
      } else {
        tmp <- "table.hline(y: %s, start: %s, end: %s, stroke: %sem + %s),"
      }
      tmp <- sprintf(tmp,
                     i,
                     min(jval),
                     max(jval) + 1,
                     line_width,
                     line_color)
      out <- lines_insert(out, tmp, "tinytable lines after", "after")
      }

    jline <- NULL
    if (grepl("r", line)) jline <- c(jline, jval + 1)
    if (grepl("l", line)) jline <- c(jline, jval)
    jline <- unique(jline)
    for (j in jline) {
      tmp <- sprintf(
        "table.vline(x: %s, start: %s, end: %s, stroke: %sem + %s),",
        j,
        min(ival),
        max(ival)+1,
        line_width,
        line_color)
      out <- lines_insert(out, tmp, "tinytable lines after", "after")
    }

  }

  x@table_string <- out

  return(x)
})

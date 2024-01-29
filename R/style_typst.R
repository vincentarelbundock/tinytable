#' Internal styling function
#'
#' @inheritParams style_tt
#' @keywords internal
#' @noRd
style_typst <- function(x,
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
                        line = NULL,
                        line_color = "black",
                        line_width = .1,
                        colspan = NULL,
                        indent = 0,
                        ...) {

  if (meta(x, "output") != "typst") return(x)

  out <- x

  ival <- if (is.null(i)) seq_len(meta(x, "nrows")) else i
  jval <- if (is.null(j)) seq_len(meta(x, "ncols")) else j

  # only columns means we also want to style headers
  if (is.null(i) && !is.null(j)) {
    ival <- c(-1 * rev(seq_len(meta(x)$nhead) - 1), ival)
  }

  # 0- & header-indexing
  jval <- jval - 1
  ival <- ival - 1 + meta(x, "nhead")

  if (isTRUE(grepl("^#", color))) color <- sprintf('rgb("%s")', color)
  if (isTRUE(grepl("^#", background))) background <- sprintf('rgb("%s")', background)
  if (isTRUE(grepl("^#", line_color))) line_color <- sprintf('rgb("%s")', line_color)

  style <- ""

  if (!is.null(fontsize)) {
    tmp <- sprintf("if (i.contains(cell.y) and j.contains(cell.x)) { cell.content = { set text(%sem); cell.content } };", fontsize)
    style <- paste0(style, "\n", tmp)
  }

  if (isTRUE(monospace)) {
    tmp <- "if (i.contains(cell.y) and j.contains(cell.x)) { cell.content = math.mono(cell.content) };"
    style <- paste0(style, "\n", tmp)
  }

  if (isTRUE(italic)) {
    tmp <- "if (i.contains(cell.y) and j.contains(cell.x)) { cell.content = emph(cell.content) };"
    style <- paste0(style, "\n", tmp)
  }

  if (isTRUE(bold)) {
    tmp <- "if (i.contains(cell.y) and j.contains(cell.x)) { cell.content = strong(cell.content) };"
    style <- paste0(style, "\n", tmp)
  }

  if (isTRUE(underline)) {
    tmp <- "if (i.contains(cell.y) and j.contains(cell.x)) { cell.content = underline(cell.content) };"
    style <- paste0(style, "\n", tmp)
  }

  if (isTRUE(strikeout)) {
    tmp <- "if (i.contains(cell.y) and j.contains(cell.x)) { cell.content = strike(cell.content) };"
    style <- paste0(style, "\n", tmp)
  }

  if (!is.null(background)) {
    tmp <- sprintf("if (i.contains(cell.y) and j.contains(cell.x)) { cell.fill = %s };", background)
    style <- paste0(style, "\n", tmp)
  }

  if (!is.null(color)) {
    tmp <- sprintf("if (i.contains(cell.y) and j.contains(cell.x)) { cell.content = { set text(%s); cell.content } };", color)
    style <- paste0(style, "\n", tmp)
  }


  if (style != "") {
    idx <- sprintf(
      "let i = (%s,);
let j = (%s,);",
      paste(ival, collapse = ","),
      paste(jval, collapse = ","))
    style <- paste0(idx, "\n", style)
  }

  out <- typst_insert(out, style, type = "style")

  # align
  if (!is.null(align)) {
    for (idx in seq_along(jval)) {
      k <- switch(
        align[idx],
        c = "center",
        r = "right",
        l = "left"
      )
      tmp <- sprintf("if (cell.x == %s) { cell.align = %s };", jval[idx], k)
      out <- typst_insert(out, tmp, type = "style")
    }
  }


  # Lines are not part of cellspec/rowspec/columnspec. Do this separately.
  if (!is.null(line)) {
    iline <- NULL
    if (grepl("b", line)) iline <- c(iline, ival + 1) # -1 for 0-indexing
    if (grepl("t", line)) iline <- c(iline, ival)
    iline <- unique(iline)
    for (i in iline) {
      tmp <- sprintf(
        "hlinex(y: %s, start: %s, end: %s, stroke: %sem + %s),",
        i,
        min(jval),
        max(jval) + 1,
        line_width,
        line_color)
      out <- typst_insert(out, tmp, type = "lines")
    }

    jline <- NULL
    if (grepl("r", line)) jline <- c(jline, jval + 1)
    if (grepl("l", line)) jline <- c(jline, jval)
    jline <- unique(jline)
    for (j in jline) {
      tmp <- sprintf(
        "vlinex(x: %s, start: %s, end: %s, stroke: %sem + %s),",
        j,
        min(ival),
        max(ival),
        line_width,
        line_color)
      out <- typst_insert(out, tmp, type = "lines")
    }

  }


  return(out)
}
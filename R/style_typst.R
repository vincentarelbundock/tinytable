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
                        line_width = 0.1,
                        colspan = NULL,
                        indent = 0,
                        midrule = FALSE, # undocumented, only used by `group_tt()`
                        ...) {
    out <- x@table_string

    text_style_flag <- isTRUE(bold) || isTRUE(italic) || isTRUE(monospace) || isTRUE(underline) || isTRUE(strikeout) || !is.null(color) || !is.null(fontsize) || indent > 0
    fill_style_flag <- !is.null(background)

    # gutters are used for group_tt(j) but look ugly with cell fill
    if (fill_style_flag) {
      x <- style_tt(x, finalize = function(x) {
        x@table_string <- lines_drop(
          x@table_string,
          "column-gutter:",
          fixed = TRUE)
        return(x)
      })
    }

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
      indent_value <- if (indent > 0) paste0(indent, "em") else "false"
      counter <- 0
      for (k in ival) {
        for (w in jval) {
          counter <- counter + 1
          style <- sprintf(
            "    (y: %s, x: %s, color: %s, underline: %s, italic: %s, bold: %s, mono: %s, strikeout: %s, fontsize: %s, indent: %s),",
            k,
            w,
            color[counter],
            tolower(underline[counter]),
            tolower(italic[counter]),
            tolower(bold[counter]),
            tolower(monospace[counter]),
            tolower(strikeout[counter]),
            fontsize[counter],
            indent_value
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

    if (!is.null(align)) {
        align_value <- sapply(align,
            switch,
            c = "center",
            d = "center",
            r = "right",
            l = "left")
        # reset defaults for all columns
        if (is.null(i) && length(align_value) == length(jval) && length(align_value) == ncol(x)) {
            align_default <- sprintf(
                "  #let align-default-array = ( %s ,) // tinytable align-default-array here", 
                paste(align_value, collapse = ", "))
            out <- lines_drop(out, "// tinytable align-default-array here", fixed = TRUE) 
            out <- lines_insert(out, align_default, "tinytable align-default-array after", "after")
        } else if (length(align_value) %in% c(1, length(ival) * length(jval))) {
            if (length(align_value) == 1) align_value <- rep(align_value, length(ival) * length(jval))
            counter <- 0
            for (k in ival) {
                for (w in jval) {
                    counter <- counter + 1
                    fill <- sprintf(
                        "    (y: %s, x: %s, align: %s),",
                        k,
                        w,
                        align_value[counter])
                    out <- lines_insert(out, fill, "tinytable cell align after", "after")
                }
            }
        } else {
            stop("Wrong number of elements in `align` argument.", call. = FALSE)
        }
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
        tmp <- sprintf(
          tmp,
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
          max(ival) + 1,
          line_width,
          line_color)
        out <- lines_insert(out, tmp, "tinytable lines after", "after")
      }
    }

    x@table_string <- out

    return(x)
  })

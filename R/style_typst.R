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

    # i is a logical matrix mask
    if (is.matrix(i) && is.logical(i) && nrow(i) == nrow(x) && ncol(i) == ncol(x)) {
      assert_null(j)
      settings <- which(i == TRUE, arr.ind = TRUE)
      settings <- stats::setNames(data.frame(settings), c("i", "j"))
      jval <- NULL
    } else {
      ival <- sanitize_i(i, x)
      jval <- sanitize_j(j, x)
      # order may be important for recycling
      settings <- expand.grid(i = ival, j = jval, tabularray = "")
      if (is.null(i) && !is.null(j)) {
        settings <- settings[order(settings$i, settings$j), ]
      }
    }

    # 0- & header-indexing
    settings$j <- settings$j - 1
    if (x@nhead > 0) {
      settings$i <- settings$i - 1 + x@nhead
    }

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
      if (length(color) == 1) color <- rep(color, nrow(settings))
      if (length(underline) == 1) underline <- rep(underline, nrow(settings))
      if (length(italic) == 1) italic <- rep(italic, nrow(settings))
      if (length(bold) == 1) bold <- rep(bold, nrow(settings))
      if (length(monospace) == 1) monospace <- rep(monospace, nrow(settings))
      if (length(strikeout) == 1) strikeout <- rep(strikeout, nrow(settings))
      if (length(fontsize) == 1) fontsize <- rep(fontsize, nrow(settings))
      indent_value <- if (indent > 0) paste0(indent, "em") else "false"
      counter <- 0
      settings$color <- color
      settings$underline <- underline
      settings$italic <- italic
      settings$bold <- bold
      settings$monospace <- monospace
      settings$strikeout <- strikeout
      settings$fontsize <- fontsize
      settings$indent <- indent_value
      sp <- split(settings, settings[, 3:ncol(settings)])
      sp <- lapply(sp, function(x) {
        x$i <- sprintf("(%s,)", paste(unique(x$i), collapse = ", "))
        x$j <- sprintf("(%s,)", paste(unique(x$j), collapse = ", "))
        x[1, , ]
      })
      sp <- do.call(rbind, sp)
      for (idx in seq_len(nrow(sp))) {
        k <- sp[idx, "i"]
        w <- sp[idx, "j"]
        counter <- counter + 1
        style <- sprintf(
          "    (y: %s, x: %s, color: %s, underline: %s, italic: %s, bold: %s, mono: %s, strikeout: %s, fontsize: %s, indent: %s),",
          k,
          w,
          sp$color[counter],
          tolower(sp$underline[counter]),
          tolower(sp$italic[counter]),
          tolower(sp$bold[counter]),
          tolower(sp$monospace[counter]),
          tolower(sp$strikeout[counter]),
          sp$fontsize[counter],
          sp$indent[counter]
        )
        out <- lines_insert(out, style, "tinytable cell style after", "after")
      }
    }

    if (fill_style_flag) {
      if (length(background) == 1) background <- rep(background, nrow(settings))
      counter <- 0
      settings$background <- background
      sp <- split(settings, settings$background)
      sp <- lapply(sp, function(x) {
        x$i <- sprintf("(%s,)", paste(unique(x$i), collapse = ", "))
        x$j <- sprintf("(%s,)", paste(unique(x$j), collapse = ", "))
        x[1, , ]
      })
      sp <- do.call(rbind, sp)
      for (idx in seq_len(nrow(sp))) {
        k <- sp[idx, "i"]
        w <- sp[idx, "j"]
        counter <- counter + 1
        fill <- sprintf(
          "    (y: %s, x: %s, fill: %s),",
          k,
          w,
          sp$background[counter])
        out <- lines_insert(out, fill, "tinytable cell fill after", "after")
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
      } else if (length(align_value) %in% c(1, nrow(settings))) {
        if (length(align_value) == 1) align_value <- rep(align_value, nrow(settings))
        counter <- 0
        settings$align <- align_value
        sp <- split(settings, settings$align)
        sp <- lapply(sp, function(x) {
          x$i <- sprintf("(%s,)", paste(unique(x$i), collapse = ", "))
          x$j <- sprintf("(%s,)", paste(unique(x$j), collapse = ", "))
          x[1, , ]
        })
        sp <- do.call(rbind, sp)
        for (idx in seq_len(nrow(sp))) {
          k <- sp[idx, "i"]
          w <- sp[idx, "j"]
          counter <- counter + 1
          fill <- sprintf(
            "    (y: %s, x: %s, align: %s),",
            k,
            w,
            sp$align[counter])
          out <- lines_insert(out, fill, "tinytable cell align after", "after")
        }
      } else {
        stop("Wrong number of elements in `align` argument.", call. = FALSE)
      }
    }

    # Lines are not part of cellspec/rowspec/columnspec. Do this separately.
    if (!is.null(line)) {
      iline <- NULL
      if (grepl("b", line)) iline <- c(iline, settings$i + 1) # -1 for 0-indexing
      if (grepl("t", line)) iline <- c(iline, settings$i)
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
          settings$j,
          max(settings$j) + 1,
          line_width,
          line_color)
        out <- lines_insert(out, tmp, "tinytable lines after", "after")
      }

      jline <- NULL
      if (grepl("r", line)) jline <- c(jline, settings$j + 1)
      if (grepl("l", line)) jline <- c(jline, settings$j)
      jline <- unique(jline)
      for (j in jline) {
        tmp <- sprintf(
          "table.vline(x: %s, start: %s, end: %s, stroke: %sem + %s),",
          j,
          min(settings$i),
          max(settings$i) + 1,
          line_width,
          line_color)
        out <- lines_insert(out, tmp, "tinytable lines after", "after")
      }
    }

    x@table_string <- out

    return(x)
  })

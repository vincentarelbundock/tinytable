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

    if (is.null(line)) {
        line <- NA
        line_color <- NA
        line_width <- NA
    }
    if (is.null(color)) color <- NA
    if (is.null(background)) background <- NA
    color <- ifelse(grepl("^#", color), sprintf('rgb("%s")', color), color)
    line_color <- ifelse(grepl("^#", line_color), sprintf('rgb("%s")', line_color), line_color)
    background <- ifelse(grepl("^#", background), sprintf('rgb("%s")', background), background)

    if (is.null(color)) color <- NA
    if (is.null(fontsize)) fontsize <- NA
    indent <- if (indent > 0) paste0(indent, "em") else NA

    sett <- style_settings_typst(
        x = x,
        i = i,
        j = j,
        color = color,
        underline = underline,
        italic = italic,
        bold = bold,
        monospace = monospace,
        strikeout = strikeout,
        fontsize = fontsize,
        indent = indent,
        background = background,
        line = line,
        line_width = line_width,
        line_color = line_color,
        align = align)

    x@style <- unique(rbind(x@style, sett))

    return(x)
  })



style_apply_typst <- function(x) {
    sty <- x@style

    lin <- sty[, c("i", "j", "line", "line_color", "line_width")]
    lin <- unique(lin[!is.na(lin$line),])

    sty <- sty[, !colnames(sty) %in% c("line", "line_color", "line_width")]
    sty <- unique(sty)

    no_style <- apply(sty[, 3:ncol(sty)], 1, function(k) {
        all(is.na(k) | k == FALSE)
    })

    if (all(no_style)) return(x)

    sty <- sty[!no_style,, drop = FALSE]

    last_style <- function(x) {
        if (all(is.na(x))) {
            x <- NA
        } else if (is.logical(x)) {
            x <- any(x[!is.na(x)])
        } else {
            x <- utils::tail(x[!is.na(x)], 1)
        }
        return(x)
    }
    sty <- split(sty, list(sty$i, sty$j))
    sty <- lapply(sty, function(k) lapply(k, last_style))
    sty <- do.call(rbind, lapply(sty, data.frame))

    # Typst-specific stuff

    # 0- & header-indexing
    sty$i <- sty$i + x@nhead - 1

    for (col in colnames(sty)) {
        if (is.logical(sty[[col]])) {
            sty[[col]] <- ifelse(sty[[col]] & !is.na(sty[[col]]), "true", "none")
        } else {
            sty[[col]][is.na(sty[[col]])] <- "none"
        }
    }

    # array representation for duplicate styles = cleaner .typ file
    idx <- apply(sty[, 3:ncol(sty)], 1, paste, collapse = "|")
    sty <- split(sty, idx, drop = FALSE)
    sty <- lapply(sty, function(k) {
        k$i <- sprintf("(%s,)", paste(unique(k$i), collapse = ", "))
        k$j <- sprintf("(%s,)", paste(unique(k$j), collapse = ", "))
        k[1,]
    })
    sty <- do.call(rbind, sty)


    for (row in seq_len(nrow(sty))) {
        style <- sprintf(
            "    (y: %s, x: %s, color: %s, underline: %s, italic: %s, bold: %s, mono: %s, strikeout: %s, fontsize: %s, indent: %s, background: %s, align: %s),",
            sty$i[row],
            sty$j[row],
            sty$color[row],
            sty$underline[row],
            sty$italic[row],
            sty$bold[row],
            sty$monospace[row],
            sty$strikeout[row],
            sty$fontsize[row],
            sty$indent[row],
            sty$background[row],
            sty$align[row]
        )
        x@table_string <- lines_insert(x@table_string, style, "tinytable cell style after", "after")

    }

    lin$i <- lin$i + x@nhead
    lin_split <- split(lin, lin[, 3:ncol(lin)])
    for (ls in lin_split) {
        line_h <- "table.hline(y: %s, start: %s, end: %s, stroke: %sem + %s),"
        line_v <- "table.vline(x: %s, start: %s, end: %s, stroke: %sem + %s),"
        if (any(grepl("b", ls$line))) {
            for (h in unique(ls$i)) {
                template <- sprintf(line_h, h, min(ls$j) - 1, max(ls$j) + 1, ls$line_width[1], ls$line_color[1])
                x@table_string <- lines_insert(x@table_string, template, "tinytable lines before", "before")
            }
        }
        if (any(grepl("t", ls$line))) {
            for (h in unique(ls$i)) {
                template <- sprintf(line_h, h - 1, min(ls$j) - 1, max(ls$j) + 1, ls$line_width[1], ls$line_color[1])
                x@table_string <- lines_insert(x@table_string, template, "tinytable lines before", "before")
            }
        }
        if (any(grepl("l", ls$line))) {
            for (v in unique(ls$j)) {
                template <- sprintf(line_v, v - 1, min(ls$i) - 1, max(ls$i), ls$line_width[1], ls$line_color[1])
                x@table_string <- lines_insert(x@table_string, template, "tinytable lines before", "before")
            }
        }
        if (any(grepl("r", ls$line))) {
            for (v in unique(ls$j)) {
                template <- sprintf(line_v, v - 1, min(ls$i), max(ls$i) + 1, ls$line_width[1], ls$line_color[1])
                x@table_string <- lines_insert(x@table_string, template, "tinytable lines before", "before")
            }
        }
    }

    return(x)
}


style_settings_typst <- function(x, i, j, color, underline, italic, bold, monospace, strikeout, fontsize, indent, background, line, line_color, line_width, align) {
    if (is.matrix(i) && is.logical(i) && nrow(i) == nrow(x) && ncol(i) == ncol(x)) {
        assert_null(j)
        settings <- which(i == TRUE, arr.ind = TRUE)
        settings <- stats::setNames(data.frame(settings), c("i", "j"))
        jval <- NULL
    } else {
        ival <- sanitize_i(i, x)
        jval <- sanitize_j(j, x)
    }
    settings <- expand.grid(i = ival, j = jval)
    settings <- settings[order(settings$i, settings$j),]
    settings[["color"]] <- color
    settings[["underline"]] <- underline
    settings[["italic"]] <- italic
    settings[["bold"]] <- bold
    settings[["monospace"]] <- monospace
    settings[["strikeout"]] <- strikeout
    settings[["fontsize"]] <- fontsize
    settings[["indent"]] <- indent
    settings[["background"]] <- background
    settings[["line"]] <- line
    settings[["line_color"]] <- line_color
    settings[["line_width"]] <- line_width

    if (!is.null(align)) {
        settings[["align"]] <- sapply(align,
            switch,
            c = "center",
            d = "center",
            r = "right",
            l = "left")
    } else {
        settings[["align"]] <- NA
    }
    return(settings)
}



# Lines are not part of cellspec/rowspec/columnspec. Do this separately.






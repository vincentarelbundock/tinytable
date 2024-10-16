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

    return(x)
  })



style_apply_typst <- function(x) {
    sty <- x@style

    # gutters are used for group_tt(j) but look ugly with cell fill
    if (!all(is.na(sty$background))) {
        x@table_string <- lines_drop(x@table_string, "column-gutter:", fixed = TRUE)
    }

    sty$align <- ifelse(is.na(sty$align), NA, 
        sapply(sty$align, function(k) switch(k,
            c = "center",
            d = "center",
            r = "right",
            l = "left")))

    lin <- sty[, c("i", "j", "line", "line_color", "line_width")]
    lin <- unique(lin[!is.na(lin$line),])

    sty <- sty[, !colnames(sty) %in% c("line", "line_color", "line_width")]
    sty <- unique(sty)

    no_style <- apply(sty[, 3:ncol(sty)], 1, function(k) {
        all(is.na(k) | k == FALSE)
    })

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
    sty$j <- sty$j - 1

    for (col in colnames(sty)) {
        if (is.logical(sty[[col]])) {
            sty[[col]] <- ifelse(sty[[col]] & !is.na(sty[[col]]), "true", "none")
        } else {
            sty[[col]][is.na(sty[[col]])] <- "none"
        }
    }

    # array representation for duplicate styles = cleaner .typ file
    if (!all(no_style)) {
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
    }

    # Lines are not part of cellspec/rowspec/columnspec. Do this separately.
    lin$i <- lin$i + x@nhead
    # not sure why, but seems necessary
    if (x@nhead == 0) lin$i <- lin$i + 1

    lin_split <- split(lin, lin[, 3:ncol(lin)])
    for (ls in lin_split) {
        line_h <- "table.hline(y: %s, start: %s, end: %s, stroke: %sem + %s),"
        line_v <- "table.vline(x: %s, start: %s, end: %s, stroke: %sem + %s),"
        if (any(grepl("b", ls$line))) {
            for (h in unique(ls$i)) {
                template <- sprintf(line_h, h, min(ls$j) - 1, max(ls$j), ls$line_width[1], ls$line_color[1])
                x@table_string <- lines_insert(x@table_string, template, "tinytable lines before", "before")
            }
        }
        if (any(grepl("t", ls$line))) {
            for (h in unique(ls$i)) {
                template <- sprintf(line_h, h - 1, min(ls$j) - 1, max(ls$j), ls$line_width[1], ls$line_color[1])
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
                template <- sprintf(line_v, v, min(ls$i) - 1, max(ls$i), ls$line_width[1], ls$line_color[1])
                x@table_string <- lines_insert(x@table_string, template, "tinytable lines before", "before")
            }
        }
    }

    return(x)
}



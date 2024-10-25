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

    sty$align[which(sty$align == "l")] <- "left"
    sty$align[which(sty$align == "c")] <- "center"
    sty$align[which(sty$align == "d")] <- "center"
    sty$align[which(sty$align == "r")] <- "right"

    sty$i <- sty$i
    sty$j <- sty$j - 1
    rec <- expand.grid(
        i = c(-(seq_len(x@nhead) - 1), seq_len(x@nrow)),
        j = seq_len(x@ncol) - 1
    )
    css <- rep("", nrow(rec))

    for (row in seq_len(nrow(sty))) {
        idx_i <- sty$i[row]
        if (is.na(idx_i)) idx_i <- unique(rec$i)
        idx_j <- sty$j[row]
        if (is.na(idx_j)) idx_j <- unique(rec$j)
        idx <- rec$i == idx_i & rec$j == idx_j
        if (isTRUE(sty[row, "bold"])) css[idx] <- paste(css[idx], "bold: true,")
        if (isTRUE(sty[row, "italic"])) css[idx] <- paste(css[idx], "italic: true,")
        if (isTRUE(sty[row, "underline"])) css[idx] <- paste(css[idx], "underline: true,")
        if (isTRUE(sty[row, "strikeout"])) css[idx] <- paste(css[idx], "strikeout: true,")
        if (isTRUE(sty[row, "monospace"])) css[idx] <- paste(css[idx], "monospace: true,")
        if (!is.na(sty[row, "color"])) css[idx] <- paste(css[idx], paste0("color: ", sty[row, "color"], ","))
        if (!is.na(sty[row, "background"])) css[idx] <- paste(css[idx], paste0("background: ", sty[row, "background"], ","))
        if (!is.na(sty[row, "fontsize"])) css[idx] <- paste(css[idx], paste0("fontsize: ", sty[row, "fontsize"], ","))
        if (!is.na(sty[row, "indent"])) css[idx] <- paste(css[idx], paste0("indent: ", sty[row, "indent"], ","))
        if (!is.na(sty[row, "align"])) css[idx] <- paste(css[idx], paste0("align: ", sty[row, "align"], ","))

    }

    rec$css <- gsub(" +", " ", trimws(css))
    rec <- rec[rec$css != "", ]
    # TODO: spans before styles, as in bootstrap

    # Unique style arrays
    uni <- split(rec, rec$css)
    browser()

    pairs <- sapply(uni, function(x) paste(sprintf("(%s, %s),", x$i, x$j), collapse = " "))
    styles <- sapply(uni, function(x) x$css[1])
    styles <- sprintf("(pairs: (%s), %s),", pairs, styles) 

    for (s in styles) {
        x@table_string <- lines_insert(x@table_string, s, "tinytable cell style after", "after")
    }


#(y: (1, 3,), x: (0, 1, 2, 3, 4,), color: none, underline: none, italic: none, bold: none, mono: none, strikeout: none, fontsize: none, indent: none, background: rgb("#ededed"), align: none),
    #
    # # Lines are not part of cellspec/rowspec/columnspec. Do this separately.
    # lin$i <- lin$i + x@nhead
    # # not sure why, but seems necessary
    # if (x@nhead == 0) lin$i <- lin$i + 1
    #
    # lin_split <- split(lin, lin[, 3:ncol(lin)])
    # for (ls in lin_split) {
    #     line_h <- "table.hline(y: %s, start: %s, end: %s, stroke: %sem + %s),"
    #     line_v <- "table.vline(x: %s, start: %s, end: %s, stroke: %sem + %s),"
    #     if (any(grepl("b", ls$line))) {
    #         for (h in unique(ls$i)) {
    #             template <- sprintf(line_h, h, min(ls$j) - 1, max(ls$j), ls$line_width[1], ls$line_color[1])
    #             x@table_string <- lines_insert(x@table_string, template, "tinytable lines before", "before")
    #         }
    #     }
    #     if (any(grepl("t", ls$line))) {
    #         for (h in unique(ls$i)) {
    #             template <- sprintf(line_h, h - 1, min(ls$j) - 1, max(ls$j), ls$line_width[1], ls$line_color[1])
    #             x@table_string <- lines_insert(x@table_string, template, "tinytable lines before", "before")
    #         }
    #     }
    #     if (any(grepl("l", ls$line))) {
    #         for (v in unique(ls$j)) {
    #             template <- sprintf(line_v, v - 1, min(ls$i) - 1, max(ls$i), ls$line_width[1], ls$line_color[1])
    #             x@table_string <- lines_insert(x@table_string, template, "tinytable lines before", "before")
    #         }
    #     }
    #     if (any(grepl("r", ls$line))) {
    #         for (v in unique(ls$j)) {
    #             template <- sprintf(line_v, v, min(ls$i) - 1, max(ls$i), ls$line_width[1], ls$line_color[1])
    #             x@table_string <- lines_insert(x@table_string, template, "tinytable lines before", "before")
    #         }
    #     }
    # }
    #
    return(x)
}



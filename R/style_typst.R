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

    insert_field <- function(x, name = "bold", value = "true") {
        old <- sprintf("%s: .*,", name)
        new <- sprintf("%s: %s,", name, value)
        out <- ifelse(grepl(old, x), 
            sub(old, new, x),
            sprintf("%s, %s", x, new))
        return(out)
    }

    for (row in seq_len(nrow(sty))) {
        idx_i <- sty$i[row]
        if (is.na(idx_i)) idx_i <- unique(rec$i)
        idx_j <- sty$j[row]
        if (is.na(idx_j)) idx_j <- unique(rec$j)
        idx <- rec$i == idx_i & rec$j == idx_j
        if (isTRUE(sty[row, "bold"])) css[idx] <- insert_field(css[idx], "bold", "true")
        if (isTRUE(sty[row, "italic"])) css[idx] <- insert_field(css[idx], "italic", "true")
        if (isTRUE(sty[row, "underline"])) css[idx] <- insert_field(css[idx], "underline", "true")
        if (isTRUE(sty[row, "strikeout"])) css[idx] <- insert_field(css[idx], "strikeout", "true")
        if (isTRUE(sty[row, "monospace"])) css[idx] <- insert_field(css[idx], "monospace", "true")
        if (!is.na(sty[row, "align"])) css[idx] <- insert_field(css[idx], "align", sty[row, "align"])

        fs <- sty[row, "indent"]
        if (!is.na(fs)) {
            css[idx] <- insert_field(css[idx], "indent", sprintf("%sem", fs))
        }

        fs <- sty[row, "fontsize"]
        if (!is.na(fs)) {
            css[idx] <- insert_field(css[idx], "fontsize", sprintf("%sem", fs))
        }

        col <- sty[row, "color"]
        if (!is.na(col)) {
            if (grepl("^#", col)) col <- sprintf('rgb("%s")', col)
            css[idx] <- insert_field(css[idx], "color", col)
        }

        bg <- sty[row, "background"]
        if (!is.na(bg)) {
            if (grepl("^#", bg)) bg <- sprintf('rgb("%s")', bg)
            css[idx] <- insert_field(css[idx], "background", bg)
        }
    }

    css <- gsub(" +", " ", trimws(css))
    css <- sub("^,", "", trimws(css))
    css <- gsub(",+", ",", trimws(css))
    rec$css <- css
    rec <- rec[rec$css != "", ]
    # TODO: spans before styles, as in bootstrap

    # Unique style arrays
    uni <- split(rec, rec$css)

    pairs <- sapply(uni, function(x) paste(sprintf("(%s, %s),", x$j, x$i), collapse = " "))
    styles <- sapply(uni, function(x) x$css[1])
    styles <- sprintf("(pairs: (%s), %s),", pairs, styles) 

    for (s in styles) {
        x@table_string <- lines_insert(x@table_string, s, "tinytable cell style after", "after")
        # x@table_string <- lines_insert(x@table_string, s, "tinytable cell align after", "after")
    }


    split_chunks <- function(x) {
        x <- sort(x)
        breaks <- c(0, which(diff(x) != 1), length(x))
        result <- list()
        for (i in seq_along(breaks)[-length(breaks)]) {
            chunk <- x[(breaks[i] + 1):breaks[i + 1]]
            result[[i]] <- c(min = min(chunk), max = max(chunk))
        }
        out <- data.frame(do.call(rbind, result))
        out$max <- out$max + 1
        return(out)
    }

    hlines <- function(x) {
        j <- sort(x$j)
        idx <- split_chunks(x$j)
        color <- if (is.na(x$line_color[1])) "black" else x$line_color[1]
        width <- if (is.na(x$line_width[1])) 0.1 else x$line_width[1]
        width <- sprintf("%sem", width)
        out <- ""
        if (grepl("t", x$line[1])) {
            tmp <- "table.hline(y: %s, start: %s, end: %s, stroke: %s + %s),"
            tmp <- sprintf(tmp, x$i[1], idx$min, idx$max, width, color)
            out <- paste(out, tmp)
        }
        if (grepl("b", x$line[1])) {
            tmp <- "table.hline(y: %s, start: %s, end: %s, stroke: %s + %s),"
            tmp <- sprintf(tmp, x$i[1] + 1, idx$min, idx$max, width, color)
            out <- paste(out, tmp)
        }
        return(out)
    }

    vlines <- function(x) {
        j <- sort(x$i)
        idx <- split_chunks(x$i)
        color <- if (is.na(x$line_color[1])) "black" else x$line_color[1]
        width <- if (is.na(x$line_width[1])) 0.1 else x$line_width[1]
        width <- sprintf("%sem", width)
        out <- ""
        if (grepl("l", x$line[1])) {
            tmp <- "table.vline(x: %s, start: %s, end: %s, stroke: %s + %s),"
            tmp <- sprintf(tmp, x$i[1], idx$min, idx$max, width, color)
            out <- paste(out, tmp)
        }
        if (grepl("r", x$line[1])) {
            tmp <- "table.hline(y: %s, start: %s, end: %s, stroke: %s + %s),"
            tmp <- sprintf(tmp, x$i[1] + 1, idx$min, idx$max, width, color)
            out <- paste(out, tmp)
        }
        return(out)
    }

    lin <- sty[grepl("b|t", sty$line),, drop = FALSE]
    if (nrow(lin) > 0) {
        lin <- split(lin, list(lin$i, lin$line, lin$line_color, lin$line_width))
        lin <- Filter(function(x) nrow(x) > 0, lin)
        lin <- lapply(lin, hlines)
        for (l in lin) {
            x@table_string <- lines_insert(x@table_string, l, "tinytable lines before", "before")
        }
    }

    lin <- sty[grepl("l|r", sty$line),, drop = FALSE]
    if (nrow(lin) > 0) {
        lin <- split(lin, list(lin$j, lin$line, lin$line_color, lin$line_width))
        lin <- Filter(function(x) nrow(x) > 0, lin)
        lin <- lapply(lin, vlines)
        for (l in lin) {
            x@table_string <- lines_insert(x@table_string, l, "tinytable lines before", "before")
        }
    }

    return(x)
}



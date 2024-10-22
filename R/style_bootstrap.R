#' Internal styling function
#'
#' @inheritParams style_tt
#' @keywords internal
#' @noRd
setMethod(
  f = "style_eval",
  signature = "tinytable_bootstrap",
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
                        alignv = NULL,
                        line = NULL,
                        line_color = "black",
                        line_width = 0.1,
                        colspan = NULL,
                        rowspan = NULL,
                        indent = 0,
                        bootstrap_class = NULL,
                        bootstrap_css = NULL,
                        bootstrap_css_rule = NULL,
                        ...) {

    return(x)
  })



style_apply_bootstrap <- function(x) {
    sty <- x@style

    # bootstrap classes and rules
    if (length(x@bootstrap_css_rule) == 1) {
      x@table_string <- bootstrap_setting(x@table_string, x@bootstrap_css_rule, component = "css")
    }

    # i=NULL includes groups
    idx <- is.na(sty$i)
    styna <- sty[idx, !colnames(sty) %in% "i", drop = FALSE]
    if (nrow(styna) > 0) {
        idx_i <- seq_len(x@nrow + x@ngroupi)
        if (x@nhead > 0) idx_i <- sort(unique(c(-(0:(x@nhead - 1)), idx_i)))
        idx_i <- data.frame(i = idx_i)
        idx_i <- merge(idx_i, styna, all = TRUE)
        sty <- rbind(idx_i, sty[!idx,, drop = FALSE])
    }

        # if (pre_group_i && inherits(x, "tinytable")) {
        #     out <- seq_len(nrow(x) - x@ngroupi)
        # } else {
        #     out <- seq_len(nrow(x))
        # }
        # if (inherits(x, "tinytable") && x@nhead > 0) {
        #     out <- c(-1 * (1:x@nhead - 1), out)
        # }

    sty$alignv[which(sty$alignv == "t")] <- "top"
    sty$alignv[which(sty$alignv == "b")] <- "bottom"
    sty$alignv[which(sty$alignv == "m")] <- "middle"

    sty$align[which(sty$align == "l")] <- "left"
    sty$align[which(sty$align == "c")] <- "center"
    sty$align[which(sty$align == "d")] <- "center"
    sty$align[which(sty$align == "r")] <- "right"

    sty <- last_style(sty)

    if (!isTRUE(nrow(sty) > 0)) return(x)

    css_arguments <- rep("", nrow(sty))
    idx <- which(sty$bold)
    css_arguments[idx] <- paste(css_arguments[idx], "font-weight: bold;")
    idx <- which(sty$italic)
    css_arguments[idx] <- paste(css_arguments[idx], "font-style: italic;")
    idx <- which(sty$underline)
    css_arguments[idx] <- paste(css_arguments[idx], "text-decoration: underline;")
    idx <- which(sty$strikeout)
    css_arguments[idx] <- paste(css_arguments[idx], "text-decoration: line-through;")
    idx <- which(sty$monospace)
    css_arguments[idx] <- paste(css_arguments[idx], "font-family: monospace;")
    idx <- which(!is.na(sty$color))
    css_arguments[idx] <- paste(css_arguments[idx], paste0("color: ", sty$color[idx], ";"))
    idx <- which(!is.na(sty$background))
    css_arguments[idx] <- paste(css_arguments[idx], paste0("background-color: ", sty$background[idx], ";"))
    idx <- which(!is.na(sty$fontsize))
    css_arguments[idx] <- paste(css_arguments[idx], paste0("font-size: ", sty$fontsize[idx], "em;"))
    idx <- which(!is.na(sty$alignv))
    css_arguments[idx] <- paste(css_arguments[idx], paste0("vertical-align: ", sty$alignv[idx], ";"))
    idx <- which(!is.na(sty$align))
    css_arguments[idx] <- paste(css_arguments[idx], paste0("text-align: ", sty$align[idx], ";"))
    idx <- which(!is.na(sty$indent))
    css_arguments[idx] <- paste(css_arguments[idx], paste0("padding-left: ", sty$indent[idx], "em;"))
    idx <- which(!is.na(sty$bootstrap_css))
    css_arguments[idx] <- paste(css_arguments[idx], sty$bootstrap_css[idx])

    lincol <- ifelse(is.na(sty$line_color), 
        sprintf("solid %sem; border-color: black;", sty$line_width),
        sprintf("solid %s %sem", sty$line_color, sty$line_width))
    lin <- rep("", nrow(sty))
    lin <- ifelse(!grepl("t", sty$line), lin, paste0(lin, sprintf("border-top: %s;", lincol)))
    lin <- ifelse(!grepl("b", sty$line), lin, paste0(lin, sprintf("border-bottom: %s;", lincol)))
    lin <- ifelse(!grepl("l", sty$line), lin, paste0(lin, sprintf("border-left: %s;", lincol)))
    lin <- ifelse(!grepl("r", sty$line), lin, paste0(lin, sprintf("border-right: %s;", lincol)))
    directions <- ifelse(is.na(sty$line), 0, sapply(strsplit(sty$line, ""), function(x) length(unique(x))))
    idx <- which(directions == 4)
    lin[idx] <- sprintf("border: %s;", lincol[idx])

    clean <- function(x) {
        x <- gsub(";+", ";", x)
        x <- gsub(" +;", ";", x)
        x <- gsub(" +", " ", x)
        x <- trimws(x)
        x
    }
    css_arguments <- clean(css_arguments)
    lin <- clean(lin)

    sty$css_arguments <- css_arguments
    sty$lin_arguments <- lin

    # line styles 
    lin <- sty[sty$lin_arguments != "",, drop = FALSE]
    lin_table <- data.frame(lin_arguments = unique(lin$lin_arguments))
    lin_table$id_lin <- sapply(seq_len(nrow(lin_table)), function(i) get_id(stem = "tinytable_css_"))
    idx <- merge(lin[, c("i", "j", "lin_arguments")], lin_table, all.x = TRUE)
    if (nrow(idx) > 0) {
        idx <- split(idx, idx$id)
        for (i in seq_along(idx)) {
            id_lin <- idx[[i]]$id[1]
            arr <- sprintf("{ i: %s, j: %s }, ", idx[[i]]$i, idx[[i]]$j)
            arr <- c("          {", " positions: [ ", arr, " ],", " css_id: '", id_lin, "',", "}, ")
            arr <- paste(arr, collapse = "")
            x@table_string <- lines_insert(x@table_string, arr, "tinytable style arrays after", "after")
            entry <- sprintf("      .table td.%s, .table th.%s { %s }", id_lin, id_lin, idx[[i]]$lin_arguments[1])
            x@table_string <- lines_insert(x@table_string, entry, "tinytable css entries after", "after")
        }
    }

    # non-line styles 
    css <- sty[sty$css_arguments != "",, drop = FALSE]
    css_table <- data.frame(css_arguments = unique(css$css_arguments))
    css_table$id_css <- sapply(seq_len(nrow(css_table)), function(i) get_id(stem = "tinytable_css_"))
    idx <- merge(css[, c("i", "j", "css_arguments")], css_table, all.x = TRUE)
    if (nrow(idx) > 0) {
        idx <- split(idx, idx$id)
        for (i in seq_along(idx)) {
            id_css <- idx[[i]]$id[1]
            arr <- sprintf("{ i: %s, j: %s }, ", idx[[i]]$i, idx[[i]]$j)
            arr <- c("          {", " positions: [ ", arr, " ],", " css_id: '", id_css, "',", "}, ")
            arr <- paste(arr, collapse = "")
            x@table_string <- lines_insert(x@table_string, arr, "tinytable style arrays after", "after")
            entry <- sprintf("      .table td.%s, .table th.%s { %s }", id_css, id_css, idx[[i]]$css_arguments[1])
            x@table_string <- lines_insert(x@table_string, entry, "tinytable css entries after", "after")
        }
    }

    # spans
    for (row in seq_len(nrow(sty))) {
        rowspan <- if (!is.na(sty$rowspan[row])) sty$rowspan[row] else 1
        colspan <- if (!is.na(sty$colspan[row])) sty$colspan[row] else 1
        if (rowspan > 1 || colspan > 1) {
            id <- get_id(stem = "spanCell_")
            listener <- "      window.addEventListener('load', function () { %s(%s, %s, %s, %s) })"
            listener <- sprintf(listener, id, sty$i[row], sty$j[row], rowspan, colspan)
            x@table_string <- lines_insert(x@table_string, listener, "tinytable span after", "after")
            # x@table_string <- bootstrap_setting(x@table_string, listener, component = "cell")
        }
    }


    return(x)
}



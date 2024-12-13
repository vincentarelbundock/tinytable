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
    if (length(x@bootstrap_css_rule) == 1) {
      x@table_string <- bootstrap_setting(x@table_string, x@bootstrap_css_rule, component = "css")
    }

    sty <- x@style


    sty$alignv[which(sty$alignv == "t")] <- "top"
    sty$alignv[which(sty$alignv == "b")] <- "bottom"
    sty$alignv[which(sty$alignv == "m")] <- "middle"

    sty$align[which(sty$align == "l")] <- "left"
    sty$align[which(sty$align == "c")] <- "center"
    sty$align[which(sty$align == "d")] <- "center"
    sty$align[which(sty$align == "r")] <- "right"

    rec <- expand.grid(
      i = c(-(seq_len(x@nhead) - 1), seq_len(x@nrow)),
      j = seq_len(x@ncol)
    )
    css <- rep("", nrow(rec))

    for (row in seq_len(nrow(sty))) {
      # index: sty vs rec
      idx_i <- sty$i[row]
      if (is.na(idx_i)) idx_i <- unique(rec$i)
      idx_j <- sty$j[row]
      if (is.na(idx_j)) idx_j <- unique(rec$j)
      idx <- rec$i == idx_i & rec$j == idx_j

      if (isTRUE(sty[row, "bold"])) css[idx] <- paste(css[idx], "font-weight: bold;")
      if (isTRUE(sty[row, "italic"])) css[idx] <- paste(css[idx], "font-style: italic;")
      if (isTRUE(sty[row, "underline"])) css[idx] <- paste(css[idx], "text-decoration: underline;")
      if (isTRUE(sty[row, "strikeout"])) css[idx] <- paste(css[idx], "text-decoration: line-through;")
      if (isTRUE(sty[row, "monospace"])) css[idx] <- paste(css[idx], "font-family: monospace;")
      if (!is.na(sty[row, "color"])) css[idx] <- paste(css[idx], paste0("color: ", sty[row, "color"], ";"))
      if (!is.na(sty[row, "background"])) css[idx] <- paste(css[idx], paste0("background-color: ", sty[row, "background"], ";"))
      if (!is.na(sty[row, "fontsize"])) css[idx] <- paste(css[idx], paste0("font-size: ", sty[row, "fontsize"], "em;"))
      if (!is.na(sty[row, "alignv"])) css[idx] <- paste(css[idx], paste0("vertical-align: ", sty[row, "alignv"], ";"))
      if (!is.na(sty[row, "align"])) css[idx] <- paste(css[idx], paste0("text-align: ", sty[row, "align"], ";"))
      if (!is.na(sty[row, "indent"])) css[idx] <- paste(css[idx], paste0("padding-left: ", sty[row, "indent"], "em;"))
      if (!is.na(sty[row, "bootstrap_css"])) css[idx] <- paste(css[idx], sty[row, "bootstrap_css"])

      lin <- ""
      line <- sty$line[row]
      line_width <- sty$line_width[row]
      line_color <- sty$line_color[row]
      line_color <- if (is.na(line_color)) "black" else line_color
      line_width <- if (is.na(line_width)) 0.1 else line_width
      left <- grepl("l", line)
      right <- grepl("r", line)
      top <- grepl("t", line)
      bottom <- grepl("b", line)
      if (all(c(left, right, top, bottom))) {
        template <- "border: solid %s %sem;"
      } else if (any(c(left, right, top, bottom))) {
        template <- "border: solid %s %sem;"
        if (left) template <- "border-left: solid %s %sem;"
        if (right) template <- "border-right: solid %s %sem;"
        if (top) template <- "border-top: solid %s %sem;"
        if (bottom) template <- "border-bottom: solid %s %sem;"
      } else {
        template <- ""
      }
      if (template != "") {
        lin <- paste(lin, sprintf(template, line_color, line_width))
      }
      css[idx] <- paste(css[idx], lin)
    }

    css <- gsub(" +", " ", trimws(css))

    # JS 0-indexing
    rec$i <- rec$i - 1 + x@nhead
    rec$j <- rec$j - 1


    # spans: before styles because we return(x) if there is no style
    for (row in seq_len(nrow(sty))) {
      rowspan <- if (!is.na(sty$rowspan[row])) sty$rowspan[row] else 1
      colspan <- if (!is.na(sty$colspan[row])) sty$colspan[row] else 1
      if (rowspan > 1 || colspan > 1) {
        id <- get_id(stem = "spanCell_")
        listener <- "      window.addEventListener('load', function () { %s(%s, %s, %s, %s) })"
        listener <- sprintf(listener, id, sty$i[row], sty$j[row] - 1, rowspan, colspan)
        x@table_string <- lines_insert(x@table_string, listener, "tinytable span after", "after")
        # x@table_string <- bootstrap_setting(x@table_string, listener, component = "cell")
      }
    }


    rec$css_arguments <- css
    rec <- rec[rec$css_arguments != "", , drop = FALSE]
    if (nrow(rec) == 0) {
      return(x)
    }

    # Unique CSS arguments assigne by arrays
    css_table <- unique(rec[, c("css_arguments"), drop = FALSE])
    css_table$id_css <- sapply(seq_len(nrow(css_table)), function(i) get_id(stem = "tinytable_css_"))
    idx <- merge(rec[, c("i", "j", "css_arguments")], css_table, all.x = TRUE)
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

    return(x)
  }
)

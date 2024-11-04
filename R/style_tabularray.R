setMethod(
  f = "style_eval",
  signature = "tinytable_tabularray",
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
                        tabularray_inner = NULL,
                        tabularray_outer = NULL,
                        ...) {



  sty <- x@style
  sty$i <- sty$i + x@nhead

  rec <- expand.grid(
    i = c(seq_len(x@nrow + x@nhead)),
    j = seq_len(x@ncol),
    line = NA,
    line_color = NA,
    line_width = NA
  )

  sty$alignv[which(sty$alignv == "b")] <- "f"
  sty$alignv[which(sty$alignv == "t")] <- "h"
  sty$alignv[which(sty$alignv == "m")] <- "m"

  for (spec in stats::na.omit(sty$tabularray_inner)) {
    x@table_string <- tabularray_insert(x@table_string, content = spec, type = "inner")
  }

  for (spec in stats::na.omit(sty$tabularray_outer)) {
    x@table_string <- tabularray_insert(x@table_string, content = spec, type = "inner")
  }

  set <- span <- rep("", nrow(rec))

  for (row in seq_len(nrow(sty))) {

    # index: sty vs rec
    idx_i <- sty$i[row]
    if (is.na(idx_i)) idx_i <- unique(rec$i)
    idx_j <- sty$j[row]
    if (is.na(idx_j)) idx_j <- unique(rec$j)
    idx <- rec$i == idx_i & rec$j == idx_j

    cmd <- ""
    if (isTRUE(sty$bold[row])) cmd <- paste0(cmd, "\\bfseries")
    if (isTRUE(sty$italic[row])) cmd <- paste0(cmd, "\\textit")
    if (isTRUE(sty$underline[row])) cmd <- paste0(cmd, "\\tinytableTabularrayUnderline")
    if (isTRUE(sty$strikeout[row])) cmd <- paste0(cmd, "\\tinytableTabularrayStrikeout")
    if (isTRUE(sty$monospace[row])) cmd <- paste0(cmd, "\\texttt")

    col <- sty$color[row]
    if (!is.na(col)) {
      x <- color_to_preamble(x, col)
      if (grepl("^#", col)) col <- sub("^#", "c", col)
      cmd <- sprintf("%s, fg=%s", cmd, col)
    }

    bg <- sty$background[row]
    if (!is.na(bg)) {
      x <- color_to_preamble(x, bg)
      if (grepl("^#", bg)) bg <- sub("^#", "c", bg)
      cmd <- sprintf("%s, bg=%s", cmd, bg)
    }

    if (grepl("^,", cmd)) {
        tmp <- "%s, %s, "
    } else {
        tmp <- "%s, cmd=%s, "
    }
    if (trimws(cmd) != "") set[idx] <- sprintf(tmp, set[idx], cmd)

    fontsize <- sty$fontsize[row]
    if (!is.na(as.numeric(fontsize))) {
      set[idx] <- sprintf(
        "%s font=\\fontsize{%sem}{%sem}\\selectfont,", 
        set[idx], fontsize, fontsize + 0.3) 
    }

    halign <- sty$align[row]
    if (!is.na(halign)) {
        if (!identical(halign, "d")) {
            set[idx] <- sprintf("%s, halign=%s,", set[idx], halign)
        } else {
            dcol <- get_dcolumn(rec[row, "j"], x)
            set[idx] <- sprintf("%s, %s", set[idx], dcol)
        }
    }

    alignv <- sty$alignv[row]
    if (!is.na(alignv)) {
      set[idx] <- sprintf("%s, valign=%s,", set[idx], alignv)
    }

    indent <- sty$indent[row] 
    if (isTRUE(indent > 0)) {
      set[idx] <- sprintf("%s preto={\\hspace{%sem}},", set[idx], indent)
    }

    if (!is.na(sty$colspan[row])) {
      span[idx] <- paste0(span[idx], "c=", sty$colspan[row], ",")
    }

    if (!is.na(sty$rowspan[row])) {
      span[idx] <- paste0(span[idx], "r=", sty$rowspan[row], ",")
    }

    if (!is.na(sty$line[row])) {
      rec$line[idx] <- sty$line[row]
    }

    if (!is.na(sty$line_color[row])) {
      rec$line_color[idx] <- sty$line_color[row]
    }

    if (!is.na(sty$line_width[row])) {
      rec$line_width[idx] <- sty$line_width[row]
    }
  }

  clean <- function(k) {
    k <- gsub("\\s*", "", k)
    k <- gsub(",+", ",", k)
    k <- gsub("^,", "", k, perl = TRUE)
    k <- gsub(",", ", ", k)
    k <- trimws(k)
    return(k)
  }

  rec$set <- clean(set)
  rec$span <- clean(span)

  all_i <- seq_len(x@nrow + x@nhead)
  all_j <- seq_len(x@ncol)

  # complete rows and columns
  rec <- do.call(rbind, by(rec, list(rec$j, rec$set, rec$span), function(k) {
    transform(k, complete_column = all(all_i %in% k$i))
  }))
  rec <- do.call(rbind, by(rec, list(rec$i, rec$set, rec$span), function(k) {
    transform(k, complete_row = all(all_j %in% k$j))
  }))

  idx <- rec$span != "" | rec$set != ""

  # complete columns (first because of d-column)
  cols <- unique(rec[idx & rec$complete_column, c("j", "set", "span"), drop = FALSE])
  spec <- by(cols, list(cols$set, cols$span), function(k) {
    sprintf("column{%s}={%s}{%s}", paste(k$j, collapse = ","), k$span, k$set)
  })
  spec <- unique(as.vector(unlist(spec)))
  for (s in spec) {
    x@table_string <- tabularray_insert(x@table_string, content = s, type = "inner")
  }

  # complete rows
  rows <- unique(rec[
    idx & rec$complete_row & !rec$complete_column,
    c("i", "set", "span"),
    drop = FALSE])
  spec <- by(rows, list(rows$set, rows$span), function(k) {
    sprintf("row{%s}={%s}{%s}", paste(k$i, collapse = ","), k$span, k$set)
  })
  spec <- unique(as.vector(unlist(spec)))
  for (s in spec) {
    x@table_string <- tabularray_insert(x@table_string, content = s, type = "inner")
  }

  # cells
  cells <- unique(rec[idx & !rec$complete_row & !rec$complete_column, , drop = FALSE])
  spec <- by(cells, list(cells$set, cells$span), function(k) {
    ival <- paste(sort(unique(k$i)), collapse = ",")
    sprintf("cell{%s}{%s}={%s}{%s}", ival, k$j, k$span, k$set)
  })
  spec <- unique(as.vector(unlist(spec)))
  for (s in spec) {
    x@table_string <- tabularray_insert(x@table_string, content = s, type = "inner")
  }

  # lines
  rec$lin <- "solid, "
  rec$lin <- ifelse(!is.na(rec$line_color), 
      paste0(rec$lin, rec$line_color), rec$lin)
  rec$lin <- ifelse(!is.na(rec$line_width), 
      paste0(rec$lin, sprintf(", %sem", rec$line_width)), rec$lin)
  rec$lin[is.na(rec$line)] <- NA

  # horizontal lines
  horizontal <- rec[grepl("b|t", rec$line), c("i", "j", "lin", "line"), drop = FALSE]
  horizontal <- rbind(
    horizontal[grepl("t", horizontal$line),, drop = FALSE],
    transform(horizontal[grepl("b", horizontal$line),, drop = FALSE], i = i + 1)
  )
  spec <- by(horizontal, list(horizontal$i, horizontal$lin), function(k) {
    jval <- paste(sort(unique(k$j)), collapse = ",")
    sprintf("hline{%s}={%s}{%s}", k$i, jval, k$lin)
  })
  spec <- unique(as.vector(unlist(spec)))
  for (s in spec) {
    x@table_string <- tabularray_insert(x@table_string, content = s, type = "inner")
  }

  # vertical lines
  vertical <- rec[grepl("l|r", rec$line), c("i", "j", "lin", "line"), drop = FALSE]
  vertical <- rbind(
    vertical[grepl("l", vertical$line),, drop = FALSE],
    transform(vertical[grepl("r", vertical$line),, drop = FALSE], j = j + 1)
  )
  spec <- by(vertical, list(vertical$j, vertical$lin), function(k) {
    ival <- paste(sort(unique(k$i)), collapse = ",")
    sprintf("vline{%s}={%s}{%s}", k$j, ival, k$lin)
  })
  spec <- unique(as.vector(unlist(spec)))
  for (s in spec) {
    x@table_string <- tabularray_insert(x@table_string, content = s, type = "inner")
  }

  return(x)

})



tabularray_insert <- function(x, content = NULL, type = "body") {

  out <- x

  out <- strsplit(out, "\n")[[1]]
  comment <- switch(type,
  "body" = "% tabularray inner close",
  "outer" = "% tabularray outer close",
  "inner" = "% tabularray inner close")
  idx <- grep(comment, out)

  if (length(content) > 0) {
    content <- trimws(content)
    if (!grepl(",$", content) && type != "body") {
      content <- paste0(content, ",")
    }
    if (type == "body") {
      out <- c(out[1:idx], content, out[(idx + 1):length(out)])
    } else {
      out <- c(out[1:(idx - 1)], content, out[idx:length(out)])
    }
  }

  out <- paste(out, collapse = "\n")

  return(out)
}


color_to_preamble <- function(x, col) {
  if (grepl("^#", col)) { # hex color need to be defined in LaTeX
    col <- sub("^#", "c", col)
    regex <- sprintf("DefineColor.*%s", col)
    if (!grepl(regex, x@table_string)) {
      b <- sprintf("\\tinytableDefineColor{%s}{HTML}{%s}", col, sub("^c", "", col))
      x@table_string <- tabularray_insert(x@table_string, content = b, type = "body")
    }
  }
  return(x)
}



get_dcolumn <- function(j, x) {
    siunitx <- get_option("tinytable_siunitx_table_format", default = "table-format=-%s.%s,table-align-text-before=false,table-align-text-after=false,input-symbols={-,\\*+()}")
    num <- unlist(x@table_dataframe[, j])
    num <- strsplit(num, "\\.")
    num <- lapply(num, function(k) if (length(k) == 1) c(k, " ") else k)
    left <- sapply(num, function(k) k[[1]])
    right <- sapply(num, function(k) k[[2]])
    left <- max(nchar(gsub("\\D", "", left)))
    right <- max(nchar(gsub("\\D", "", right)))
    out <- sprintf(siunitx, left, right)
    out <- sprintf("si={%s},", out)
    return(out)
}


## not longer used, but took a while to collect and might be useful in the future
# out <- list(
#   rows_keys = c("halign", "valign", "ht", "bg", "fg", "font", "mode", "cmd", "abovesep", "belowsep", "rowsep", "preto", "appto", "indent"),
#   columns_keys = c("halign", "valign", "wd", "co", "bg", "fg", "font", "mode", "cmd", "leftsep", "rightsep", "colsep", "preto", "appto", "indent"),
#   hborders_keys = c("pagebreak", "abovespace", "belowspace"),
#   vborders_keys = c("leftspace", "rightspace"),
#   cells_keys = c("halign", "valign", "wd", "bg", "fg", "font", "mode", "cmd", "preto", "appto"),
#   outer_specs_keys = c("baseline", "long", "tall", "expand"),
#   inner_specs_keys = c("rulesep", "hlines", "vline", "hline", "vlines", "stretch", "abovesep", "belowsep", "rowsep", "leftsep", "rightsep", "colsep", "hspan", "vspan", "baseline"),
#   span = c("r", "c")
# )

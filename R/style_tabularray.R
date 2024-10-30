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

    return(x)
  })


style_apply_tabularray <- function(x) {

  sty <- x@style
  sty$i <- sty$i + x@nhead

  rec <- expand.grid(
    i = c(seq_len(x@nrow + x@nhead)),
    j = seq_len(x@ncol)
  )

  sty$alignv[which(sty$alignv == "b")] <- "f"
  sty$alignv[which(sty$alignv == "t")] <- "h"
  sty$alignv[which(sty$alignv == "m")] <- "m"

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

    if (cmd != "") set[idx] <- sprintf("%s, cmd=%s, ", set[idx], cmd)

    fontsize <- sty$fontsize[row]
    if (is.na(is.numeric(fontsize))) {
      set[idx] <- sprintf(
        "%s font=\\fontsize{%sem}{%sem}\\selectfont,", 
        set[idx], fontsize, fontsize + 0.3) 
    }

    halign <- sty$align[row]
    if (!is.na(halign)) {
        if (!identical(halign, "d")) {
            set[idx] <- sprintf("%s, halign=%s,", set[idx], halign)
        } else {
            dcol <- get_dcolumn(rec[row]$j[1], x)
            set[idx] <- sprintf("%s, %s", set[idx], dcol)
        }
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

  }

  clean <- function(x) {
    x <- gsub("^\\s*,", "", x)
    x <- gsub(",\\s*,", ",,", x)
    x <- gsub("\\s+", " ", x)
    x <- gsub(",+", ",", x)
    x <- trimws(x)
    return(x)
  }

  rec$set <- clean(set)
  rec$span <- clean(span)

  rec <- rec[rec$set != "", , drop = FALSE]

  recj <- split(rec, rec$j)
  for (rj in recj) {
    all_i <- seq_len(x@nrow + x@nhead)

    flag <- nrow(rj) == length(all_i) && 
      all(rj$i == all_i) && 
      length(unique(rj$set)) == 1 &&
      length(unique(rj$span)) == 1

    # prioritize unique columns because tables usually have more rows than columns
    if (isTRUE(flag)) {
      spec <- sprintf("column{%s}={%s}{%s}", 
        rj$j[1], 
        rj$span[1],
        rj$set[1])
      x@table_string <- tabularray_insert(x@table_string, content = spec, type = "inner")
    } else {
      for (rj_i in seq_len(nrow(rj))) {
        if (rj$set[rj_i] != "") {
          spec <- sprintf("cell{%s}{%s}={%s}{%s}", 
            rj$i[rj_i], 
            rj$j[rj_i], 
            rj$span[rj_i],
            rj$set[rj_i])
          x@table_string <- tabularray_insert(x@table_string, content = spec, type = "inner")
        }
      }
    }
  }

  return(x)
}
  #     out <- tabularray_insert(out, content = spec, type = "inner")

  
  # if (!is.null(align)) {
  #   if (length(align) == 1) align <- rep(align, length(jval))
  #
  # # Lines are not part of cellspec/rowspec/columnspec. Do this separately.
  # if (!is.null(line)) {
  #   iline <- jline <- NULL
  #   if (grepl("t", line)) iline <- c(iline, ival + x@nhead)
  #   if (grepl("b", line)) iline <- c(iline, ival + x@nhead + 1)
  #   if (grepl("l", line)) jline <- c(jline, jval)
  #   if (grepl("r", line)) jline <- c(jline, jval + 1)
  #   iline <- unique(iline)
  #   jline <- unique(jline)
  #   line_width <- paste0(line_width, "em")
  #   if (!is.null(iline)) {
  #     tmp <- sprintf(
  #       "hline{%s}={%s}{solid, %s, %s},",
  #       paste(iline, collapse = ","),
  #       paste(jval, collapse = ","),
  #       line_width,
  #       sub("^#", "c", line_color)
  #     )
  #     out <- tabularray_insert(out, content = tmp, type = "inner")
  #   }
  #   if (!is.null(jline)) {
  #     tmp <- sprintf(
  #       "vline{%s}={%s}{solid, %s, %s},",
  #       paste(jline, collapse = ","),
  #       paste(ival + x@nhead, collapse = ","),
  #       line_width,
  #       sub("^#", "c", line_color)
  #     )
  #     out <- tabularray_insert(out, content = tmp, type = "inner")
  #   }
  # }
  #
  # out <- tabularray_insert(out, content = tabularray_inner, type = "inner")
  # out <- tabularray_insert(out, content = tabularray_outer, type = "outer")
  #
  # x@table_string <- out
  #
#   return(x)
# }  


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

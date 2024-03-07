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
                             width = NULL,
                             align = NULL,
                             alignv = NULL,
                             line = NULL,
                             line_color = "black",
                             line_width = .1,
                             colspan = NULL,
                             rowspan = NULL,
                             indent = 0,
                             tabularray_inner = NULL,
                             tabularray_outer = NULL,
                             ...) {

  out <- x@table_string

  ival <- if (is.null(i)) seq_len(nrow(x)) else i
  jval <- if (is.null(j)) seq_len(ncol(x)) else j

  # order may be important for recycling 
  settings <- expand.grid(i = ival, j = jval, tabularray = "", stringsAsFactors = FALSE)
  if (is.null(i) && !is.null(j)) {
    settings <- settings[order(settings$i, settings$j), ]
  }

  # header index
  if ("i" %in% names(settings)) {
    settings$i <- settings$i + x@nhead
  }

  # colspan and rowspan require cell level, so we keep the full settings DF, even
  # in tabularray, where we can sometimes use rowspec or colspec when one is empty
  if (is.null(colspan) && is.null(rowspan)) {
    if (is.null(i) && is.null(j)) {
      settings <- unique(settings[, c("i", "tabularray"), drop = FALSE])
    } else if (is.null(i)) {
      settings <- unique(settings[, c("j", "tabularray"), drop = FALSE])
    } else if (is.null(j)) {
      settings <- unique(settings[, c("i", "tabularray"), drop = FALSE])
    }
  }

  span <- ""
  span <- if (!is.null(colspan)) paste0(span, "c=", colspan, ",") else span
  span <- if (!is.null(rowspan)) paste0(span, "r=", rowspan, ",") else span

  if (!is.null(alignv)) {
    alignv_tabularray <- switch(alignv,
      "b" = "f",
      "t" = "h",
      "m" = "m"
    )
    settings$tabularray <- sprintf("%s valign=%s,", settings$tabularray, alignv_tabularray)
  }

  # convert to tabularray now that we've filled the bootstrap settings
  if (is.numeric(fontsize)) settings$tabularray <- sprintf("%s font=\\fontsize{%sem}{%sem}\\selectfont,", settings$tabularray, fontsize, fontsize + 0.3) 
  if (!is.null(width)) settings$tabularray <- sprintf("%s wd=%s,", settings$tabularray, width)
  if (indent > 0) settings$tabularary <- sprintf("%s preto={\\hspace{%sem}},", settings$tabularray, indent)

  if (!is.null(align)) {
    if (length(align) == 1) align <- rep(align, length(jval))

    # explicit j input
    siunitx <- getOption("tinytable_siunitx_table-format", default = "table-format=-%s.%s,table-align-text-before=false,table-align-text-after=false,input-symbols={-,\\*+()}")
    if ("j" %in% colnames(settings)) {
      for (idx in seq_along(jval)) {
        a_tmp <- align[idx]
        j_tmp <- jval[idx]
        rowidx <- settings$j == j_tmp
        if (a_tmp == "d") {
          num <- x@table_dataframe[[j_tmp]]
          num <- strsplit(num, "\\.")
          num <- lapply(num, function(k) if (length(k) == 1) c(k, " ") else k)
          left <- sapply(num, function(k) k[[1]])
          right <- sapply(num, function(k) k[[2]])
          left <- max(nchar(gsub("\\D", "", left)))
          right <- max(nchar(gsub("\\D", "", right)))
          tmp <- sprintf(siunitx, left, right)
          settings$tabularray <- ifelse(
            rowidx,
            sprintf("%s si={%s},", settings$tabularray, tmp),
            settings$tabularray)
        } else {
          settings$tabularray <- ifelse(
            rowidx,
            sprintf("%s halign=%s,", settings$tabularray, a_tmp),
            settings$tabularray)
        }
      }

      # no explicit j input
      } else {
        a_tmp <- align[1]
        if (a_tmp == "d") {
          num <- unlist(x@table_dataframe[, jval])
          num <- strsplit(num, "\\.")
          num <- lapply(num, function(k) if (length(k) == 1) c(k, " ") else k)
          left <- sapply(num, function(k) k[[1]])
          right <- sapply(num, function(k) k[[2]])
          left <- max(nchar(gsub("\\D", "", left)))
          right <- max(nchar(gsub("\\D", "", right)))
          tmp <- sprintf(siunitx, left, right)
          settings$tabularray <- sprintf("%s si={%s},", settings$tabularray, tmp)
        } else {
          settings$tabularray <- sprintf(
            "%s halign=%s,",
            settings$tabularray, a_tmp)
        }
      }
    }

  vectorize_tabularray <- function(z) {
    if (is.null(z)) {
      return(rep(FALSE, nrow(settings)))
    }
    if (check_flag(z))  {
      return(rep(z, nrow(settings)))
    }
    return(z)
  }

  bold <- vectorize_tabularray(bold)
  italic <- vectorize_tabularray(italic)
  underline <- vectorize_tabularray(underline)
  strikeout <- vectorize_tabularray(strikeout)
  monospace <- vectorize_tabularray(monospace)
  cmd <- rep("", nrow(settings))
  cmd <- ifelse(bold, paste0(cmd, "\\bfseries"), cmd)
  cmd <- ifelse(italic, paste0(cmd, "\\textit"), cmd)
  cmd <- ifelse(underline, paste0(cmd, "\\tinytableTabularrayUnderline"), cmd)
  cmd <- ifelse(strikeout, paste0(cmd, "\\tinytableTabularrayStrikeout"), cmd)
  cmd <- ifelse(monospace, paste0(cmd, "\\texttt"), cmd)
  settings$tabularray <- sprintf("%s, cmd=%s,", settings$tabularray, cmd)

  # hex must be treated differently in LaTeX
  cols <- c(color, background, line_color)
  cols_done <- NULL
  if (!is.null(cols)) {
    hex <- cols[grepl("^#", cols)]
    for (h in hex) {
      b <- sprintf(
        "\\tinytableDefineColor{%s}{HTML}{%s}",
        sub("^#", "c", h), sub("^#", "", h))
      if (!b %in% cols_done) {
        out <- tabularray_insert(out, content = b, type = "body")
        cols_done <- c(cols_done, b)
      }
    }
  }
  if (!is.null(background)) {
    settings$tabularray <- sprintf("%s bg=%s,", settings$tabularray, sub("^#", "c", background))
  }
  if (!is.null(color)) {
    settings$tabularray <- sprintf("%s fg=%s,", settings$tabularray, sub("^#", "c", color))
  }

  settings$tabularray <- trimws(gsub("cmd=,", "", settings$tabularray))
  settings$tabularray <- trimws(gsub("\\s+", "", settings$tabularray))
  settings$tabularray <- trimws(gsub(",+", ",", settings$tabularray))


  if (!all(settings$tabularray == ",") || span != "") {
    for (k in seq_len(nrow(settings))) {
      if (all(c("i", "j") %in% colnames(settings))) {
        spec <- sprintf("cell{%s}{%s}={%s}{%s},", settings$i[k], settings$j[k], span, settings$tabularray[k])
      } else if ("i" %in% colnames(settings)) {
        spec <- sprintf("row{%s}={%s},", settings$i[k], settings$tabularray[k])
      } else if ("j" %in% colnames(settings)) {
        spec <- sprintf("column{%s}={%s},", settings$j[k], settings$tabularray[k])
      }
      out <- tabularray_insert(out, content = spec, type = "inner")
    }
  }

  # Lines are not part of cellspec/rowspec/columnspec. Do this separately.
  if (!is.null(line)) {
    iline <- jline <- NULL
    if (grepl("t", line)) iline <- c(iline, ival + x@nhead)
    if (grepl("b", line)) iline <- c(iline, ival + x@nhead + 1)
    if (grepl("l", line)) jline <- c(jline, jval)
    if (grepl("r", line)) jline <- c(jline, jval + 1)
    iline <- unique(iline)
    jline <- unique(jline)
    line_width <- paste0(line_width, "em")
    if (!is.null(iline)) {
      tmp <- sprintf(
        "hline{%s}={%s}{solid, %s, %s},",
        paste(iline, collapse = ","),
        paste(jval, collapse = ","),
        line_width,
        sub("^#", "c", line_color)
      )
      out <- tabularray_insert(out, content = tmp, type = "inner")
    }
    if (!is.null(jline)) {
      tmp <- sprintf(
        "vline{%s}={%s}{solid, %s, %s},",
        paste(jline, collapse = ","),
        paste(ival + x@nhead, collapse = ","),
        line_width,
        sub("^#", "c", line_color)
      )
      out <- tabularray_insert(out, content = tmp, type = "inner")
    }
  }

  out <- tabularray_insert(out, content = tabularray_inner, type = "inner")
  out <- tabularray_insert(out, content = tabularray_outer, type = "outer")

  x@table_string <- out

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

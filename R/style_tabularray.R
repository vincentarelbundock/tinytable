style_tabularray <- function(x,
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
                             line = NULL,
                             line_color = "black",
                             line_width = .1,
                             colspan = NULL,
                             indent = 0,
                             tabularray_inner = NULL,
                             tabularray_outer = NULL,
                             ...) {

  if (meta(x, "output") != "latex") return(x)

  out <- x

  ival <- if (is.null(i)) seq_len(meta(x, "nrows")) else i
  jval <- if (is.null(j)) seq_len(meta(x, "ncols")) else j

  # order may be important for recycling 
  settings <- expand.grid(i = ival, j = jval, tabularray = "")
  if (is.null(i) && !is.null(j)) {
    settings <- settings[order(settings$i, settings$j), ]
  }

  # header index
  if ("i" %in% names(settings)) {
    settings$i <- settings$i + meta(out, "nhead")
  }

  # colspan requires cell level, so we keep the full settings DF
  if (is.null(colspan)) {
    if (is.null(i) && is.null(j)) {
      settings <- unique(settings[, c("i", "tabularray"), drop = FALSE])
    } else if (is.null(i)) {
      settings <- unique(settings[, c("j", "tabularray"), drop = FALSE])
    } else if (is.null(j)) {
      settings <- unique(settings[, c("i", "tabularray"), drop = FALSE])
    }
  }

  span <- if (!is.null(colspan)) paste0("c=", colspan, ",") else ""

  # convert to tabularray now that we've filled the bootstrap settings
  if (is.numeric(fontsize)) settings$tabularray <- sprintf("%s font=\\fontsize{%sem}{%sem}\\selectfont,", settings$tabularray, fontsize, fontsize + 0.3) 
  if (!is.null(align)) settings$tabularray <- sprintf("%s halign=%s,", settings$tabularray, align)
  if (!is.null(width)) settings$tabularray <- sprintf("%s wd={%s},", settings$tabularray, width)
  if (indent > 0) settings$tabularary <- sprintf("%s preto={\\hspace{%sem}},", settings$tabularray, indent)

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
  cols <- c(color, background)
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

  # Lines are not part of cellspec/rowspec/columnspec. Do this separately.
  if (!is.null(line)) {
    # TODO: handle hex colors
    # browser()
    iline <- jline <- NULL
    if (grepl("t", line)) iline <- c(iline, ival + meta(x, "nhead"))
    if (grepl("b", line)) iline <- c(iline, ival + meta(x, "nhead") + 1)
    if (grepl("l", line)) jline <- c(jline, jval)
    if (grepl("r", line)) jline <- c(jline, jval + 1)
    iline <- unique(iline)
    jline <- unique(jline)
    line_width <- paste0(line_width, "em")
    tmp <- sprintf(
      "hline{%s}={%s}{solid, %s, %s},",
      paste(iline, collapse = ","),
      paste(jval, collapse = ","),
      line_width,
      line_color
    )
    out <- tabularray_insert(out, content = tmp, type = "inner")
    tmp <- sprintf(
      "vline{%s}={%s}{solid, %s, %s},",
      paste(jline, collapse = ","),
      paste(ival + meta(x, "nhead"), collapse = ","),
      line_width,
      line_color
    )
    out <- tabularray_insert(out, content = tmp, type = "inner")
  }

  out <- tabularray_insert(out, content = tabularray_inner, type = "inner")
  out <- tabularray_insert(out, content = tabularray_outer, type = "outer")

  return(out)
}  

tabularray_insert <- function(x, content = NULL, type = "body") {
  if (is.null(content)) return(x)

  m <- meta(x)
  out <- strsplit(x, "\n")[[1]]
  comment <- switch(type,
  "body" = "% tabularray inner close",
  "outer" = "% tabularray outer close",
  "inner" = "% tabularray inner close")
  idx <- grep(comment, out)

  content <- trimws(content)
  if (!grepl(",$", content) && type != "body") {
    content <- paste0(content, ",")
  }

  if (type == "body") {
    out <- c(out[1:idx], content, out[(idx + 1):length(out)])
  } else {
    out <- c(out[1:(idx - 1)], content, out[idx:length(out)])
  }

  out <- paste(out, collapse = "\n")
  class(out) <- class(x)
  attr(out, "tinytable_meta") <- m
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

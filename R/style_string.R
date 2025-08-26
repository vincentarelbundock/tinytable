style_string_html <- function(n, styles) {
  if (isTRUE(styles[["italic"]])) {
    n <- sprintf("<i>%s</i>", n)
  }
  if (isTRUE(styles[["strikeout"]])) {
    n <- sprintf("<s>%s</s>", n)
  }
  if (isTRUE(styles[["underline"]])) {
    n <- sprintf("<u>%s</u>", n)
  }
  if (isTRUE(styles[["bold"]])) {
    n <- sprintf("<b>%s</b>", n)
  }
  if (isTRUE(styles[["monospace"]])) {
    n <- sprintf("<code>%s</code>", n)
  }
  if (!is.null(styles[["color"]])) {
    n <- sprintf("<span style='color:%s'>%s</span>", styles[["color"]], n)
  }
  if (!is.null(styles[["fontsize"]])) {
    n <- sprintf(
      "<span style='font-size:%sem'>%s</span>",
      styles[["fontsize"]],
      n
    )
  }
  n
}

style_string_latex <- function(n, styles) {
  if (isTRUE(styles[["italic"]])) {
    n <- sprintf("\\textit{%s}", n)
  }
  if (isTRUE(styles[["strikeout"]])) {
    n <- sprintf("\\sout{%s}", n)
  }
  if (isTRUE(styles[["underline"]])) {
    n <- sprintf("\\underline{%s}", n)
  }
  if (isTRUE(styles[["bold"]])) {
    n <- sprintf("\\textbf{%s}", n)
  }
  if (isTRUE(styles[["monospace"]])) {
    n <- sprintf("\\texttt{%s}", n)
  }
  if (!is.null(styles[["color"]])) {
    n <- sprintf("\\textcolor{%s}{%s}", styles[["color"]], n)
  }
  if (!is.null(styles[["fontsize"]])) {
    n <- sprintf(
      "{\\fontsize{%sem}{%sem}\\selectfont %s}",
      styles[["fontsize"]],
      styles[["fontsize"]],
      n
    )
  }
  n
}

style_string_typst <- function(n, styles) {
  sty <- NULL
  if (isTRUE(styles[["italic"]])) {
    sty <- c(sty, 'style: "italic"')
  }
  if (isTRUE(styles[["bold"]])) {
    sty <- c(sty, 'weight: "bold"')
  }
  if (isTRUE(styles[["strikeout"]])) {
    # not sure how to do this
  }
  if (isTRUE(styles[["underline"]])) {
    # not sure how to do this
  }
  if (!is.null(styles[["fontsize"]])) {
    fs <- sprintf("size: %sem", styles[["fontsize"]])
    sty <- c(sty, fs)
  }
  if (!is.null(styles[["color"]])) {
    col <- styles[["color"]]
    if (grepl("^#", col)) {
      col <- sprintf('rgb("%s")', col)
    }
    col <- sprintf("fill: %s", col)
    sty <- c(sty, col)
  }
  template <- paste0("text(", paste(sty, collapse = ", "), ", [%s])")
  out <- sprintf(template, n)
  out <- sub("text(, ", "text(", out, fixed = TRUE)
  return(out)
}

style_notes <- function(x) {
  fun <- switch(
    x@output,
    "typst" = style_string_typst,
    "html" = style_string_html,
    "html_portable" = style_string_html,
    "latex" = style_string_latex,
    function(k, ...) identity(k)
  )
  for (i in seq_along(x@notes)) {
    if (length(x@notes[[i]]) == 3 && "text" %in% names(x@notes[[i]])) {
      x@notes[[i]][["text"]] <- fun(x@notes[[i]][["text"]], x@style_notes)
    } else {
      x@notes[[i]] <- fun(x@notes[[i]], x@style_notes)
    }
  }
  return(x)
}

style_string_markdown <- function(n, styles) {
  out <- n
  if (isTRUE(styles[["bold"]])) {
    out <- sprintf("**%s**", out)
  }
  if (isTRUE(styles[["italic"]])) {
    out <- sprintf("_%s_", out)
  }
  if (isTRUE(styles[["underline"]])) {
    out <- sprintf("<u>%s</u>", out)
  }
  if (isTRUE(styles[["strikeout"]])) {
    out <- sprintf("~~%s~~", out)
  }
  if (!is.null(styles[["indent"]]) && !is.na(styles[["indent"]])) {
    out <- sprintf("%s%s", strrep(" ", styles[["indent"]]), out)
  }
  return(out)
}

style_string_ansi <- function(n, styles) {
  out <- n
  if (isTRUE(styles[["bold"]])) {
    out <- sprintf("\033[1m%s\033[22m", out)
  }
  if (isTRUE(styles[["italic"]])) {
    out <- sprintf("\033[3m%s\033[23m", out)
  }
  if (isTRUE(styles[["underline"]])) {
    out <- sprintf("\033[4m%s\033[24m", out)
  }
  if (isTRUE(styles[["strikeout"]])) {
    out <- sprintf("\033[9m%s\033[29m", out)
  }
  if (!is.null(styles[["color"]]) && !is.na(styles[["color"]])) {
    ansi_color_code <- standardize_colors(styles[["color"]], format = "ansi")
    if (!is.na(ansi_color_code) && ansi_color_code != styles[["color"]]) {
      out <- sprintf("\033[%sm%s\033[39m", ansi_color_code, out)
    }
  }
  if (!is.null(styles[["background"]]) && !is.na(styles[["background"]])) {
    ansi_bg_code <- standardize_colors(styles[["background"]], format = "ansi")
    if (!is.na(ansi_bg_code) && ansi_bg_code != styles[["background"]]) {
      # Convert foreground ANSI code to background by replacing 38 with 48
      if (grepl("^38;2;", ansi_bg_code)) {
        ansi_bg_code <- sub("^38;2;", "48;2;", ansi_bg_code)
      } else if (grepl("^3[0-7]$", ansi_bg_code)) {
        # Convert standard foreground colors (30-37) to background (40-47)
        fg_code <- as.numeric(ansi_bg_code)
        ansi_bg_code <- as.character(fg_code + 10)
      } else if (grepl("^9[0-7]$", ansi_bg_code)) {
        # Convert bright foreground colors (90-97) to bright background (100-107)
        fg_code <- as.numeric(ansi_bg_code)
        ansi_bg_code <- as.character(fg_code + 10)
      }
      out <- sprintf("\033[%sm%s\033[49m", ansi_bg_code, out)
    }
  }
  if (!is.null(styles[["indent"]]) && !is.na(styles[["indent"]])) {
    out <- sprintf("%s%s", strrep(" ", styles[["indent"]]), out)
  }
  return(out)
}

style_notes <- function(x) {
  fun <- switch(
    x@output,
    "typst" = style_string_typst,
    "html" = style_string_html,
    "html_portable" = style_string_html,
    "latex" = style_string_latex,
    "markdown" = style_string_markdown,
    "gfm" = style_string_markdown,
    function(k, ...) identity(k)
  )
  # Check if we should use ANSI styling for grid output
  if (x@output %in% c("markdown", "gfm") && isTRUE(x@ansi)) {
    fun <- style_string_ansi
  }
  for (i in seq_along(x@notes)) {
    if (length(x@notes[[i]]) == 3 && "text" %in% names(x@notes[[i]])) {
      x@notes[[i]][["text"]] <- fun(x@notes[[i]][["text"]], x@style_notes)
    } else {
      x@notes[[i]] <- fun(x@notes[[i]], x@style_notes)
    }
  }
  return(x)
}

style_caption <- function(x) {
  fun <- switch(
    x@output,
    "typst" = style_string_typst,
    "html" = style_string_html,
    "html_portable" = style_string_html,
    "latex" = style_string_latex,
    "markdown" = style_string_markdown,
    "gfm" = style_string_markdown,
    function(k, ...) identity(k)
  )
  # Check if we should use ANSI styling for grid output
  if (x@output %in% c("markdown", "gfm") && isTRUE(x@ansi)) {
    fun <- style_string_ansi
  }
  if (length(x@caption) > 0) {
    x@caption <- fun(x@caption, x@style_caption)
  }
  return(x)
}

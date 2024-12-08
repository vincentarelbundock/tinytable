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
        n <- sprintf("<span style='font-size:%sem'>%s</span>", styles[["fontsize"]], n)
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
        n <- sprintf("{\\fontsize{%sem}{%sem}\\selectfont %s}", styles[["fontsize"]], styles[["fontsize"]], n)
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
        if (grepl("^#", col)) col <- sprintf('rgb("%s")', col)
        col <- sprintf("fill: %s", col)
        sty <- c(sty, col)
    }
    template <- paste0("text(", paste(sty, collapse = ", "), ", [%s])")
    out <- sprintf(template, n)
    return(out)
}



style_notes <- function(x) {
    fun <- switch(x@output,
        "typst" = style_string_typst,
        "html" = style_string_html,
        "html_portable" = style_string_html,
        "latex" = style_string_latex,
        function(k, ...) identity(k)
    )
    x@notes <- lapply(x@notes, fun, x@style_notes)
    return(x)
}

style_notes <- function(x) {
    fun <- switch(x@output,
        "typst" = style_string_typst,
        "html" = style_string_html,
        "html_portable" = style_string_html,
        "latex" = style_string_latex,
        function(k, ...) identity(k)
    )
    x@notes <- lapply(x@notes, fun, x@style_notes)
    return(x)
}

style_caption <- function(x) {
    fun <- switch(x@output,
        "typst" = style_string_typst,
        "html" = style_string_html,
        "html_portable" = style_string_html,
        "latex" = style_string_latex,
        function(k, ...) identity(k)
    )
    if (length(x@caption) > 0) {
        x@caption <- fun(x@caption, x@style_caption)
    }
    return(x)
}

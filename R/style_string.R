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
        n <- sprintf("{\\fontsize{%s}{%s}\\selectfont %s}", styles[["fontsize"]], styles[["fontsize"]], n)
    }
    n
}

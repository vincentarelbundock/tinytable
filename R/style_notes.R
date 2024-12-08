setGeneric(
    name = "style_notes",
    def = function(x, ...) standardGeneric("style_notes")
)

# default method
setMethod(
    f = "style_notes",
    signature = "ANY",
    definition = function(x, ...) {
        return(x)
    })


# HTML: bootstrap
setMethod(
    f = "style_notes",
    signature = "tinytable_bootstrap",
    definition = function(x, ...) {
        styles <- x@style_notes
        x@notes <- lapply(x@notes, style_string_html, styles)
        return(x)
    })


# LaTeX: tabularray
setMethod(
    f = "style_notes",
    signature = "tinytable_tabularray",
    definition = function(x, ...) {
        styles <- x@style_notes
        x@notes <- lapply(x@notes, style_string_html, styles)
        return(x)
    })


# LaTeX: tabularray
setMethod(
    f = "style_notes",
    signature = "tinytable_typst",
    definition = function(x, ...) {
        styles <- x@style_notes

        if (length(x@notes) == 0) {
            return(x)
        }

        sty <- NULL
        if (isTRUE(styles[["italic"]])) {
            sty <- c(sty, 'style: "italic"')
        }

        if (isTRUE(styles[["bold"]])) {
            sty <- c(sty, 'weight: "bold"')
        }

        template <- paste0("text(", paste(sty, collapse = ", "), ", [%s])")
        x@notes <- lapply(x@notes, function(k) sprintf(template, k))

        return(x)
    })



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
        n <- sprintf("<span style='font-size:%s'>%s</span>", styles[["fontsize"]], n)
    }
    n
}

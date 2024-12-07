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
        if (isTRUE(styles[["italic"]])) {
            x@notes <- lapply(x@notes, function(n) sprintf("<i>%s</i>", n))
        }
        if (isTRUE(styles[["bold"]])) {
            x@notes <- lapply(x@notes, function(n) sprintf("<b>%s</b>", n))
        }
        return(x)
    })


# LaTeX: tabularray
setMethod(
    f = "style_notes",
    signature = "tinytable_tabularray",
    definition = function(x, ...) {
        styles <- x@style_notes
        if (isTRUE(styles[["italic"]])) {
            x@notes <- lapply(x@notes, function(n) sprintf("\\emph{%s}", n))
        }
        if (isTRUE(styles[["bold"]])) {
            x@notes <- lapply(x@notes, function(n) sprintf("\\textbf{%s}", n))
        }
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

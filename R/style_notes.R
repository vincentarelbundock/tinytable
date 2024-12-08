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
        x@notes <- lapply(x@notes, style_string_latex, styles)
        return(x)
    })


# Typst
setMethod(
    f = "style_notes",
    signature = "tinytable_typst",
    definition = function(x, ...) {
        styles <- x@style_notes
        x@notes <- lapply(x@notes, style_string_typst, styles)
        return(x)
    })

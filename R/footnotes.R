footnote_markers <- function(x) {
    notes <- meta(x, "notes")
    for (idx in seq_along(notes)) {
        n <- notes[[idx]]
        sup <- names(notes)[idx]
        if (is.list(n)) {
            if (meta(x)$output == "latex") {
                x[n$i, n$j] <- paste0(x[n$i, n$j], "\\textsuperscript{", sup, "}")
                if (0 %in% n$i) colnames(x)[n$j] <- paste0(colnames(x)[n$j], "\\textsuperscript{", sup, "}")
            } else if (meta(x)$output == "html") {
                x[n$i, n$j] <- paste0(x[n$i, n$j], "<sup>", sup, "</sup>")
                if (0 %in% n$i) colnames(x)[n$j] <- paste0(colnames(x)[n$j], "<sup>", sup, "</sup>")
            } else if (meta(x)$output == "typst") {
                x[n$i, n$j] <- paste0(x[n$i, n$j], "#super[", sup, "]")
                if (0 %in% n$i) colnames(x)[n$j] <- paste0(colnames(x)[n$j], "#super[", sup, "]")
            } else {
                x[n$i, n$j] <- paste0(x[n$i, n$j], "^", sup, "^")
                if (0 %in% n$i) colnames(x)[n$j] <- paste0(colnames(x)[n$j], "^", sup, "^")
            }
        }
    }
    return(x)
}
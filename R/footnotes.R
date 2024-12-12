footnote_markers <- function(x) {
  notes <- x@notes
  tab <- x@table_dataframe
  for (idx in seq_along(notes)) {
    n <- notes[[idx]]
    sup <- names(notes)[idx]
    if (is.list(n)) {
      if (x@output == "latex") {
        tab[n$i, n$j] <- paste0(tab[n$i, n$j], "\\textsuperscript{", sup, "}")
        if (0 %in% n$i) colnames(tab)[n$j] <- paste0(colnames(tab)[n$j], "\\textsuperscript{", sup, "}")
        if (0 %in% n$i) x@names[n$j] <- paste0(x@names[n$j], "\\textsuperscript{", sup, "}")
      } else if (x@output == "html") {
        tab[n$i, n$j] <- paste0(tab[n$i, n$j], "<sup>", sup, "</sup>")
        if (0 %in% n$i) colnames(tab)[n$j] <- paste0(colnames(tab)[n$j], "<sup>", sup, "</sup>")
        if (0 %in% n$i) x@names[n$j] <- paste0(x@names[n$j], "<sup>", sup, "</sup>")
      } else if (x@output == "typst") {
        tab[n$i, n$j] <- paste0(tab[n$i, n$j], "#super[", sup, "]")
        if (0 %in% n$i) colnames(tab)[n$j] <- paste0(colnames(tab)[n$j], "#super[", sup, "]")
        if (0 %in% n$i) x@names[n$j] <- paste0(x@names[n$j], "#super[", sup, "]")
      } else {
        tab[n$i, n$j] <- paste0(tab[n$i, n$j], "^", sup, "^")
        if (0 %in% n$i) colnames(tab)[n$j] <- paste0(colnames(tab)[n$j], "^", sup, "^")
        if (0 %in% n$i) x@names[n$j] <- paste0(x@names[n$j], "^", sup, "^")
      }
    }
  }
  x@table_dataframe <- tab
  return(x)
}

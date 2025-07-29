footnote_markers <- function(x) {
  notes <- x@notes
  tab <- x@body_data

  # Define superscript syntax for each output format
  superscript_syntax <- list(
    latex = "\\textsuperscript{%s}",
    html = "<sup>%s</sup>",
    typst = "#super[%s]",
    markdown = "^%s^"
  )

  # Get the appropriate superscript format
  sup_format <- superscript_syntax[[x@output]] %||%
    superscript_syntax[["markdown"]]

  # Helper function to add superscript to a value
  add_superscript <- function(value, sup) {
    paste0(value, sprintf(sup_format, sup))
  }

  # Helper function to update column names and x@names
  update_column_names <- function(x, n, sup) {
    if (0 %in% n$i) {
      colnames(tab)[n$j] <- add_superscript(colnames(tab)[n$j], sup)
      x@names[n$j] <- add_superscript(x@names[n$j], sup)
    }
    return(x)
  }

  for (idx in seq_along(notes)) {
    n <- notes[[idx]]
    sup <- names(notes)[idx]

    if (is.list(n)) {
      # Add superscript to table cells
      tab[n$i, n$j] <- add_superscript(tab[n$i, n$j], sup)

      # Update column names and x@names if needed
      x <- update_column_names(x, n, sup)
    }
  }

  x@body_data <- tab
  return(x)
}


finalize_grid <- function(x) {
    if (meta(x)$output != "markdown") return(x)

    out <- x

    # formal grid specification in pandoc includes lines everywhere
    # important for docx output
    hlines <- getOption("tinytable_grid_hlines", default = TRUE)
    if (isTRUE(hlines)) {
      out <- grid_hlines(out)
    }

    cap <- meta(x, "caption")
    if (is.character(cap) && length(cap) == 1) {
        out <- paste0(out, "\n", "Table: ", cap, "\n")
    }

    return(out)
}
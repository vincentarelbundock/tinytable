theme_resize <- function(x, width = 1, ...) {
    assert_class(x, "tinytable")
    assert_numeric(width, len = 1, lower = 0.01, upper = 1)
    fn <- function(table) {
        if (!isTRUE(table@output == "latex")) return(table)

        lines <- table@table_string
        lines <- strsplit(lines, "\n")[[1]]

        idx <- grep("\\begin{tblr}", lines, fixed = TRUE)
        lines <- c(
            lines[1:(idx - 1)],
            sprintf("\\resizebox{%s\\linewidth}{!}{", width),
            lines[idx:length(lines)])

        idx <- grep("\\end{tblr}", lines, fixed = TRUE)
        lines <- c(
            lines[1:idx],
            "}",
            lines[(idx + 1):length(lines)])

        table@table_string <- paste(lines, collapse = "\n")

        return(table)
    }

    x <- style_tt(x, finalize = fn)

    return(x)
}


#' Themes for tinytable
#' 
#' @param x A `tinytable` object
#' @param theme String. Name of the theme to apply.
#'   + "resize" (LaTeX)
#'   + "multipage" (LaTeX & Typst)
#'   + "scrollbox" (HTML)
#'   + "tooltip" (HTML)
#' @param ... Additional arguments passed the themeing function. See the "Arguments" section below for a list of supported arguments for each theme.
#' 
#' @section Themes:
#' 
#'   + resize: Resize a `tinytable` to fit the line width, while maintaining proportions
#' 
#' @section Arguments:
#' 
#' resize
#' 
#' + `width`: A numeric value between 0.01 and 1, representing the proportion of the line width to use
#' 
#' @export
#' @return A modified `tinytable` object
theme_tt <- function(x, theme, ...) {
    assert_choice(theme, c("resize"))

    fn <- switch(theme,
        resize = theme_resize)

    out <- fn(x, ...)

    return(out)
}


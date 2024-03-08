theme_default <- function(x, ...) {
    fn <- function(table) {
        if (isTRUE(table@output == "typst")) {
            table <- style_eval(table, i = 0 - x@nhead, line = "t", line_width = .1)
            table <- style_eval(table, i = 0, line = "b", line_width = .05)
            table <- style_eval(table, i = nrow(x), line = "b", line_width = .1)
        }

        return(table)
    }
    x <- style_tt(x, finalize = fn)
    return(x)
}


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


theme_void <- function(x, ...) {
    assert_class(x, "tinytable")
    fn <- function(table) {
        if (isTRUE(table@output == "latex")) {
            s <- table@table_string
            s <- gsub("\\\\toprule|\\\\bottomrule|\\\\midrule", "", s)
            l <- strsplit(s, "\n")[[1]]
            l <- l[which(trimws(l) != "")]
            table@table_string <- paste(l, collapse = "\n")
        }
        return(table)
    }
    x <- style_tt(x, finalize = fn)
    return(x)
}


theme_grid <- function(x, ...) {
    assert_class(x, "tinytable")
    fn <- function(table) {
        if (isTRUE(table@output == "typst")) {
            table@table_string <- sub(
                "auto-lines: false,",
                "auto-lines: true,",
                table@table_string)
        }
        return(table)
    }
    x <- theme_tt(x, theme = "void") # only affects LaTeX
    x <- style_tt(x, tabularray_inner = "hlines, vlines,")
    return(x)
}


theme_striped <- function(x, ...) {
    assert_class(x, "tinytable")
    fn <- function(table) {
        if (isTRUE(table@output == "typst")) {
            x <- table
            x <- style_eval(x, i = 1 - x@nhead, line = "t", line_width = .1)
            x <- style_eval(x, i = 0, line = "b", line_width = .05)
            x <- style_eval(x, i = nrow(x), line = "b", line_width = .1)
            x <- style_eval(x, i = seq(1, nrow(x), by = 2), background = "#ededed")
            table <- x
        }
        return(table)
    }
    x <- style_tt(x, finalize = fn, tabularray_inner = "row{even}={bg=black!5!white}")
    return(x)
}


theme_bootstrap <- function(x, ...) {
    assert_class(x, "tinytable")
    x <- theme_tt(x, theme = "void") # only affects LaTeX
    x <- style_tt(x, tabularray_inner = "hlines={gray8},")
    return(x)
}


theme_multipage <- function(x, rowhead = 0, rowfoot = 0, ...) {
    assert_class(x, "tinytable")
    assert_integerish(rowhead, lower = 0, len = 1)
    assert_integerish(rowfoot, lower = 0, len = 1)
    cap <- sprintf("caption={%s}", x@caption)
    x@caption <- ""
    fn <- function(table) {
        if (!isTRUE(table@output == "latex")) return(table)

        tab <- table@table_string
        tab <- sub("\\\\begin\\{tblr", "\\\\begin\\{longtblr", tab)
        tab <- sub("\\\\end\\{tblr", "\\\\end\\{longtblr", tab)

        tab <- strsplit(tab, "\n")[[1]]
        idx <- grepl("^\\\\caption\\{|^\\\\begin\\{table|^\\\\end\\{table|^\\\\centering", trimws(tab))
        tab <- tab[!idx]
        tab <- paste(tab, collapse = "\n")

        table@table_string <- tab

        table <- style_eval(table, tabularray_outer = cap)

        if (rowhead > 0) {
            table <- style_eval(table, tabularray_inner = sprintf("rowhead=%s", rowhead))
        }

        if (rowfoot > 0) {
            table <- style_eval(table, tabularray_inner = sprintf("rowfoot=%s", rowfoot))
        }

        return(table)
    }
    x <- style_tt(x, finalize = fn)
    return(x)
}


theme_dictionary <- list(
    "default" = theme_default,
    "grid" = theme_grid,
    "resize" = theme_resize,
    "multipage" = theme_multipage,
    "striped" = theme_striped,
    "void" = theme_void,
    "bootstrap" = theme_bootstrap
)



#' Themes for tinytable
#' 
#' @description 
#' Themes are functions that apply a collection of transformations to a `tinytable` object. Whereas the other functions in `tinytable` are designed to be output-agnostic, themes can be output-specific, only applying to LaTeX or HTML, as needed. Themes can also have their own arguments
#'
#' @param x A `tinytable` object
#' @param theme String. Name of the theme to apply.
#'   + "resize" (LaTeX)
#'   + "multipage" (LaTeX)
#' @param ... Additional arguments passed the themeing function. See the "Arguments" section below for a list of supported arguments for each theme.
#' 
#' @section Themes:
#' 
#'   + resize: Resize a `tinytable` to fit the line width, while maintaining proportions
#'   + multipage: Long tables continue on the next page
#' 
#' @section Arguments:
#' 
#' resize
#' 
#' + `width`: A numeric value between 0.01 and 1, representing the proportion of the line width to use
#' 
#' multipage
#' 
#' + `rowhead`: Non-negative integer. The number of header rows to repeat on each page.
#' + `rowfoot`: Non-negative integer. The number of footer rows to repeat on each page.
#' 
#' @export
#' @return A modified `tinytable` object
theme_tt <- function(x, theme, ...) {
    td <- getOption("tinytable_themes", default = theme_dictionary)
    na <- unique(c("default", sort(names(td))))
    assert_choice(theme, na)
    fn <- td[[theme]]
    out <- fn(x, ...)
    return(out)
}


theme_default <- function(x, ...) {
    fn <- function(table) {
        if (isTRUE(table@output == "typst")) {
            table <- style_eval(table, i = 1 - table@nhead, line = "t", line_width = .1)
            table <- style_eval(table, i = 0, line = "b", line_width = .05)
            table <- style_eval(table, i = nrow(table), line = "b", line_width = .1)
        }

        return(table)
    }
    x <- style_tt(x, finalize = fn)
    x <- theme_tt(x, "placement")
    return(x)
}


theme_tabular <- function(x, ...) {
    assert_class(x, "tinytable")
    # do not change the default theme
    if (identical(x@theme[[1]], "tabular")) x@theme <- list("default")
    fn <- function(table) {
        tab <- table@table_string

        if (isTRUE(table@output == "latex")) {
            tab <- lines_drop(tab, regex = "\\\\begin\\{table\\}", position = "before")
            tab <- lines_drop(tab, regex = "\\\\begin\\{table\\}", position = "equal")
            tab <- lines_drop(tab, regex = "\\\\end\\{table\\}", position = "after")
            tab <- lines_drop(tab, regex = "\\\\end\\{table\\}", position = "equal")
            tab <- lines_drop(tab, regex = "\\\\centering", position = "equal")

        } else if (isTRUE(table@output == "html")) {
            tab <- lines_drop(tab, regex = "<table class", position = "before")
            tab <- lines_drop(tab, regex = "<\\/table>", position = "after")

        } else if (isTRUE(table@output == "typst")) {
            tab <- lines_drop(tab, regex = "tablex\\(", position = "before")
            tab <- lines_drop(tab, regex = "\\/\\/ end tablex", position = "after")
        }

        table@table_string <- tab
        return(table)
    }
    x <- style_tt(x, finalize = fn)
    return(x)
}


theme_resize <- function(x, width = 1, ...) {
    assert_class(x, "tinytable")
    assert_numeric(width, len = 1, lower = 0.01, upper = 1)
    # do not change the default theme
    if (identical(x@theme[[1]], "resize")) x@theme <- list("default")
    fn <- function(table) {
        if (!isTRUE(table@output == "latex")) return(table)

        tab <- table@table_string

        new <- sprintf("\\resizebox{%s\\linewidth}{!}{", width)
        reg <- "\\\\begin\\{tblr\\}"
        tab <- lines_insert(tab, regex = reg, new = new, position = "before")

        new <- "}"
        reg <- "\\\\end\\{tblr\\}"
        tab <- lines_insert(tab, regex = reg, new = new, position = "after")

        table@table_string <- tab

        return(table)
    }

    x <- style_tt(x, finalize = fn)
    x <- theme_tt(x, "placement")
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
        } else if (isTRUE(table@output == "markdown")) {
            tab <- table@table_string
            tab <- strsplit(tab, "\n")[[1]]
            tab <- tab[!grepl("^[\\+|-]+$", tab)]
            tab <- tab[!grepl("^[\\+|=]+$", tab)]
            tab <- gsub("|", " ", tab, fixed = TRUE)
            table@table_string <- paste(tab, collapse = "\n")
        }
        return(table)
    }
    x <- style_tt(x, finalize = fn)
    x <- theme_tt(x, "placement")
    return(x)
}


theme_grid <- function(x, ...) {
    assert_class(x, "tinytable")
    fn <- function(table) {
        if (isTRUE(table@output == "latex")) {
            s <- table@table_string
            s <- lines_drop(s, regex = "\\\\bottomrule", position = "equal")
            s <- lines_drop(s, regex = "\\\\midrule", position = "equal")
            s <- lines_drop(s, regex = "\\\\toprule", position = "equal")
            table@table_string <- s
        } else if (isTRUE(table@output == "typst")) {
            table@table_string <- sub(
                "auto-lines: false,",
                "auto-lines: true,",
                table@table_string)
        }
        return(table)
    }
    x <- style_tt(x, tabularray_inner = "hlines, vlines,", finalize = fn)
    x <- theme_tt(x, "placement")
    return(x)
}


theme_striped <- function(x, ...) {
    assert_class(x, "tinytable")
    fn <- function(table) {
        if (isTRUE(table@output == "typst")) {
            table <- style_eval(table, i = 1 - table@nhead, line = "t", line_width = .1)
            table <- style_eval(table, i = 0, line = "b", line_width = .05)
            table <- style_eval(table, i = nrow(table), line = "b", line_width = .1)
            table <- style_eval(table, i = seq(1, nrow(table), by = 2), background = "#ededed")
        }
        return(table)
    }
    x <- style_tt(x, finalize = fn, tabularray_inner = "row{even}={bg=black!5!white}")
    x <- theme_tt(x, "placement")
    return(x)
}


theme_bootstrap <- function(x, ...) {
    assert_class(x, "tinytable")
    fn <- function(table) {
        if (isTRUE(table@output == "markdown")) {
            tab <- table@table_string
            tab <- strsplit(tab, "\n")[[1]]
            tab <- tab[!grepl("^[\\+|-]+$", tab)]
            tab <- gsub("|", " ", tab, fixed = TRUE)
            table@table_string <- paste(tab, collapse = "\n")
        }
        return(table)
    }
    x <- theme_tt(x, theme = "void") # only affects LaTeX
    x <- style_tt(x, tabularray_inner = "hlines={gray8},", finalize = fn)
    return(x)
}


theme_placement <- function(x, latex_float = getOption("tinytable_theme_placement_latex_float", default = NULL)) {
    assert_string(latex_float, null.ok = TRUE)
    # do not change the defaul theme
    if (identical(x@theme[[1]], "placement")) x@theme <- list("default")
    fn <- function(table) {
        if (table@output == "latex" && !is.null(latex_float)) {
            tab <- table@table_string
            template <- sub(
                "\\\\begin\\{table\\}",
                sprintf("\\\\begin{table}[%s]", latex_float),
                tab)
            table@table_string <- template
        }
        return(table)
    }
    x <- style_tt(x, finalize = fn)
    return(x)
}


theme_multipage <- function(x, rowhead = 0, rowfoot = 0, ...) {
    assert_class(x, "tinytable")
    # do not change the defaul theme
    if (identical(x@theme[[1]], "multipage")) x@theme <- list("default")
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
    "placement" = theme_placement,
    "striped" = theme_striped,
    "void" = theme_void,
    "bootstrap" = theme_bootstrap,
    "tabular" = theme_tabular
)



#' Themes for `tinytable`
#' 
#' @description 
#' A theme is a function which applies a collection of transformations to a `tinytable` object. Whereas the other `tinytable` functions such as `format_tt()` and `style_tt()` aim to be output-agnostic, themes can be output-specific, only applying to LaTeX, HTML, or Typst, as needed. 
#' 
#' Each theme can have specific arguments, which are passed to the `theme_tt()` function. See the "Arguments" section below.
#'
#' @param x A `tinytable` object
#' @param theme String. Name of the theme to apply. One of: 
#'   + "grid": Vertical and horizontal rules around each cell.
#'   + "void": No rules
#'   + "bootstrap": Similar appearance to the default Bootstrap theme in HTML
#'   + "striped": Grey stripes on alternating rows
#'   + "tabular": No table environment (LaTeX) or Javascript/CSS (HTML)
#'   + "resize": Scale a LaTeX `tinytable` to fit the `width` argument.
#'   + "multipage": Long tables continue on the next page (LaTeX only)
#'   + "placement": Position of the table environment (LaTeX)
#' @param ... Additional arguments passed the themeing function. See the "Arguments" section below for a list of supported arguments for each theme.
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
#' placement
#' 
#' + `latex_float`: String to insert in square brackets after the LaTeX table environment, ex: "H", "htbp". The default value is controlled by a global option:
#'    - `options("tinytable_theme_placement_latex_float" = "H")`
#' 
#' @examples
#' library(tinytable)
#' 
#' x <- mtcars[1:4, 1:4]
#' 
#' # equivalent calls
#' tt(x, theme = "striped")
#' 
#' tt(x) |> theme_tt("striped")
#' 
#' # resize w/ argument
#' x <- cbind(mtcars[1:10,], mtcars[1:10,])
#' tt(x) |>
#'   theme_tt("resize", width = .9) |>
#'   print("latex")
#' 
#' @return A modified `tinytable` object
#' @export
theme_tt <- function(x, theme, ...) {
    if (is.null(theme)) return(x)
    if (is.function(theme)) return(theme(x, ...))
    td <- getOption("tinytable_themes", default = theme_dictionary)
    na <- unique(sort(names(td)))
    assert_choice(theme, na)
    fn <- td[[theme]]
    out <- fn(x, ...)
    return(out)
}


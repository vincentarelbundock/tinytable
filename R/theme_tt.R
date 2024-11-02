theme_default <- function(x, ...) {
    if (isTRUE(x@output == "html")) {
      x <- style_tt(x, 
          bootstrap_class = "table table-borderless",
          i = nrow(x), 
          line = "b", 
          line_color = "#d3d8dc", 
          line_width = 0.1)
      x <- style_tt(x, 
          bootstrap_class = "table table-borderless",
          i = 0, 
          line = "bt", 
          line_color = "#d3d8dc", 
          line_width = 0.1)
    }
    x <- theme_tt(x, "placement")
    return(x)
}


theme_tabular <- function(x, 
                          style = get_option("tinytable_theme_tabular_style", "tabular"), 
                          ...) {
    assert_class(x, "tinytable")

    assert_choice(style, c("tabular", "tabularray"))

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
            if (style == "tabular") {
                tab <- lines_drop_between(tab, regex_start = "tabularray outer open", regex_end = "tabularray inner close")
                tab <- lines_drop(tab, regex = "tabularray outer close", position = "equal")
                tab <- lines_drop(tab, regex = "tabularray inner open", position = "equal")
                tab <- lines_drop(tab, regex = "tabularray inner close", position = "equal")
                tab <- lines_drop(tab, regex = "^colspec=\\{", position = "equal")
                tab <- gsub("cmidrule\\[(.*?)\\]", "cmidrule(\\1)", tab)
                tab <- gsub("\\{tblr\\}\\[*", "{tabular}", tab)
                tab <- gsub("\\{talltblr\\}\\[", "{tabular}", tab)
                tab <- gsub("\\{talltblr\\}", "{tabular}", tab)
                tab <- gsub("\\{longtblr\\}\\[", "{tabular}", tab)
                tab <- gsub("\\{longtblr\\}", "{tabular}", tab)
                tab <- gsub("\\\\toprule|\\\\midrule|\\\\bottomrule", "\\\\hline", tab)
                tab <- sub("\\s*%% tabularray outer open", "", tab)
                tab <- sub("\\s*%% TinyTableHeader", "", tab)
                # align
                a <- sprintf("begin{tabular}{%s}", strrep("l", ncol(table)))
                tab <- sub("begin{tabular}", a, tab, fixed = TRUE)
            }

        } else if (isTRUE(table@output == "html")) {
            tab <- lines_drop(tab, regex = "<table class", position = "before")
            tab <- lines_drop(tab, regex = "<\\/table>", position = "after")

        } else if (isTRUE(table@output == "typst")) {
            tab <- lines_drop(tab, regex = "table\\(", position = "before")
            tab <- lines_drop(tab, regex = "\\/\\/ end table", position = "after")
        }

        table@table_string <- tab
        return(table)
    }
    x <- style_tt(x, finalize = fn)
    return(x)
}


theme_resize <- function(x, 
                         width = get_option("tinytable_theme_resize_width", 1), 
                         direction = get_option("tinytable_theme_resize_direction", "down"), 
                         ...) {
    assert_class(x, "tinytable")
    assert_numeric(width, len = 1, lower = 0.01, upper = 1)
    assert_choice(direction, c("down", "up", "both"))
    # do not change the default theme
    if (identical(x@theme[[1]], "resize")) x@theme <- list("default")
    fn <- function(table) {
        if (!isTRUE(table@output == "latex")) return(table)

        tab <- table@table_string

        if (direction == "both") {
          new <- sprintf("\\resizebox{%s\\linewidth}{!}{", width)
        } else if (direction == "down") {
          new <- sprintf("\\resizebox{\\ifdim\\width>\\linewidth %s\\linewidth\\else\\width\\fi}{!}{", width)
        } else if (direction == "up") {
          new <- sprintf("\\resizebox{\\ifdim\\width<\\linewidth %s\\linewidth\\else\\width\\fi}{!}{", width)
        }

        reg <- "\\\\begin\\{tblr\\}|\\\\begin\\{talltblr\\}"
        tab <- lines_insert(tab, regex = reg, new = new, position = "before")

        new <- "}"
        reg <- "\\\\end\\{tblr\\}|\\\\end\\{talltblr\\}"
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
        } else if (isTRUE(table@output == "typst")) {
            tab <- table@table_string
            tab <- lines_drop(tab, regex = "table.hline", position = "all", fixed = TRUE)
            table@table_string <- tab
        }
        return(table)
    }
    x <- style_tt(x, finalize = fn,
                  bootstrap_class = "table table-borderless")
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
                "stroke: none,",
                "stroke: (paint: black),",
                table@table_string)
        }
        return(table)
    }
    x <- style_tt(x, tabularray_inner = "hlines, vlines,", finalize = fn,
        bootstrap_class = "table table-bordered")
    x <- theme_tt(x, "placement")
    return(x)
}


theme_striped <- function(x, ...) {
    assert_class(x, "tinytable")
    x <- style_tt(x,
        tabularray_inner = "row{even}={bg=black!5!white}",
        bootstrap_class = "table table-striped",
        output = "latex")
    x <- style_tt(x, 
        i = seq(1, nrow(x), by = 2),
        background = "#ededed",
        output = "typst")
    x <- theme_tt(x, "default")
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
        } else if (isTRUE(table@output == "typst")) {
            table <- style_tt(table, i = 0:nrow(table), line = "bt", line_width = 0.05, line_color = "silver")
        }
        return(table)
    }
    x <- theme_tt(x, theme = "void") # only affects LaTeX
    x <- style_tt(x, tabularray_inner = "hlines={gray8},", finalize = fn)
    return(x)
}


theme_placement <- function(x, 
                            horizontal = get_option("tinytable_theme_placement_horizontal", default = NULL),
                            latex_float = get_option("tinytable_theme_placement_latex_float", default = NULL)) {
    # do not change the defaul theme
    if (identical(x@theme[[1]], "placement")) x@theme <- list("default")
    fn <- function(table) {
        tab <- table@table_string
        if (table@output == "latex" && !is.null(latex_float)) {
            assert_string(latex_float, null.ok = TRUE)
            tab <- sub(
                "\\\\begin\\{table\\}([^\\[])",
                sprintf("\\\\begin{table}[%s]\\1", latex_float),
                tab)
        } else if (table@output == "typst" && !is.null(horizontal)) {
            assert_choice(horizontal, c("l", "c", "r"))
            if (horizontal == "l") {
                tab <- sub("#align(center,", "#align(left,", tab, fixed = TRUE)
            } else if (horizontal == "r") {
                tab <- sub("#align(center,", "#align(right,", tab, fixed = TRUE)
            }
        }
        table@table_string <- tab
        return(table)
    }
    x <- style_tt(x, finalize = fn)
    return(x)
}


theme_multipage <- function(x, 
                            rowhead = get_option("tinytable_theme_multipage_rowhead", 0L), 
                            rowfoot = get_option("tinytable_theme_multipage_rowfoot", 0L), 
                            ...) {
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
        tab <- sub("\\\\begin\\{talltblr", "\\\\begin\\{longtblr", tab)
        tab <- sub("\\\\end\\{talltblr", "\\\\end\\{longtblr", tab)

        tab <- strsplit(tab, "\n")[[1]]
        idx <- grepl("^\\\\caption\\{|^\\\\begin\\{table|^\\\\end\\{table|^\\\\centering", trimws(tab))
        tab <- tab[!idx]
        tab <- paste(tab, collapse = "\n")

        table@table_string <- tab

        table <- style_tt(table, tabularray_outer = cap)

        if (rowhead > 0) {
            table <- style_tt(table, tabularray_inner = sprintf("rowhead=%s", rowhead))
        }

        if (rowfoot > 0) {
            table <- style_tt(table, tabularray_inner = sprintf("rowfoot=%s", rowfoot))
        }

        return(table)
    }
    x <- style_tt(x, finalize = fn)
    return(x)
}


theme_rotate <- function(x, angle = 90, ...) {
    assert_numeric(angle, len = 1, lower = 0, upper = 360)
    fn <- function(table) {
        if (isTRUE(table@output == "latex")) {
            rot <- sprintf("\\begin{table}\n\\rotatebox{%s}{", angle)
            table@table_string <- sub(
                "\\begin{table}",
                rot,
                table@table_string,
                fixed = TRUE)
            table@table_string <- sub(
                "\\end{table}",
                "}\n\\end{table}",
                table@table_string,
                fixed = TRUE)
        } else if (isTRUE(table@output == "typst")) {
            rot <- sprintf("#rotate(-%sdeg, reflow: true, [\n  #figure(", angle)
            table@table_string <- sub(
                "#figure(",
                rot,
                table@table_string,
                fixed = TRUE)
            table@table_string <- sub(
                ") // end figure",
                ") ]) // end figure",
                table@table_string,
                fixed = TRUE)
        }
        table <- style_tt(table, finalize = fn)
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
    "rotate" = theme_rotate,
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
#'   + "bootstrap": Similar appearance to the default Bootstrap theme in HTML
#'   + "grid": Vertical and horizontal rules around each cell.
#'   + "multipage": Long tables continue on the next page (LaTeX only)
#'   + "placement": Position of the table environment (LaTeX)
#'   + "rotate": Rotate a LaTeX or Typst table.
#'   + "resize": Scale a LaTeX `tinytable` to fit the `width` argument.
#'   + "striped": Grey stripes on alternating rows
#'   + "tabular": Remove table environment (LaTeX) or Javascript/CSS (HTML)
#'   + "void": No rules
#' @param ... Additional arguments passed the themeing function. See the "Arguments" section below for a list of supported arguments for each theme.
#' @section Arguments:
#' 
#' multipage
#' 
#' + `rowhead`: Non-negative integer. The number of header rows to repeat on each page.
#'   - Set globally with `options("tinytable_theme_multipage_rowhead" = 1L)`
#' + `rowfoot`: Non-negative integer. The number of footer rows to repeat on each page.
#'   - Set globally with `options("tinytable_theme_multipage_rowfoot" = 1L)`
#' 
#' tabular
#'
#' + `style`: 
#'   - "tabular": Drop all LaTeX dependencies and floating environments, except `\\begin{tabular}` 
#'   - "tabularray": Drop all LaTeX dependencies and floating environments, except `\\begin{tblr}` 
#'   - Set globally with `options("tinytable_theme_tabular_style" = "tblr")`
#'
#' placement
#' 
#' + `horizontal` (Typst only): "l", "c", or "r" to align the table horizontally in the page.
#'    - Set globally with `options("tinytable_theme_placement_horizontal" = "l")`
#' + `latex_float`: String to insert in square brackets after the LaTeX table environment, ex: "H", "htbp". The default value is controlled by a global option:
#'    - Set globally with `options("tinytable_theme_placement_latex_float" = "H")`
#' 
#' resize
#' 
#' + `width`: A numeric value between 0.01 and 1, representing the proportion of the line width to use
#'   - Set globally with `options("tinytable_theme_resize_width" = 0.9)`
#' + `direction`: "down", "up", "both" A string indicating if the table should be scaled in one direction. For example, "down" will only resize the table if it exceeds `\linewidth`
#'   - Set globally with `options("tinytable_theme_resize_direction" = "down")`
#' 
#' rotate
#' 
#' + `angle`: Angle of the rotation. For example, `angle=90`` applies a half counter-clockwise turn.
#' + Caveats: 
#'   - LaTeX and Typst only.
#'   - Typst: In Quarto documents, rotation does not work because Quarto takes over the figure environment.
#'   - LaTeX: In Quarto documents, captions must be specified using the `caption` argument in `tt()` rather than via Quarto chunk options. 
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
    td <- theme_dictionary
    na <- unique(sort(names(td)))
    assert_choice(theme, na)
    fn <- td[[theme]]
    out <- fn(x, ...)
    return(out)
}


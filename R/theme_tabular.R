theme_tabular <- function(x, 
                          style = get_option("tinytable_theme_tabular_style", "tabular"), 
                          ...) {

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
                if (!is.null(table@style$align)) {
                    alignment_string <- paste(table@style$align, collapse = "")
                    a <- sprintf("begin{tabular}{%s}", alignment_string)
                } else {
                    a <- sprintf("begin{tabular}{%s}", strrep("l", ncol(table)))
                }
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



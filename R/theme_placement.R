# Don't do much in here
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



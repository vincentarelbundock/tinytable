theme_placement_factory <- function(
    horizontal = get_option("tinytable_theme_placement_horizontal", default = NULL),
    latex_float = get_option("tinytable_theme_placement_latex_float", default = NULL)) {

    function(x) {
        tab <- x@table_string
        if (x@output == "latex" && !is.null(latex_float)) {
            assert_string(latex_float, null.ok = TRUE)
            tab <- sub(
                "\\\\begin\\{table\\}([^\\[])",
                sprintf("\\\\begin{table}[%s]\\1", latex_float),
                tab)
        } else if (x@output == "typst" && !is.null(horizontal)) {
            assert_choice(horizontal, c("l", "c", "r"))
            if (horizontal == "l") {
                tab <- sub("#align(center,", "#align(left,", tab, fixed = TRUE)
            } else if (horizontal == "r") {
                tab <- sub("#align(center,", "#align(right,", tab, fixed = TRUE)
            }
        }
        x@table_string <- tab
        return(x)
    }
}


theme_placement <- function(
    x, 
    horizontal = get_option("tinytable_theme_placement_horizontal", default = NULL),
    latex_float = get_option("tinytable_theme_placement_latex_float", default = NULL)) {
    fn <- theme_placement_factory(horizontal = horizontal, latex_float = latex_float)
    x <- style_tt(x, finalize = fn)
    return(x)
}



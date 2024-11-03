theme_grid <- function(x, ...) {
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
    return(x)
}

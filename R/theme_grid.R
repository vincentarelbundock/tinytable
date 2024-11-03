theme_grid <- function(x, ...) {
    fn <- function(table) {
        if (isTRUE(table@output == "latex")) {
            table <- theme_void_fn(table)
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

theme_bootstrap <- function(x, ...) {

    fn <- theme_placement_factory(
        horizontal = get_option("tinytable_theme_default_horizontal", "center"),
        latex_float = get_option("tinytable_theme_placement_latex_float", default = NULL))
    x <- style_tt(x, finalize = fn)

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

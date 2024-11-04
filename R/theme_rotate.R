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


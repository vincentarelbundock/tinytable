
customize_resize <- function(x) {
    assert_class(x, "tinytable")
    if (isTRUE(x@output == "latex")) {
        out <- x@table_string
        out <- sub("\\begin{tblr}", "\\resizebox{.8\\linewidth}{!}{\\begin{tblr}", out, fixed = TRUE)
        out <- sub("\\end{tblr}", "\\end{tblr}}", out, fixed = TRUE)
        x@table_string <- out
    }
    return(x)
}


customize_scroll <- function(x) {
    assert_class(x, "tinytable")
    if (isTRUE(x@output == "html")) {
        out <- x@table_string
        out <- sub("\\begin{tblr}", "\\resizebox{.8\\linewidth}{!}{\\begin{tblr}", out, fixed = TRUE)
        out <- sub("\\end{tblr}", "\\end{tblr}}", out, fixed = TRUE)
        x@table_string <- out
    }
    return(x)
}


customize_tt_build <- function() {
    out <- list(
        "resize" = customize_resize,
        "scroll" = customize_scroll
    )
    return(out)
}


#' export
customize_tt <- customize_tt_build()


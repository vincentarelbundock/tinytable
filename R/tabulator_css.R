# =============================================================================
# TABULATOR CSS FINALIZATION
# =============================================================================

#' Apply custom CSS rules
#' @param x tinytable object
#' @return Modified tinytable object
#' @keywords internal
#' @noRd
tabulator_apply_css <- function(x) {
    if (nchar(x@tabulator_css_rule) > 0) {
        # Replace $TINYTABLE_ID with actual table ID
        table_id <- paste0("tinytable_", x@id)
        custom_css <- gsub(
            "\\$TINYTABLE_ID",
            paste0("#", table_id),
            x@tabulator_css_rule
        )
        css_block <- sprintf("<style>\n%s\n</style>", custom_css)

        # Replace CSS placeholder
        x@table_string <- gsub(
            "$tinytable_TABULATOR_CSS",
            css_block,
            x@table_string,
            fixed = TRUE
        )
    } else {
        # Clean up CSS placeholder if no custom CSS
        x@table_string <- gsub(
            "$tinytable_TABULATOR_CSS",
            "",
            x@table_string,
            fixed = TRUE
        )
    }

    return(x)
}


#' Apply post-initialization JavaScript
#' @param x tinytable object
#' @return Modified tinytable object
#' @keywords internal
#' @noRd
tabulator_apply_post_init <- function(x) {
    if (nchar(x@tabulator_post_init) > 0) {
        # Replace POST_INIT placeholder
        x@table_string <- gsub(
            "$tinytable_TABULATOR_POST_INIT",
            x@tabulator_post_init,
            x@table_string,
            fixed = TRUE
        )
    } else {
        # Clean up POST_INIT placeholder if no post-init JS
        x@table_string <- gsub(
            "$tinytable_TABULATOR_POST_INIT",
            "",
            x@table_string,
            fixed = TRUE
        )
    }

    return(x)
}

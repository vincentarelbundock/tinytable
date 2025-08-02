# =============================================================================
# TABULATOR OPTIONS FINALIZATION
# =============================================================================

#' Apply tabulator options
#' @param x tinytable object
#' @return Modified tinytable object
#' @keywords internal
#' @noRd
tabulator_apply_options <- function(x) {
    if (nchar(x@tabulator_options) > 0) {
        options_string <- if (
            is.null(x@tabulator_options) || !is.character(x@tabulator_options)
        ) {
            ""
        } else {
            paste0(
                x@tabulator_options,
                ifelse(nchar(x@tabulator_options) > 0, ",", "")
            )
        }
        x@table_string <- gsub(
            "$tinytable_TABULATOR_OPTIONS",
            options_string,
            x@table_string,
            fixed = TRUE
        )
    } else {
        # Clean up placeholder if no options
        x@table_string <- gsub(
            "$tinytable_TABULATOR_OPTIONS",
            "",
            x@table_string,
            fixed = TRUE
        )
    }

    return(x)
}

# =============================================================================
# TABULATOR OPTIONS FINALIZATION
# =============================================================================

#' Apply tabulator options
#' @param x tinytable object
#' @return Modified tinytable object
#' @keywords internal
#' @noRd
tabulator_apply_options <- function(x) {
    options_string <- if (nchar(x@tabulator_options) > 0) {
        paste0(x@tabulator_options, ",")
    } else {
        ""
    }
    x@table_string <- gsub(
        "$tinytable_TABULATOR_OPTIONS",
        options_string,
        x@table_string,
        fixed = TRUE
    )
    return(x)
}

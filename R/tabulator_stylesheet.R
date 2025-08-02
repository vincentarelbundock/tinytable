# =============================================================================
# TABULATOR STYLESHEET HANDLING
# =============================================================================

# Tabulator theme mapping
TABULATOR_THEMES <- list(
    default = "tabulator.min.css",
    simple = "tabulator_simple.min.css",
    midnight = "tabulator_midnight.min.css",
    modern = "tabulator_modern.min.css",
    site = "tabulator_site.min.css",
    site_dark = "tabulator_site_dark.min.css",
    bootstrap3 = "tabulator_bootstrap.min.css",
    bootstrap4 = "tabulator_bootstrap4.min.css",
    bootstrap5 = "tabulator_bootstrap5.min.css",
    semanticui = "tabulator_semanticui.min.css",
    bulma = "tabulator_bulma.min.css",
    materialize = "tabulator_materialize.min.css"
)

# Tabulator CDN base URL
TABULATOR_CDN_BASE <- "https://cdn.jsdelivr.net/npm/tabulator-tables@6.3/dist/css/"

# Default tabulator theme
TABULATOR_DEFAULT_THEME <- "bootstrap5"

#' Helper function to handle stylesheet theme selection
#' @param x tinytable object
#' @param stylesheet Theme name or custom URL
#' @return Modified tinytable object
#' @keywords internal
#' @noRd
tabulator_stylesheet <- function(x, stylesheet) {
    # Check if it's a custom URL
    if (startsWith(stylesheet, "http")) {
        css_link <- sprintf('<link href="%s" rel="stylesheet">', stylesheet)
    } else {
        # Validate theme choice
        valid_themes <- names(TABULATOR_THEMES)

        if (!stylesheet %in% valid_themes) {
            warning(
                "Invalid theme '",
                stylesheet,
                "'. Valid themes are: ",
                paste(valid_themes, collapse = ", "),
                ". Or provide a custom URL starting with 'http'. Using default ",
                TABULATOR_DEFAULT_THEME
            )
            stylesheet <- TABULATOR_DEFAULT_THEME
        }

        css_file <- TABULATOR_THEMES[[stylesheet]]
        css_link <- sprintf(
            '<link href="%s%s" rel="stylesheet">',
            TABULATOR_CDN_BASE,
            css_file
        )
    }

    # Replace the CSS link in the table string
    x@table_string <- gsub(
        '<link href="https://cdn.jsdelivr.net/npm/tabulator-tables@6.3/dist/css/tabulator_bootstrap5.min.css" rel="stylesheet">',
        css_link,
        x@table_string,
        fixed = TRUE
    )

    return(x)
}

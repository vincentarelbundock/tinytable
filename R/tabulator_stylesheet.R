# =============================================================================
# TABULATOR STYLESHEET HANDLING
# =============================================================================

# CDN URLs and the default CSS block are defined once in R/tabulator_build.R

# Tabulator theme mapping
tabulator_themes <- list(
    tinytable = tinytable_tabulator_theme_cdn,
    tabulator = "tabulator.min.css",
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

# Default tabulator theme
tabulator_default_theme <- tabulator_themes[["tinytable"]]

#' Helper function to handle stylesheet theme selection
#' @param x tinytable object
#' @param stylesheet Theme name or custom URL
#' @return Modified tinytable object
#' @keywords internal
#' @noRd
tabulator_stylesheet <- function(x, stylesheet) {
    if (identical(stylesheet, "tinytable")) {
        stylesheet <- tabulator_themes[["tinytable"]]
    }

    # Check if it's a custom URL
    if (startsWith(stylesheet, "http")) {
        css_link <- sprintf(
            '<link href="%s" rel="stylesheet">\n    <link href="%s" rel="stylesheet">\n    <link href="%s" rel="stylesheet">',
            tabulator_css_cdn,
            stylesheet,
            fontawesome_css_cdn
        )
    } else {
        # Validate theme choice
        valid_themes <- names(tabulator_themes)
        valid_themes <- c("tinytable", sort(setdiff(valid_themes, "tinytable")))

        if (!stylesheet %in% valid_themes) {
            warning(
                "Invalid theme '",
                stylesheet,
                "'. Valid themes are: ",
                paste(valid_themes, collapse = ", "),
                ". Or provide a custom CDN URL starting with 'http'. Using the default `tinytable` theme.",
                call. = FALSE
            )
            css_link <- tabulator_default_css_block
        } else {
            css_file <- tabulator_themes[[stylesheet]]
            css_link <- sprintf(
                '<link href="%s" rel="stylesheet">\n    <link href="%s%s" rel="stylesheet">\n    <link href="%s" rel="stylesheet">',
                tabulator_css_cdn,
                tabulator_cdn_base,
                css_file,
                fontawesome_css_cdn
            )
        }
    }

    # Replace the CSS links in the table string (both base Tabulator CSS and tinytable theme)
    x@table_string <- gsub(
        tabulator_default_css_block,
        css_link,
        x@table_string,
        fixed = TRUE
    )

    return(x)
}

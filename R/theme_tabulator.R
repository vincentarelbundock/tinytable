# Helper function to handle CDN theme selection
tabulator_cdn_helper <- function(theme = "bootstrap5") {
  # Check if it's a custom CDN URL
  if (startsWith(theme, "http")) {
    return(sprintf('<link href="%s" rel="stylesheet">', theme))
  }

  # Map theme names to CSS filenames
  theme_map <- list(
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

  # Validate theme choice using theme_map names
  valid_themes <- names(theme_map)

  if (!theme %in% valid_themes) {
    warning(
      "Invalid theme '",
      theme,
      "'. Valid themes are: ",
      paste(valid_themes, collapse = ", "),
      ". Or provide a custom CDN URL starting with 'http'. Using default bootstrap5"
    )
    theme <- "bootstrap5"
  }

  css_file <- theme_map[[theme]]
  sprintf(
    '<link href="https://cdn.jsdelivr.net/npm/tabulator-tables@6.3/dist/css/%s" rel="stylesheet">',
    css_file
  )
}

# Helper function to convert options to JavaScript string
tabulator_options_helper <- function(options = NULL) {
  if (is.null(options)) {
    return("")
  }

  if (is.character(options)) {
    # If options is a string, use it directly
    return(paste0(options, ifelse(nchar(options) > 0, ",", "")))
  } else if (is.list(options)) {
    # If options is a list, convert to JavaScript object notation
    options_parts <- character(0)
    for (name in names(options)) {
      value <- options[[name]]
      if (is.character(value)) {
        options_parts <- c(options_parts, sprintf('%s: "%s"', name, value))
      } else if (is.logical(value)) {
        options_parts <- c(
          options_parts,
          sprintf("%s: %s", name, tolower(as.character(value)))
        )
      } else if (is.numeric(value)) {
        options_parts <- c(options_parts, sprintf("%s: %s", name, value))
      } else if (is.list(value)) {
        # For nested objects, convert to JSON
        options_parts <- c(
          options_parts,
          sprintf("%s: %s", name, jsonlite::toJSON(value, auto_unbox = TRUE))
        )
      }
    }
    options_string <- paste(options_parts, collapse = ",\n        ")
    return(paste0(options_string, ifelse(nchar(options_string) > 0, ",", "")))
  } else {
    warning("options must be a character string or named list")
    return("")
  }
}


theme_tabulator <- function(
    x,
    cdn = get_option("tinytable_theme_tabulator_cdn", default = "bootstrap5"),
    options = get_option(
      "tinytable_theme_tabulator_options",
      default = list(
        pagination = "local",
        paginationSize = 50,
        height = "500px",
        headerSort = TRUE,
        resizableColumns = TRUE,
        movableColumns = TRUE
      )
    ),
    format_column_name = get_option(
      "tinytable_theme_tabulator_format_column_name",
      default = NULL
    ),
    format_column_type = get_option(
      "tinytable_theme_tabulator_format_column_type",
      default = NULL
    ),
    ...) {
  # Only apply to tabulator output
  tabulator_theme_fn <- function(table) {
    if (!isTRUE(table@output == "tabulator")) {
      return(table)
    }

    # Replace CDN theme in template
    if (!is.null(cdn)) {
      css_link <- tabulator_cdn_helper(cdn)

      # Replace the CSS link in the table string
      table@table_string <- gsub(
        '<link href="https://cdn.jsdelivr.net/npm/tabulator-tables@6.3/dist/css/tabulator_bootstrap5.min.css" rel="stylesheet">',
        css_link,
        table@table_string,
        fixed = TRUE
      )
    }

    # Add custom options to tabulator configuration
    if (!is.null(options)) {
      options_string <- tabulator_options_helper(options)

      # Replace the placeholder in the template
      table@table_string <- gsub(
        "$tinytable_TABULATOR_OPTIONS",
        options_string,
        table@table_string,
        fixed = TRUE
      )
    }

    # Handle custom column formatters (only use what user specifies)
    if (!is.null(format_column_name) || !is.null(format_column_type)) {
      # Use only the formatters specified by the user - no defaults or merging
      column_type_formatters <- format_column_type
      column_name_formatters <- format_column_name

      # Regenerate columns JSON with custom formatters
      # Extract data from the existing JSON
      data_match <- regmatches(
        table@table_string,
        regexpr("data: \\[.*?\\]", table@table_string)
      )
      if (length(data_match) > 0) {
        # This is complex - for now we'll warn that this requires regenerating the entire table
        warning(
          "Custom column formatters require regenerating the table. Consider applying theme_tabulator() before other styling operations."
        )
      }
    }

    return(table)
  }

  # Apply the theme function using style_tt's finalize mechanism
  x <- style_tt(x, finalize = tabulator_theme_fn)

  return(x)
}

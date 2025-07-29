library(jsonlite)
library(checkmate)
library(Rdatasets)

# Function to clean problematic characters for Tabulator.js
# Mainly handles column name cleaning
tabulator_clean_rows <- function(df) {
    # Replace dots in names to avoid Tabulator field path issues
    names(df) <- gsub("\\.", "_", names(df))
    return(df)
}

# Function to clean data frame for Tabulator (both quotes and column names)
tabulator_clean_data <- function(df) {
    # Clean column names
    df_clean <- tabulator_clean_rows(df)

    # Clean problematic quotes
    df_clean <- as.data.frame(lapply(df_clean, function(x) {
        if (is.character(x)) {
            # Clean problematic double quotes that break JSON
            x <- gsub('"', "'", x) # Replace all double quotes with single quotes
            return(x)
        } else {
            return(x)
        }
    }))

    return(df_clean)
}

# Function to convert data frame to JSON rows with proper cleaning
tabulator_insert_rows <- function(df) {
    # Convert to JSON
    js_data <- toJSON(
        df,
        dataframe = "rows",
        auto_unbox = TRUE,
        pretty = FALSE,
        na = "null",
        force = TRUE
    )

    return(js_data)
}

tabulator_insert_options <- function(options = NULL) {
    default_options <- '      
        layout: "fitDataTable",
        pagination: "local",
        paginationSize: 25,
        paginationSizeSelector: [25, 100, 250],
        pagination: "local",
        paginationCounter: "rows",
        responsiveLayout: "collapse",
        height: "500px",
        clipboard: true,
        placeholder: "No Data Available",
        emptyCalback: function() {
            console.log("Table is empty");
        }
        
        '

    if (is.null(options)) {
        return(default_options)
    } else {
        # User provided a complete options string
        return(options)
    }
}

tabulator_cdn <- function(theme = "bootstrap5") {
    # Check if it's a custom CDN URL
    if (startsWith(theme, "http")) {
        return(sprintf('<link href="%s" rel="stylesheet">', theme))
    }

    # Validate theme choice
    valid_themes <- c(
        "default",
        "simple",
        "midnight",
        "modern",
        "site",
        "site_dark",
        "bootstrap3",
        "bootstrap4",
        "bootstrap5",
        "semantic_ui",
        "bulma",
        "materialize"
    )

    if (!theme %in% valid_themes) {
        stop(
            "Invalid theme '",
            theme,
            "'. Valid themes are: ",
            paste(valid_themes, collapse = ", "),
            ". Or provide a custom CDN URL starting with 'http'."
        )
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
        semantic_ui = "tabulator_semanticui.min.css",
        bulma = "tabulator_bulma.min.css",
        materialize = "tabulator_materialize.min.css"
    )

    css_file <- theme_map[[theme]]
    sprintf(
        '<link href="https://cdn.jsdelivr.net/npm/tabulator-tables@6.3/dist/css/%s" rel="stylesheet">',
        css_file
    )
}

# Default formatter dictionaries
default_formatter_column_type <- list(
    # Numeric types
    integer = list(
        formatter = "number",
        formatterParams = list(decimal = ".", thousand = ",", precision = 0)
    ),
    double = list(
        formatter = "number",
        formatterParams = list(decimal = ".", thousand = ",", precision = 2)
    ),
    numeric = list(
        formatter = "number",
        formatterParams = list(decimal = ".", thousand = ",", precision = 2)
    ),

    # Logical types
    logical = list(
        formatter = "tickCross"
    ),

    # Character types
    character = list(
        formatter = "plaintext"
    ),
    factor = list(
        formatter = "plaintext"
    ),

    # Date/time types
    Date = list(
        formatter = "date",
        formatterParams = list(
            outputFormat = "YYYY-MM-DD",
            invalidPlaceholder = ""
        )
    ),
    POSIXct = list(
        formatter = "date",
        formatterParams = list(
            outputFormat = "YYYY-MM-DD HH:mm:ss",
            invalidPlaceholder = ""
        )
    ),
    POSIXlt = list(
        formatter = "date",
        formatterParams = list(
            outputFormat = "YYYY-MM-DD HH:mm:ss",
            invalidPlaceholder = ""
        )
    )
)

default_formatter_column_name <- list(
    # Price columns get money formatting
    price = list(
        formatter = "money",
        formatterParams = list(
            decimal = ".",
            thousand = ",",
            precision = 2,
            symbol = "£"
        )
    ),
    url = list(
        formatter = "link"
    )
)


# Function to build column definitions using formatter dictionaries
tabulator_build_columns <- function(
    df,
    formatter_column_type = default_formatter_column_type,
    formatter_column_name = default_formatter_column_name
) {
    # Special column name patterns that override default formatters
    special_columns <- formatter_column_name

    # Use provided formatters or defaults
    formatter_dict <- formatter_column_type

    # Build column definitions
    cols <- lapply(names(df), function(nm) {
        col_data <- df[[nm]]
        col_type <- class(col_data)[1]
        col_name_lower <- tolower(nm)

        # Check for special column patterns first
        special_formatter <- NULL
        for (pattern in names(special_columns)) {
            if (grepl(pattern, col_name_lower)) {
                special_formatter <- special_columns[[pattern]]
                break
            }
        }

        # Use special formatter if found, otherwise use default for data type
        if (!is.null(special_formatter)) {
            formatter_config <- special_formatter
        } else if (col_type %in% names(formatter_dict)) {
            formatter_config <- formatter_dict[[col_type]]
        } else {
            # Fallback to plaintext for unknown types
            formatter_config <- formatter_dict[["character"]]
        }

        # Build column definition
        col_def <- list(
            title = tools::toTitleCase(gsub("_", " ", nm)),
            field = nm
        )

        # Add formatter and parameters
        col_def$formatter <- formatter_config$formatter
        if (!is.null(formatter_config$formatterParams)) {
            col_def$formatterParams <- formatter_config$formatterParams
        }

        return(col_def)
    })

    return(cols)
}

theme_tabulator <- function(
    df,
    template = "tabulator.html",
    out = "tabulator_filled.html",
    options = NULL,
    cdn = "bootstrap5",
    formatter_column_type = default_formatter_column_type,
    formatter_column_name = default_formatter_column_name
) {
    if (!file.exists(template)) {
        stop("Template not found: ", template)
    }

    # Clean data for double quotes in cells and dots in column names
    df_clean <- tabulator_clean_data(df)

    # Convert data frame to JSON with proper cleaning
    js_data <- tabulator_insert_rows(df_clean)

    # Build js columns array using the formatter dictionary
    cols <- tabulator_build_columns(
        df_clean,
        formatter_column_type = formatter_column_type,
        formatter_column_name = formatter_column_name
    )
    js_cols <- toJSON(cols, auto_unbox = TRUE, pretty = TRUE, na = "null")

    # Get options using the extracted function
    js_opts <- tabulator_insert_options(options)

    # Get theme CDN link
    cdn_link <- tabulator_cdn(cdn)

    # Read template
    lines <- readLines(template, warn = FALSE)

    # Replace placeholders
    html <- paste(lines, collapse = "\n")
    html <- gsub("\\$TABULATOR_DATA", js_data, html)
    html <- gsub("\\$TABULATOR_COLUMNS", js_cols, html)
    html <- gsub("\\$TABULATOR_CDN", cdn_link, html)
    html <- gsub("\\$TABULATOR_OPTIONS", js_opts, html)

    writeLines(html, out)
    invisible(out)
}

# # Test the NA handling
# theme_tabulator(test_df)

# Test the comprehensive data frame
source("example_data.R")
theme_tabulator(
    dat,
    formatter_column_type = default_formatter_column_type,
    formatter_column_name = default_formatter_column_name
)

# =============================================================================
# TABULATOR COLUMN CONSTANTS
# =============================================================================

# Data type constants
TABULATOR_NUMERIC_TYPES <- c("integer", "numeric", "double")
TABULATOR_DATE_TYPES <- c("Date", "POSIXct", "POSIXlt")
TABULATOR_FORMATTABLE_TYPES <- c(
    TABULATOR_NUMERIC_TYPES,
    "logical",
    TABULATOR_DATE_TYPES
)
TABULATOR_DATA_TYPE_FORMATTERS <- list(
    numeric = "money",
    integer = "number",
    double = "money",
    logical = "tickCross",
    Date = "datetime",
    POSIXct = "datetime",
    POSIXlt = "datetime"
)

# =============================================================================
# TABULATOR COLUMN FORMATTING
# =============================================================================

#' Format tabulator column based on data type
#'
#' @param x A tinytable object
#' @param j Column index or name to format
#' @param ... Formatting arguments
#' @return Column definition list or NULL if not formattable
#' @keywords internal
#' @noRd
format_tabulator_column <- function(x, j, ...) {
    # Sanitize column reference
    j_clean <- sanitize_j(j, x)
    if (length(j_clean) != 1) {
        stop("Column reference must resolve to a single column", call. = FALSE)
    }

    # Check that column names exist
    if (is.null(x@names) || length(x@names) == 0) {
        stop("Column names are required for tabulator tables", call. = FALSE)
    }

    # Get column info
    col_name <- x@names[j_clean]
    col_type <- class(x@data[[j_clean]])[1]

    # Only apply formatters to formattable columns
    if (!(col_type %in% TABULATOR_FORMATTABLE_TYPES)) {
        return(NULL)
    }

    # Create field name (clean for JavaScript)
    field_name <- tabulator_clean_column_name(col_name)

    # Build column definition based on type
    col_def <- tabulator_column_specification(
        col_name,
        field_name,
        col_type,
        ...
    )

    return(col_def)
}

#' Build column definition based on data type
#' @param col_name Original column name
#' @param field_name Cleaned field name
#' @param col_type Column data type
#' @param ... Formatting arguments
#' @return Column definition list
#' @keywords internal
#' @noRd
tabulator_column_specification <- function(
    col_name,
    field_name,
    col_type,
    ...
) {
    args <- list(...)

    # Basic column definition
    col_def <- list(
        title = col_name,
        field = field_name
    )

    # Add formatter based on type
    if (col_type %in% TABULATOR_NUMERIC_TYPES) {
        col_def <- tabulator_format_numeric(col_def, args)
    } else if (col_type == "logical") {
        col_def <- tabulator_format_boolean(col_def, args)
    } else if (col_type %in% TABULATOR_DATE_TYPES) {
        col_def <- tabulator_format_date(col_def, col_type, args)
    }

    return(col_def)
}

#' Add numeric formatter to column definition
#' @param col_def Column definition
#' @param args Formatting arguments
#' @return Updated column definition
#' @keywords internal
#' @noRd
tabulator_format_numeric <- function(col_def, args) {
    digits <- args$digits %||%
        get_option("tinytable_format_digits", default = NULL)
    num_fmt <- args$num_fmt %||%
        get_option("tinytable_format_num_fmt", default = "significant")
    num_mark_big <- args$num_mark_big %||%
        get_option("tinytable_format_num_mark_big", default = "")
    num_mark_dec <- args$num_mark_dec %||%
        get_option(
            "tinytable_format_num_mark_dec",
            default = getOption("OutDec", default = ".")
        )
    num_zero <- args$num_zero %||%
        get_option("tinytable_format_num_zero", default = FALSE)

    if (num_fmt == "money") {
        col_def$formatter <- "money"
        col_def$formatterParams <- list(
            decimal = num_mark_dec,
            thousand = num_mark_big,
            precision = digits %||% 2,
            symbol = "",
            symbolAfter = FALSE
        )
    } else {
        col_def$formatter <- "number"
        if (!is.null(digits)) {
            col_def$formatterParams <- list(precision = digits)
        }
    }

    return(col_def)
}

#' Add boolean formatter to column definition
#' @param col_def Column definition
#' @param args Formatting arguments
#' @return Updated column definition
#' @keywords internal
#' @noRd
tabulator_format_boolean <- function(col_def, args) {
    bool <- args$bool %||% get_option("tinytable_format_bool", default = NULL)

    if (!is.null(bool) && is.function(bool)) {
        col_def$formatter <- "plaintext"
    } else {
        col_def$formatter <- "tickCross"
    }

    return(col_def)
}

#' Add date formatter to column definition
#' @param col_def Column definition
#' @param col_type Column data type
#' @param args Formatting arguments
#' @return Updated column definition
#' @keywords internal
#' @noRd
tabulator_format_date <- function(col_def, col_type, args) {
    date <- args$date %||% get_option("tinytable_format_date", default = NULL)

    col_def$formatter <- "datetime"
    col_def$sorter <- "datetime"

    if (col_type == "Date") {
        input_format <- "yyyy-MM-dd"
    } else {
        input_format <- "yyyy-MM-dd HH:mm:ss"
    }

    output_format <- date %||%
        if (col_type == "Date") "M/d/yyyy" else "M/d/yyyy HH:mm:ss"

    col_def$formatterParams <- list(
        inputFormat = input_format,
        outputFormat = output_format,
        invalidPlaceholder = ""
    )

    col_def$sorterParams <- list(
        format = input_format,
        alignEmptyValues = "bottom"
    )

    return(col_def)
}

#' Helper function to get value with fallback
#' @param value Primary value
#' @param fallback Fallback value
#' @return Value or fallback
#' @keywords internal
#' @noRd
`%||%` <- function(value, fallback) {
    if (is.null(value)) fallback else value
}

# =============================================================================
# TABULATOR COLUMN APPLICATION
# =============================================================================

#' Apply column formatters from lazy_format operations
#' @param x tinytable object
#' @return Modified tinytable object
#' @keywords internal
#' @noRd
tabulator_apply_column_formatters <- function(x) {
    if (length(x@lazy_format) == 0) {
        return(x)
    }

    # Use columns list directly from S4 object (no JSON parsing needed)
    if (length(x@tabulator_columns) == 0) {
        return(x)
    }

    columns_list <- x@tabulator_columns

    # Create formatters for columns that had format_tt applied
    for (l in x@lazy_format) {
        if (!is.null(l$date_format)) {
            x <- tabulator_apply_date_formatting(x, l)
        }

        if (tabulator_has_numeric_formatting(l)) {
            x <- tabulator_apply_numeric_formatting(x, l)
        }
    }

    # Update columns with formatters
    x <- tabulator_update_columns_with_formatters(x)

    return(x)
}

#' Apply date formatting to columns
#' @param x tinytable object
#' @param l lazy_format operation
#' @return Modified tinytable object
#' @keywords internal
#' @noRd
tabulator_apply_date_formatting <- function(x, l) {
    if (is.null(l$j)) {
        # Apply to all date columns when j is NULL
        j_clean <- seq_along(x@data)
    } else {
        j_clean <- sanitize_j(l$j, x)
    }

    for (col_idx in j_clean) {
        col_name <- x@names[col_idx]
        col_data <- x@data[[col_idx]]

        if (inherits(col_data, c("Date", "POSIXct", "POSIXlt"))) {
            formatter_js <- format_tabulator_column(
                x,
                j = col_idx,
                date = l$date_format
            )
            if (!is.null(formatter_js)) {
                x@tabulator_column_formatters[[col_name]] <- formatter_js
            }
        }
    }

    return(x)
}

#' Check if lazy_format has numeric formatting
#' @param l lazy_format operation
#' @return TRUE if numeric formatting is present
#' @keywords internal
#' @noRd
tabulator_has_numeric_formatting <- function(l) {
    !is.null(l$digits) || !is.null(l$num_mark_big) || !is.null(l$num_suffix)
}

#' Apply numeric formatting to columns
#' @param x tinytable object
#' @param l lazy_format operation
#' @return Modified tinytable object
#' @keywords internal
#' @noRd
tabulator_apply_numeric_formatting <- function(x, l) {
    if (is.null(l$j)) {
        # Apply to all numeric columns when j is NULL
        j_clean <- seq_along(x@data)
    } else {
        j_clean <- sanitize_j(l$j, x)
    }

    for (col_idx in j_clean) {
        col_name <- x@names[col_idx]
        col_data <- x@data[[col_idx]]

        if (inherits(col_data, c("integer", "numeric", "double"))) {
            formatter_js <- format_tabulator_column(
                x,
                j = col_idx,
                digits = l$digits,
                num_fmt = l$num_fmt,
                num_zero = l$num_zero,
                num_suffix = l$num_suffix,
                num_mark_big = l$num_mark_big,
                num_mark_dec = l$num_mark_dec
            )
            if (!is.null(formatter_js)) {
                x@tabulator_column_formatters[[col_name]] <- formatter_js
            }
        }
    }

    return(x)
}

#' Update columns with formatters
#' @param x tinytable object
#' @return Modified tinytable object
#' @keywords internal
#' @noRd
tabulator_update_columns_with_formatters <- function(x) {
    columns_list <- x@tabulator_columns

    for (i in seq_along(columns_list)) {
        col_title <- columns_list[[i]][["title"]]
        if (col_title %in% names(x@tabulator_column_formatters)) {
            # Use the stored formatter list directly (no JSON parsing needed)
            formatter_obj <- x@tabulator_column_formatters[[col_title]]
            columns_list[[i]][["formatter"]] <- formatter_obj[["formatter"]]
            if (!is.null(formatter_obj[["formatterParams"]])) {
                columns_list[[i]][["formatterParams"]] <- formatter_obj[[
                    "formatterParams"
                ]]
            }
            if (!is.null(formatter_obj[["sorter"]])) {
                columns_list[[i]][["sorter"]] <- formatter_obj[["sorter"]]
            }
            if (!is.null(formatter_obj[["sorterParams"]])) {
                columns_list[[i]][["sorterParams"]] <- formatter_obj[[
                    "sorterParams"
                ]]
            }
        }
    }

    x@tabulator_columns <- columns_list
    return(x)
}

#' Apply column styles
#' @param x tinytable object
#' @return Modified tinytable object
#' @keywords internal
#' @noRd
tabulator_apply_column_styles <- function(x) {
    if (
        length(x@tabulator_column_styles) == 0 ||
            length(x@tabulator_columns) == 0
    ) {
        return(x)
    }

    columns_list <- x@tabulator_columns

    # Update columns with styles (alignment)
    for (i in seq_along(columns_list)) {
        col_title <- columns_list[[i]][["title"]]
        if (col_title %in% names(x@tabulator_column_styles)) {
            # Apply stored column styles
            style_obj <- x@tabulator_column_styles[[col_title]]
            if (!is.null(style_obj$hozAlign)) {
                columns_list[[i]][["hozAlign"]] <- style_obj$hozAlign
            }
            if (!is.null(style_obj$vertAlign)) {
                columns_list[[i]][["vertAlign"]] <- style_obj$vertAlign
            }
        }
    }

    x@tabulator_columns <- columns_list
    return(x)
}

# =============================================================================
# TABULATOR COLUMN CONVERSION
# =============================================================================

#' Convert columns to JSON and replace in template
#' @param x tinytable object
#' @return Modified tinytable object
#' @keywords internal
#' @noRd
tabulator_convert_columns_to_json <- function(x) {
    if (length(x@tabulator_columns) == 0) {
        return(x)
    }

    # Convert to JSON and replace in template
    new_columns_json <- df_to_json(x@tabulator_columns, auto_unbox = TRUE)

    # Replace both patterns - placeholder and existing columns array
    x@table_string <- gsub(
        "\\$tinytable_TABULATOR_COLUMNS",
        new_columns_json,
        x@table_string,
        fixed = TRUE
    )
    x@table_string <- gsub(
        "columns: \\[.*?\\]",
        paste0("columns: ", new_columns_json),
        x@table_string
    )

    return(x)
}

#' Convert columns to string format
#' @param columns Columns object
#' @return JSON string
#' @keywords internal
#' @noRd
tabulator_convert_columns_to_string <- function(columns) {
    if (is.list(columns) && !is.null(columns$json_string)) {
        # This is a JSON string wrapped in a list (from theme function)
        columns$json_string
    } else if (is.list(columns)) {
        # This is a proper R list of column definitions
        df_to_json(columns, auto_unbox = TRUE)
    } else {
        # Fallback for direct character assignment (shouldn't happen now)
        columns
    }
}

# =============================================================================
# TABULATOR COLUMN HANDLING
# =============================================================================

#' Handle custom columns replacement
#' @param x tinytable object
#' @return Modified tinytable object
#' @keywords internal
#' @noRd
tabulator_handle_custom_columns <- function(x) {
    if (length(x@tabulator_columns) == 0) {
        return(x)
    }

    # Handle different column formats for backward compatibility
    columns_json <- tabulator_convert_columns_to_string(x@tabulator_columns)

    # Replace the existing columns array with custom columns
    x@table_string <- gsub(
        "columns: \\[.*?\\],",
        paste0("columns: ", columns_json, ","),
        x@table_string
    )

    # Automatically disable search when custom columns are provided
    x@tabulator_search <- FALSE

    return(x)
}

#' Finalize columns placeholder cleanup
#' @param x tinytable object
#' @return Modified tinytable object
#' @keywords internal
#' @noRd
tabulator_finalize_columns_placeholder <- function(x) {
    # Replace the columns placeholder only if it hasn't been replaced yet
    # This ensures replacement happens for cases without formatting/styling
    if (
        length(x@tabulator_columns) > 0 &&
            grepl("$tinytable_TABULATOR_COLUMNS", x@table_string, fixed = TRUE)
    ) {
        columns_json <- tabulator_convert_columns_to_string(x@tabulator_columns)
        x@table_string <- gsub(
            "$tinytable_TABULATOR_COLUMNS",
            columns_json,
            x@table_string,
            fixed = TRUE
        )
    }

    return(x)
}

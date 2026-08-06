#' Clean column names for JavaScript field identifiers
#'
#' Sanitizes every character outside `[A-Za-z0-9_]` to `_` and de-duplicates
#' the results with `make.unique()`. Always call this on the full vector of
#' column names so that de-duplication is consistent across call sites.
#'
#' @param name Character vector of column names to clean
#' @return Cleaned column names
#' @keywords internal
#' @noRd
tabulator_clean_column_name <- function(name) {
    make.unique(gsub("[^A-Za-z0-9_]", "_", name), sep = "_")
}


#' Clean data for Tabulator
#' @param x tinytable object
#' @return Cleaned data frame
#' @keywords internal
#' @noRd
tabulator_clean_data <- function(x) {
    # Process data based on column types
    data_clean <- list()

    for (i in seq_along(x@data)) {
        col_name <- names(x@data)[i]
        original_col <- x@data[[i]]
        col_type <- class(original_col)[1]

        if (col_type %in% TAB_NUM) {
            # Use raw data for numeric columns (Tabulator formatters will handle display)
            data_clean[[col_name]] <- original_col
        } else if (col_type == "Date") {
            # Convert Date to ISO string for Tabulator datetime parsing
            data_clean[[col_name]] <- format(original_col, "%Y-%m-%d")
        } else if (col_type %in% c("POSIXct", "POSIXlt")) {
            # Convert POSIXct/POSIXlt to ISO string for Tabulator datetime parsing
            data_clean[[col_name]] <- format(original_col, "%Y-%m-%dT%H:%M:%S")
        } else if (col_type == "logical") {
            # For logical columns, use formatted data if bool formatting is applied
            if (isTRUE(x@tabulator_format_bool)) {
                if (nrow(x@data_body) == nrow(x@data)) {
                    data_clean[[col_name]] <- as.character(x@data_body[[i]])
                } else {
                    # Handle case where rows were modified (groups, etc.)
                    data_clean[[col_name]] <- as.character(original_col)
                }
            } else {
                # Use raw logical data for Tabulator's default boolean handling
                data_clean[[col_name]] <- original_col
            }
        } else {
            # Use formatted data for other column types (character, factor, etc.)
            if (nrow(x@data_body) == nrow(x@data)) {
                data_clean[[col_name]] <- as.character(x@data_body[[i]])
            } else {
                # Handle case where rows were modified (groups, etc.)
                # For now, fall back to original data as character
                data_clean[[col_name]] <- as.character(original_col)
            }
        }
    }

    # Add any additional columns from data_body that aren't in data (e.g., rank columns)
    if (nrow(x@data_body) == nrow(x@data)) {
        body_cols <- names(x@data_body)
        data_cols <- names(x@data)
        extra_cols <- setdiff(body_cols, data_cols)
        for (col_name in extra_cols) {
            data_clean[[col_name]] <- x@data_body[[col_name]]
        }
    }

    # Convert to data frame
    data_clean <- as.data.frame(
        data_clean,
        stringsAsFactors = FALSE,
        check.names = FALSE
    )

    # Add stable row index for styling (1-based, matching R's row numbering)
    data_clean$`_tinytable_row_index` <- seq_len(nrow(data_clean))

    # Mark plot_tt() cells holding pre-serialized JSON arrays (sparkline,
    # histogram) as raw JSON so df_to_json() emits them unquoted
    raw_formatters <- c("tinytable_sparkline", "tinytable_histogram")
    for (col_name in names(x@tabulator_column_formatters)) {
        fmt <- x@tabulator_column_formatters[[col_name]]
        if (
            !is.null(fmt$formatter) &&
                fmt$formatter %in% raw_formatters &&
                col_name %in% names(data_clean)
        ) {
            data_clean[[col_name]] <- I(lapply(
                data_clean[[col_name]],
                function(v) {
                    if (is.character(v) && grepl("^\\[.*\\]$", v)) {
                        json_raw(v)
                    } else {
                        v
                    }
                }
            ))
        }
    }

    # Clean column names (sanitize to [A-Za-z0-9_] and de-duplicate)
    names(data_clean) <- tabulator_clean_column_name(names(data_clean))

    return(data_clean)
}

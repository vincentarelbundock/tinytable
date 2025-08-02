#' Clean column name for JavaScript
#' @param name Column name to clean
#' @return Cleaned column name
#' @keywords internal
#' @noRd
tabulator_clean_column_name <- function(name) {
    gsub("[\\. ]", "_", name)
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

        if (col_type %in% c(TABULATOR_NUMERIC_TYPES, TABULATOR_DATE_TYPES)) {
            # Use raw data for numeric and date columns (formatters will handle display)
            data_clean[[col_name]] <- original_col
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

    # Convert to data frame
    data_clean <- as.data.frame(data_clean, stringsAsFactors = FALSE)

    # Clean column names (replace dots and spaces with underscores)
    names(data_clean) <- tabulator_clean_column_name(names(data_clean))

    # Clean problematic quotes in character columns only
    for (i in seq_along(data_clean)) {
        if (is.character(data_clean[[i]])) {
            data_clean[[i]] <- gsub('"', "'", data_clean[[i]])
        }
    }

    # Convert dates to ISO strings for Tabulator datetime parsing
    for (i in seq_along(data_clean)) {
        col_data <- data_clean[[i]]
        if (inherits(col_data, "Date")) {
            data_clean[[i]] <- format(col_data, "%Y-%m-%d")
        } else if (inherits(col_data, c("POSIXct", "POSIXlt"))) {
            data_clean[[i]] <- format(col_data, "%Y-%m-%dT%H:%M:%S")
        }
    }

    return(data_clean)
}

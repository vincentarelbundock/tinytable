#' Clean column name for JavaScript
#' @param name Column name to clean
#' @return Cleaned column name
#' @keywords internal
#' @noRd
tabulator_clean_column_name <- function(name) {
    gsub("[\\. -]", "_", name)
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
    data_clean <- as.data.frame(data_clean, stringsAsFactors = FALSE)

    # Add stable row index for styling (1-based, matching R's row numbering)
    data_clean$`_tinytable_row_index` <- seq_len(nrow(data_clean))

    # Clean column names (replace dots and spaces with underscores)
    names(data_clean) <- tabulator_clean_column_name(names(data_clean))

    # Clean problematic quotes in character columns only
    for (i in seq_along(data_clean)) {
        if (is.character(data_clean[[i]])) {
            data_clean[[i]] <- gsub('"', "'", data_clean[[i]])
        }
    }

    return(data_clean)
}

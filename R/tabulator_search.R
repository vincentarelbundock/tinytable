# =============================================================================
# TABULATOR SEARCH FUNCTIONALITY
# =============================================================================

clean_search_placeholders <- function(x, top = "", bottom = "", listener = "") {
  x@table_string <- sub("$tinytable_TABULATOR_SEARCH_TOP", top, x@table_string, fixed = TRUE)
  x@table_string <- sub("$tinytable_TABULATOR_SEARCH_BOTTOM", bottom, x@table_string, fixed = TRUE)
  x@table_string <- sub("$tinytable_TABULATOR_SEARCH_LISTENER", listener, x@table_string, fixed = TRUE)
  x
}

#' Create search listener JavaScript
#' @param search_id Search ID
#' @param columns_json Columns JSON string
#' @return Search listener JavaScript
#' @keywords internal
#' @noRd
tabulator_search_listener <- function(search_id, columns_json) {
  # Simple string replacement approach
  js <- '
        // Expose table reference globally for search functionality
        window.table_tinytable_SEARCH_ID = table_tinytable_SEARCH_ID;

        // Search functionality (runs within the table IIFE scope)
        const columns_SEARCH_ID = COLUMNS_JSON;
        const searchFields_SEARCH_ID = columns_SEARCH_ID.map(col => col.field);

        // Attach search listener using the globally exposed table reference
        const searchElement = document.getElementById("search_SEARCH_ID");
        if (searchElement) {
          searchElement.addEventListener("input", function () {
            const term = this.value.trim();
            if (!term) {
              window.table_tinytable_SEARCH_ID.clearFilter();
            } else {
              window.table_tinytable_SEARCH_ID.setFilter(function(data) {
                return searchFields_SEARCH_ID.some(field => {
                  const value = data[field];
                  if (value === null || value === undefined) return false;
                  return String(value).toLowerCase().includes(term.toLowerCase());
                });
              });
            }
          });
        }
    '

  # Replace placeholders
  js <- gsub("SEARCH_ID", search_id, js, fixed = TRUE)
  js <- gsub("COLUMNS_JSON", columns_json, js, fixed = TRUE)

  return(js)
}

#' Apply column header filters
#' @param x tinytable object
#' @return Modified tinytable object
#' @keywords internal
#' @noRd
tabulator_apply_column_search <- function(x) {
  x <- clean_search_placeholders(x)

  # Add headerFilter to each column definition based on data type
  if (length(x@tabulator_columns) > 0) {
    for (i in seq_along(x@tabulator_columns)) {
      col_title <- x@tabulator_columns[[i]][["title"]]

      # Find the column in the original data to check its type
      col_idx <- which(x@names == col_title)

      if (length(col_idx) == 1 && col_idx <= ncol(x@data)) {
        col_data <- x@data[[col_idx]]

        # Determine appropriate filter type based on column data type
        if (is.numeric(col_data)) {
          # Use number filter for numeric columns with min/max operators
          x@tabulator_columns[[i]][["headerFilter"]] <- "number"
          x@tabulator_columns[[i]][["headerFilterPlaceholder"]] <- ""
          x@tabulator_columns[[i]][["headerFilterFunc"]] <- ">="
        } else if (inherits(col_data, c("Date", "POSIXct", "POSIXlt"))) {
          # Use input filter for dates (could be enhanced with date picker)
          x@tabulator_columns[[i]][["headerFilter"]] <- "input"
          x@tabulator_columns[[i]][["headerFilterPlaceholder"]] <- ""
        } else if (is.logical(col_data)) {
          # Use select filter for logical columns
          x@tabulator_columns[[i]][["headerFilter"]] <- "tickCross"
          x@tabulator_columns[[i]][["headerFilterParams"]] <- c(
            tabulator_tickcross_params(),
            list(tristate = TRUE)
          )
          x@tabulator_columns[[i]][["headerFilterPlaceholder"]] <- "All"
        } else {
          # Use input filter for text columns
          x@tabulator_columns[[i]][["headerFilter"]] <- "input"
          x@tabulator_columns[[i]][["headerFilterPlaceholder"]] <- ""
        }
      } else {
        # Default to input filter if we can't determine the type
        x@tabulator_columns[[i]][["headerFilter"]] <- "input"
        x@tabulator_columns[[i]][["headerFilterPlaceholder"]] <- ""
      }
    }
  }

  # Re-serialize columns to JSON after adding search filters
  columns_json <- df_to_json(x@tabulator_columns)
  x@table_string <- gsub("$tinytable_TABULATOR_COLUMNS", columns_json, x@table_string, fixed = TRUE)
  x@table_string <- gsub("columns: \\[.*?\\]", paste0("columns: ", columns_json), x@table_string)

  return(x)
}

#' Apply search functionality to table
#' @param x tinytable object
#' @return Modified tinytable object
#' @keywords internal
#' @noRd
tabulator_apply_search <- function(x) {
  if (is.null(x@tabulator_search)) {
    x <- clean_search_placeholders(x)
    return(x)
  }

  # Handle column header filters
  if (x@tabulator_search == "column") {
    x <- tabulator_apply_column_search(x)
    return(x)
  }

  # Create unique search ID
  search_id <- gsub(".*tinytable_", "", x@id)

  # Get columns JSON directly from the S4 object
  if (length(x@tabulator_columns) == 0) {
    return(x)
  }

  columns_json <- df_to_json(x@tabulator_columns)

  # Determine search position
  search_position <- x@tabulator_search

  # Create search bar HTML
  search_bar_template <- '
        <div class="mb-3"><input type="text" id="search_%s" class="form-control" placeholder="Search table..." style="margin-bottom: 10px;"></div>
    '
  search_bar_html <- sprintf(search_bar_template, search_id)

  # Create search listener JS
  search_listener_js <- tabulator_search_listener(search_id, columns_json)

  # Replace search bar placeholder based on position
  if (search_position == "top") {
    x <- clean_search_placeholders(x, top = search_bar_html, listener = search_listener_js)
  } else if (search_position == "bottom") {
    x <- clean_search_placeholders(x, bottom = search_bar_html, listener = search_listener_js)
  }

  return(x)
}

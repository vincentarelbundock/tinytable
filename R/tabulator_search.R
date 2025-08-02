# =============================================================================
# TABULATOR SEARCH FUNCTIONALITY
# =============================================================================

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

#' Apply search functionality to table
#' @param x tinytable object
#' @return Modified tinytable object
#' @keywords internal
#' @noRd
tabulator_apply_search <- function(x) {
    if (!isTRUE(x@tabulator_search)) {
        # Clean up placeholders if search is disabled
        x@table_string <- sub(
            "$tinytable_TABULATOR_SEARCH",
            "",
            x@table_string,
            fixed = TRUE
        )

        x@table_string <- sub(
            "$tinytable_TABULATOR_SEARCH_LISTENER",
            "",
            x@table_string,
            fixed = TRUE
        )
        return(x)
    }

    # Create unique search ID
    search_id <- gsub(".*tinytable_", "", x@id)

    # Extract columns JSON from the table string
    columns_match <- regmatches(
        x@table_string,
        regexpr("columns: \\[.*?\\]", x@table_string)
    )

    columns_json <- if (length(columns_match) > 0) {
        # Extract just the JSON array part
        gsub("columns: (\\[.*?\\])", "\\1", columns_match)
    } else {
        NULL
    }

    if (is.null(columns_json)) {
        return(x)
    }

    # Create search bar HTML
    search_bar_template <- '
        <div class="mb-3"><input type="text" id="search_%s" class="form-control" placeholder="Search table..." style="margin-bottom: 10px;"></div>
    '
    search_bar_html <- sprintf(search_bar_template, search_id)

    # Create search listener JS
    search_listener_js <- tabulator_search_listener(search_id, columns_json)

    # Replace search bar placeholder (before table)
    x@table_string <- sub(
        "$tinytable_TABULATOR_SEARCH",
        search_bar_html,
        x@table_string,
        fixed = TRUE
    )

    # Replace search listener placeholder (after table)
    x@table_string <- sub(
        "$tinytable_TABULATOR_SEARCH_LISTENER",
        search_listener_js,
        x@table_string,
        fixed = TRUE
    )

    # Final cleanup of any remaining search placeholders (safety net)
    x@table_string <- gsub(
        "$tinytable_TABULATOR_SEARCH",
        "",
        x@table_string,
        fixed = TRUE
    )

    x@table_string <- gsub(
        "$tinytable_TABULATOR_SEARCH_LISTENER",
        "",
        x@table_string,
        fixed = TRUE
    )

    return(x)
}

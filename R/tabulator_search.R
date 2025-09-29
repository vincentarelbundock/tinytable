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

#' Apply column header filters
#' @param x tinytable object
#' @return Modified tinytable object
#' @keywords internal
#' @noRd
tabulator_apply_column_search <- function(x) {
  # Clean up search placeholders (not needed for column search)
  x@table_string <- sub(
    "$tinytable_TABULATOR_SEARCH_TOP",
    "",
    x@table_string,
    fixed = TRUE
  )

  x@table_string <- sub(
    "$tinytable_TABULATOR_SEARCH_BOTTOM",
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

  # Add headerFilter to each column definition
  if (length(x@tabulator_columns) > 0) {
    for (i in seq_along(x@tabulator_columns)) {
      # Add input header filter to each column
      x@tabulator_columns[[i]][["headerFilter"]] <- "input"
    }
  }

  return(x)
}

#' Apply search functionality to table
#' @param x tinytable object
#' @return Modified tinytable object
#' @keywords internal
#' @noRd
tabulator_apply_search <- function(x) {
  if (is.null(x@tabulator_search)) {
    # Clean up placeholders if search is disabled
    x@table_string <- sub(
      "$tinytable_TABULATOR_SEARCH_TOP",
      "",
      x@table_string,
      fixed = TRUE
    )

    x@table_string <- sub(
      "$tinytable_TABULATOR_SEARCH_BOTTOM",
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
    x@table_string <- sub(
      "$tinytable_TABULATOR_SEARCH_TOP",
      search_bar_html,
      x@table_string,
      fixed = TRUE
    )
    x@table_string <- sub(
      "$tinytable_TABULATOR_SEARCH_BOTTOM",
      "",
      x@table_string,
      fixed = TRUE
    )
  } else if (search_position == "bottom") {
    x@table_string <- sub(
      "$tinytable_TABULATOR_SEARCH_TOP",
      "",
      x@table_string,
      fixed = TRUE
    )
    x@table_string <- sub(
      "$tinytable_TABULATOR_SEARCH_BOTTOM",
      search_bar_html,
      x@table_string,
      fixed = TRUE
    )
  }

  # Replace search listener placeholder (after table)
  x@table_string <- sub(
    "$tinytable_TABULATOR_SEARCH_LISTENER",
    search_listener_js,
    x@table_string,
    fixed = TRUE
  )

  # Final cleanup of any remaining search placeholders (safety net)
  x@table_string <- gsub(
    "$tinytable_TABULATOR_SEARCH_TOP",
    "",
    x@table_string,
    fixed = TRUE
  )

  x@table_string <- gsub(
    "$tinytable_TABULATOR_SEARCH_BOTTOM",
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

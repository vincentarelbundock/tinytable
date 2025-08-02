tabulator_search_bar <- '
    <div class="mb-3"><input type="text" id="search_%s" class="form-control" placeholder="Search table..." style="margin-bottom: 10px;"></div>
'

tabulator_search_listener <- '
        // Expose table reference globally for search functionality
        window.table_tinytable_%s = table_tinytable_%s;

        // Search functionality (runs within the table IIFE scope)
        const columns_%s = %s;
        const searchFields_%s = columns_%s.map(col => col.field);

        // Attach search listener using the globally exposed table reference
        const searchElement = document.getElementById("search_%s");
        if (searchElement) {
          searchElement.addEventListener("input", function () {
            const term = this.value.trim();
            if (!term) {
              window.table_tinytable_%s.clearFilter();
            } else {
              window.table_tinytable_%s.setFilter(function(data) {
                return searchFields_%s.some(field => {
                  const value = data[field];
                  if (value === null || value === undefined) return false;
                  return String(value).toLowerCase().includes(term.toLowerCase());
                });
              });
            }
          });
        }
'


setMethod(
  f = "finalize",
  signature = "tinytable_tabulator",
  definition = function(x, ...) {

    # Replace stylesheet theme from S4 slot
    if (nchar(x@tabulator_stylesheet) > 0) {
      x <- tabulator_cdn_helper(x, x@tabulator_stylesheet)
    }

    # Apply column formatters if they exist or create them from formatting applied to data
    # Check if formatting was applied by looking at lazy_format operations or styles
    main_condition <- length(x@lazy_format) > 0 || length(x@tabulator_column_formatters) > 0 || length(x@tabulator_column_styles) > 0
    
    if (main_condition) {
      # Use columns list directly from S4 object (no JSON parsing needed)
      if (length(x@tabulator_columns) > 0) {
        columns_list <- x@tabulator_columns

        # Create formatters for columns that had format_tt applied
        for (l in x@lazy_format) {
          if (!is.null(l$date_format)) {
            # Date formatting was applied
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
          }

          if (
            (!is.null(l$digits) ||
              !is.null(l$num_mark_big) ||
              !is.null(l$num_suffix))
          ) {
            # Numeric formatting was applied
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
          }
        }

        # Update columns with formatters
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

        # Convert to JSON and replace in template
        new_columns_json <- df_to_json(
          columns_list,
          auto_unbox = TRUE
        )
        
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
      }
    }

    # Always replace the columns placeholder if columns exist (moved to end of function)
    # This ensures replacement happens regardless of formatting conditions

    # Replace tabulator options from S4 slot
    if (nchar(x@tabulator_options) > 0) {
      options_string <- tabulator_options_helper(x@tabulator_options)
      x@table_string <- gsub(
        "$tinytable_TABULATOR_OPTIONS",
        options_string,
        x@table_string,
        fixed = TRUE
      )
    } else {
      # Clean up placeholder if no options
      x@table_string <- gsub(
        "$tinytable_TABULATOR_OPTIONS",
        "",
        x@table_string,
        fixed = TRUE
      )
    }

    # Replace custom columns if provided
    if (length(x@tabulator_columns) > 0) {
      # Handle different column formats for backward compatibility
      if (is.list(x@tabulator_columns) && !is.null(x@tabulator_columns$json_string)) {
        # This is a JSON string wrapped in a list (from theme function)
        columns_json <- x@tabulator_columns$json_string
      } else if (is.list(x@tabulator_columns)) {
        # This is a proper R list of column definitions
        columns_json <- df_to_json(x@tabulator_columns, auto_unbox = TRUE)
      } else {
        # Fallback for direct character assignment (shouldn't happen now)
        columns_json <- x@tabulator_columns
      }
      
      # Replace the existing columns array with custom columns
      x@table_string <- gsub(
        "columns: \\[.*?\\],",
        paste0("columns: ", columns_json, ","),
        x@table_string
      )
      # Automatically disable search when custom columns are provided
      x@tabulator_search <- FALSE
    }

    # Handle search functionality
    if (isTRUE(x@tabulator_search)) {
      # Create unique search ID
      search_id <- gsub(".*tinytable_", "", x@id)

      # Extract columns JSON from the table string
      columns_match <- regmatches(
        x@table_string,
        regexpr("columns: \\[.*?\\]", x@table_string)
      )

      if (length(columns_match) > 0) {
        # Extract just the JSON array part
        columns_json <- gsub("columns: (\\[.*?\\])", "\\1", columns_match)

        # Table variable name from the generated HTML (matches template exactly)
        table_var <- paste0("table_tinytable_", search_id)

        # Create search bar HTML - uses the same unique search_id
        search_bar_html <- sprintf(
          tabulator_search_bar,
          search_id # search_%s (input ID)
        )

        # Create search listener JS - uses the same unique search_id
        search_listener_js <- sprintf(
          tabulator_search_listener,
          search_id, # window.table_tinytable_%s =
          search_id, # table_tinytable_%s;
          search_id, # columns_%s
          columns_json, # %s (columns JSON)
          search_id, # searchFields_%s
          search_id, # columns_%s in map()
          search_id, # search_%s (getElementById - same ID as input)
          search_id, # tinytable_%s for window.table_tinytable_xxx.clearFilter()
          search_id, # tinytable_%s for window.table_tinytable_xxx.setFilter()
          search_id # searchFields_%s in setFilter
        )

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
      }
    } else {
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
    }

    # Process custom CSS rule if provided
    if (nchar(x@tabulator_css_rule) > 0) {
      # Replace $TINYTABLE_ID with actual table ID
      table_id <- paste0("tinytable_", x@id)
      custom_css <- gsub("\\$TINYTABLE_ID", paste0("#", table_id), x@tabulator_css_rule)
      css_block <- sprintf("<style>\n%s\n</style>", custom_css)


      # Replace CSS placeholder
      x@table_string <- gsub(
        "$tinytable_TABULATOR_CSS",
        css_block,
        x@table_string,
        fixed = TRUE
      )
    } else {
      # Clean up CSS placeholder if no custom CSS
      x@table_string <- gsub(
        "$tinytable_TABULATOR_CSS",
        "",
        x@table_string,
        fixed = TRUE
      )
    }

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

    # Replace the columns placeholder only if it hasn't been replaced yet
    # This ensures replacement happens for cases without formatting/styling
    if (length(x@tabulator_columns) > 0 && grepl("$tinytable_TABULATOR_COLUMNS", x@table_string, fixed = TRUE)) {
      # Handle different column formats for backward compatibility
      if (is.list(x@tabulator_columns) && !is.null(x@tabulator_columns$json_string)) {
        # This is a JSON string wrapped in a list (from theme function)
        columns_json <- x@tabulator_columns$json_string
      } else if (is.list(x@tabulator_columns)) {
        # This is a proper R list of column definitions
        columns_json <- df_to_json(x@tabulator_columns, auto_unbox = TRUE)
      } else {
        # Fallback for direct character assignment (shouldn't happen now)
        columns_json <- x@tabulator_columns
      }
      x@table_string <- gsub(
        "$tinytable_TABULATOR_COLUMNS",
        columns_json,
        x@table_string,
        fixed = TRUE
      )
    }

    return(x)
  })

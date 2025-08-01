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
    assert_dependency("jsonlite")

    # Process any finalize functions that were stored in @lazy_finalize
    for (fn in x@lazy_finalize) {
      x <- fn(x)
    }

    # Replace stylesheet theme from S4 slot
    if (nchar(x@tabulator_stylesheet) > 0) {
      x <- tabulator_cdn_helper(x, x@tabulator_stylesheet)
    }

    # Apply column formatters if they exist or create them from formatting applied to data
    # Check if formatting was applied by looking at lazy_format operations or styles
    if (
      length(x@lazy_format) > 0 || 
      length(x@tabulator_column_formatters) > 0 ||
      length(x@tabulator_column_styles) > 0
    ) {
      # Parse existing columns and update with formatters
      current_columns <- regmatches(
        x@table_string,
        regexpr("columns: \\[.*?\\]", x@table_string)
      )

      if (length(current_columns) > 0) {
        # Extract JSON array content
        json_content <- gsub("columns: \\[(.*)\\]", "\\1", current_columns)

        # Parse existing columns
        columns_list <- jsonlite::fromJSON(
          paste0("[", json_content, "]"),
          simplifyDataFrame = FALSE
        )

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
            # Parse the stored formatter
            formatter_obj <- jsonlite::fromJSON(x@tabulator_column_formatters[[
              col_title
            ]])
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

        # Convert back to JSON and replace in template
        new_columns_json <- jsonlite::toJSON(
          columns_list,
          auto_unbox = TRUE,
          pretty = TRUE
        )
        x@table_string <- gsub(
          "columns: \\[.*?\\]",
          paste0("columns: ", new_columns_json),
          x@table_string
        )
      }
    }

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
    if (nchar(x@tabulator_columns) > 0) {
      # Replace the existing columns array with custom columns
      x@table_string <- gsub(
        "columns: \\[.*?\\],",
        paste0("columns: ", x@tabulator_columns, ","),
        x@table_string
      )
      # Automatically disable search when custom columns are provided
      x@tabulator_search <- FALSE
    }
    
    # Automatically disable search when custom options are provided
    if (nchar(x@tabulator_options) > 0) {
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
      css_block <- sprintf('<style>\n%s\n</style>', custom_css)
      
      
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

    return(x)
  })

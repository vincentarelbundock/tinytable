setMethod(
  f = "finalize",
  signature = "tinytable_tabulator",
  definition = function(x, ...) {
    assert_dependency("jsonlite")

    # Process any finalize functions that were stored in @lazy_finalize
    for (fn in x@lazy_finalize) {
      x <- fn(x)
    }

    # Replace CDN theme from S4 slot
    if (nchar(x@tabulator_cdn) > 0) {
      x <- tabulator_cdn_helper(x, x@tabulator_cdn)
    }

    # Apply column formatters if they exist or create them from formatting applied to data
    # Check if formatting was applied by looking at lazy_format operations
    if (
      length(x@lazy_format) > 0 || length(x@tabulator_column_formatters) > 0
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
          if (!is.null(l$j) && !is.null(l$date_format)) {
            # Date formatting was applied
            j_clean <- sanitize_j(l$j, x)
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
            !is.null(l$j) &&
              (!is.null(l$digits) ||
                !is.null(l$num_mark_big) ||
                !is.null(l$num_suffix))
          ) {
            # Numeric formatting was applied
            j_clean <- sanitize_j(l$j, x)
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

    return(x)
  }
)

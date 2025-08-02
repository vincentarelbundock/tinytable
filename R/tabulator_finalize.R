setMethod(
  f = "finalize",
  signature = "tinytable_tabulator",
  definition = function(x, ...) {
    # Apply stylesheet
    if (nchar(x@tabulator_stylesheet) > 0) {
      x <- tabulator_stylesheet(x, x@tabulator_stylesheet)
    }

    # Apply column formatting and styling
    x <- tabulator_apply_column_formatters(x)
    x <- tabulator_apply_column_styles(x)
    x <- tabulator_convert_columns_to_json(x)

    # Apply options
    x <- tabulator_apply_options(x)

    # Handle custom columns
    x <- tabulator_handle_custom_columns(x)

    # Apply search functionality
    x <- tabulator_apply_search(x)

    # Apply custom CSS
    x <- tabulator_apply_css(x)

    # Final cleanup
    x <- tabulator_finalize_columns_placeholder(x)

    return(x)
  }
)

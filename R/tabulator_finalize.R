setMethod(
  f = "finalize",
  signature = "tinytable_tabulator",
  definition = function(x, ...) {
    # Apply stylesheet
    if (nchar(x@tabulator_stylesheet) > 0) {
      x <- tabulator_stylesheet(x, x@tabulator_stylesheet)
    }

    # Check if custom columns are provided
    has_custom_columns <- is.list(x@tabulator_columns) && !is.null(x@tabulator_columns$json_string)
    
    if (!has_custom_columns) {
        # Process columns (formatting, styling, conversion) only for basic columns
        x <- tabulator_apply_columns(x)
    }

    # Apply options
    x <- tabulator_apply_options(x)

    # Apply search functionality
    x <- tabulator_apply_search(x)

    # Apply custom CSS
    x <- tabulator_apply_css(x)

    # Final cleanup - handle custom columns or basic columns
    x <- tabulator_finalize_columns_placeholder(x)

    return(x)
  }
)

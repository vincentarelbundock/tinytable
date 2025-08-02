setMethod(
  f = "finalize",
  signature = "tinytable_tabulator",
  definition = function(x, ...) {
    # Apply stylesheet
    if (nchar(x@tabulator_stylesheet) > 0) {
      x <- tabulator_stylesheet(x, x@tabulator_stylesheet)
    }

    # Process columns (formatting, styling, conversion)
    x <- tabulator_apply_columns(x)

    # Apply options
    x <- tabulator_apply_options(x)

    # Handle custom columns

    # Apply search functionality
    x <- tabulator_apply_search(x)

    # Apply custom CSS
    x <- tabulator_apply_css(x)

    # Final cleanup
    x <- tabulator_finalize_columns_placeholder(x)

    return(x)
  }
)

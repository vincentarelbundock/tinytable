setMethod(
  f = "finalize",
  signature = "tinytable_tabulator",
  definition = function(x, ...) {
    # Process any finalize functions that were stored in @lazy_finalize
    for (fn in x@lazy_finalize) {
      x <- fn(x)
    }
    
    # Replace CDN theme from S4 slot
    if (nchar(x@tabulator_cdn) > 0) {
      x <- tabulator_cdn_helper(x, x@tabulator_cdn)
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
setMethod(
  f = "finalize",
  signature = "tinytable_tabulator",
  definition = function(x, ...) {
    # Process any finalize functions that were stored in @lazy_finalize
    for (fn in x@lazy_finalize) {
      x <- fn(x)
    }
    
    # Clean up any remaining placeholders that weren't replaced by themes
    x@table_string <- gsub(
      "$tinytable_TABULATOR_OPTIONS",
      "",
      x@table_string,
      fixed = TRUE
    )
    
    return(x)
  }
)
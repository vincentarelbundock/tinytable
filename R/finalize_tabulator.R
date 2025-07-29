setMethod(
  f = "finalize",
  signature = "tinytable_tabulator",
  definition = function(x, ...) {
    # For now, just return the table_string as-is
    return(x)
  }
)
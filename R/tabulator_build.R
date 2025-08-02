setMethod(
  f = "build_eval",
  signature = "tinytable_tabulator",
  definition = function(x, ...) {
    # Check that column names exist
    if (is.null(x@names) || length(x@names) == 0) {
      stop(
        "Column names are required for tabulator tables. Use `colnames(x) <- ...` to set column names.",
        call. = FALSE
      )
    }

    # Check that no format_tt() calls use the i argument (row-based formatting)
    # Tabulator only supports column-based or full-table formatting
    for (l in x@lazy_format) {
      if (!is.null(l$i)) {
        stop(
          "Row-based formatting (i argument) is not supported with tabulator output. ",
          "Use column-based formatting (j argument) or full-table formatting instead.",
          call. = FALSE
        )
      }
    }

    template <- readLines(
      system.file("templates/tabulator.html", package = "tinytable")
    )

    # (pseudo-)unique table IDs
    id <- get_id("")
    x@id <- id

    # Replace table ID in template
    template <- gsub(
      "$tinytable_TABLE_ID",
      paste0("tinytable_", id),
      template,
      fixed = TRUE
    )

    # Convert data to JSON for Tabulator
    data_clean <- tabulator_clean_data(x)
    js_data <- df_to_json(data_clean)

    # Build basic column definitions
    columns <- lapply(seq_along(x@names), function(i) {
      list(
        title = x@names[i],
        field = tabulator_clean_column_name(x@names[i])
      )
    })

    # Store columns list for later processing
    x@tabulator_columns <- columns

    # Replace data in template
    template <- gsub(
      "$tinytable_TABULATOR_DATA",
      js_data,
      template,
      fixed = TRUE
    )

    # Leave columns placeholder for finalize_tabulator.R to handle
    # (Custom columns and formatting are handled there)

    # Add default CDN (bootstrap5)
    template <- gsub(
      "$tinytable_TABULATOR_CDN",
      '<link href="https://cdn.jsdelivr.net/npm/tabulator-tables@6.3/dist/css/tabulator_bootstrap5.min.css" rel="stylesheet">',
      template,
      fixed = TRUE
    )

    # Leave CSS placeholder for finalize to handle

    # Leave options placeholder for potential theme replacement
    # (it will be cleaned up in finalize if not replaced)

    out <- paste(template, collapse = "\n")

    # Store table string for potential styling later
    x@table_string <- out

    return(x)
  }
)

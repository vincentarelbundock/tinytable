# =============================================================================
# TABULATOR CDN CONSTANTS
#
# Single source of truth for every CDN URL and the default CSS <link> block
# used by the tabulator engine. `tabulator_stylesheet()` swaps stylesheets by
# exact-match replacement of `tabulator_default_css_block`, so these must not
# be redefined elsewhere: drift between copies would silently break that
# replacement. The Tabulator version is also referenced in
# inst/templates/tabulator.html.
# =============================================================================
tabulator_js_cdn <- "https://cdn.jsdelivr.net/npm/tabulator-tables@6.3/dist/js/tabulator.min.js"
tabulator_cdn_base <- "https://cdn.jsdelivr.net/npm/tabulator-tables@6.3/dist/css/"
tabulator_css_cdn <- paste0(tabulator_cdn_base, "tabulator.min.css")
tinytable_tabulator_theme_cdn <- "https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tabulator_tinytable.min.css"
tinytable_js_cdn <- "https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.js"
fontawesome_css_cdn <- "https://cdn.jsdelivr.net/npm/@fortawesome/fontawesome-free@6.5.2/css/all.min.css"

tabulator_default_css_block <- sprintf(
  '<link href="%s" rel="stylesheet">\n    <link href="%s" rel="stylesheet">\n    <link href="%s" rel="stylesheet">',
  tabulator_css_cdn,
  tinytable_tabulator_theme_cdn,
  fontawesome_css_cdn
)

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

    # Build basic column definitions only if not already set by theme
    if (length(x@tabulator_columns) == 0) {
      # Clean the full vector at once so de-duplication is consistent
      fields <- tabulator_clean_column_name(x@names)
      columns <- lapply(seq_along(x@names), function(i) {
        list(
          title = x@names[i],
          field = fields[i]
        )
      })

      # Store columns list for later processing
      x@tabulator_columns <- columns
    }

    # Replace data in template
    template <- gsub(
      "$tinytable_TABULATOR_DATA",
      js_data,
      template,
      fixed = TRUE
    )

    # Leave columns placeholder for finalize_tabulator.R to handle
    # (Custom columns and formatting are handled there)

    # Add default CDN (base Tabulator CSS + tinytable theme)
    template <- gsub(
      "$tinytable_TABULATOR_CDN",
      tabulator_default_css_block,
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
  })

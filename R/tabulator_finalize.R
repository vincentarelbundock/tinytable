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

    # Apply post-initialization JavaScript
    x <- tabulator_apply_post_init(x)

    # Clean up JS markers (formatters are now in inst/tinytable.js)
    x@tabulator_options <- gsub("\n// NEEDS_HISTOGRAM_JS", "", x@tabulator_options, fixed = TRUE)
    x@tabulator_options <- gsub("\n// NEEDS_SPARKLINE_JS", "", x@tabulator_options, fixed = TRUE)

    # Inject tinytable.js (inline if portable, external link otherwise)
    if (isTRUE(x@html_portable)) {
      # Inline the JS for portable HTML
      js_file <- system.file("tinytable.js", package = "tinytable")
      if (file.exists(js_file)) {
        js_content <- paste(readLines(js_file), collapse = "\n")
        js_tag <- sprintf("<script>\n%s\n</script>", js_content)
      } else {
        js_tag <- ""
      }
    } else {
      # External link (will work when package is installed)
      js_tag <- '<script src="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.js"></script>'
    }

    x@table_string <- gsub(
      "$tinytable_TINYTABLE_JS",
      js_tag,
      x@table_string,
      fixed = TRUE
    )

    return(x)
  })

tabulator_js_cdn <- "https://cdn.jsdelivr.net/npm/tabulator-tables@6.3/dist/js/tabulator.min.js"
tinytable_tabulator_theme_cdn <- "https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tabulator_tinytable.min.css"

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

    if (has_custom_columns) {
      # Use custom JSON directly
      x@table_string <- gsub("$tinytable_TABULATOR_COLUMNS", x@tabulator_columns$json_string, x@table_string, fixed = TRUE)
      x@table_string <- gsub("columns: \\[.*?\\]", paste0("columns: ", x@tabulator_columns$json_string), x@table_string)
    } else {
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
    if (nchar(x@tabulator_post_init) > 0) {
        x@table_string <- gsub(
            "$tinytable_TABULATOR_POST_INIT",
            x@tabulator_post_init,
            x@table_string,
            fixed = TRUE
        )
    } else {
        x@table_string <- gsub(
            "$tinytable_TABULATOR_POST_INIT",
            "",
            x@table_string,
            fixed = TRUE
        )
    }

    # Clean up JS markers (formatters are now in inst/tinytable.js)
    x@tabulator_options <- gsub("\n// NEEDS_HISTOGRAM_JS", "", x@tabulator_options, fixed = TRUE)
    x@tabulator_options <- gsub("\n// NEEDS_SPARKLINE_JS", "", x@tabulator_options, fixed = TRUE)

    tabulator_cdn_tag <- sprintf('<script src="%s"></script>', tabulator_js_cdn)

    # Inline tinytable Tabulator theme stylesheet when in portable mode
    if (isTRUE(x@html_portable)) {
      x@table_string <- tabulator_inline_theme_css(x@table_string)
    }

    # Inject tinytable.js (inline if portable, external link otherwise)
    if (isTRUE(x@html_portable)) {
      # Inline the JS for portable HTML
      js_file <- system.file("tinytable.js", package = "tinytable")
      if (file.exists(js_file)) {
        js_content <- paste(readLines(js_file, warn = FALSE), collapse = "\n")
        js_tag <- sprintf("<script>\n%s\n</script>", js_content)
      } else {
        js_tag <- ""
      }

      tabulator_inline <- tabulator_inline_tabulator_js()
      if (!is.null(tabulator_inline)) {
        inline_tag <- paste0("    <script>\n", tabulator_inline, "\n    </script>")
        x@table_string <- sub(
          paste0("    ", tabulator_cdn_tag),
          inline_tag,
          x@table_string,
          fixed = TRUE
        )
      } else {
        warning(
          "Failed to inline Tabulator JavaScript in portable mode; falling back to CDN reference.",
          call. = FALSE
        )
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

    # Handle custom script inclusion
    if (!is.null(x@html_script)) {
      x@table_string <- gsub(
        "$tinytable_SCRIPT",
        x@html_script,
        x@table_string,
        fixed = TRUE
      )
    } else {
      # Remove placeholder if no script is provided
      x@table_string <- lines_drop(
        x@table_string,
        "\\$tinytable_SCRIPT",
        fixed = FALSE,
        unique = FALSE
      )
    }

    return(x)
  })

tabulator_inline_tabulator_js <- function() {
  custom_path <- get_option("tinytable_tabulator_js_path", default = NULL)
  if (!is.null(custom_path) && file.exists(custom_path)) {
    return(paste(readLines(custom_path, warn = FALSE), collapse = "\n"))
  }

  package_path <- system.file("tabulator.min.js", package = "tinytable")
  if (nzchar(package_path) && file.exists(package_path)) {
    return(paste(readLines(package_path, warn = FALSE), collapse = "\n"))
  }

  inline_url <- get_option("tinytable_tabulator_js_url", default = tabulator_js_cdn)
  if (is.null(inline_url) || !nzchar(inline_url)) {
    return(NULL)
  }

  # Attempt to fetch from remote CDN as a last resort
  js_source <- tryCatch({
    con <- url(inline_url)
    on.exit(close(con), add = TRUE)
    suppressWarnings(paste(readLines(con, warn = FALSE), collapse = "\n"))
  }, error = function(e) NULL)

  return(js_source)
}

tabulator_inline_theme_css <- function(table_string) {
  css_path <- system.file("tabulator_tinytable.min.css", package = "tinytable")
  if (!file.exists(css_path)) {
    return(table_string)
  }

  css_content <- paste(readLines(css_path, warn = FALSE), collapse = "\n")
  pattern <- sprintf('([ \t]*)<link href="%s" rel="stylesheet">', tinytable_tabulator_theme_cdn)

  match <- regexpr(pattern, table_string, perl = TRUE)
  if (match[1] > 0) {
    match_len <- attr(match, "match.length")
    match_str <- substr(table_string, match[1], match[1] + match_len - 1)
    indent <- sub(pattern, "\\1", match_str, perl = TRUE)
    replacement <- paste0(indent, "<style>\n", css_content, "\n", indent, "</style>")
    table_string <- paste0(
      substr(table_string, 1, match[1] - 1),
      replacement,
      substr(table_string, match[1] + match_len, nchar(table_string))
    )
  }

  return(table_string)
}

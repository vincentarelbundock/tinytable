setMethod(
  f = "tt_eval",
  signature = "tinytable_tabulator",
  definition = function(x, ...) {
    assert_dependency("jsonlite")

    # Check that column names exist
    if (is.null(x@names) || length(x@names) == 0) {
      stop(
        "Column names are required for tabulator tables. Use `colnames(x) <- ...` to set column names.",
        call. = FALSE
      )
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
    # Use raw data for numeric/logical columns, formatted data for others
    data_clean <- list()

    for (i in seq_along(x@data)) {
      col_name <- names(x@data)[i]
      original_col <- x@data[[i]]
      col_type <- class(original_col)[1]

      if (
        col_type %in%
          c(
            "integer",
            "numeric",
            "double",
            "logical",
            "Date",
            "POSIXct",
            "POSIXlt"
          )
      ) {
        # Use raw data for numeric, logical, and date columns (formatters will handle display)
        data_clean[[col_name]] <- original_col
      } else {
        # Use formatted data for other column types (character, factor, etc.)
        if (nrow(x@data_body) == nrow(x@data)) {
          data_clean[[col_name]] <- as.character(x@data_body[[i]])
        } else {
          # Handle case where rows were modified (groups, etc.)
          # For now, fall back to original data as character
          data_clean[[col_name]] <- as.character(original_col)
        }
      }
    }

    # Convert to data frame
    data_clean <- as.data.frame(data_clean, stringsAsFactors = FALSE)

    # Clean column names (replace dots with underscores)
    names(data_clean) <- gsub("\\.", "_", names(data_clean))

    # Clean problematic quotes in character columns only
    for (i in seq_along(data_clean)) {
      if (is.character(data_clean[[i]])) {
        data_clean[[i]] <- gsub('"', "'", data_clean[[i]])
      }
    }

    # Convert dates to ISO strings for Tabulator datetime parsing
    for (i in seq_along(data_clean)) {
      col_data <- data_clean[[i]]
      if (inherits(col_data, "Date")) {
        # Convert Date to ISO string
        data_clean[[i]] <- format(col_data, "%Y-%m-%d")
      } else if (inherits(col_data, c("POSIXct", "POSIXlt"))) {
        # Convert datetime to ISO string with time
        data_clean[[i]] <- format(col_data, "%Y-%m-%dT%H:%M:%S")
      }
    }

    # Convert to JSON
    js_data <- jsonlite::toJSON(
      data_clean,
      dataframe = "rows",
      auto_unbox = TRUE,
      pretty = FALSE,
      na = "null"
    )

    # Build column definitions
    columns <- lapply(names(data_clean), function(nm) {
      original_name <- colnames(x@data_body)[which(
        gsub("\\.", "_", colnames(x@data_body)) == nm
      )]
      # Use original data for type detection since @data_body is processed to character
      col_data <- x@data[[original_name]]
      col_type <- class(col_data)[1]

      # Basic column definition
      col_def <- list(
        title = original_name,
        field = nm
      )

      # Add basic formatter based on type
      if (col_type %in% c("integer", "numeric", "double")) {
        col_def$formatter <- "money"
        if (col_type == "integer") {
          col_def$formatterParams <- list(
            decimal = ".",
            thousand = ",",
            precision = 0,
            symbol = "",
            symbolAfter = FALSE,
            negativeSign = TRUE
          )
        } else {
          col_def$formatterParams <- list(
            decimal = ".",
            thousand = ",",
            precision = 2,
            symbol = "",
            symbolAfter = FALSE,
            negativeSign = TRUE
          )
        }
      } else if (col_type == "logical") {
        col_def$formatter <- "tickCross"
      } else if (col_type == "Date") {
        col_def$formatter <- "datetime"
        col_def$sorter <- "datetime"
        col_def$formatterParams <- list(
          inputFormat = "yyyy-MM-dd",
          outputFormat = "yyyy-MM-dd",
          invalidPlaceholder = ""
        )
      } else if (col_type %in% c("POSIXct", "POSIXlt")) {
        col_def$formatter <- "datetime"
        col_def$sorter <- "datetime"
        col_def$formatterParams <- list(
          inputFormat = "yyyy-MM-ddTHH:mm:ss",
          outputFormat = "yyyy-MM-dd HH:mm:ss",
          invalidPlaceholder = ""
        )
      } else {
        # For other types (character, factor, etc.), use plaintext
        # since data comes pre-formatted from x@data_body
        col_def$formatter <- "plaintext"
      }

      return(col_def)
    })

    js_columns <- trimws(jsonlite::toJSON(columns, auto_unbox = TRUE, pretty = FALSE))

    # Replace data and columns in template
    template <- gsub(
      "$tinytable_TABULATOR_DATA",
      js_data,
      template,
      fixed = TRUE
    )

    template <- gsub(
      "$tinytable_TABULATOR_COLUMNS",
      js_columns,
      template,
      fixed = TRUE
    )

    # Add default CDN (bootstrap5)
    template <- gsub(
      "$tinytable_TABULATOR_CDN",
      '<link href="https://cdn.jsdelivr.net/npm/tabulator-tables@6.3/dist/css/tabulator_bootstrap5.min.css" rel="stylesheet">',
      template,
      fixed = TRUE
    )

    # Leave options placeholder for potential theme replacement
    # (it will be cleaned up in finalize if not replaced)

    out <- paste(template, collapse = "\n")

    # Store table string for potential styling later
    x@table_string <- out

    return(x)
  })

# Helper function to set up tabulator-specific S4 class
tabulator_setting <- function(x, new, component = "option") {
  att <- attributes(x)
  out <- strsplit(x, "\n")[[1]]

  if (component == "option") {
    # Find the options line and insert before it
    idx <- grep("$tinytable_TABULATOR_OPTIONS", out, fixed = TRUE)
    if (length(idx) > 0) {
      out <- c(
        out[1:(idx - 1)],
        new,
        out[idx:length(out)]
      )
    }
  }

  out <- paste(out, collapse = "\n")
  attributes(out) <- att
  class(out) <- class(x)
  return(out)
}

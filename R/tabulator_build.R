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
    # Use raw data for numeric/logical columns, formatted data for others
    # Exception: use formatted data for logical columns if bool argument is used
    data_clean <- list()

    # Check if any lazy_format calls use the bool argument
    has_bool_formatting <- function(col_idx) {
      for (l in x@lazy_format) {
        if (!is.null(l$bool)) {
          # Check if this applies to our column
          if (is.null(l$j)) {
            # Global formatting - applies to all columns
            return(TRUE)
          } else {
            # Column-specific formatting
            j_clean <- sanitize_j(l$j, x)
            if (col_idx %in% j_clean) {
              return(TRUE)
            }
          }
        }
      }
      return(FALSE)
    }

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
            "Date",
            "POSIXct",
            "POSIXlt"
          )
      ) {
        # Use raw data for numeric and date columns (formatters will handle display)
        data_clean[[col_name]] <- original_col
      } else if (col_type == "logical") {
        # For logical columns, use formatted data if bool argument is used
        if (has_bool_formatting(i)) {
          if (nrow(x@data_body) == nrow(x@data)) {
            data_clean[[col_name]] <- as.character(x@data_body[[i]])
          } else {
            # Handle case where rows were modified (groups, etc.)
            data_clean[[col_name]] <- as.character(original_col)
          }
        } else {
          # Use raw logical data for Tabulator's default boolean handling
          data_clean[[col_name]] <- original_col
        }
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

    # Clean column names (replace dots and spaces with underscores)
    names(data_clean) <- gsub("[\\. ]", "_", names(data_clean))

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
    js_data <- df_to_json(
      data_clean,
      dataframe = "rows"
    )

    # Build column definitions
    columns <- lapply(names(data_clean), function(nm) {
      original_name <- colnames(x@data_body)[which(
        gsub("[\\. ]", "_", colnames(x@data_body)) == nm
      )]
      # Use original data for type detection since @data_body is processed to character
      col_data <- x@data[[original_name]]
      col_type <- class(col_data)[1]

      # Basic column definition - no formatters unless explicitly applied via format_tt()
      col_def <- list(
        title = original_name,
        field = nm
      )

      return(col_def)
    })

    # Store columns list directly in S4 object for later processing
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

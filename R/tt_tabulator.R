setMethod(
  f = "tt_eval",
  signature = "tinytable_tabulator",
  definition = function(x, ...) {
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
    # Use original data to preserve types, but apply same row selection as data_body
    if (nrow(x@data_body) == nrow(x@data)) {
      # No row modifications, use original data
      data_clean <- x@data
    } else {
      # Row modifications exist, use data_body but try to preserve types
      data_clean <- x@data_body
      # Convert back to appropriate types where possible
      for (i in seq_along(data_clean)) {
        original_col <- x@data[[names(x@data)[i]]]
        if (is.logical(original_col)) {
          # Convert logical strings back to logical
          data_clean[[i]] <- as.logical(data_clean[[i]])
        } else if (is.numeric(original_col)) {
          # Keep numeric as numeric if not formatted
          suppressWarnings({
            numeric_version <- as.numeric(data_clean[[i]])
            if (!any(is.na(numeric_version) & !is.na(data_clean[[i]]))) {
              data_clean[[i]] <- numeric_version
            }
          })
        }
      }
    }
    
    # Clean column names (replace dots with underscores)
    names(data_clean) <- gsub("\\.", "_", names(data_clean))
    
    # Clean problematic quotes in character columns only
    for (i in seq_along(data_clean)) {
      if (is.character(data_clean[[i]])) {
        data_clean[[i]] <- gsub('"', "'", data_clean[[i]])
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
      original_name <- colnames(x@data_body)[which(gsub("\\.", "_", colnames(x@data_body)) == nm)]
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
        col_def$formatter <- "number"
        if (col_type == "integer") {
          col_def$formatterParams <- list(decimal = ".", thousand = ",", precision = 0)
        } else {
          col_def$formatterParams <- list(decimal = ".", thousand = ",", precision = 2)
        }
      } else if (col_type == "logical") {
        col_def$formatter <- "tickCross"
      } else if (col_type == "Date") {
        col_def$formatter <- "date"
        col_def$formatterParams <- list(outputFormat = "YYYY-MM-DD")
      } else if (col_type %in% c("POSIXct", "POSIXlt")) {
        col_def$formatter <- "date"
        col_def$formatterParams <- list(outputFormat = "YYYY-MM-DD HH:mm:ss")
      } else {
        col_def$formatter <- "plaintext"
      }
      
      return(col_def)
    })

    js_columns <- jsonlite::toJSON(columns, auto_unbox = TRUE, pretty = FALSE)

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